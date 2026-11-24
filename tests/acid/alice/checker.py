#!/usr/bin/env python3
"""ALICE checker — invoked once per reconstructed crash state:

    checker.py <reconstructed-dir> <stdout-at-crash-file>

Exit 0 declares the crashed state consistent; anything else is a crash
vulnerability that ALICE reports and preserves under traces_dir.

Two layers of checking, deliberately thin — the state enumeration and the
persistence model are ALICE's, the oracle only asserts what a TerminusDB
store must satisfy:

  1. Structural, on every state: every *.label parses as
     `version\n<40-hex-layer>`, every referenced .larch layer exists complete
     in its shard dir, no foreign files, `.tmp/` residue permitted.
  2. Server boot, on a sample: boot `terminusdb serve` on the reconstructed
     dir and require the server to come up and every ACKed commit to be
     retrievable. A committed write that a crash can lose is a durability
     violation; a store the server cannot open is an atomicity violation.

Environment:
  ACID_BOOT_MOD  run the boot check on roughly 1/N reconstructed states,
                 sampled deterministically by directory name (default 5)
  ACID_DB        database name used by the driver (default "acid")
"""
import base64
import os
import re
import subprocess
import sys
import time
import urllib.error
import urllib.request
import zlib

LABEL_RE = re.compile(rb"^(\d+)\n([0-9a-f]{40})?\n?$")
LAYER_FILE_RE = re.compile(r"^[0-9a-f]{40}\.larch$")
ROLLUP_FILE_RE = re.compile(r"^[0-9a-f]{40}\.rollup\.hex$")
PREFIX_DIR_RE = re.compile(r"^[0-9a-f]{3}$")
ALLOWED_ROOT_FILES = {"STORAGE_VERSION"}

BOOT_MOD = int(os.environ.get("ACID_BOOT_MOD", "5"))
DB = os.environ.get("ACID_DB", "acid")
PORT = "6399"
BASE = "http://127.0.0.1:%s" % PORT
AUTH = "Basic " + base64.b64encode(b"admin:root").decode()
TDB = os.environ.get("ACID_TDB", "/app/terminusdb/terminusdb")


def fail(msg):
    sys.stderr.write("CHECKER FAIL: %s\n" % msg)
    sys.exit(1)


def read_acks(stdout_path):
    with open(stdout_path) as f:
        return [line.split()[1] for line in f if line.startswith("ACK ")]


def check_structure(root):
    labels = []
    for dirpath, _dirs, files in os.walk(root):
        rel = os.path.relpath(dirpath, root)
        for name in files:
            path = os.path.join(dirpath, name)
            if rel == ".tmp":
                # Unrenamed tmp labels are expected crash residue; the store
                # reaps them on open. Their contents are never authoritative.
                continue
            if name in ALLOWED_ROOT_FILES or ".deleted_" in name:
                # .deleted_* files are ALICE artefacts preserving unlinked
                # inodes in reconstructed dirs — not real store files.
                continue
            if name.endswith(".label"):
                if rel != ".":
                    fail("label outside store root: %s" % path)
                data = open(path, "rb").read()
                m = LABEL_RE.match(data)
                if not m:
                    fail("torn label %s: %r" % (path, data[:100]))
                labels.append((path, m.group(2)))
            elif LAYER_FILE_RE.match(name):
                parent = os.path.basename(dirpath)
                if not PREFIX_DIR_RE.match(parent) or not name.startswith(parent):
                    fail("layer in wrong shard dir: %s" % path)
                # Empty/partial unreferenced layers are expected crash
                # residue (creat without surviving append); GC reaps them.
                # Only a *referenced* layer must be complete — checked below.
            elif ROLLUP_FILE_RE.match(name):
                parent = os.path.basename(dirpath)
                if not PREFIX_DIR_RE.match(parent):
                    fail("rollup in wrong shard dir: %s" % path)
            else:
                fail("unexpected file in store: %s" % path)
    for path, layer in labels:
        if not layer:
            continue
        name = layer.decode()
        lp = os.path.join(os.path.dirname(path), name[:3], name + ".larch")
        if not os.path.isfile(lp):
            fail("label %s references missing layer %s"
                 % (os.path.basename(path), lp))
        if os.path.getsize(lp) == 0:
            fail("label %s references empty layer %s"
                 % (os.path.basename(path), lp))


def http_get(path):
    r = urllib.request.Request(BASE + path)
    r.add_header("Authorization", AUTH)
    try:
        with urllib.request.urlopen(r, timeout=10) as resp:
            return resp.status, resp.read()
    except urllib.error.HTTPError as e:
        return e.code, e.read()


def boot_check(root, acks):
    env = dict(os.environ)
    env.update({
        "TERMINUSDB_SERVER_DB_PATH": root,
        "TERMINUSDB_SERVER_PORT": PORT,
        "TERMINUSDB_SERVER_NAME": "127.0.0.1",
        "TERMINUSDB_ADMIN_PASS": "root",
        "TERMINUSDB_JWT_ENABLED": "false",
        "TERMINUSDB_ENABLE_OPTIMIZER": "0",
        "TERMINUSDB_INDEXER_BACKEND": "none",
        "TERMINUSDB_WORKER_POOL_SIZE": "2",
        "TERMINUSDB_COMMIT_QUEUE_TIMEOUT": "5",
        "TERMINUSDB_LOG_LEVEL": "ERROR",
        "TERMINUSDB_PLUGINS_PATH":
            os.environ.get("TERMINUSDB_PLUGINS_PATH", "/plugins"),
        "TERMINUSDB_BUILTIN_PLUGINS_PATH":
            os.environ.get("TERMINUSDB_BUILTIN_PLUGINS_PATH", "/builtin_plugins"),
    })
    with open(os.devnull, "wb") as devnull:
        proc = subprocess.Popen([TDB, "serve"], env=env,
                                stdout=devnull, stderr=devnull)
    try:
        for _ in range(120):
            if proc.poll() is not None:
                fail("server exited while booting reconstructed state "
                     "(code %s)" % proc.returncode)
            try:
                urllib.request.urlopen(BASE + "/api/ok", timeout=1)
                break
            except Exception:
                time.sleep(0.5)
        else:
            fail("server never became ready on reconstructed state")

        for tag in acks:
            if tag == "db":
                status, _ = http_get(
                    "/api/document/admin/%s?graph_type=instance&as_list=true" % DB)
                if status != 200:
                    fail("acknowledged database %s unreadable after crash: "
                         "%d" % (DB, status))
            elif tag.startswith("schema"):
                status, body = http_get(
                    "/api/document/admin/%s?graph_type=schema&as_list=true&id=Acid" % DB)
                if status != 200 or b'"@id":"Acid"' not in body:
                    fail("acknowledged schema commit lost after crash")
            elif tag.startswith("doc-") or tag.startswith("cas-"):
                doc_id = "Acid/" + tag.split("-", 1)[1]
                status, body = http_get(
                    "/api/document/admin/%s?graph_type=instance&as_list=true&id=%s"
                    % (DB, doc_id))
                if status != 200 or doc_id.encode() not in body:
                    fail("acknowledged commit %s lost after crash "
                         "(document %s missing)" % (tag, doc_id))
    finally:
        proc.kill()
        proc.wait()


def main():
    root = os.path.abspath(sys.argv[1])
    stdout_path = sys.argv[2]

    check_structure(root)

    acks = read_acks(stdout_path)
    sample = zlib.crc32(root.encode()) % BOOT_MOD == 0
    if acks and sample:
        boot_check(root, acks)


if __name__ == "__main__":
    main()
