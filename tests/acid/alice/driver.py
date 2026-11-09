#!/usr/bin/env python3
"""Commit driver for the ALICE-traced workload.

Performs a bounded commit sequence against a freshly initialised store. Each
successful (2xx) commit prints `ACK <tag>` to stdout; ALICE hands the checker
the stdout prefix that preceded each simulated crash, so an ACKed commit is
by definition one the client was told succeeded — it must survive.

Environment:
  ACID_COMMITS  instance-document commits to perform (default 8)
  ACID_PORT     server port inside the container (default 6363)
  ACID_DB       database name (default "acid")
"""
import base64
import json
import os
import sys
import time
import urllib.error
import urllib.request

PORT = os.environ.get("ACID_PORT", "6363")
BASE = "http://127.0.0.1:%s" % PORT
DB = os.environ.get("ACID_DB", "acid")
COMMITS = int(os.environ.get("ACID_COMMITS", "8"))
AUTH = "Basic " + base64.b64encode(b"admin:root").decode()


def req(method, path, body=None, headers=None):
    r = urllib.request.Request(
        BASE + path, method=method,
        data=json.dumps(body).encode() if body is not None else None)
    r.add_header("Authorization", AUTH)
    if body is not None:
        r.add_header("Content-Type", "application/json")
    for k, v in (headers or {}).items():
        r.add_header(k, v)
    try:
        with urllib.request.urlopen(r, timeout=30) as resp:
            return resp.status, dict(resp.headers), resp.read()
    except urllib.error.HTTPError as e:
        return e.code, dict(e.headers), e.read()


def data_version(headers):
    for k, v in headers.items():
        if k.lower() == "terminusdb-data-version":
            return v
    return ""


def ack(tag, headers):
    sys.stdout.write("ACK %s %s\n" % (tag, data_version(headers)))
    sys.stdout.flush()


def commit(tag, method, path, body=None, headers=None):
    status, resp_headers, resp_body = req(method, path, body, headers)
    if not 200 <= status < 300:
        sys.exit("driver: %s failed: %d %r" % (tag, status, resp_body[:200]))
    ack(tag, resp_headers)


def wait_ready():
    for _ in range(240):
        try:
            urllib.request.urlopen(BASE + "/api/ok", timeout=2)
            return
        except Exception:
            time.sleep(0.5)
    sys.exit("driver: server never became ready")


def main():
    wait_ready()

    commit("db", "POST", "/api/db/admin/%s" % DB,
           {"label": "acid", "comment": "acid harness", "schema": True})

    schema = {
        "@id": "Acid",
        "@type": "Class",
        "val": "xsd:string",
    }
    commit("schema-base", "POST",
           "/api/document/admin/%s?graph_type=schema&author=acid&message=schema" % DB,
           schema)

    # Unguarded commits: each one moves the instance label and the commit-log
    # labels — a multi-label commit exercised at every syscall boundary.
    for i in range(1, COMMITS + 1):
        doc = {"@id": "Acid/%d" % i, "@type": "Acid", "val": "v%d" % i}
        commit("doc-%d" % i, "POST",
               "/api/document/admin/%s?graph_type=instance&author=acid&message=doc-%d" % (DB, i),
               doc)

    # Guarded commits: carry TerminusDB-Data-Version so the versioned label
    # CAS (nb_force_set_head) is the commit path under test.
    for i in range(1, 3):
        status, headers, _ = req(
            "GET", "/api/document/admin/%s?graph_type=instance&as_list=true" % DB)
        dv = data_version(headers)
        doc = {"@id": "Acid/cas-%d" % i, "@type": "Acid", "val": "cas%d" % i}
        commit("cas-%d" % i, "POST",
               "/api/document/admin/%s?graph_type=instance&author=acid&message=cas-%d" % (DB, i),
               doc, {"TerminusDB-Data-Version": dv})

    print("driver done")


if __name__ == "__main__":
    main()
