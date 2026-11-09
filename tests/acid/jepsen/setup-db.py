#!/usr/bin/env python3
"""Node-side setup for the Jepsen suite: create the database and the append
workload's schema idempotently, against localhost:6363. Invoked over SSH by
the Jepsen db/setup! step."""
import base64
import json
import urllib.error
import urllib.request

BASE = "http://127.0.0.1:6363"
DB = "ja"
AUTH = "Basic " + base64.b64encode(b"admin:root").decode()


def req(method, path, body=None):
    r = urllib.request.Request(
        BASE + path, method=method,
        data=json.dumps(body).encode() if body is not None else None)
    r.add_header("Authorization", AUTH)
    if body is not None:
        r.add_header("Content-Type", "application/json")
    try:
        with urllib.request.urlopen(r, timeout=15) as resp:
            return resp.status
    except urllib.error.HTTPError as e:
        return e.code


status = req("POST", "/api/db/admin/%s" % DB,
             {"label": "ja", "comment": "jepsen acid suite", "schema": True})
if status not in (200, 201):
    raise SystemExit("db create failed: %d" % status)

schema = {
    "@id": "JepsenList",
    "@type": "Class",
    "vals": {"@type": "List", "@class": "xsd:string"},
}
status = req("POST",
             "/api/document/admin/%s?graph_type=schema&author=jepsen&message=schema" % DB,
             schema)
if status not in (200, 201):
    raise SystemExit("schema insert failed: %d" % status)

print("setup ok")
