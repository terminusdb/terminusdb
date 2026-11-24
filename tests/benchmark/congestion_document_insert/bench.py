#!/usr/bin/env python3
"""Public congestion benchmark for parallel document inserts.

Based on the enterprise congestion_document_insert and api_simple_insert
benchmarks. Measures the throughput of concurrent document writers against the
same branch using the public /api/document endpoint with NDJSON bulk inserts.

After the timed insertions, the benchmark verifies that every document ID
returned by the server is queryable, raising an error if any inserted document
is missing from the database.

Default configuration:
  - 4 writers
  - 3 chunks per writer
  - 2 000 documents per chunk
  - 24 000 documents total

Environment variables:
  TERMINUSDB_HOST  - server URL (default: http://127.0.0.1:6363)
  TERMINUSDB_USER  - admin user (default: admin)
  TERMINUSDB_PASS  - admin password (default: root)
  WRITERS          - number of concurrent writers (default: 4)
  CHUNKS_PER_WRITER - chunks each writer sends (default: 3)
  DOCS_PER_CHUNK   - documents per NDJSON chunk (default: 2000)
  OUTPUT           - path to JSON results file
                     (default: ../benchmark_results/congestion_document_insert.json)
"""






import base64
import io
import json
import os
import sys
import time
import urllib.parse
import uuid
from concurrent.futures import ThreadPoolExecutor, as_completed
from http import client as http_client


DEFAULT_HOST = "http://127.0.0.1:6363"
DEFAULT_USER = "admin"
DEFAULT_PASS = "root"

DB_NAME = "admin/congestion_document_insert"


class Client:
    """Minimal HTTP client for one TerminusDB request at a time."""

    def __init__(self, host=None, user=None, password=None):
        host = host or os.environ.get("TERMINUSDB_HOST", DEFAULT_HOST)
        user = user or os.environ.get("TERMINUSDB_USER", DEFAULT_USER)
        password = password or os.environ.get("TERMINUSDB_PASS", DEFAULT_PASS)

        parsed = urllib.parse.urlparse(host)
        self.hostname = parsed.hostname or "127.0.0.1"
        self.port = parsed.port or (443 if parsed.scheme == "https" else 80)
        self.use_https = parsed.scheme == "https"
        self.auth = "Basic " + base64.b64encode(f"{user}:{password}".encode()).decode()

    def request(self, method, path, body=None, content_type="application/json"):
        headers = {
            "Authorization": self.auth,
            "Content-Type": content_type,
            "Connection": "close",
        }
        conn = (
            http_client.HTTPSConnection(self.hostname, self.port, timeout=30)
            if self.use_https
            else http_client.HTTPConnection(self.hostname, self.port, timeout=30)
        )
        try:
            conn.request(method, path, body=body, headers=headers)
            response = conn.getresponse()
            data = response.read()
            return response.status, data
        finally:
            conn.close()


def create_db(client, db_name):
    status, data = client.request(
        "POST",
        f"/api/db/{db_name}",
        body=json.dumps({"label": "Congestion Document Insert Benchmark", "comment": "benchmark"}),
    )
    if status not in (200, 201):
        raise RuntimeError(f"Failed to create database {db_name}: {status} {data!r}")


def insert_schema(client, db_name):
    schema = {
        "@type": "Class",
        "@id": "Simple",
        "@key": {"@type": "Random"},
        "name": "xsd:string",
    }
    status, data = client.request(
        "POST",
        f"/api/document/{db_name}?graph_type=schema&author=benchmark&message=schema",
        body=json.dumps(schema),
    )
    if status not in (200, 201):
        raise RuntimeError(f"Failed to insert schema: {status} {data!r}")


def delete_db(client, db_name):
    try:
        client.request("DELETE", f"/api/db/{db_name}")
    except Exception as exc:
        print(f"Warning: failed to delete database {db_name}: {exc}", file=sys.stderr)


def build_chunk(writer_idx, chunk_idx, docs_per_chunk):
    """Build a chunk as NDJSON bytes for the /api/document endpoint."""
    buffer = io.StringIO()
    for i in range(docs_per_chunk):
        doc = {
            "@type": "Simple",
            "name": f"w{writer_idx}-c{chunk_idx}-d{i}-{uuid.uuid4().hex}",
        }
        buffer.write(json.dumps(doc))
        buffer.write("\n")
    return buffer.getvalue().encode("utf-8")


def insert_chunk(db_name, writer_idx, chunk_idx, docs_per_chunk):
    client = Client()
    body = build_chunk(writer_idx, chunk_idx, docs_per_chunk)
    status, data = client.request(
        "POST",
        f"/api/document/{db_name}?graph_type=instance&author=benchmark&message=congestion+chunk",
        body=body,
        content_type="application/json",
    )
    if status not in (200, 201):
        raise RuntimeError(f"Insert failed for writer {writer_idx} chunk {chunk_idx}: {status} {data!r}")
    ids = json.loads(data)
    return {
        "writer": writer_idx,
        "chunk": chunk_idx,
        "inserts": docs_per_chunk,
        "ids": ids,
    }


def _writer_loop(db_name, writer_idx, chunks_per_writer, docs_per_chunk):
    results = []
    for c in range(chunks_per_writer):
        results.append(insert_chunk(db_name, writer_idx, c, docs_per_chunk))
    return results


def run_parallel(db_name, writers, chunks_per_writer, docs_per_chunk):
    start = time.perf_counter()
    with ThreadPoolExecutor(max_workers=writers) as executor:
        futures = {
            executor.submit(_writer_loop, db_name, w, chunks_per_writer, docs_per_chunk): w
            for w in range(writers)
        }
        results = []
        for future in as_completed(futures):
            results.extend(future.result())
    elapsed = time.perf_counter() - start
    return elapsed, results


def run_sequential(db_name, writers, chunks_per_writer, docs_per_chunk):
    total_chunks = writers * chunks_per_writer
    start = time.perf_counter()
    results = []
    for c in range(total_chunks):
        results.append(insert_chunk(db_name, 0, c, docs_per_chunk))
    elapsed = time.perf_counter() - start
    return elapsed, results


def _normalize_id(doc_id):
    """Return the short form of a document id.

    Insert responses return full ids like 'terminusdb:///data/Simple/XXX' while
    GET /api/document returns short ids like 'Simple/XXX'. We normalize to the
    short form for comparison.
    """
    if doc_id.startswith("terminusdb:///data/"):
        return doc_id[len("terminusdb:///data/"):]
    return doc_id


def verify_consistency(client, db_name, results):
    """Verify that every id returned by the inserts can be queried back."""
    expected_ids = {
        _normalize_id(doc_id)
        for result in results
        for doc_id in result.get("ids", [])
    }

    status, data = client.request(
        "GET",
        f"/api/document/{db_name}?graph_type=instance",
    )
    if status != 200:
        raise RuntimeError(f"Failed to list documents from {db_name}: {status} {data!r}")

    found_ids = set()
    for line in data.splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            doc = json.loads(line)
        except json.JSONDecodeError as exc:
            raise RuntimeError(f"Invalid document list response from {db_name}: {line!r}") from exc
        if "@id" in doc:
            found_ids.add(doc["@id"])

    missing = expected_ids - found_ids
    extra = found_ids - expected_ids
    if missing:
        raise RuntimeError(
            f"Consistency check failed for {db_name}: {len(missing)} inserted documents missing "
            f"(e.g. {next(iter(missing))}). Found {len(found_ids)} documents, expected {len(expected_ids)}."
        )
    if extra:
        raise RuntimeError(
            f"Consistency check failed for {db_name}: {len(extra)} unexpected documents found "
            f"(e.g. {next(iter(extra))}). Found {len(found_ids)} documents, expected {len(expected_ids)}."
        )

    return len(expected_ids)


def summarize_results(elapsed, results, docs_per_chunk):
    total_docs = len(results) * docs_per_chunk
    docs_per_second = total_docs / elapsed if elapsed > 0 else 0
    ms_per_doc = (elapsed * 1000) / total_docs if total_docs > 0 else 0

    return {
        "total_ms": elapsed * 1000,
        "documents": total_docs,
        "chunks": len(results),
        "documents_per_second": docs_per_second,
        "ms_per_document": ms_per_doc,
    }


def main():
    writers = int(os.environ.get("WRITERS", "4"))
    chunks_per_writer = int(os.environ.get("CHUNKS_PER_WRITER", "3"))
    docs_per_chunk = int(os.environ.get("DOCS_PER_CHUNK", "2000"))
    output = os.environ.get("OUTPUT", "../benchmark_results/congestion_document_insert.json")

    parallel_db = f"{DB_NAME}_parallel"
    sequential_db = f"{DB_NAME}_sequential"

    setup_client = Client()

    # Parallel scenario.
    delete_db(setup_client, parallel_db)
    create_db(setup_client, parallel_db)
    insert_schema(setup_client, parallel_db)
    try:
        parallel_elapsed, parallel_results = run_parallel(
            parallel_db, writers, chunks_per_writer, docs_per_chunk
        )
        parallel_verified = verify_consistency(setup_client, parallel_db, parallel_results)
    finally:
        delete_db(setup_client, parallel_db)

    parallel_metrics = summarize_results(parallel_elapsed, parallel_results, docs_per_chunk)
    parallel_metrics["verified_documents"] = parallel_verified

    # Sequential scenario.
    delete_db(setup_client, sequential_db)
    create_db(setup_client, sequential_db)
    insert_schema(setup_client, sequential_db)
    try:
        sequential_elapsed, sequential_results = run_sequential(
            sequential_db, writers, chunks_per_writer, docs_per_chunk
        )
        sequential_verified = verify_consistency(setup_client, sequential_db, sequential_results)
    finally:
        delete_db(setup_client, sequential_db)

    sequential_metrics = summarize_results(sequential_elapsed, sequential_results, docs_per_chunk)
    sequential_metrics["verified_documents"] = sequential_verified

    congestion_ratio = (
        parallel_metrics["total_ms"] / sequential_metrics["total_ms"]
        if sequential_metrics["total_ms"] > 0 else 0
    )

    result = {
        "benchmark": "congestion_document_insert",
        "metrics": {
            "writers": writers,
            "chunks_per_writer": chunks_per_writer,
            "docs_per_chunk": docs_per_chunk,
            "parallel": parallel_metrics,
            "sequential": sequential_metrics,
            "congestion_ratio": congestion_ratio,
        },
    }

    os.makedirs(os.path.dirname(output), exist_ok=True)
    with open(output, "w") as f:
        json.dump(result, f, indent=2)
        f.write("\n")

    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
