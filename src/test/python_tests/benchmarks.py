#!/usr/bin/env python3b

from terminusdb_client import WOQLClient, WOQLQuery
import pytest


import time
import sys
import random

db = 'testdb_python_insert_test'
client = WOQLClient("https://127.0.0.1:6363")
client.connect(user="admin", account="admin", key="root")
try:
    client.delete_database(db, "admin")
except:
    pass
client.create_database(db, "admin", include_schema=False)

schema_db = 'testdb_python_insert_schema_test'
schema_client = WOQLClient("https://127.0.0.1:6363")
schema_client.connect(user="admin", account="admin", key="root")
try:
    schema_client.delete_database(schema_db, "admin")
except:
    pass
schema_client.create_database(schema_db, "admin")

def random_string_number():
    return str(random.randint(1, 1000000))

def insert_triple():
    subj = random_string_number()
    pred = random_string_number()
    value = random_string_number()
    WOQLQuery().using(f"admin/{db}").add_triple(subj, pred, value).execute(client)

def test_insert_triple_speed(benchmark):
    benchmark.pedantic(insert_triple, rounds=30)

def generate_turtle_triple():
    subj = random.randint(1, 1000)
    pred = random.randint(1, 100)
    value = random.randint(1, 1000)
    return f"doc:subj{subj} scm:pred{pred} doc:value{value} .\n"

def generate_turtle_string():
    s = "@prefix doc: <http://example.com/data/> .\n"
    s += "@prefix scm: <http://example.com/ontology#> .\n"
    for i in range(0,100):
        s += generate_turtle_triple()
    return s

def load_turtle_triples():
    contents = generate_turtle_string()
    client.insert_triples(
        "instance","main",
        contents,
        f"Adding random turtle")

def test_bulk_update(benchmark):
    benchmark.pedantic(load_turtle_triples, rounds=30)

def load_schema():
    filename = f'../worldOnt.ttl'
    ttl_file = open(filename)
    ttl_contents = ttl_file.read()
    ttl_file.close()
    schema_client.update_triples(
        "schema","main",
        ttl_contents,
        "Adding schema")

def test_create_schema(benchmark):
    benchmark.pedantic(load_schema, rounds=30)

def load_instance():
    filename = f'../world.ttl'
    ttl_file = open(filename)
    ttl_contents = ttl_file.read()
    ttl_file.close()
    schema_client.update_triples(
        "instance","main",
        ttl_contents,
        "Adding data")

def test_schema_bulk_update(benchmark):
    load_schema()
    benchmark.pedantic(load_instance, rounds=30)

