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
