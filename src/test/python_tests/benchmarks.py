#!/usr/bin/env python3

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
