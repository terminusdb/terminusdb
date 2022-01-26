#!/usr/bin/env python3

"""
Very simple script to check for missing autoloading imports.
It does the following step by steps:

1. It downloads the SWI Prolog help page and gets all the
   built-in library predicates
2. It traverses every prolog file
3. While it reads a file, it will check whether a predicate
   exists without a certain library import

It will skip two libraries: help and statistics. We will
probably never use them and they lead to false positives
that are hard to get away it. I know that this is a very
dirty hack that could probably be better implemented in
Prolog itself, but I do think that this issue is so
important for our code quality, that a more-or-less
hacky linter is better than no linter at all.
"""

from datetime import date
import json
import glob
import re
import requests
import sys
from bs4 import BeautifulSoup

def filter_exceptions(libraries: [dict]) -> [dict]:
    # Exceptions because they will probably never be used
    exceptions = ['library(help)', 'library(statistics)']
    return [x for x in libraries if x['library'] not in exceptions]

def get_libraries() -> [dict]:
    text = requests.get('https://www.swi-prolog.org/pldoc/man?section=library').text
    soup = BeautifulSoup(text, 'html.parser')
    libraries = []
    for element in soup.select('h3 .sec-title'):
        library_name = element.text
        predicates = [x.text for x in element.find_parent('h3').find_next_sibling('table').find_all('a')]
        libraries.append({'library': library_name,
                        'predicates': predicates})
    return filter_exceptions(libraries)


def remove_arity(libraries: [dict]) -> [dict]:
    for library in libraries:
        library['predicates'] = [x[0:-2] for x in library['predicates']]

def unique_predicates(libraries: [dict]) -> [dict]:
    for library in libraries:
        library['predicates'] = set(library['predicates'])


def filter_too_hard(libraries: [dict]) -> [dict]:
    # A slash can occur almost everyhwere and is hard to look for
    for library in libraries:
        library['predicates'] = [x for x in library['predicates'] if x != '/']

def skip_prolog_line(line: str) -> bool:
    if line[0] == '%' or (":-" in line and line[0:2] != ":-"):
        return True
    return False

def check_prolog_files(libs: [dict]) -> ():
    prolog_files = glob.glob("**/*.pl", recursive=True)
    has_error = False
    for prolog_file in prolog_files:
        with open(prolog_file, 'r') as f:
            prolog_text = "".join([x for x in f.readlines() if not skip_prolog_line(x)])
        for library in libs:
            lib_name = library['library']
            for predicate in library['predicates']:
                search = f"^\s+{predicate}\("
                try:
                    if re.search(search, prolog_text, re.MULTILINE) and lib_name not in prolog_text:
                        print(f"Predicate {predicate} used but not imported from {lib_name} in file {prolog_file}")
                        has_error = True
                except:
                    continue # Some weird predicates are very hard to search for with regex
    if has_error:
        sys.exit(1)


def main() -> ():
    libraries = get_libraries()
    remove_arity(libraries)
    filter_too_hard(libraries)
    check_prolog_files(libraries)


if __name__ == '__main__':
    main()
