# Python Client Installation

## Requirements

TerminusDB 4.0 (you can install with [Bootstrap](https://github.com/terminusdb/terminusdb-bootstrap))

Python >= 3.6

## Install using pip

Install form PyPI:

``python -m pip install terminusdb-client``

this only include the core Python Client (WOQLClient) and WOQLQuery.

If you want to use woqlDataframe:

``python -m pip install terminusdb-client[dataframe]``

if you are installing form `zsh` you have to quote the argument like this:

``python -m pip install ‘terminusdb-client[dataframe]’``

    Install from source:

``python -m pip install git+https://github.com/terminusdb/terminusdb-client-python.git``
