TerminusDB Python client supports Python version 3.6 or above. It provides Python methods to perform database management, query database,  schema building, import and update data.

**This guide assumes you already have [TerminusDB installed](https://terminusdb.com/hub/download).**

## Download Python

Official distribution of Python can be downloaded at [python.org](https://www.python.org/downloads/). There are also other popular distributions available. For example [Anaconda distribution](https://www.anaconda.com/products/individual)

## Setup virtual environment for Python (Optional)

It is recommended to set up a separate virtual environment for your Python project. To do that, you can either use [venv](https://docs.python.org/3/tutorial/venv.html) that is provided with your official Python distribution or other popular tools like [conda](https://docs.conda.io/projects/conda/en/latest/index.html), [pipenv](https://pipenv.pypa.io/en/latest/) and [poetry](https://python-poetry.org/).

## Install TerminusDB Client for Python

TerminusDB Client can be installed using `pip`

`python -m pip install terminusdb-client`

This provides you with the basic Python client.

Optionally, you may want to install including the WOQLDataframe which allow you to convert your query result into a pandas DataFrame

`python -m pip install 'terminusdb-client[dataframe]'`

For details about the versions of the Python clients and more advance installation options, please refer to the [README of the GitHub repo](https://github.com/terminusdb/terminusdb-client-python).
