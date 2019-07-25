# RegulumDB

RegulumDB open source model driven graph database for knowledge graph representation designed 
specifically for the web-age. 

RegulumDB features a RESTful API for interacting with knowledge graphs via the JSON-LD exchange 
format. This means you can easily string together applications within your own toolchain which 
utilise the powerful features of graph search and graph storage. 

## Latest Version 

v0.1.0

## Getting Started

To use RegulumDB, you will need the SWIPL installation of prolog. To install this in Debian variants
simply use the apt package manager: 

```
apt install swi-prolog
```

The RegulumDB source tree should then be cloned from GitHub: 

```
git clone https://github.com/regulumdb/regulumdb
```

You should copy `config/config-example.pl` to `config/config.pl` and then 
edit the values there to set things such as server name and server 
port. 

At this point you can enter the regulumDB directory and start the server: 

```
./start.pl
```

Now you are ready to interact with the HTTP server. 

## Documentation 

The Server API is documented here: TBD

WOQL Syntax is documented here: TBD

## Why 

RegulumDB will: 

* Make complex data models easy, maintainable and enforced. 
* Overcome the Object Impedance mismatch without turning your Database into an incomprehensible soup. 
* Allow you to search for repeating patterns using recursion. 
* Give you powerful temporal queries using finite domain constraint logic. 
* Enable the sharing of data using linked open data formats RDF and JSON-LD making scientific or organisational information sharing easy.
* Help you automate the production of UI and data-entry by *knowing* what data *means*.

## Issues 

We currently have no client, so you'll have to use the RESTful API directly. You can find 
examples at the RegulumDB repositories...

In Python: https://github.com/regulumdb/regulum-python-sdk

In JavaScript: 
AND here: 

## Changes in this Version 

* Added additional typing information and documentation. 
* Added database bootsrapping for access permissions and controls using the capabilities ontology.

## TODO

* Need to finish porting of all XSD and OWL constraint checking. 

## Authors

Gavin Mendel-Gleason <gavin@datachemist.com>

Matthijs van Otterdijk <matthijs@datachemist.com>

Put your Name here by contributing!

## Copyright

This file is part of RegulumDB.

RegulumDB is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RegulumDB is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RegulumDB.  If not, see <https://www.gnu.org/licenses/>.
