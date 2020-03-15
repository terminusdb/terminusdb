:- module(triple, [
              update/5 % we're explicitely exporting this so it can be excluded elsehwere
          ]).

:- reexport(triple/base_type).
:- reexport(triple/casting).
:- reexport(triple/database_utils).
:- reexport(triple/iana).
:- reexport(triple/literals).
:- reexport(triple/temp_graph).
:- reexport(triple/terminus_bootstrap).
:- reexport(triple/triplestore).

:- reexport(triple/turtle_utils).
:- reexport(triple/upgrade_db).
