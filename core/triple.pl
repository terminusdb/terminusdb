:- module(triple, [
              update/5 % we're explicitely exporting this so it can be excluded elsehwere
          ]).
:- reexport([triple/base_type,
             triple/casting,
             triple/database_utils,
             triple/iana,
             triple/literals,
             triple/temp_graph,
             triple/terminus_bootstrap,
             triple/triplestore,
             triple/turtle_utils,
             triple/upgrade_db
            ]).
