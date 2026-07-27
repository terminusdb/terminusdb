:- module(plugin_api_api, [
    api_insert_documents/9,
    branch_create/5,
    delete_db/5
]).

:- reexport(core(api/api_graphql)).
:- reexport(core(api/api_document), [api_insert_documents/9]).
:- reexport(core(api/db_branch), [branch_create/5]).
:- reexport(core(api/db_delete), [delete_db/5]).
