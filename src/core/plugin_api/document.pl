:- module(plugin_api_document, [
    commits_changed_id/5,
    commit_info_dict/3,
    get_document/3,
    all_class_frames/3,
    schema_metadata_descriptor/3,
    database_prefixes/2,
    compress_dict_uri/3,
    prefix_expand/3
]).

:- reexport(core(document/history), [commits_changed_id/5, commit_info_dict/3]).
:- reexport(core(document), [get_document/3, all_class_frames/3,
                             schema_metadata_descriptor/3,
                             database_prefixes/2]).
:- reexport(core(query/jsonld), [compress_dict_uri/3, prefix_expand/3]).
