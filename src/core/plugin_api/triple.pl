:- module(plugin_api_triple, [
    super_user_authority/1,
    database_schema/2,
    database_instance/2,
    xrdf/4,
    decimal_precision/1,
    rational_to_decimal_string/3
]).

:- reexport(core(triple), [super_user_authority/1, database_schema/2, database_instance/2, xrdf/4]).
:- reexport(core(triple/casting), [decimal_precision/1, rational_to_decimal_string/3]).
