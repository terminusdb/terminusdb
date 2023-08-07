:- module(api_error, [api_error_jsonld/3,
                      api_error_jsonld/4,
                      json_http_code/2,
                      json_cli_code/2,
                      status_http_code/2,
                      status_cli_code/2,
                      generic_exception_jsonld/2
                     ]).

:- use_module(core(util)).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(yall)).
:- use_module(library(plunit)).

:- use_module(core(query)).

/**
 * api_error_jsonld(+API,+Error,-JSON) is det.
 *
 * Binds JSON to an appropriate JSON-LD object for the given error and API.
 *
 */
api_error_jsonld(API, Error, JSON) :-
    (   api_global_error_jsonld(Error, API, JSON)
    ->  true
    ;   api_error_jsonld_(API, Error, JSON)
    ).

%% Errors that are common to all error types
api_global_error_jsonld(error(missing_parameter(Param), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Missing parameter: ~s", [Param]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:MissingParameter',
                              'api:parameter' : Param },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(malformed_parameter(Param), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Malformed parameter: ~s", [Param]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:MalformedParameter',
                              'api:parameter' : Param },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(bad_parameter_type(Param, Expected_Type, Value), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    (   Expected_Type = boolean
    ->  Expected_Type_Displayed = "boolean (true, false)"
    ;   Expected_Type = graph
    ->  Expected_Type_Displayed = "graph (schema, instance)"
    ;   Expected_Type = nonnegative_integer
    ->  Expected_Type_Displayed = "non-negative integer"
    ;   Expected_Type = atom
    ->  Expected_Type_Displayed = "string"
    ;   memberchk(Expected_Type, [non_empty_atom, non_empty_string])
    ->  Expected_Type_Displayed = "non-empty string"
    ;   Expected_Type_Displayed = Expected_Type
    ),
    format(string(Msg), "Expected parameter '~s' of type '~s` but found: ~q", [Param, Expected_Type_Displayed, Value]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:BadParameterType',
                              'api:parameter' : Param,
                              'api:expected_type' : Expected_Type_Displayed,
                              'api:value' : Value },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(missing_file(File_Name), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Missing file: ~s", [File_Name]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:MissingFile',
                              'api:file_name' : File_Name },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(existence_error(source_sink, File_Name), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "File not found: ~s", [File_Name]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:FileNotFound',
                              'api:file_name' : File_Name },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(bad_data_version(Data_Version),_),Type,JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Data_Version_String), "~w", [Data_Version]),
    format(string(Msg), "Bad data version: ~s", [Data_Version_String]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:BadDataVersion',
                              'api:data_version' : Data_Version_String },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(data_version_mismatch(
                                  data_version(Requested_Label, Requested_Value),
                                  data_version(Actual_Label, Actual_Value)), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    atomic_list_concat([Requested_Label, ':', Requested_Value], Requested_Data_Version),
    atomic_list_concat([Actual_Label, ':', Actual_Value], Actual_Data_Version),
    format(string(Msg), "Requested data version in header does not match actual data version.", []),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:DataVersionMismatch",
                              'api:requested_data_version' : Requested_Data_Version,
                              'api:actual_data_version' : Actual_Data_Version }
            }.
api_global_error_jsonld(error(type_not_found(Unknown_Type), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Type not found in the schema: ~q", [Unknown_Type]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:TypeNotFound',
                              'api:type' : Unknown_Type },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(unknown_organization(Organization), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Unknown organization name: ~s", [Organization]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : 'api:not_found',
             'api:error' : _{ '@type' : 'api:UnknownOrganizationName',
                              'api:organization_name' : Organization },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(unknown_database(Organization, Database), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Unknown database: ~s/~s", [Organization, Database]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : 'api:not_found',
             'api:error' : _{'@type' : 'api:UnknownDatabase',
                             'api:database_name' : Database,
                             'api:organization_name' : Organization},
             'api:message' : Msg
            }.
api_global_error_jsonld(error(invalid_organization_name(Organization), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Invalid organization name: ~q", [Organization]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:InvalidOrganizationName',
                              'api:organization_name' : Organization },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(invalid_database_name(DB), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Invalid database name: ~q", [DB]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:InvalidDatabaseName',
                              'api:database_name' : DB },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(http_open_error(existence_error(_, URL)), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "HTTP request could not fetch URL: ~w", [URL]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:HttpRequestFailedFetch',
                              'api:url' : URL },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(http_open_error(domain_error(_, URL)), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "HTTP request could not be made to URL: ~w", [URL]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:HttpRequestFailedBadUrl',
                              'api:url' : URL },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(http_open_error(socket_error(_, Err_Msg)), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "HTTP request failed with socket error: ~w", [Err_Msg]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:HttpRequestFailedSocketError',
                              'api:message' : Err_Msg },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(http_open_error(permission_error(_, URL)), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "HTTP request authentication failed for URL: ~w", [URL]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:HttpRequestFailedAuthenticationError',
                              'api:url' : URL },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(http_open_error(Err), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Reason), "~w", [Err]),
    format(string(Msg), "HTTP request failed for an unknown reason: ~w", [Reason]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:HttpRequestFailed',
                              'api:reason' : Reason },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(remote_connection_failure(Status, Response), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    (   (Status = 401 ; Status = 403)
    ->  Opening_Msg = "Remote authentication failed"
    ;   Opening_Msg = "Remote connection failed"
    ),
    % Needs more options....
    (   \+ is_dict(Response)
    ->  format(string(Msg), "~s: ~q", [Opening_Msg,Response])
    ;   _{'@type': "api:UnpackErrorResponse", 'api:error' : Error} :< Response
    ->  (   \+ is_dict(Error)
        ->  format(string(Msg), "~s: ~q", [Opening_Msg, Error])
        ;   _{'@type' : "api:NotALinearHistory"} :< Error
        ->  format(string(Msg), "~s: Remote history has diverged", [Opening_Msg])
        ;   get_dict('api:message', Response, Response_Msg)
        ->  format(string(Msg), "~s: ~s", [Opening_Msg, Response_Msg])
        ;   format(string(Msg), "~s for an unknown reason", [Opening_Msg])
        )
    ;   get_dict('api:message', Response, Response_Msg)
    ->  format(string(Msg), "~s: ~s", [Opening_Msg, Response_Msg])
    ;   format(string(Msg), "~s for an unknown reason", [Opening_Msg])
    ),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:RemoteConnectionFailure",
                              'api:status' : Status,
                              'api:response' : Response}
            }.
api_global_error_jsonld(error(document_not_found(Id), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Document not found: ~q", [Id]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:not_found",
             'api:error' : _{ '@type' : 'api:DocumentNotFound',
                              'api:document_id' : Id },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(document_not_found(Id, Document), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Document not found: ~q", [Id]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:not_found",
             'api:error' : _{ '@type' : 'api:DocumentNotFound',
                              'api:document_id' : Id,
                              'api:document': Document },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(submitted_id_does_not_match_generated_id(Submitted_Id, Generated_Id), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Document was submitted with id ~q, but id ~q was generated", [Submitted_Id, Generated_Id]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:SubmittedIdDoesNotMatchGeneratedId',
                              'api:submitted_id': Submitted_Id,
                              'api:generated_id': Generated_Id },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(invalid_absolute_path(Path), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Bad descriptor path: ~w", [Path]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:BadDescriptorPath',
                              'api:descriptor' : Path},
             'api:message' : Msg
            }.
api_global_error_jsonld(error(unknown_local_branch(Branch), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Unknown local branch: ~w", [Branch]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UknownLocalBranch",
                              'api:branch' : Branch}
            }.
api_global_error_jsonld(error(origin_branch_does_not_exist(Branch), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Origin branch does not exist as specified ~w", [Branch]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:OriginBranchDoesNotExist",
                              'api:branch' : Branch}
            }.
api_global_error_jsonld(error(invalid_ref_path(Path), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Invalid ref path: ~w", [Path]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:InvalidRefPath",
                              'api:ref' : Path}
            }.
api_global_error_jsonld(error(unresolvable_commit_id(Id), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Invalid ref path: ~w", [Id]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnresolvableCommitId",
                              'api:commit' : Id}
            }.
api_global_error_jsonld(error(unresolvable_absolute_descriptor(Descriptor),_), Type, JSON) :-
    error_type(Type, Type_Displayed),
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "Unable to resolve an invalid absolute path for descriptor ~q", [Path]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg}.
api_global_error_jsonld(error(branch_does_not_exist(Descriptor), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The branch does not exist for ~q", [Path]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path}
            }.
api_global_error_jsonld(error(not_a_valid_commit_or_branch(Ref),_), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "The specified commit or branch ~q is not valid.", [Ref]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:NotValidRefError',
                              'api:ref' : Ref},
             'api:message' : Msg
            }.
api_global_error_jsonld(error(server_outdated(Store_Version,Server_Version), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "The storage directory contains a store newer than this server supports. This server expects store version ~q but version ~q is in place.", [Server_Version, Store_Version]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:ServerOutdated',
                              'api:store_version': Store_Version,
                              'api:server_version': Server_Version
                            },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(store_outdated(Store_Version,Server_Version), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "The storage directory contains an outdated store. Please upgrade your store. This server expects store version ~q but version ~q is in place.", [Server_Version, Store_Version]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:ServerOutdated',
                              'api:store_version': Store_Version,
                              'api:server_version': Server_Version
                            },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(no_database_store_version, _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "The store has no defined database version and so can not be opened.", []),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:NoDatabaseStoreVersion' },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(invalid_path(Path), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    format(string(Msg), "Resource path invalid: ~q", [Path]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:InvalidResourcePath',
                              'api:resource_path' : Path },
             'api:message' : Msg
            }.
api_global_error_jsonld(error(unexpected_descriptor_type(Descriptor, Desc_Type), _), Type, JSON) :-
    error_type(Type, Type_Displayed),
    resolve_absolute_string_descriptor(Path,Descriptor),
    format(string(Msg), "Unexpected resource type, with resource: ~q and type: ~q", [Path, Desc_Type]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:UnexpectedResourceType',
                              'api:resource_type' : Desc_Type,
                              'api:resource_path' : Path },
             'api:message' : Msg
            }.

:- multifile api_error_jsonld_/3.
%% DB Exists
api_error_jsonld_(check_db, error(bad_parameter_value(Param, Expected_Value, Value), _), JSON) :-
    format(string(Msg), "Expected parameter '~s' to have '~q' but found: ~q", [Param, Expected_Value, Value]),
    JSON = _{'@type' : 'api:DbExistsErrorResponse',
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:BadParameterValue',
                              'api:parameter' : Param,
                              'api:expected_value' : Expected_Value,
                              'api:value' : Value },
             'api:message' : Msg}.
%% DB Create
api_error_jsonld_(create_db,error(database_already_exists(Organization_Name, Database_Name),_), JSON) :-
    JSON = _{'@type' : 'api:DbCreateErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:DatabaseAlreadyExists',
                             'api:database_name' : Database_Name,
                             'api:organization_name' : Organization_Name},
             'api:message' : 'Database already exists.'}.
api_error_jsonld_(create_db,error(database_in_inconsistent_state,_), JSON) :-
    JSON = _{'@type' : 'api:DbCreateErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:DatabaseInInconsistentState'},
             'api:message' : 'Database is in an inconsistent state. Partial creation has taken place, but server could not finalize the database.'}.
api_error_jsonld_(create_db, error(missing_required_prefix(Prefix_Name), _), JSON) :-
    format(string(Msg), "The database requires the following prefix: ~w", [Prefix_Name]),
    JSON = _{'@type' : 'api:DbCreateErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:MissingRequiredPrefix',
                             'api:prefix_name' : Prefix_Name},
             'api:message' : Msg}.
api_error_jsonld_(create_db, error(invalid_uri_prefix(Prefix_Name, Prefix_Value), _), JSON) :-
    format(string(Msg), "The value for the prefix ~q (~w) is not a valid URI prefix.", [Prefix_Name, Prefix_Value]),
    JSON = _{'@type' : 'api:DbCreateErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:InvalidPrefix',
                             'api:prefix_name' : Prefix_Name,
                             'api:prefix_value' : Prefix_Value},
             'api:message' : Msg}.
api_error_jsonld_(create_db, error(database_name_too_long(Organization, Database), _), JSON) :-
    format(string(Msg), "The combination of organization ~s and database ~s results in a database name that is too long.", [Organization, Database]),
    JSON = _{'@type' : 'api:DbCreateErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:DatabaseNameTooLong',
                             'api:organization_name' : Organization,
                             'api:database_value' : Database},
             'api:message' : Msg}.
%% DB Delete
api_error_jsonld_(delete_db,error(database_not_finalized(Organization,Database), _),JSON) :-
    format(string(Msg), "Database ~s/~s is not in a deletable state.", [Organization, Database]),
    JSON = _{'@type' : 'api:DbDeleteErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:DatabaseNotFinalized',
                             'api:database_name' : Database,
                             'api:organization_name' : Organization},
             'api:message' : Msg}.
api_error_jsonld_(delete_db,error(database_files_do_not_exist(Organization,Database), _), JSON) :-
    format(string(Msg), "Database files for ~s/~s were missing unexpectedly.", [Organization, Database]),
    JSON = _{'@type' : 'api:DbDeleteErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:DatabaseFilesDoNotExist',
                             'api:database_name' : Database,
                             'api:organization_name' : Organization},
             'api:message' : Msg}.
api_error_jsonld_(update_db,error(schema_check_failure([Witness|_]), _), JSON) :-
    format(string(Msg), "Schema did not validate after turning schema checking on", []),
    JSON = _{'@type' : 'api:UpdateDBErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:SchemaValidationError',
                             'api:witness' : Witness},
             'api:message' : Msg}.
% CSV
api_error_jsonld_(csv,error(unknown_encoding(Enc), _), JSON) :-
    format(string(Msg), "Unrecognized encoding (try utf-8): ~q", [Enc]),
    JSON = _{'@type' : 'api:CsvErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:UnknownEncoding',
                             'api:format' : Enc},
             'api:message' : Msg}.
api_error_jsonld_(csv,error(invalid_graph_descriptor(Path), _), JSON) :-
    format(string(Msg), "Unable to find write graph for ~q", [Path]),
    JSON = _{'@type' : 'api:CsvErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:BadAbsoluteGraphDescriptor',
                             'api:absolute_graph_descriptor' : Path},
             'api:message' : Msg}.
api_error_jsonld_(csv,error(schema_check_failure([Witness|_]), _), JSON) :-
    format(string(Msg), "Schema did not validate after this update", []),
    JSON = _{'@type' : 'api:CsvErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:SchemaValidationError',
                             'api:witness' : Witness},
             'api:message' : Msg}.
api_error_jsonld_(csv,error(no_csv_name_supplied, _), JSON) :-
    format(string(Msg), "You did not provide a 'name' get parameter with the name of the CSV", []),
    JSON = _{'@type' : 'api:CsvErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:NoCsvName'},
             'api:message' : Msg}.
api_error_jsonld_(csv,error(unresolvable_absolute_descriptor(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "Unable to resolve an invalid absolute path for descriptor ~q", [Path]),
    JSON = _{'@type' : 'api:CsvErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg}.
api_error_jsonld_(csv,error(woql_syntax_error(badly_formed_ast(Term)),_), JSON) :-
    term_string(Term,String),
    format(string(Msg), "Badly formed ast after compilation with term: ~q", [Term]),
    JSON = _{'@type' : 'api:CSVErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:WOQLSyntaxError',
                              'api:error_term' : String},
             'api:message' : Msg
            }.
api_error_jsonld_(csv,error(no_known_csv(Name),_), JSON) :-
    format(string(Msg), "No csv named: ~q", [Name]),
    JSON = _{'@type' : 'api:CSVErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:NoKnownCSVError',
                              'api:error_term' : Name},
             'api:message' : Msg
            }.
% Triples
api_error_jsonld_(triples,error(unknown_format(Format), _), JSON) :-
    format(string(Msg), "Unrecognized format: ~q", [Format]),
    JSON = _{'@type' : 'api:TriplesErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:TriplesUnknownFormat',
                             'api:format' : Format},
             'api:message' : Msg}.
api_error_jsonld_(triples,error(invalid_graph_descriptor(Path), _), JSON) :-
    format(string(Msg), "Invalid graph descriptor: ~q", [Path]),
    JSON = _{'@type' : 'api:TriplesErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:BadAbsoluteGraphDescriptor',
                             'api:absolute_graph_descriptor' : Path},
             'api:message' : Msg}.
api_error_jsonld_(triples,error(unknown_graph(Graph_Descriptor), _), JSON) :-
    resolve_absolute_string_graph_descriptor(Path, Graph_Descriptor),
    format(string(Msg), "Invalid graph descriptor (this graph may not exist): ~q", [Graph_Descriptor]),
    JSON = _{'@type' : 'api:TriplesErrorResponse',
             'api:status' : 'api:not_found',
             'api:error' : _{'@type' : 'api:UnresolvableAbsoluteGraphDescriptor',
                             'api:absolute_graph_descriptor' : Path},
             'api:message' : Msg}.
api_error_jsonld_(triples,error(schema_check_failure([Witness|_]), _), JSON) :-
    format(string(Msg), "Schema did not validate after this update", []),
    JSON = _{'@type' : 'api:TriplesErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:SchemaValidationError',
                             'api:witness' : Witness},
             'api:message' : Msg}.
api_error_jsonld_(frame,error(instance_uri_has_unknown_prefix(K),_), JSON) :-
    format(string(Msg), "Instance uri has unknown prefix: ~q", [K]),
    term_string(K, Key),
    JSON = _{'@type' : 'api:FrameErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:InstanceUriHasUnknownPrefix',
                              'api:instance_uri' : Key},
             'api:message' : Msg
            }.
api_error_jsonld_(frame,error(class_uri_has_unknown_prefix(K),_), JSON) :-
    format(string(Msg), "Class uri has unknown prefix: ~q", [K]),
    term_string(K, Key),
    JSON = _{'@type' : 'api:FrameErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:ClassUriHasUnknownPrefix',
                              'api:class_uri' : Key},
             'api:message' : Msg
            }.
api_error_jsonld_(frame,error(could_not_create_class_frame(Class),_), JSON) :-
    format(string(Msg), "Could not create class frame for class: ~q", [Class]),
    term_string(Class, Class_String),
    JSON = _{'@type' : 'api:FrameErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:CouldNotCreateClassFrame',
                              'api:class_uri' : Class_String},
             'api:message' : Msg
            }.
api_error_jsonld_(frame,error(could_not_create_class_frame,_), JSON) :-
    format(string(Msg), "Could not create class frames for all classes", []),
    JSON = _{'@type' : 'api:FrameErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:CouldNotCreateClassFrames' },
             'api:message' : Msg
            }.
api_error_jsonld_(frame,error(could_not_create_filled_class_frame(Instance),_), JSON) :-
    format(string(Msg), "Could not create filled class frame for instance: ~q", [Instance]),
    term_string(Instance, Instance_String),
    JSON = _{'@type' : 'api:FrameErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:CouldNotCreateFilledClassFrame',
                              'api:instance_uri' : Instance_String},
             'api:message' : Msg
            }.
api_error_jsonld_(frame,error(unresolvable_collection(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor could not be resolved to a resource: ~q", [Path]),
    JSON = _{'@type' : 'api:FrameErrorResponse',
             'api:status' : 'api:not_found',
             'api:error' : _{ '@type' : 'api:UnresolvableAbsoluteDescriptor',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(frame,error(woql_syntax_error(badly_formed_ast(Term)),_), JSON) :-
    term_string(Term,String),
    format(string(Msg), "Badly formed ast after compilation with term: ~q", [Term]),
    JSON = _{'@type' : 'api:FrameErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:WOQLSyntaxError',
                              'api:error_term' : String},
             'api:message' : Msg
            }.
api_error_jsonld_(frame,error(casting_error(Val,Type),_), JSON) :-
    format(string(ValS), "~w", [Val]),
    format(string(Msg), "The value ~s could not be cast as ~q", [ValS,Type]),
    JSON = _{'@type' : 'api:FrameErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:BadCast',
                              'api:value' : ValS,
                              'api:type' : Type},
             'api:message' : Msg
            }.
api_error_jsonld_(woql,error(find_resource_pre_flight_failure_for(AST), _), JSON) :-
    format(string(Msg), "Unable to find and process a resource from the AST ~q", [AST]),
    JSON = _{'@type' : "api:WoqlErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:FindResourceFailure" }
            }.
api_error_jsonld_(woql,error(not_a_valid_descriptor(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The source path ~q is not a valid descriptor for branching", [Path]),
    JSON = _{'@type' : "api:WoqlErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NotASourceBranchDescriptorError",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(woql,error(unresolvable_collection(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor could not be resolved to a resource: ~q", [Path]),
    JSON = _{'@type' : 'api:WoqlErrorResponse',
             'api:status' : 'api:not_found',
             'api:error' : _{ '@type' : 'api:UnresolvableAbsoluteDescriptor',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(woql,error(woql_syntax_error(badly_formed_ast(Term)),_), JSON) :-
    term_string(Term,String),
    format(string(Msg), "Badly formed ast after compilation with term: ~q", [Term]),
    JSON = _{'@type' : 'api:WoqlErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:WOQLSyntaxError',
                              'api:error_term' : String},
             'api:message' : Msg
            }.
api_error_jsonld_(woql,error(woql_syntax_error(Term),_), JSON) :-
    term_string(Term,String),
    format(string(Msg), "Unknown syntax error in WOQL: ~q", [String]),
    JSON = _{'@type' : 'api:WoqlErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:WOQLSyntaxError',
                              'api:error_term' : String},
             'api:message' : Msg
            }.
api_error_jsonld_(woql,error(woql_syntax_error(Query,Path,Element), _), JSON) :-
    json_woql_path_element_error_message(Query,Path,Element,Message),
    reverse(Path,Director),
    Error = _{'@type' : 'vio:WOQLSyntaxError',
              'vio:path' : Director,
              'vio:query' : Query},
    JSON = _{'@type' : 'api:WoqlErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : Error,
             'api:message' : Message
            }.
api_error_jsonld_(woql,error(schema_check_failure(Witnesses),_), JSON) :-
    format(string(Msg), "There was an error when schema checking", []),
    JSON = _{'@type' : 'api:WoqlErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:WOQLSchemaCheckFailure',
                              'api:witnesses' : Witnesses},
             'api:message' : Msg
            }.
api_error_jsonld_(woql,error(woql_instantiation_error(Vars),_), JSON) :-
    format(string(Msg), "The following variables were unbound but must be bound: ~q", [Vars]),
    JSON = _{'@type' : 'api:WoqlErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:WOQLModeError',
                              'api:error_vars' : Vars},
             'api:message' : Msg
            }.
api_error_jsonld_(woql,error(unresolvable_absolute_descriptor(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The WOQL query referenced an invalid absolute path for descriptor ~q", [Path]),
    JSON = _{'@type' : "api:WoqlErrorResponse",
             'api:status' : 'api:not_found',
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(woql,error(branch_does_not_exist(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The WOQL query referenced a non existing branch for descriptor ~q", [Path]),
    JSON = _{'@type' : "api:WoqlErrorResponse",
             'api:status' : 'api:not_found',
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:BranchDoesNotExistError",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(woql,error(casting_error(Val,Type),_), JSON) :-
    format(string(ValS), "~w", [Val]),
    format(string(Msg), "The value ~s could not be cast as ~q", [ValS,Type]),
    JSON = _{'@type' : 'api:WoqlErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:BadCast',
                              'api:value' : ValS,
                              'api:type' : Type},
             'api:message' : Msg
            }.
api_error_jsonld_(woql,error(existence_error(matching_rule,Term),_), JSON) :-
    format(string(TermW), "~q", [Term]),
    format(string(Msg), "The program: '~w' used a predicate with unhandled arguments", [Term]),
    JSON = _{'@type' : 'api:WoqlErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:ExistenceError',
                              'api:value' : TermW},
             'api:message' : Msg
            }.
api_error_jsonld_(clone,error(no_remote_authorization,_),JSON) :-
    format(string(Msg), "No remote authorization supplied", []),
    JSON = _{'@type' : 'api:CloneErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:AuthorizationError'},
             'api:message' : Msg
            }.
api_error_jsonld_(clone,error(database_already_exists(Organization_Name, Database_Name),_), JSON) :-
    JSON = _{'@type' : 'api:CloneErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:DatabaseAlreadyExists',
                             'api:database_name' : Database_Name,
                             'api:organization_name' : Organization_Name},
             'api:message' : 'Database already exists.'
             }.
api_error_jsonld_(clone,error(database_in_inconsistent_state,_), JSON) :-
    JSON = _{'@type' : 'api:CloneErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:DatabaseInInconsistentState'},
             'api:message' : 'Database is in an inconsistent state. Partial creation has taken pla.e, but server could not finalize the database.'
            }.
api_error_jsonld_(fetch,error(no_remote_authorization,_),JSON) :-
    format(string(Msg), "No remote authorization supplied", []),
    JSON = _{'@type' : 'api:FetchErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:AuthorizationError'},
             'api:message' : Msg
            }.
api_error_jsonld_(fetch,error(unresolvable_collection(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor (which should be a repository) could not be resolved to a resource: ~q", [Path]),
    JSON = _{'@type' : 'api:FetchErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:UnresolvableAbsoluteDescriptor',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(fetch,error(fetch_remote_has_no_url(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The remote being fetched has no URL ~q", [Path]),
    JSON = _{'@type' : 'api:FetchErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:RemoteHasNoURL",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(fetch,error(fetch_requires_repository(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The remote being fetched has no repository specified ~q", [Path]),
    JSON = _{'@type' : 'api:FetchErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnknownRemoteRepository",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(rebase,error(invalid_target_absolute_path(Path),_), JSON) :-
    format(string(Msg), "The following rebase target absolute resource descriptor string is invalid: ~q", [Path]),
    JSON = _{'@type' : 'api:RebaseErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:BadAbsoluteTargetDescriptor',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(rebase,error(invalid_source_absolute_path(Path),_), JSON) :-
    format(string(Msg), "The following rebase source absolute resource descriptor string is invalid: ~q", [Path]),
    JSON = _{'@type' : 'api:RebaseErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:BadAbsoluteSourceDescriptor',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(rebase,error(rebase_requires_target_branch(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following rebase target absolute resource descriptor does not describe a branch: ~q", [Path]),
    JSON = _{'@type' : 'api:RebaseErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:NotATargetBranchDescriptorError',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(rebase,error(rebase_requires_source_branch(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following rebase source absolute resource descriptor does not describe a branch: ~q", [Path]),
    JSON = _{'@type' : 'api:RebaseErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:NotASourceBranchDescriptorError',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(rebase,error(unresolvable_target_descriptor(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following target descriptor could not be resolved to a branch: ~q", [Path]),
    JSON = _{'@type' : 'api:RebaseErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:UnresolvableTargetAbsoluteDescriptor',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(rebase,error(unresolvable_source_descriptor(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following source descriptor could not be resolved to a branch: ~q", [Path]),
    JSON = _{'@type' : 'api:RebaseErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:UnresolvableSourceAbsoluteDescriptor',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(rebase,error(rebase_commit_application_failed(continue_on_valid_commit(Their_Commit_Id), _Commits),_), JSON) :-
    format(string(Msg), "While rebasing, commit ~q applied cleanly, but the 'continue' strategy was specified, indicating this should have errored", [Their_Commit_Id]),
    JSON = _{'@type' : 'api:RebaseErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:RebaseContinueOnValidCommit',
                              'api:their_commit' : Their_Commit_Id},
             'api:message' : Msg
            }.
api_error_jsonld_(rebase,error(rebase_commit_application_failed(fixup_on_valid_commit(Their_Commit_Id), _Commits),_), JSON) :-
    format(string(Msg), "While rebasing, commit ~q applied cleanly, but the 'fixup' strategy was specified, indicating this should have errored", [Their_Commit_Id]),
    JSON = _{'@type' : 'api:RebaseErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:RebaseFixupOnValidCommit',
                              'api:their_commit' : Their_Commit_Id},
             'api:message' : Msg
            }.
api_error_jsonld_(rebase,error(rebase_commit_application_failed(schema_validation_error(Their_Commit_Id, Fixup_Witnesses),_Commits),_), JSON) :-
    format(string(Msg), "Rebase failed on commit ~q due to schema validation errors", [Their_Commit_Id]),
    JSON = _{'@type' : 'api:RebaseErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:RebaseSchemaValidationError',
                              'api:their_commit' : Their_Commit_Id,
                              'api:witness' : Fixup_Witnesses},
             'api:message' : Msg
            }.
api_error_jsonld_(rebase,error(rebase_commit_application_failed(fixup_error(Their_Commit_Id, Fixup_Witnesses), _Commits),_), JSON) :-
    format(string(Msg), "Rebase failed on commit ~q due to fixup error: ~q", [Their_Commit_Id,Fixup_Witnesses]),
    JSON = _{'@type' : 'api:RebaseErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:RebaseFixupError',
                              'api:their_commit' : Their_Commit_Id,
                              'api:witness' : Fixup_Witnesses},
             'api:message' : Msg
            }.
api_error_jsonld_(pack,error(unresolvable_collection(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor (which should be a repository) could not be resolved to a resource: ~q", [Path]),
    JSON = _{'@type' : 'api:PackErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:UnresolvableAbsoluteDescriptor',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(pack,error(not_a_repository_descriptor(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor is not a repository descriptor: ~q", [Path]),
    JSON = _{'@type' : 'api:PackErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:NotARepositoryDescriptorError',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(unpack,error(not_a_linear_history_in_unpack(_History),_), JSON) :-
    JSON = _{'@type' : "api:UnpackErrorResponse",
             'api:status' : "api:failure",
             'api:error' : _{'@type' : "api:NotALinearHistory"},
             'api:message' : "Not a linear history"
            }.
api_error_jsonld_(unpack,error(unknown_layer_reference(Layer_Id),_), JSON) :-
    JSON = _{'@type' : "api:UnpackErrorResponse",
             'api:status' : "api:failure",
             'api:message' : "A layer in the pack has an unknown parent",
             'api:error' : _{ '@type' : "api:UnknownLayerReference",
                              'api:layer_reference' : Layer_Id}
            }.
api_error_jsonld_(unpack,error(unresolvable_absolute_descriptor(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The database to unpack to has not been found at absolute path ~q", [Path]),
    JSON = _{'@type' : "api:UnpackErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(unpack,error(not_a_repository_descriptor(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor is not a repository descriptor: ~q", [Path]),
    JSON = _{'@type' : 'api:UnpackErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:NotARepositoryDescriptorError',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(push,error(push_has_no_repository_head(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following repository has no head: ~q", [Path]),
    JSON = _{'@type' : 'api:PushErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:RepositoryHasNoRepositoryHeadError',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(push,error(push_requires_branch(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following absolute resource descriptor string does not specify a branch: ~q", [Path]),
    JSON = _{'@type' : 'api:PushErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:NotABranchDescriptorError',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(push,error(unresolvable_absolute_descriptor(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The branch described by the path ~q does not exist", [Path]),
    JSON = _{'@type' : "api:PushErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(push,error(unknown_remote_repository(Remote_Repo),_), JSON) :-
    format(string(Msg), "Unknown remote repository: ~w", [Remote_Repo]),
    JSON = _{'@type' : 'api:PushErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : "api:UnknownRemoteRepository",
                              'api:remote_repository' : Remote_Repo},
             'api:message' : Msg
            }.
api_error_jsonld_(pull,error(not_a_valid_remote_branch(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The remote branch described by the path ~q does not exist", [Path]),
    JSON = _{'@type' : "api:PullErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:fetch_status' : false,
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(pull,error(pull_divergent_history(Common_Commit,Head_Has_Updated), _), JSON) :-
    format(string(Msg), "History diverges from commit ~q", [Common_Commit]),
    JSON = _{'@type' : "api:PullErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:fetch_status' : Head_Has_Updated,
             'api:error' : _{ '@type' : 'api:HistoryDivergedError',
                              'api:common_commit' : Common_Commit
                            }
            }.
api_error_jsonld_(pull,error(pull_no_common_history(Head_Has_Updated), _), JSON) :-
    format(string(Msg), "There is no common history between branches", []),
    JSON = _{'@type' : "api:PullErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:fetch_status' : Head_Has_Updated,
             'api:error' : _{ '@type' : 'api:NoCommonHistoryError'
                            }
            }.
api_error_jsonld_(pull,error(branch_does_not_exist(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The branch does not exist for ~q", [Path]),
    JSON = _{'@type' : 'api:PullErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(pull,error(fetch_remote_has_no_url(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The remote being fetched has no URL ~q", [Path]),
    JSON = _{'@type' : 'api:PullErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:RemoteHasNoURL",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(branch,error(invalid_target_absolute_path(Path),_), JSON) :-
    format(string(Msg), "Invalid target absolute resource descriptor: ~q", [Path]),
    JSON = _{'@type' : 'api:BranchErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:BadTargetAbsoluteDescriptor',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(branch,error(invalid_origin_absolute_path(Path),_), JSON) :-
    format(string(Msg), "Invalid origin absolute resource descriptor: ~q", [Path]),
    JSON = _{'@type' : 'api:BranchErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:BadOriginAbsoluteDescriptor',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(branch,error(target_not_a_branch_descriptor(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The target ~q is not a branch descriptor", [Path]),
    JSON = _{'@type' : "api:BranchErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NotATargetBranchDescriptorError",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(branch,error(source_not_a_valid_descriptor(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The source path ~q is not a valid descriptor for branching", [Path]),
    JSON = _{'@type' : "api:BranchErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NotASourceBranchDescriptorError",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(branch,error(unknown_origin_database(Organization, Database), _), JSON) :-
    format(string(Msg), "Unknown origin database: ~s/~s", [Organization, Database]),
    JSON = _{'@type' : "api:BranchErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnknownOriginDatabase",
                              'api:database_name' : Database,
                              'api:organization_name' : Organization}
            }.
api_error_jsonld_(branch,error(repository_is_not_local(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "Attempt to branch from remote repository ~s", [Path]),
    JSON = _{'@type' : "api:BranchErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NotLocalRepositoryError",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(branch,error(branch_does_not_exist(Branch_Name), _), JSON) :-
    format(string(Msg), "Branch ~s does not exist", [Branch_Name]),
    JSON = _{'@type' : "api:BranchErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:BranchDoesNotExistError",
                              'api:branch_name' : Branch_Name}
            }.
api_error_jsonld_(branch,error(deleting_branch_main, _), JSON) :-
    format(string(Msg), "Branch 'main' should not be deleted as it is special", []),
    JSON = _{'@type' : "api:BranchErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:BranchMainDeletionError"}
            }.
api_error_jsonld_(branch,error(branch_already_exists(Branch_Name), _), JSON) :-
    format(string(Msg), "Branch ~s already exists", [Branch_Name]),
    JSON = _{'@type' : "api:BranchErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:BranchExistsError",
                              'api:branch_name' : Branch_Name}
            }.
api_error_jsonld_(branch,error(origin_cannot_be_branched(Origin_Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Origin_Descriptor),
    format(string(Msg), "Origin is not a branchable path ~q", [Path]),
    JSON = _{'@type' : "api:BranchErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NotBranchableError",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(user_update,error(user_update_failed_without_error(Name,Document),_),JSON) :-
    atom_json_dict(Atom, Document,[]),
    format(string(Msg), "Update to user ~q failed without an error while updating with document ~q", [Name, Atom]),
    JSON = _{'@type' : "api:UserUpdateErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UserUpdateFailedWithoutError",
                              'api:user_name' : Name}
            }.
api_error_jsonld_(user_update,error(malformed_update_user_document(Document,Expected),_),JSON) :-
    format(string(Msg), "An update to a user which does not already exist was attempted with a document missing required fields ~q", [Expected]),
    JSON = _{'@type' : "api:UserUpdateErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:document' : Document,
             'api:error' : _{ '@type' : "api:MalformedAddUserDocument"}
            }.
api_error_jsonld_(user_delete,error(user_delete_failed_without_error(Name),_),JSON) :-
    format(string(Msg), "Delete of user ~q failed without an error", [Name]),
    JSON = _{'@type' : "api:UserDeleteErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UserDeleteFailedWithoutError",
                              'api:user_name' : Name}
            }.
api_error_jsonld_(add_organization,error(unknown_user(Name),_), JSON) :-
    format(string(Msg), "Attempt to add an Unknown user: ~q", [Name]),
    JSON = _{'@type' : "api:AddOrganizationErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnknownUser",
                              'api:user_name' : Name}
            }.
api_error_jsonld_(add_organization,error(organization_already_exists(Name),_), JSON) :-
    format(string(Msg), "The organization ~q already exists", [Name]),
    JSON = _{'@type' : "api:AddOrganizationErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:OrganizationAlreadyExists",
                              'api:organization_name' : Name}
            }.
api_error_jsonld_(add_organization,error(organization_creation_requires_superuser,_), JSON) :-
    format(string(Msg), "Organization creation requires super user authority", []),
    JSON = _{'@type' : "api:AddOrganizationErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:RequiresSuperuserAuthority"}
            }.
api_error_jsonld_(update_organization,error(organization_update_requires_superuser,_), JSON) :-
    format(string(Msg), "Organization update requires super user authority", []),
    JSON = _{'@type' : "api:UpdateOrganizationErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:RequiresSuperuserAuthority"}
            }.
api_error_jsonld_(delete_organization,error(delete_organization_requires_superuser,_), JSON) :-
    format(string(Msg), "Organization deletion requires super user authority", []),
    JSON = _{'@type' : "api:DeleteOrganizationErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:RequiresSuperuserAuthority"}
            }.
api_error_jsonld_(update_role,error(no_manage_capability(Organization,Resource_Name), _), JSON) :-
    format(string(Msg), "The organization ~q has no manage capability over the resource ~q", [Organization, Resource_Name]),
    JSON = _{'@type' : "api:UpdateRoleErrorResponse",
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoManageCapability",
                              'api:organization_name' : Organization,
                              'api:resource_name' : Resource_Name}
            }.
api_error_jsonld_(squash,error(not_a_branch_descriptor(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The path ~s is not a branch descriptor", [Path]),
    JSON = _{'@type' : 'api:SquashErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NotABranchDescriptorError",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(squash,error(unresolvable_absolute_descriptor(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The path ~s can not be resolved to a resource", [Path]),
    JSON = _{'@type' : 'api:SquashErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : 'api:UnresolvableAbsoluteDescriptor',
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(reset,error(not_a_branch_descriptor(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The path ~s is not a branch descriptor", [Path]),
    JSON = _{'@type' : 'api:ResetErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NotABranchDescriptorError",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(reset,error(not_a_commit_descriptor(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The path ~s is not a commit descriptor", [Path]),
    JSON = _{'@type' : 'api:ResetErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NotACommitDescriptorError",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(reset,error(different_repositories(Descriptor1,Descriptor2),_),
                    JSON) :-
    resolve_absolute_string_descriptor(Path1, Descriptor1),
    resolve_absolute_string_descriptor(Path2, Descriptor2),
    format(string(Msg), "The repository of the path to be reset ~s is not in the same repository as the commit which is in ~s", [Path1,Path2]),
    JSON = _{'@type' : 'api:ResetErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:DifferentRepositoriesError",
                              'api:source_absolute_descriptor' : Path1,
                              'api:target_absolute_descriptor': Path2
                            }
            }.
api_error_jsonld_(reset,error(branch_does_not_exist(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The branch does not exist for ~q", [Path]),
    JSON = _{'@type' : 'api:ResetErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(optimize,error(not_a_valid_descriptor_for_optimization(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The path ~s is not an optimizable descriptor", [Path]),
    JSON = _{'@type' : 'api:OptimizeErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NotAValidOptimizationDescriptorError",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(optimize,error(label_version_changed(Name,Version),_), JSON) :-
    format(string(Msg), "The label ~q to be optimized has moved since loaded as version ~q",
           [Name,Version]),
    JSON = _{'@type' : 'api:OptimizeErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:LabelVersionChanged',
                              'api:version' : Version,
                              'api:label_name' : Name},
             'api:message' : Msg
            }.
api_error_jsonld_(store_init,error(storage_already_exists(Path),_),JSON) :-
    format(string(Msg), "There is already a database initialized at path ~s", [Path]),
    JSON = _{'@type' : 'api:StoreInitErrorResponse',
             'api:status' : 'api:failure',
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:StorageAlreadyInitializedError",
                              'api:path' : Path}
            }.
api_error_jsonld_(store_init,error(permission_error(create,directory,Path),_), JSON) :-
    format(string(Msg), "You do not have permissions to create directory ~s", [Path]),
    JSON = _{'@type' : 'api:StoreInitErrorResponse',
             'api:status' : 'api:failure',
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:DirectoryPermissionsError",
                              'api:path' : Path}
            }.
api_error_jsonld_(info,error(access_not_authorized(Auth),_),JSON) :-
    format(string(Msg), "Access to `info` is not authorised with auth ~q",
           [Auth]),
    term_string(Auth, Auth_String),
    JSON = _{'api:status' : 'api:forbidden',
             'api:message' : Msg,
             'auth' : Auth_String,
             'action' : 'info',
             'scope' : 'system'
            }.
api_error_jsonld_(remote,error(unresolvable_descriptor(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The repository does not exist for ~q", [Path]),
    JSON = _{'@type' : 'api:RemoteErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(remote,error(remote_does_not_exist(Name),_), JSON) :-
    format(string(Msg), "The remote does not exist for ~q", [Name]),
    JSON = _{'@type' : 'api:RemoteErrorResponse',
             'api:status' : 'api:not_found',
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:RemoteDoesNotExist",
                              'api:remote_name' : Name}
            }.
api_error_jsonld_(remote,error(remote_exists(Name),_), JSON) :-
    format(string(Msg), "The remote already exist for ~q", [Name]),
    JSON = _{'@type' : 'api:RemoteErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:RemoteExists",
                              'api:remote_name' : Name}
            }.
api_error_jsonld_(rollup,error(unresolvable_collection(Descriptor),_), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor could not be resolved to a resource: ~q", [Path]),
    JSON = _{'@type' : 'api:RollupErrorResponse',
             'api:status' : 'api:not_found',
             'api:error' : _{ '@type' : 'api:UnresolvableAbsoluteDescriptor',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld_(diff,error(explicitly_copied_key_has_changed(Key,Before,After),_), JSON) :-
    format(string(Msg), "The explicitly copied key ~q is not the same in both documents. Before: ~q, After~q", [Key,Before,After]),
    JSON = _{'@type' : 'api:DiffErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:ExplicitlyCopiedKeyError',
                              'api:key' : Key,
                              'api:before' : Before,
                              'api:after' : After
                            },
             'api:message' : Msg
            }.
api_error_jsonld_(role,error(no_unique_id_for_role_name(Name),_), JSON) :-
    format(string(Msg), "There is more than one id for role ~s. Consider deleting duplicates if you want to refer to them by name rather than id.", [Name]),
    JSON = _{'@type' : 'api:RoleErrorResponse',
             'api:status' : "api:not_found",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoUniqueIdForRoleName",
                              'api:role_name' : Name}
            }.
api_error_jsonld_(role,error(no_id_for_role_name(Name),_), JSON) :-
    format(string(Msg), "There is no role with the name ~s.", [Name]),
    JSON = _{'@type' : 'api:RoleErrorResponse',
             'api:status' : "api:not_found",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoIdForRoleName",
                              'api:role_name' : Name}
            }.
api_error_jsonld_(role,error(can_not_insert_existing_object_with_id(Id),_),JSON) :-
    format(string(Msg), "A role with the id '~s' already exists.  Consider renaming, deleting the old role, or updating the old role.", [Id]),
    JSON = _{'@type' : 'api:RoleErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoUniqueIdForRoleName",
                              'api:role_id' : Id}
            }.
api_error_jsonld_(role,
                  error(
                      schema_check_failure(
                          [json{'http://terminusdb.com/schema/system#action':
                                List}]), _), JSON) :-
    (   List = [Error]
    ->  get_dict(value, Error, Value),
        format(string(Msg), "The action ~s is not a valid Action type for the system schema", [Value]),
        Values = [Value]
    ;   maplist([Error, Value]>>get_dict(value,Error,Value),
                List, Values),
        format(string(Msg), "One of the actions ~q is not a valid Action type for the system schema",
               [Values])
    ),
    JSON = _{'@type' : 'api:RoleErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:InvalidActionType",
                              'api:action' : Values}
            }.
api_error_jsonld_(organization,error(no_id_for_organization_name(Name),_), JSON) :-
    format(string(Msg), "There is no organization with the name ~s.", [Name]),
    JSON = _{'@type' : 'api:OrganizationErrorResponse',
             'api:status' : "api:not_found",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoIdForOrganizationName",
                              'api:organization_name' : Name}
            }.
api_error_jsonld_(organization,error(no_id_for_user_name(Name),_), JSON) :-
    format(string(Msg), "There is no user with the name ~s.", [Name]),
    JSON = _{'@type' : 'api:OrganizationErrorResponse',
             'api:status' : "api:not_found",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoIdForRoleName",
                              'api:user_name' : Name}
            }.
api_error_jsonld_(organization,error(can_not_insert_existing_object_with_id(Id),_),JSON) :-
    format(string(Msg), "An organization with the id '~s' already exists.  Consider renaming, deleting the old role, or updating the old role.", [Id]),
    JSON = _{'@type' : 'api:OrganizationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoUniqueIdForOrganizationName",
                              'api:organization_id' : Id}
            }.
api_error_jsonld_(organization,
                  error(
                      schema_check_failure([witness{'@type':instance_not_cardinality_one,
                                                    class:'http://terminusdb.com/schema/system#Resource',
                                                    instance:Capability,
                                                    predicate:'http://terminusdb.com/schema/system#scope'}]), _), JSON) :-
    format(string(Msg), "The organization can not be removed as it is referred to by a capability. Remove the grant of this capability to the organization before removing.", []),
    JSON = _{'@type' : 'api:OrganizationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:DanglingOrganizationReferencedError",
                              'api:capability' : Capability}
            }.
api_error_jsonld_(organization,error(organization_still_has_databases(Organization,Databases),_),JSON) :-
    format(string(Msg), "The organization ~s still has databases: ~w.", [Organization,Databases]),
    JSON = _{'@type' : 'api:OrganizationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:OrganizationRefersToDatabases",
                              'api:organization' : Organization,
                              'api:databases' : Databases}
            }.
api_error_jsonld_(user,error(no_id_for_user_name(Name),_), JSON) :-
    format(string(Msg), "No id associated with user name ~s", [Name]),
    JSON = _{'@type' : 'api:UserErrorResponse',
             'api:status' : "api:not_found",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoIdForUserName",
                              'api:user_name' : Name}
            }.
api_error_jsonld_(user,error(can_not_delete_super_user,_), JSON) :-
    format(string(Msg), "You can not delete the super user!", []),
    JSON = _{'@type' : 'api:UserErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:CanNotDeleteSuperUser"}
            }.
api_error_jsonld_(user,error(can_not_insert_existing_object_with_id(Id),_), JSON) :-
    format(string(Msg), "A user with this name already exists with id ~s", [Id]),
    JSON = _{'@type' : 'api:UserErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoUniqueIdForUser",
                              'api:user_id' : Id}
            }.
api_error_jsonld_(capability,error(no_role_to_revoke,_), JSON) :-
    format(string(Msg), "You must specify a role to revoke", []),
    JSON = _{'@type' : 'api:CapabilityErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoRoleToRevoke" }
            }.
api_error_jsonld_(capability,error(no_id_for_user_name(Name),_), JSON) :-
    format(string(Msg), "No id associated with user name ~s", [Name]),
    JSON = _{'@type' : 'api:CapabilityErrorResponse',
             'api:status' : "api:not_found",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoIdForUserName",
                              'api:user_name' : Name}
            }.
api_error_jsonld_(capability,error(no_id_for_resource_name(Name), _), JSON) :-
    format(string(Msg), "No id associated with resource name ~s", [Name]),
    JSON = _{'@type' : 'api:CapabilityErrorResponse',
             'api:status' : "api:not_found",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoIdForResourceName",
                              'api:resource_name' : Name}
            }.
api_error_jsonld_(capability,error(no_id_for_role_name(Name), _), JSON) :-
    format(string(Msg), "No id associated with role name ~s", [Name]),
    JSON = _{'@type' : 'api:CapabilityErrorResponse',
             'api:status' : "api:not_found",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoIdForRoleName",
                              'api:role_name' : Name}
            }.
api_error_jsonld_(capability,error(no_unique_id_for_database_name(Name),_), JSON) :-
    format(string(Msg), "There is more than one id for database ~s. Consider deleting duplicates if you want to refer to them by name rather than id, or specify the organization.", [Name]),
    JSON = _{'@type' : 'api:CapabilityErrorResponse',
             'api:status' : "api:not_found",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoUniqueIdForDatabaseName",
                              'api:database_name' : Name}
            }.
api_error_jsonld_(capability,error(deleted_roles_do_not_exist_in_capability(Roles,Capability),_),JSON) :-
    format(string(Msg), "Deleted roles ~q do not exist for capability ~s", [Roles,Capability]),
    JSON = _{'@type' : 'api:CapabilityErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:RevokedRolesDoNotExistForCapability",
                              'api:roles' : Roles,
                              'api:capability' : Capability }
            }.
api_error_jsonld_(capability,error(no_capability_for_user_with_scope(User,Scope),_), JSON) :-
    format(string(Msg), "There was no capability for the user ~s with scope ~s", [User, Scope]),
    JSON = _{'@type' : 'api:CapabilityErrorResponse',
             'api:status' : "api:not_found",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoCapabilityForUserWithScope",
                              'api:user' : User,
                              'api:scope' : Scope }
            }.
api_error_jsonld_(patch,error(patch_conflicts(Conflicts)), JSON) :-
    format(string(Msg), "The patch did not apply cleanly because of the attached conflicts", []),
    JSON = _{'@type' : 'api:PatchResponse',
             'api:status' : "api:conflict",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:PatchConflict",
                              'api:conflicts' : Conflicts }
            }.
api_error_jsonld_(woql, Error, JSON) :-
    api_document_error_jsonld(woql, Error, JSON).
api_error_jsonld_(access_documents, Error, JSON) :-
    api_document_error_jsonld(access_documents, Error, JSON).
api_error_jsonld_(get_documents, Error, JSON) :-
    api_document_error_jsonld(get_documents, Error, JSON).
api_error_jsonld_(insert_documents, Error, JSON) :-
    api_document_error_jsonld(insert_documents, Error, JSON).
api_error_jsonld_(replace_documents, Error, JSON) :-
    api_document_error_jsonld(replace_documents, Error, JSON).
api_error_jsonld_(delete_documents, Error, JSON) :-
    api_document_error_jsonld(delete_documents, Error, JSON).
api_error_jsonld_(diff, Error, JSON) :-
    api_document_error_jsonld(diff, Error, JSON).
api_error_jsonld_(log, error(resource_has_no_history(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "Resource has no history, so log unavailable: ~s", [Path]),
    JSON = _{'@type' : 'api:LogErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:ResourceHasNoHistory",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld_(migration, error(no_migration_at_commit(Commit_Id), _), JSON) :-
    format(string(Msg), "Unable to find a migration for schema change at commit id: ~q", [Commit_Id]),
    JSON = _{'@type' : 'api:MigrationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoMigrationAtCommit",
                              'api:commit_id' : Commit_Id}
            }.
api_error_jsonld_(migration, error(bad_cast_in_schema_migration(Class,Property,Old_Type,New_Type), _), JSON) :-
    format(string(Msg), "Unable to cast value from ~q to ~q in class ~q on property ~q", [Old_Type,New_Type,Class,Property]),
    JSON = _{'@type' : 'api:MigrationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:MigrationCastFailure",
                              'api:class' : Class,
                              'api:property' : Property,
                              'api:new_type' : New_Type,
                              'api:old_type' : Old_Type }
            }.
api_error_jsonld_(migration, error(no_common_history, _), JSON) :-
    format(string(Msg), "Unable to find a common history between these two resources", []),
    JSON = _{'@type' : 'api:MigrationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoCommonHistory"}
            }.
api_error_jsonld_(migration, error(no_common_migration_prefix(Our_Migration,Their_Migration), _), JSON) :-
    format(string(Msg), "No common prefix between migrations", []),
    JSON = _{'@type' : 'api:MigrationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoCommonMigrationPrefix",
                              'api:their_migration' : Their_Migration,
                              'api:our_migration' : Our_Migration
                            }
            }.
api_error_jsonld_(migration, error(schema_operation_failed(Op, Before), _), JSON) :-
    format(string(Msg), "The schema operation '~q' failed and could not be performed", [Op]),
    JSON = _{'@type' : 'api:MigrationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:SchemaOperationFailed",
                              'api:schema_operation' : Op,
                              'api:schema' : Before
                            }
            }.
api_error_jsonld_(migration, error(class_already_exists(Class), _), JSON) :-
    format(string(Msg), "It is impossible to move to the class ~q as it already exists", [Class]),
    JSON = _{'@type' : 'api:MigrationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:ClassExists",
                              'api:class' : Class
                            }
            }.
api_error_jsonld_(migration, error(not_an_irrefutable_weakening_operation(
                                      upcast_class_property,
                                      Class,
                                      Property,
                                      New_Type), _), JSON) :-
    atom_json_dict(Type, New_Type, []),
    format(string(Msg), "It is impossible to upcast the property ~q on class ~q to a stricter type ~q", [Class,Property,Type]),
    JSON = _{'@type' : 'api:MigrationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UpcastNotASchemaWeakening",
                              'api:class' : Class,
                              'api:property' : Property,
                              'api:new_type' : New_Type
                            }
            }.
api_error_jsonld_(migration, error(not_irrefutable_property_creation(Class, Property, Type), _), JSON) :-
    atom_json_dict(Type_Atom, Type, []),
    format(string(Msg), "It is impossible to prove that the creation of the property ~q on the class ~q with type ~q will be valid, as it may not match user data", [Class, Property, Type_Atom]),
    JSON = _{'@type' : 'api:MigrationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:PropertyCreationNotASchemaWeakening",
                              'api:class' : Class,
                              'api:property' : Property,
                              'api:new_type' : Type
                            }
            }.
api_error_jsonld_(migration, error(not_implemented(Operation), _), JSON) :-
    format(string(Msg), "The operation ~q is not yet implemented", [Operation]),
    JSON = _{'@type' : 'api:MigrationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:OperationNotImplemented",
                              'api:operation' : Operation
                            }
            }.
api_error_jsonld_(migration, error(instance_operation_failed(Operation), _), JSON) :-
    format(string(Msg), "The operation ~q failed for an unknown reason", [Operation]),
    JSON = _{'@type' : 'api:MigrationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:InstanceOperationFailure",
                              'api:operation' : Operation
                            }
            }.
api_error_jsonld_(migration, error(weakening_failure(Witness), _), JSON) :-
    get_dict(message,Witness,Message),
    format(string(Msg), "Weakening failure: ~s", [Message]),
    JSON = _{'@type' : 'api:MigrationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:WeakeningFailure",
                              'api:witness' : Witness
                            }
            }.
api_error_jsonld_(migration, error(unknown_schema_migration_operation(Operation), _), JSON) :-
    atom_json_dict(OpString, Operation, []),
    format(string(Msg), "Unknown or poorly formed schema migration operation: ~s", [OpString]),
    JSON = _{'@type' : 'api:MigrationErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnknownSchemaMigrationOperation",
                              'api:operation' : Operation
                            }
            }.
api_error_jsonld_(index, error(handlebars_template_error(Msg, Line, Character)), JSON) :-
    JSON = _{'@type' : 'api:IndexErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:HandlebarsTemplateError",
                              'api:line' : Line,
                              'api:character': Character
                            }
            }.
api_error_jsonld_(index, error(indexing_requires_superuser), JSON) :-
    JSON = _{'@type' : 'api:IndexErrorResponse',
             'api:status' : "api:failure",
             'api:message' : "Indexing requires superuser authority",
             'api:error' : _{ '@type' : "api:IndexingRequiresSuperuserAuthorityError"}
            }.
api_error_jsonld_(concat, error(instance_layer_missing_in_merged_data(Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(String, Descriptor),
    format(string(Msg), "One of the descriptors used in the merge operation did not have an associated instance layer: ~s", [String]),
    JSON = _{'@type' : 'api:ConcatErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:InstanceLayerMissingInConcat",
                              'api:descriptor' : String
                            }
            }.
api_error_jsonld_(concat, error(not_a_base_layer(Layer, Descriptor), _), JSON) :-
    resolve_absolute_string_descriptor(String, Descriptor),
    format(string(Msg), "One of the descriptors (~s) used in the merge operation had an instance layer which is not a base layer: ~s", [String, Layer]),
    JSON = _{'@type' : 'api:ConcatErrorResponse',
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NotABaseLayer",
                              'api:descriptor' : String,
                              'api:layer' : Layer
                            }
            }.

error_type(API, Type) :-
    do_or_die(
        error_type_(API, Type),
        error(unrecognized_error_type(API), _)
    ).

error_type_(add_organization, 'api:AddOrganizationErrorResponse').
error_type_(bundle, 'api:BundleErrorResponse').
error_type_(check_db, 'api:DbExistsErrorResponse').
error_type_(clone, 'api:CloneErrorResponse').
error_type_(create_db, 'api:DbCreateErrorResponse').
error_type_(csv, 'api:CsvErrorResponse').
error_type_(list_db, 'api:DbDeleteErrorResponse').
error_type_(delete_db, 'api:DbDeleteErrorResponse').
error_type_(delete_documents, 'api:DeleteDocumentErrorResponse').
error_type_(delete_organization, 'api:DeleteOrganizationErrorResponse').
error_type_(fetch, 'api:FetchErrorResponse').
error_type_(frame, 'api:FrameErrorResponse').
error_type_(access_documents, 'api:AccessDocumentErrorResponse').
error_type_(get_documents, 'api:GetDocumentErrorResponse').
error_type_(insert_documents, 'api:InsertDocumentErrorResponse').
error_type_(optimize, 'api:OptimizeErrorResponse').
error_type_(pack, 'api:PackErrorResponse').
error_type_(prefix, 'api:PrefixErrorResponse').
error_type_(pull, 'api:PullErrorResponse').
error_type_(push, 'api:PushErrorResponse').
error_type_(remote, 'api:RemoteErrorResponse').
error_type_(replace_documents, 'api:ReplaceDocumentErrorResponse').
error_type_(reset, 'api:ResetErrorResponse').
error_type_(rollup, 'api:RollupErrorResponse').
error_type_(squash, 'api:SquashErrorResponse').
error_type_(unbundle, 'api:UnbundleErrorResponse').
error_type_(unpack, 'api:UnpackErrorResponse').
error_type_(woql, 'api:WoqlErrorResponse').
error_type_(user, 'api:UserErrorResponse').
error_type_(organization, 'api:OrganizationErrorResponse').
error_type_(role, 'api:RoleErrorResponse').
error_type_(capability, 'api:CapabilityErrorResponse').
error_type_(log, 'api:LogErrorResponse').
error_type_(history, 'api:HistoryErrorResponse').
error_type_(update_db, 'api:DbUpdateErrorResponse').
error_type_(branch, 'api:BranchErrorResponse').
error_type_(triples, 'api:TriplesErrorResponse').
error_type_(diff, 'api:DiffErrorResponse').
error_type_(apply, 'api:ApplyErrorResponse').
error_type_(toplevel, 'api:TopLevelResponse').
error_type_(patch, 'api:PatchErrorResponse').
error_type_(migration, 'api:MigrationErrorResponse').
error_type_(concat, 'api:ConcatErrorResponse').

% Graph <Type>
api_error_jsonld(graph,error(invalid_absolute_graph_descriptor(Path),_), Type, JSON) :-
    format(string(Msg), "The following absolute graph descriptor string is invalid: ~q", [Path]),
    JSON = _{'@type' : Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:BadAbsoluteGraphDescriptor',
                              'api:absolute_graph_descriptor' : Path},
             'api:message' : Msg
            }.
api_error_jsonld(graph,error(not_a_branch_descriptor(Descriptor),_), Type, JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The path ~s is not a branch descriptor", [Path]),
    JSON = _{'@type' : Type,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NotABranchDescriptorError",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld(graph,error(unresolvable_absolute_descriptor(Descriptor), _), Type, JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The path ~q can not be resolve to a resource", [Path]),
    JSON = _{'@type' : Type,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld(graph,error(branch_does_not_exist(Descriptor), _), Type, JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The branch does not exist for ~q", [Path]),
    JSON = _{'@type' : Type,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path}
            }.
api_error_jsonld(graph,error(graph_already_exists(Descriptor,Graph_Name), _), Type, JSON) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The branch ~q already has a graph named ~q", [Path, Graph_Name]),
    JSON = _{'@type' : Type,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:BranchAlreadyExists",
                              'api:graph_name' : Graph_Name,
                              'api:absolute_descriptor' : Path}
            }.

document_error_type(access_documents, 'api:AccessDocumentErrorResponse').
document_error_type(get_documents, 'api:GetDocumentErrorResponse').
document_error_type(insert_documents, 'api:InsertDocumentErrorResponse').
document_error_type(replace_documents, 'api:ReplaceDocumentErrorResponse').
document_error_type(delete_documents, 'api:DeleteDocumentErrorResponse').
document_error_type(diff, 'api:DiffErrorResponse').
document_error_type(apply, 'api:ApplyErrorResponse').
document_error_type(woql, 'api:WoqlErrorResponse').
%document_error_type(patch, 'api:PatchErrorResponse').

api_document_error_jsonld(Type,error(unable_to_elaborate_schema_document(Document),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "The submitted schema document could not be elaborated due to an unknown syntax error.", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:DocumentElaborationImpossible',
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(invalid_path(Path), _), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Resource path invalid: ~q", [Path]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:InvalidResourcePath',
                              'api:resource_path' : Path },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(unresolvable_collection(Descriptor),_), JSON) :-
    document_error_type(Type, JSON_Type),
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor could not be resolved to a resource: ~q", [Path]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:not_found',
             'api:error' : _{ '@type' : 'api:UnresolvableAbsoluteDescriptor',
                              'api:absolute_descriptor' : Path},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(branch_does_not_exist(Descriptor), _), JSON) :-
    document_error_type(Type, JSON_Type),
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The branch does not exist for ~q", [Path]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                              'api:absolute_descriptor' : Path}
            }.
api_document_error_jsonld(Type, error(same_ids_in_one_transaction(Ids), _), JSON) :-
    document_error_type(Type, JSON_Type),

    format(string(Msg), "Tried to mutate document with same id multiple times", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:SameDocumentIdsMutatedInOneTransaction',
                              'api:duplicate_ids' : Ids},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(document_access_impossible(Descriptor, Graph_Type, Read_Write), _), JSON) :-
    document_error_type(Type, JSON_Type),
    resolve_absolute_string_descriptor(Descriptor_String, Descriptor),
    format(string(Msg), "action '~q' on graph type ~q is impossible on resource ~q", [Read_Write, Graph_Type, Descriptor_String]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:DocumentAccessImpossible',
                              'api:resource_path' : Descriptor_String,
                              'api:graph_type': Graph_Type,
                              'api:access_type': Read_Write},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(syntax_error(json(What)), _), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Submitted JSON data is invalid", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:JSONInvalid'},
             'api:message' : Msg,
             'api:what': What
            }.
api_document_error_jsonld(Type, error(inserting_context(Document), _), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Inserting contexts is not allowed without using a 'full replace'.", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:InsertingContext',
                              'api:document' : Document},
             'api:message' : Msg}.
api_document_error_jsonld(Type, error(missing_field(Field, Document), _), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Missing '~s' field in submitted document.", [Field]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:MissingField',
                              'api:field' : Field,
                              'api:document' : Document},
             'api:message' : Msg}.
api_document_error_jsonld(Type, error(type_not_found(Document_Type, Document), _), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Type in submitted document not found in the schema: ~q", [Document_Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:TypeNotFound',
                              'api:type' : Document_Type,
                              'api:document' : Document},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(unexpected_array_value(Value, Expected_Type),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Unexpected array value: ~q, expected type: ~q", [Value, Expected_Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:UnexpectedArrayValue',
                              'api:value' : Value,
                              'api:expected_type' : Expected_Type },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(unexpected_boolean_value(Value, Expected_Type),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Unexpected boolean value: ~q, expected type: ~q", [Value, Expected_Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:UnexpectedBooleanValue',
                              'api:value' : Value,
                              'api:expected_type' : Expected_Type },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(schema_check_failure(Witnesses),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Schema check failure", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:SchemaCheckFailure',
                              'api:witnesses' : Witnesses },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(schema_type_unknown(Schema_Type),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "The '@type' field referred to an unimplemented schema type.", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:UnknownSchemaType',
                              'api:type' : Schema_Type },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(id_could_not_be_elaborated(Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "No '@id' field found in document, and it could not be determined from its type definition", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:IdCouldNotBeDetermined',
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(submitted_document_id_does_not_have_expected_prefix(Submitted_ID, Prefix, Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Document id ~q does not have expected prefix ~q", [Submitted_ID, Prefix]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:SubmittedDocumentIdDoesNotHaveExpectedPrefix',
                              'api:submitted_id': Submitted_ID,
                              'api:prefix': Prefix,
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(submitted_id_does_not_match_generated_id(Submitted_ID, Generated_ID, Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Document was submitted with id ~q, but id ~q was generated", [Submitted_ID, Generated_ID]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:SubmittedIdDoesNotMatchGeneratedId',
                              'api:submitted_id': Submitted_ID,
                              'api:generated_id': Generated_ID,
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(unrecognized_property(Document_Type, Property, _Value, Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Submitted document contained unrecognized property ~q for type ~q", [Property, Document_Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:UnrecognizedProperty',
                              'api:property': Property,
                              'api:type': Document_Type,
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(casting_error(Value, Destination_Type, Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "value ~q could not be casted to a ~q", [Value, Destination_Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:BadCast',
                              'api:value' : Value,
                              'api:type' : Destination_Type,
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(can_not_replace_at_hashed_id(Document), _),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "JSON documents with a hash value id can not be replaced as they are immutable.", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:CanNotReplaceImmutable',
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_is_only_supported_for_instance_graphs,_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query documents are currently only supported for instance graphs", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryDocumentOnlySupportedForInstanceGraphs'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_is_only_supported_for_one_graph_type,_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query documents are currently only supported for a single graph type", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryDocumentOnlySupportedForOneGraphType'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_error(unknown_prefix(_Query)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query document used an undefined prefix", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryDocumentHasUndefinedPrefix'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_error(unrecognized_query_document(_Query)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query document has an unrecognised form", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryUnrecognized'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_error(missing_type(Query)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Missing type for query: ~q", [Query]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryMissingType'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_error(unknown_property_for_type(Type, Prop)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query contains an unrecognised property ~q for type ~q", [Prop, Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryUnrecognizedPropertyForType'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_error(casting_error(Query, Type)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query contains a value ~q which can not be cast to the given type ~q", [Query, Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryCastingError'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_error(regex_not_a_string(Regex)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query contains a regex ~q which is not a string", [Regex]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryRegexNotString'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_error(regex_not_valid(Regex)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query contains a regex ~q which is not valid syntax", [Regex]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryRegexInvalid'},
             'api:message' : Msg
            }.
api_document_error_jsonld(get_documents,error(query_error(regex_against_non_string(Type, Regex)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query contains a regex ~q which is attempting to match a ~q", [Regex,Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryRegexNotAString'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_error(unknown_query_format_for_datatype(Type, Query)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query format ~q is invalid for type ~q", [Query,Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryInvalidFormat'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_error(unknown_type(Type)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query has an unknown type ~q", [Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryUnknownType'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_error(not_a_query_document_list(Documents)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query did not specify a list of documents in '@one-of': ~q", [Documents]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryOneOfNotDocumentList'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_error(not_a_subclass(Type, Query_Type_Ex)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query provided a type ~q which was not subsumed by documents of type ~q.", [Query_Type_Ex, Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryTypeNotSubsumed'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(query_error(type_not_found(Query_Type)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query provided a type ~q which was not found in the schema.", [Query_Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryTypeNotFound'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(query_error(not_a_dict(Query)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query provided is not a dict: ~q", [Query]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryNotADict'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(query_error(not_a_dict(Query)),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query provided is not a dict: ~q", [Query]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:QueryNotADict'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(document_insertion_failed_unexpectedly(Document),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Document insertion failed unexpectedly", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:server_error',
             'api:error' : _{ '@type' : 'api:DocumentInsertionFailedUnexpectedly',
                              'api:document': Document},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(raw_json_and_schema_disallowed,_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Document insertion of raw JSON documents is not possible for the schema graph", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:DocumentInsertionRawJSONSchemaDisallowed'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(not_a_valid_json_object_id(Id),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Document insertion of raw JSON documents is not possible for the schema graph", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:DocumentInsertionInvalidJSONDocumentId',
                              'api:id' : Id},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(can_not_insert_class_with_reserve_name(Id),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Document for class can not be inserted due to use of reserved name", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:DocumentInsertionReservedName',
                              'api:id': Id},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(document_insertion_failed_unexpectedly(Document),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Query documents are currently only supported for instance graphs", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:DocumentInsertionFailedUnexpectedly',
                              'api:document': Document},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(can_not_insert_existing_object_with_id(Id, Document), _), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Tried to insert a new document with id ~q, but an object with that id already exists", [Id]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:DocumentIdAlreadyExists',
                              'api:document_id' : Id,
                              'api:document' : Document},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(unknown_language_tag(Tag,Document), _), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Unknown language tag used ~q", [Tag]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:UnknownLanguageTag',
                              'api:language' : Tag,
                              'api:document' : Document},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(no_language_tag_for_multilingual(Document), _), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Multiple languages are used, but not all with language tags", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:NoLanguageTagForMultilingual',
                              'api:document' : Document},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(language_tags_repeated(Tags,Document),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "The same language tags ~q were used repeatedly", [Tags]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:LanguageTagsRepeated',
                              'api:languages' : Tags,
                              'api:document' : Document},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(no_context_found_in_schema, _), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "No context found in submitted schema", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:NoContextFoundInSchema'},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(invalid_enum_values(Values), _), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Enum values are not valid", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:InvalidEnumValues',
                              'api:document' : Values },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(missing_targets, _), JSON) :-
    document_error_type(Type, JSON_Type),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:MissingTargets' },
             'api:message' : "Missing target(s) for deletion"
            }.
api_document_error_jsonld(Type, error(unable_to_assign_ids(Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Unable to assign ids for document.", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:UnableToAssignIdsToDocument',
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(document_key_type_unknown(Key_Type, Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Document @key type unknown: ~q. It must be ValueHash, Hash, Lexical, or Random.", [Key_Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:DocumentKeyTypeUnknown',
                              'api:key_type': Key_Type,
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(document_key_type_missing(Key, Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Document @key missing @type: ~q", [Key]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:DocumentKeyTypeMissing',
                              'api:key': Key,
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(subdocument_key_missing(Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Subdocument @key missing", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:SubdocumentKeyMissing',
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(document_key_not_object(Key_Value, Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Document @key value is not an object: ~q", [Key_Value]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:DocumentKeyNotObject',
                              'api:key_value' : Key_Value,
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(key_missing_required_field(Field,Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "The required field ~q is missing from the submitted document", [Field]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:RequiredKeyFieldMissing',
                              'api:field' : Field,
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(empty_key(Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Empty key found in the submitted document", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:EmptyKey',
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(bad_field_value(Field, Value, Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Unexpected value ~q found in the field ~q of the submitted document", [Value, Field]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:BadFieldValue',
                              'api:field' : Field,
                              'api:value' : Value,
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(key_missing_fields(Key_Type, Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Missing required @fields array for key type ~q", [Key_Type]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:KeyMissingFields',
                              'api:key_type' : Key_Type,
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(key_fields_not_an_array(Fields, Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Key @fields value is not an array.", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:KeyFieldsNotAnArray',
                              'api:fields' : Fields,
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(key_fields_is_empty(Document),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Key @fields value is empty.", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:KeyFieldsIsEmpty',
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(not_all_captures_found(Refs),_),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Ids were referenced but never captured.", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:NotAllCapturesFound',
                              'api:captures' : Refs },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(inserted_subdocument_as_document(Document), _),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Tried to insert a subdocument as a document without specifying backlinks.", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:InsertedSubdocumentAsDocument',
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(capture_already_bound(Capture_Id, Document), _), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Tried to insert a document with capture id ~q which was bound earlier.", [Capture_Id]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:CaptureIdAlreadyBound',
                              'api:capture': Capture_Id,
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(wrong_array_dimensions(Array,Dimensions,Document),_), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Document insertion failed as array ~q had wrong dimension ~q", [Array,Dimensions]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:DocumentArrayWrongDimensions',
                              'api:array' : Array,
                              'api:dimensions' : Dimensions,
                              'api:document': Document},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(not_a_unit_type(Value, Document), _), JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Unit value given is not a unit type: ~q", [Value]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : 'api:failure',
             'api:error' : _{ '@type' : 'api:NotAUnitType',
                              'api:value' : Value,
                              'api:document': Document},
             'api:message' : Msg
            }.
api_document_error_jsonld(Type,error(key_has_unknown_prefix(Prefixed_Key), _), JSON) :-
    document_error_type(Type, Type_Displayed),
    format(string(Msg), "The key '~w' is utilising an unknown prefix.", [Prefixed_Key]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:UnknownPrefixError",
                              'api:key' : Prefixed_Key}
            }.
api_document_error_jsonld(Type,error(json_document_as_range(Pred,Range), _), JSON) :-
    document_error_type(Type, Type_Displayed),
    format(string(Msg), "The range '~w' used on '~w' is illegal, use 'sys:JSON'.", [Range,Pred]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:JSONDocumentInvalidRangeError",
                              'api:field' : Pred}
            }.
api_document_error_jsonld(Type,error(casting_error(Elt,Type),_), JSON) :-
    document_error_type(Type, Type_Displayed),
    format(string(Msg), "Could not cast the value ~q to the type ~q.", [Elt,Type]),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:CastingError",
                              'api:value' : Elt,
                              'api:type' : Type
                            }
            }.
api_document_error_jsonld(Type,error(no_property_specified_in_link(Parent,Document),_), JSON) :-
    document_error_type(Type, Type_Displayed),
    format(string(Msg), "Back links were used with no property specified.", []),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoPropertySpecified",
                              'api:parent' : Parent,
                              'api:document' : Document
                            }
            }.
api_document_error_jsonld(Type,error(no_ref_or_id_in_link(Parent,Document),_), JSON) :-
    document_error_type(Type, Type_Displayed),
    format(string(Msg), "Back links were used with no ref or id.", []),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoRefOrIdSpecified",
                              'api:parent' : Parent,
                              'api:document' : Document
                            }
            }.
api_document_error_jsonld(Type,error(no_ref_in_link(Parent,Document),_), JSON) :-
    document_error_type(Type, Type_Displayed),
    format(string(Msg), "Back links were used with no ref.", []),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NoRefSpecified",
                              'api:parent' : Parent,
                              'api:document' : Document
                            }
            }.
api_document_error_jsonld(Type,error(link_id_specified_but_not_valid(Parent,Document),_), JSON) :-
    document_error_type(Type, Type_Displayed),
    format(string(Msg), "The link Id did not have a valid form.", []),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:LinkIdNotValid",
                              'api:parent' : Parent,
                              'api:document' : Document
                            }
            }.
api_document_error_jsonld(Type,error(not_one_parent_of_subdocument(Parents,Document),_), JSON) :-
    document_error_type(Type, Type_Displayed),
    format(string(Msg), "A sub-document has parent cardinality other than one.", []),
    JSON = _{'@type' : Type_Displayed,
             'api:status' : "api:failure",
             'api:message' : Msg,
             'api:error' : _{ '@type' : "api:NotOneParentForSubdocument",
                              'api:parent' : Parents,
                              'api:document' : Document
                            }
            }.
api_document_error_jsonld(Type, error(embedded_subdocument_has_linked_by(Document), _),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "Tried to insert a subdocument as part of a larger document, while also specifying an '@linked-by'. A subdocument can only have one parent.", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:EmbeddedSubdocumentHasLinkedBy',
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(back_links_not_supported_in_replace(Document), _),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "It is not possible to use '@linked-by' in replace document.", []),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:LinksInReplaceError',
                              'api:document' : Document },
             'api:message' : Msg
            }.
api_document_error_jsonld(Type, error(prefix_does_not_resolve(Prefix,Document), _),JSON) :-
    document_error_type(Type, JSON_Type),
    format(string(Msg), "The prefix ~q used in the context does not resolve to a URI.", [Prefix]),
    JSON = _{'@type' : JSON_Type,
             'api:status' : "api:failure",
             'api:error' : _{ '@type' : 'api:PrefixDoesNotResolveError',
                              'api:prefix' : Prefix,
                              'api:document' : Document },
             'api:message' : Msg
            }.

/**
 * generic_exception_jsonld(Error,JSON) is det.
 *
 * Return a generic JSON-LD object associated with a given error
 *
 */
generic_exception_jsonld(access_not_authorised(Auth,Action,Scope),JSON) :-
    format(string(Msg), "Access to ~q is not authorised with action ~q and auth ~q",
           [Scope,Action,Auth]),
    term_string(Auth, Auth_String),
    term_string(Action, Action_String),
    term_string(Scope, Scope_String),
    JSON = _{'api:status' : 'api:forbidden',
             'api:message' : Msg,
             'auth' : Auth_String,
             'action' : Action_String,
             'scope' : Scope_String
            }.
generic_exception_jsonld(malformed_api_document(Document),JSON) :-
    format(string(Msg), "The input API document was malformed", []),
    JSON = _{'@type' : 'api:BadAPIDocumentErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:MalformedDocument',
                             'api:document' : Document},
             'api:message' : Msg}.
generic_exception_jsonld(bad_api_document(Document,Expected),JSON) :-
    format(string(Msg), "The input API document was missing required fields: ~q", [Expected]),
    JSON = _{'@type' : 'api:BadAPIDocumentErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:RequiredFieldsMissing',
                             'api:expected' : Expected,
                             'api:document' : Document},
             'api:message' : Msg}.
generic_exception_jsonld(bad_api_document_choices(Document,Expected),JSON) :-
    format(string(Msg), "The input API document was missing required fields from one of the following lists: ~q", [Expected]),
    JSON = _{'@type' : 'api:BadAPIDocumentErrorResponse',
             'api:status' : 'api:failure',
             'api:error' : _{'@type' : 'api:RequiredFieldsChoiceMissing',
                             'api:expected' : Expected,
                             'api:document' : Document},
             'api:message' : Msg}.
generic_exception_jsonld(missing_content_type(Expected), JSON) :-
    format(string(Msg), "Missing 'Content-Type' header. Expected value: ~w", [Expected]),
    JSON = _{'@type' : 'api:MissingContentTypeErrorResponse',
             'api:status' : 'api:failure',
             'api:message' : Msg}.
generic_exception_jsonld(missing_content_length, JSON) :-
    JSON = _{'@type' : 'api:MissingContentLengthErrorResponse',
             'api:status' : 'api:failure',
             'api:message' : "Missing 'Content-Length' header."}.
generic_exception_jsonld(bad_content_type(ContentType, Expected), JSON) :-
    format(string(Msg), "Bad 'Content-Type' header value: ~q. Expected value: ~q", [ContentType, Expected]),
    JSON = _{'@type' : 'api:BadContentTypeErrorResponse',
             'api:status' : 'api:failure',
             'api:message' : Msg}.
generic_exception_jsonld(syntax_error(M),JSON) :-
    format(atom(OM), '~q', [M]),
    JSON = _{'api:status' : 'api:failure',
             'system:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                     'vio:literal' : OM}]}.
generic_exception_jsonld(duplicate_key(Key), JSON) :-
    format(string(Msg), "Duplicate field in request: ~q", [Key]),
    JSON = _{'@type' : 'api:DuplicateField',
             'api:status' : 'api:failure',
             'api:message' : Msg}.
generic_exception_jsonld(woql_syntax_error(JSON,Path,Element),JSON) :-
    json_woql_path_element_error_message(JSON,Path,Element,Message),
    reverse(Path,Director),
    JSON = _{'@type' : 'vio:WOQLSyntaxError',
             'api:message' : Message,
             'vio:path' : Director,
             'vio:query' : JSON}.
generic_exception_jsonld(type_error(T,O),JSON) :-
    format(atom(M),'Type error for ~q which should be ~q', [O,T]),
    format(atom(OA), '~q', [O]),
    format(atom(TA), '~q', [T]),
    JSON = _{'api:status' : 'api:failure',
             'api:message' : M,
             'system:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                     'vio:message' : M,
                                     'vio:type' : TA,
                                     'vio:literal' : OA}]}.
generic_exception_jsonld(graph_sync_error(JSON),JSON).
generic_exception_jsonld(time_limit_exceeded,JSON) :-
    JSON = _{'api:status' : 'api:failure',
             'api:message' : 'Connection timed out'
            }.
generic_exception_jsonld(unqualified_resource_id(Doc_ID),JSON) :-
    format(atom(MSG), 'Document resource ~s could not be expanded', [Doc_ID]),
    JSON = _{'api:status' : 'terminus_failure',
             'api:message' : MSG,
             'system:object' : Doc_ID}.
generic_exception_jsonld(unknown_deletion_error(Doc_ID),JSON) :-
    format(atom(MSG), 'unqualfied deletion error for id ~s', [Doc_ID]),
    JSON = _{'api:status' : 'terminus_failure',
             'api:message' : MSG,
             'system:object' : Doc_ID}.
generic_exception_jsonld(schema_check_failure(Witnesses),JSON) :-
    JSON = _{'api:message' : 'Schema check failure',
             'api:status' : 'api:failure',
             'system:witnesses' : Witnesses}.
generic_exception_jsonld(database_not_found(DB),JSON) :-
    format(atom(MSG), 'Database ~s does not exist', [DB]),
    JSON = _{'api:message' : MSG,
             'api:status' : 'api:failure'}.
generic_exception_jsonld(database_files_do_not_exist(DB),JSON) :-
    format(atom(M), 'Database fields do not exist for database with the name ~q', [DB]),
    JSON = _{'api:message' : M,
             'api:status' : 'api:failure'}.
generic_exception_jsonld(database_already_exists(DB),JSON) :-
    format(atom(MSG), 'Database ~s already exists', [DB]),
    JSON = _{'api:status' : 'api:failure',
             'system:object' : DB,
             'api:message' : MSG,
             'system:method' : 'system:create_database'}.
generic_exception_jsonld(database_could_not_be_created(DB),JSON) :-
    format(atom(MSG), 'Database ~s could not be created', [DB]),
    JSON = _{'api:status' : 'api:failure',
             'api:message' : MSG,
             'system:method' : 'system:create_database'}.
generic_exception_jsonld(could_not_create_class_frame(Class),JSON) :-
    format(atom(MSG), 'Class Frame could not be generated for class ~s', [Class]),
    JSON = _{ 'api:message' : MSG,
              'api:status' : 'api:failure',
              'system:class' : Class}.
generic_exception_jsonld(could_not_create_filled_class_frame(Instance),JSON) :-
    format(atom(MSG), 'Class Frame could not be generated for instance ~s', [Instance]),
    JSON = _{ 'api:message' : MSG,
              'api:status' : 'api:failure',
              'system:instance' : Instance}.
generic_exception_jsonld(malformed_json_payload(JSON_String),JSON) :-
    format(atom(MSG), 'Submitted object was not valid JSON', []),
    JSON = _{'api:status' : 'api:failure',
             'api:message' : MSG,
             'system:object' : JSON_String}.
generic_exception_jsonld(no_document_for_key(Key),JSON) :-
    format(atom(MSG), 'No document in request for key ~q', [Key]),
    JSON = _{'api:status' : 'api:failure',
             'api:message' : MSG,
             'system:key' : Key}.
generic_exception_jsonld(no_parameter_key_in_query_parameters(Key,Query_Parameters),JSON) :-
    format(atom(MSG), 'No parameter key ~q in query parameters', [Key]),
    maplist([A=B,A-B]>>true, Query_Parameters, Transformed),
    dict_pairs(Query_Dict, json, Transformed),
    JSON = _{'api:status' : 'api:failure',
             'api:message' : MSG,
             'system:key' : Key,
             'system:query_parameters' : Query_Dict}.
generic_exception_jsonld(no_parameter_key_in_document(Key,Document),JSON) :-
    format(atom(MSG), 'No parameter key ~q in submitted document', [Key]),
    JSON = _{'api:status' : 'api:failure',
             'api:message' : MSG,
             'system:key' : Key,
             'system:object' : Document}.
generic_exception_jsonld(no_parameter_key_for_method(Key,Method),JSON) :-
    format(atom(MSG), 'No parameter key ~q for method ~q', [Key,Method]),
    JSON = _{'api:status' : 'api:failure',
             'api:message' : MSG,
             'system:object' : Key}.
generic_exception_jsonld(no_parameter_key(Key),JSON) :-
    format(atom(MSG), 'No parameter key ~q', [Key]),
    JSON = _{'api:status' : 'api:failure',
             'api:message' : MSG,
             'system:object' : Key}.
generic_exception_jsonld(branch_already_exists(Branch_Name),JSON) :-
    format(string(Msg), "branch ~w already exists", [Branch_Name]),
    JSON = _{'api:status' : 'api:failure',
             'api:message' : Msg}.
generic_exception_jsonld(origin_branch_does_not_exist(Branch_Name),JSON) :-
    format(string(Msg), "origin branch ~w does not exist", [Branch_Name]),
    JSON = _{'api:status' : 'api:failure',
             'api:message' : Msg}.
generic_exception_jsonld(origin_commit_does_not_exist(Commit_Id),JSON) :-
    format(string(Msg), "origin commit ~w does not exist", [Commit_Id]),
    JSON = _{'api:status' : 'api:failure',
             'api:message' : Msg}.
generic_exception_jsonld(origin_cannot_be_branched(Descriptor),JSON) :-
    format(string(Msg), "origin ~w cannot be branched", [Descriptor]),
    JSON = _{'api:status' : 'api:failure',
             'api:message' : Msg}.
generic_exception_jsonld(transaction_retry_exceeded, JSON) :-
    JSON = _{'api:status' : 'api:server_error',
             'api:message': "Transaction retry count exceeded in internal operation"}.

json_http_code(JSON,Code) :-
    Status = (JSON.'api:status'),
    atom_string(Atom_Status,Status),
    status_http_code(Atom_Status,Code).

json_cli_code(JSON,Code) :-
    Status = (JSON.'api:status'),
    atom_string(Atom_Status,Status),
    status_cli_code(Atom_Status,Code).

:- multifile status_http_code/2.
status_http_code('api:success',200).
status_http_code('api:failure',400).
status_http_code('api:unauthorized',401).
status_http_code('api:forbidden',403).
status_http_code('api:not_found',404).
status_http_code('api:method_not_allowed',405).
status_http_code('api:conflict',409).
status_http_code('api:server_error',500).

status_cli_code('api:success',0).
status_cli_code('api:failure',1).
status_cli_code('api:not_found',2).
status_cli_code('api:unauthorized',13).
status_cli_code('api:forbidden',13).
status_cli_code('api:method_not_allowed',126).
status_cli_code('api:server_error',131).

:- begin_tests(error_reporting).

:- use_module(core(query/json_woql)).

test(bad_schema_document, []) :-
    api_error_jsonld(get_documents,
                     error(unable_to_elaborate_schema_document(_{'@type' : 'Garbage'}),_),
                     JSON),
    JSON = _{'@type':'api:GetDocumentErrorResponse',
             'api:error':_{'@type':'api:DocumentElaborationImpossible',
                           'api:document':_{'@type':'Garbage'}},
             'api:message':"The submitted schema document could not be elaborated due to an unknown syntax error.",
             'api:status':"api:failure"}.

:- end_tests(error_reporting).
