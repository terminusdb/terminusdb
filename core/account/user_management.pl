:- module(user_management,
          [
              add_user/5,
              add_user/6,
              agent_name_uri/3,
              agent_name_exists/2,
              delete_user/1,
              delete_user/2,
              delete_organization/1,
              update_user/2,
              update_user/3
          ]).

:- use_module(core(util)).
:- use_module(core(api)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(capabilities).

/** <module> User
 *
 * User management. This is required for testing the capabilities system,
 * but may later be exposed for a user API.
 */

delete_organization(Name) :-
    create_context(system_descriptor{}, Context),
    with_transaction(
        Context,
        (   organization_name_uri(Context, Name, Organization_Uri),
            ask(Context,
                (   t(Organization_Uri, rdf:type, system:'Organization'),
                    delete_object(Organization_Uri)
                ))
        ),
        _).

add_user(Nick, Email, Comment, Pass_Opt, User_URI) :-
    create_context(system_descriptor{},
                   commit_info{
                       author: "add_user/4",
                       message: "internal system operation"
                   }, SystemDB),
    add_user(SystemDB, Nick, Email, Comment, Pass_Opt, User_URI).

add_user(SystemDB, Nick, Email, Comment, Pass_Opt, User_URI) :-

    (   agent_name_exists(SystemDB, Nick)
    ->  throw(error(user_already_exists(Nick)))
    ;   true),

    (   organization_name_exists(SystemDB, Nick)
    ->  throw(error(organization_already_exists(Nick)))
    ;   true),

    with_transaction(
        SystemDB,
        (
            ask(SystemDB,
                (   random_idgen(doc:'User',[Nick^^xsd:string], User_URI),
                    random_idgen(doc:'Role',["founder"^^xsd:string], Role_URI),
                    random_idgen(doc:'Capability',[Nick^^xsd:string], Capability_URI),
                    random_idgen(doc:'Organization',[Nick^^xsd:string], Organization_URI),
                    insert(User_URI, rdf:type, system:'User'),
                    insert(User_URI, rdfs:comment, Comment@en),
                    insert(User_URI, system:agent_name, Nick^^xsd:string),
                    insert(User_URI, system:user_identifier, Email^^xsd:string),
                    insert(User_URI, system:role, Role_URI),
                    insert(Role_URI, rdf:type, system:'Role'),
                    insert(Role_URI, system:capability, Capability_URI),
                    insert(Capability_URI, rdf:type, system:'Capability'),
                    insert(Capability_URI, system:direct_capability_scope, Organization_URI),
                    insert(Capability_URI, system:action, system:create_database),
                    insert(Capability_URI, system:action, system:manage_capabilities),
                    insert(Capability_URI, system:action, system:delete_database),
                    insert(Capability_URI, system:action, system:class_frame),
                    insert(Capability_URI, system:action, system:clone),
                    insert(Capability_URI, system:action, system:fetch),
                    insert(Capability_URI, system:action, system:push),
                    insert(Capability_URI, system:action, system:branch),
                    insert(Capability_URI, system:action, system:rebase),
                    insert(Capability_URI, system:action, system:meta_read_access),
                    insert(Capability_URI, system:action, system:commit_read_access),
                    insert(Capability_URI, system:action, system:instance_read_access),
                    insert(Capability_URI, system:action, system:instance_write_access),
                    insert(Capability_URI, system:action, system:schema_read_access),
                    insert(Capability_URI, system:action, system:schema_write_access),
                    insert(Capability_URI, system:action, system:manage_capabilities),
                    insert(Organization_URI, rdf:type, system:'Organization'),
                    insert(Organization_URI, system:organization_name, Nick^^xsd:string),
                    insert(doc:admin_organization, system:resource_includes, Organization_URI)
                ),
                [compress_prefixes(false)]
               ),

            (   Pass_Opt = some(Pass)
            ->  crypto_password_hash(Pass,Hash),
                ask(SystemDB,
                    insert(User_URI, system:user_key_hash, Hash^^xsd:string)
                   )
            ;   true)
        ),
        _Meta_Data
    ).

update_user(Name, Document) :-
    create_context(system_descriptor{},
                   commit_info{
                       author: "update_user/2",
                       message: "internal system operation"
                   }, SystemDB),
    update_user(SystemDB, Name, Document).

update_user(SystemDB, Name, Document) :-
    with_transaction(
        SystemDB,
        (   agent_name_uri(SystemDB, Name, User_Uri),

            (   get_dict(user_identifier, Document, Identifier)
            ->  ask(SystemDB,
                    (   t(User_Uri, system:user_identifier, Old_ID^^xsd:string),
                        delete(User_Uri, system:user_identifier, Old_ID^^xsd:string),
                        insert(User_Uri, system:user_identifier, Identifier^^xsd:string)))
            ;   true),

            (   get_dict(agent_name, Document, Agent_Name)
            ->  ask(SystemDB,
                    (   t(User_Uri, system:agent_name, Old_Name^^xsd:string),
                        delete(User_Uri, system:agent_name, Old_Name^^xsd:string),
                        insert(User_Uri, system:agent_name, Agent_Name^^xsd:string)))
            ;   true),

            (   get_dict(comment, Document, Comment)
            ->  ask(SystemDB,
                    (   t(User_Uri, rdfs:comment, Old_Comment@en),
                        delete(User_Uri, rdfs:comment, Old_Comment@en),
                        insert(User_Uri, rdfs:comment, Comment@en)))
            ;   true),

            (   get_dict(password, Document, Password)
            ->  crypto_password_hash(Password,Hash),
                ask(SystemDB,
                    (   t(User_Uri, system:user_key_hash, Old_Hash^^xsd:string),
                        delete(User_Uri, system:user_key_hash, Old_Hash^^xsd:string),
                        insert(User_Uri, system:user_key_hash, Hash^^xsd:string)))
            ;   true)
        ),
        _).


/*
 * delete_user(+User_ID) is semidet.
 */
delete_user(User_Name) :-
    create_context(system_descriptor{}, Context),
    delete_user(Context, User_Name).

delete_user(Context, User_Name) :-
    with_transaction(
        Context,
        (   user_name_uri(Context, User_Name, User_URI),
            ask(Context,
                (   t(User_URI, rdf:type, system:'User'),
                    delete_object(User_URI)
                ))
        ),
        _).


:- begin_tests(user_management).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).

test(add_user, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    add_user("Gavin", "gavin@terminusdb.com", "here.i.am",  some('password'), URI),

    agent_name_uri(system_descriptor{}, "Gavin", URI),

    once(ask(system_descriptor{},
             t(URI, system:user_identifier, "gavin@terminusdb.com"^^xsd:string))).


test(test_user_ownership, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    Name = "Gavin",
    add_user(Name, "gavin@terminusdb.com", "here.i.am", some('password'), User_URI),

    create_db_without_schema(Name, "test"),

    open_descriptor(system_descriptor{}, System),

    resolve_absolute_string_descriptor("Gavin/test", Descriptor),

    create_context(Descriptor, Context1),
    Commit_Info = commit_info{ author: "Gavin",
                               message : "Testing" },
    merge_dictionaries(
        query_context{
            commit_info : Commit_Info,
            files : Files,
            system: System,
            update_guard : _Guard0,
            authorization : User_URI
        }, Context1, Context1_0),

    with_transaction(
        Context1_0,
        ask(Context1_0,
            insert(a, b, c)),
        _),

    create_context(Descriptor, Context2),
    Commit_Info = commit_info{ author: "Gavin",
                               message : "Testing" },

    merge_dictionaries(
        query_context{
            commit_info : Commit_Info,
            files : Files,
            system: System,
            update_guard : _Guard1,
            authorization : User_URI
        }, Context2, _Context2_0),

    once(ask(Descriptor, t(a,b,c))).

test(test_user_update, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ]) :-

    Name = "Gavin",
    add_user(Name, "gavin@datachemist.com", "here.i.am", some("password"), User_URI),

    Agent_Name = "Bill",
    Password = "my_pass_is_strong",
    Comment = "Never again bill",
    Identifier = "gavin@terminusdb.com",

    Document =
    _{ agent_name : Agent_Name,
       password : Password,
       user_identifier : Identifier,
       comment : Comment },

    update_user(Name, Document),

    %findall(P-Q, ask(system_descriptor{}, t(User_URI, P, Q)), PQs),
    %writeq(PQs),
    %nl,
    once(ask(system_descriptor{},
             (   t(User_URI, system:agent_name, Agent_Name^^xsd:string),
                 t(User_URI, system:user_key_hash, Hash^^xsd:string),
                 t(User_URI, rdfs:comment, Comment@en),
                 t(User_URI, system:user_identifier, Identifier^^xsd:string)),
             [compress_prefixes(false)]
            )),
    atom_string(Hash_Atom, Hash),
    crypto_password_hash(Password, Hash_Atom).



:- end_tests(user_management).
