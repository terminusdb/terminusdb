:- module(api_access_control, [
              api_get_roles/3,
              api_get_role_from_name/4,
              api_get_role_from_id/4,
              api_add_role/4,
              api_update_role/3,
              api_delete_role/3,
              api_get_organizations/3,
              api_get_organization_from_name/4,
              api_get_organization_from_id/4,
              api_add_organization/4,
              api_delete_organization/3,
              api_grant_capability/3,
              api_revoke_capability/3,
              api_get_user_from_name/4,
              api_get_resource_from_name/4,
              api_add_user/4,
              api_delete_user/3,
              api_get_users/3,
              api_get_user_from_name/4,
              api_get_user_from_id/4,
              api_update_user_password/4
          ]).

:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(document)).
:- use_module(core(transaction)).
:- use_module(library(plunit)).
:- use_module(library(crypto)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(yall)).

api_get_roles(SystemDB, Auth, Roles) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),
    get_roles(SystemDB, Roles).

get_roles(SystemDB,Roles) :-
    findall(Role,
            ask(SystemDB,
                (   t(Id, rdf:type, '@schema':'Role'),
                    get_document(Id, Role)
                )),
            Roles).

api_get_role_from_id(SystemDB, Auth, Id, Role) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),
    do_or_die(
        get_role_from_id(SystemDB, Id, Role),
        error(no_role_with_given_id(Id), _)
    ).

%% get_role_from_name(+,+,-) is det + error
get_role_from_id(SystemDB, Id, Role) :-
    ask(SystemDB,
        (   t(Id, rdf:type, '@schema':'Role'),
            get_document(Id, Role)
        )).

api_get_role_from_name(SystemDB, Auth, RoleName, Role) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),
    get_role_from_name(SystemDB, RoleName, Role).

%% get_role_from_name(+,+,-) is det + error
get_role_from_name(SystemDB, RoleName, Role) :-
    findall(Role,
            ask(SystemDB,
                (   t(Id, name, RoleName^^xsd:string),
                    t(Id, rdf:type, '@schema':'Role'),
                    get_document(Id, Role)
                )),
            Roles),
    (   Roles = [Role]
    ->  true
    ;   Roles = []
    ->  throw(error(no_id_for_role_name(RoleName), _))
    ;   throw(error(no_unique_id_for_role_name(RoleName),_))
    ).

api_add_role(_, Auth, Role, Id) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),

    % Make sure that we only insert role types.
    dict_field_verifier(
        Role,
        _{ '@id' : (*),
           name: (*),
           action: (*) },
        New_Role
    ),
    put_dict('@type', New_Role, 'Role', Typed_Role),

    create_context(system_descriptor{}, commit_info{author: "admin", message: "API: Add Role"},
                   System_Context),
    with_transaction(
        System_Context,
        insert_document(System_Context,Typed_Role,Id),
        _
    ).

api_update_role(_, Auth, Role) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),

    % Make sure that we only insert role types.
    dict_field_verifier(
        Role,
        _{ '@id' : (*),
           name: (*),
           action: (*)},
        New_Role
    ),
    put_dict('@type', New_Role, 'Role', Typed_Role),

    create_context(system_descriptor{}, commit_info{author: "admin", message: "API: Update Role"},
                   System_Context),

    with_transaction(
        System_Context,
        replace_document(System_Context,Typed_Role,_),
        _
    ).

api_delete_role(_, Auth, Role_Id) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),

    create_context(system_descriptor{}, commit_info{author: "admin", message: "API: Delete Role"},
                   System_Context),

    % Make sure we are actually dealing with a role document
    prefix_expand(Role_Id, _{ '@base' : 'terminusdb://system/data/'
                            }, Expanded_Role_Id),
    do_or_die(
        get_document_uri_by_type(System_Context, 'http://terminusdb.com/schema/system#Role',
                                 Expanded_Role_Id),
        error(document_not_found(Role_Id), _)
    ),

    with_transaction(
        System_Context,
        delete_document(System_Context,Role_Id),
        _
    ).

api_get_organizations(SystemDB, Auth, Organizations) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),
    get_organizations(SystemDB,Organizations).

get_organizations(SystemDB, Organizations) :-
    findall(Organization,
            ask(SystemDB,
                (   t(Id,rdf:type,'@schema':'Organization'),
                    get_document(Id, Organization))),
            Organizations).

api_get_organization_from_name(SystemDB, Auth, Name, Organization) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),
    do_or_die(
        get_organization_from_name(SystemDB, Name, Organization),
        error(no_id_for_organization_name(Name), _)).

get_organization_from_name(SystemDB, Name, Organization) :-
    ask(SystemDB,
        (   t(Id,name,Name^^xsd:string),
            t(Id,rdf:type,'@schema':'Organization'),
            get_document(Id,Organization))).

api_get_organization_from_id(SystemDB, Auth, Id, Organization) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),
    do_or_die(
        get_organization_from_id(SystemDB, Id, Organization),
        error(no_organization_with_given_id(Id), _)).

get_organization_from_id(SystemDB, Id, Organization) :-
    ask(SystemDB,
        (   t(Id,rdf:type,'@schema':'Organization'),
            get_document(Id,Organization))).

api_add_organization(_, Auth, Organization, Id) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),

    % Make sure that we only insert role types.
    dict_field_verifier(
        Organization,
        _{ name: (*) },
        New_Organization
    ),
    put_dict('@type', New_Organization, 'Organization', Typed_Organization),

    create_context(system_descriptor{}, commit_info{author: "admin", message: "API: Add Organization"},
                   System_Context),
    with_transaction(
        System_Context,
        insert_document(System_Context,Typed_Organization,Id),
        _
    ).

api_delete_organization(_, Auth, Organization_Id) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),

    create_context(system_descriptor{}, commit_info{author: "admin", message: "API: Delete Organization"},
                   System_Context),

    % Make sure we are actually dealing with a Organization document
    prefix_expand(Organization_Id,
                  _{ '@base' : 'terminusdb://system/data/' },
                  Expanded_Organization_Id),
    do_or_die(
        get_document_uri_by_type(System_Context, 'http://terminusdb.com/schema/system#Organization',
                                 Expanded_Organization_Id),
        error(document_not_found(Organization_Id), _)
    ),

    with_transaction(
        System_Context,
        delete_document(System_Context,Organization_Id),
        _
    ).

grant_document_to_ids(SystemDB, Auth, Grant_Document, json{ scope: Scope_Id,
                                                            user: User_Id,
                                                            roles: Role_Ids }) :-
    _{ scope: Scope,
       user: User,
       roles: Roles } :< Grant_Document,

    % lookup user
    api_get_user_from_name(SystemDB,Auth,User,User_Object),
    get_dict('@id', User_Object, User_Id),
    % lookup resource
    api_get_resource_from_name(SystemDB,Auth,Scope,Scope_Object),
    get_dict('@id', Scope_Object, Scope_Id),
    % lookup roles
    maplist({SystemDB,Auth}/[Role,Role_Id]>>(
                api_get_role_from_name(SystemDB,Auth,Role,Role_Object),
                get_dict('@id', Role_Object, Role_Id)),
            Roles,Role_Ids).

api_grant_capability(SystemDB, Auth, Grant_Document) :-
    grant_document_to_ids(SystemDB, Auth, Grant_Document, Grant_Document_Ids),
    _{ scope: Scope_Id,
       user: User_Id,
       roles: Role_Ids } :< Grant_Document_Ids,

    create_context(SystemDB,
                   commit_info{author: "admin", message: "API: Add Grant"},
                   System_Context),

    assert_auth_action_scope(System_Context, Auth, '@schema':'Action/manage_capabilities', Scope_Id),

    (   ask(System_Context,
            (   t(User_Id, capability, Capability_Id),
                t(Capability_Id, scope, Scope_Id)))
        % has capability with scope.
    ->  with_transaction(
            System_Context,
            (   get_document(System_Context, Capability_Id, Capability),
                (   get_dict('role', Capability, Old_Role_Ids)
                ->  true
                ;   Old_Role_Ids = []),
                append(Role_Ids,Old_Role_Ids, New_Role_Ids),
                sort(New_Role_Ids,Sorted_Roles),
                put_dict('role',Capability,Sorted_Roles, New_Capability),
                replace_document(System_Context,New_Capability)
            ),
            _)
    ;   % has no capability yet
        with_transaction(
            System_Context,
            (   insert_document(System_Context,
                                json{ '@type' : "Capability",
                                      'scope' : Scope_Id,
                                      'role' : Role_Ids },
                                Capability_Id),
                get_document(System_Context, User_Id, User),
                (   get_dict(capability, User, User_Capabilities)
                ->  true
                ;   User_Capabilities = []),
                put_dict(capability, User, [Capability_Id|User_Capabilities], New_User),
                replace_document(System_Context, New_User)
            ),
            _)
    ).

api_revoke_capability(SystemDB, Auth, Grant_Document) :-
    grant_document_to_ids(SystemDB, Auth, Grant_Document, Grant_Document_Ids),
    _{ scope: Scope_Id,
       user: User_Id,
       roles: Role_Ids } :< Grant_Document_Ids,

    create_context(SystemDB,
                   commit_info{author: "admin", message: "API: Add Grant"},
                   System_Context),

    assert_auth_action_scope(System_Context, Auth, '@schema':'Action/manage_capabilities', Scope_Id),

    (   ask(System_Context,
            (   t(User_Id, capability, Capability_Id),
                t(Capability_Id, scope, Scope_Id)))
        % has capability with scope.
    ->  with_transaction(
            System_Context,
            (   get_document(System_Context, Capability_Id, Capability),
                (   get_dict('role', Capability, Old_Role_Ids)
                ->  true
                ;   Old_Role_Ids = []),
                (   intersection(Old_Role_Ids,Role_Ids,Role_Ids)
                ->  subtract(Old_Role_Ids,Role_Ids,New_Role_Ids),
                    (   New_Role_Ids = [] % may as well remove the capability
                    ->  delete_document(System_Context,Capability_Id)
                    ;   put_dict('role',Capability,New_Role_Ids, New_Capability),
                        replace_document(System_Context,New_Capability)
                    )
                ;   subtract(Role_Ids,Old_Role_Ids,Missing_Role_Ids),
                    throw(
                        error(
                            deleted_roles_do_not_exist_in_capability(Missing_Role_Ids,
                                                                     Capability_Id),
                            _))
                )
            ),
            _)
    ;   throw(error(no_capability_for_user_with_scope(User_Id,Scope_Id), _))
    ).

api_get_resource_from_name(SystemDB,Auth,Name,Resource) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),

    do_or_die(
        get_resource_from_name(SystemDB,Name,Resource),
        error(no_id_for_resource_name(Name), _)).

get_resource_from_name(SystemDB,Name,Resource) :-
    ask(SystemDB,
        (   t(Id,name,Name^^xsd:string),
            isa(Id,'@schema':'Resource'),
            get_document(Id,Resource)
        )).

get_resource_from_id(SystemDB,Id,Resource) :-
    ask(SystemDB,
        (   isa(Id,'@schema':'Resource'),
            get_document(Id,Resource)
        )).

api_add_user(SystemDB,Auth,User,Id) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),

    get_dict(name, User, Name),
    (   get_dict(password, User, Password)
    ->  crypto_password_hash(Password, Hash)
    ;   Hash = null
    ),
    New_User =
    _{
        name : Name,
        key_hash : Hash
    },

    create_context(SystemDB, commit_info{author: "admin", message: "API: Add User"},
                   System_Context),
    with_transaction(
        System_Context,
        insert_document(System_Context,New_User,Id),
        _
    ).

api_delete_user(_, Auth, User_Id) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),

    do_or_die(
        \+ is_super_user(User_Id),
        error(can_not_delete_super_user, _)),

    create_context(system_descriptor{}, commit_info{author: "admin", message: "API: Delete User"},
                   System_Context),

    % Make sure we are actually dealing with a role document
    prefix_expand(User_Id, _{ '@base' : 'terminusdb://system/data/'
                            }, Expanded_User_Id),
    do_or_die(
        get_document_uri_by_type(System_Context, 'http://terminusdb.com/schema/system#User',
                                 Expanded_User_Id),
        error(document_not_found(User_Id), _)
    ),

    with_transaction(
        System_Context,
        delete_document(System_Context,User_Id),
        _
    ).

api_get_user_from_name(SystemDB,Auth,Name,User) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),

    do_or_die(
        get_user_from_name(SystemDB, Name, User),
        error(no_id_for_user_name(Name), _)).

get_user_from_name(SystemDB,Name,User) :-
    ask(SystemDB,
        (   t(Id,name,Name^^xsd:string),
            t(Id,rdf:type,'@schema':'User')
        )),
    get_user_from_id(SystemDB,Id,User).

api_get_user_from_id(SystemDB, Auth, Id, User) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),
    do_or_die(
        get_user_from_id(SystemDB, Id, User),
        error(no_user_with_give_id(Id), _)).

get_user_from_id(SystemDB, Id, User) :-
    ask(SystemDB,
        (   t(Id, rdf:type, '@schema':'User'),
            get_document(Id, User_Raw))),
    del_dict(key_hash, User_Raw, _, User_Begin),

    (   get_dict(capability, User_Begin, Capability_Ids)
    ->  findall(Capability,
                (   member(Capability_Id, Capability_Ids),
                    get_capability_from_id(SystemDB, Capability_Id, Capability)),
                Capabilities),
        put_dict(_{capability:Capabilities}, User_Begin, User)
    ;   User_Begin = User
    ).

get_capability_from_id(SystemDB, Id, Capability) :-
    ask(SystemDB,
        (   t(Id, rdf:type, '@schema':'Capability'),
            get_document(Id, Capability_Raw))),

    (   get_dict(role, Capability_Raw, Role_Ids)
    ->  findall(Role,
                (   member(Role_Id, Role_Ids),
                    get_role_from_id(SystemDB, Role_Id, Role)),
                Roles),
        put_dict(_{role : Roles}, Capability_Raw, Capability_Role)
    ;   Capability_Raw = Capability_Role
    ),
    get_dict(scope, Capability_Role, Scope),
    get_resource_from_id(SystemDB,Scope,Resource),
    put_dict(_{scope : Resource}, Capability_Role, Capability).

api_get_users(SystemDB, Auth, Users) :-
    do_or_die(
        is_super_user(Auth),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),

    get_users(SystemDB, Users).

get_users(SystemDB, Users) :-
    findall(
        User,
        (   ask(SystemDB,
                t(Id, rdf:type, '@schema':'User')),
            get_user_from_id(SystemDB,Id,User)
        ),
        Users).

api_update_user_password(System_DB, Auth, UserName, Password) :-
    api_get_user_from_name(System_DB, Auth, UserName, User),

    do_or_die(
        (   is_super_user(Auth)
            % we are the user who is changing this.
        ;   get_dict('@id', User, Auth)),
        error(access_not_authorised(Auth,'Action/manage_capabilities','SystemDatabase'), _)),

    get_dict('@id', User, Id),
    get_dict(name, User, Name),
    crypto_password_hash(Password, Hash),
    New_User =
    _{
        '@id' : Id,
        name : Name,
        key_hash : Hash
    },

    create_context(System_DB, commit_info{author: "admin", message: "API: Update User Password"},
                   System_Context),
    with_transaction(
        System_Context,
        replace_document(System_Context,New_User),
        _
    ).
