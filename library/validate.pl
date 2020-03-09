:- module(validate, [
              turtle_schema_transaction/4,
              document_transaction/5
          ]).

/** <module> Validation
 *
 * Implements schema and instance validation
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(triplestore).
:- use_module(database).
:- use_module(utils).
:- use_module(schema,[
                  database_module/2,
                  compile_schema_to_module/2
              ]).
:- use_module(validate_schema).
:- use_module(validate_instance).
:- use_module(frame).
:- use_module(jsonld).
:- use_module(library(semweb/turtle)).

:- use_module(expansions).

:- use_module(literals).

% For debugging
:- use_module(library(http/http_log)).

/*
 * graph_validation_obj { descriptor: graph_descriptor, read: layer, changed: bool }
 *
 * validation_object{ descriptor: collection_descriptor,
 *                    <parent>: transaction_obj % we still keep it a transaction obj and transform it later
 *                    instance_objects: list(graph_validation_obj)
 *                    schema_objects: list(graph_validation_obj)
 *                    inference_objects: list(graph_validation_obj)
 */

read_write_obj_to_graph_validation_obj(Read_Write_Obj, Graph_Validation_Obj, Map, Map) :-
    memberchk(Read_Write_Obj=Graph_Validation_Obj, Map),
    !.
read_write_obj_to_graph_validation_obj(Read_Write_Obj, Graph_Validation_Obj, Map, [Read_Write_Obj=Graph_Validation_Obj|Map]) :-
    Read_Write_Obj = read_write_obj{ descriptor: Descriptor,
                                     read: Layer,
                                     write: Layer_Builder },
    Graph_Validation_Obj = graph_validation_obj{ descriptor: Descriptor,
                                                 read: New_Layer,
                                                 changed: Changed },
    (   var(Layer_Builder)
    ->  New_Layer = Layer,
        Changed = false
    ;   nb_commit(Layer_Builder, New_Layer),
        Changed = true).

transaction_object_to_validation_object(Transaction_Object, Validation_Object, Map, New_Map) :-
    transaction_object{descriptor: Descriptor,
                       instance_objects: Instance_Objects,
                       schema_objects: Schema_Objects,
                       inference_objects: Inference_Objects} :< Transaction_Object,
    Intermediate_Validation_Object = validation_object{
                                         descriptor: Descriptor,
                                         instance_objects: Validation_Instance_Objects,
                                         schema_objects: Validation_Schema_Objects,
                                         inference_objects: Validation_Inference_Objects
                                     },
    (   Parent = Transaction_Object.get(parent)
    ->  transaction_object_to_validation_object(Parent, Parent_Validation_Object, Map, Map_1),
        Intermediate_Validation_Object_1 = Intermediate_Validation_Object.put(parent, Parent)
    ;   Intermediate_Validation_Object_1 = Intermediate_Validation_Object
    ),
    mapm([Object,Validation_Object]>>read_write_obj_to_graph_validation_obj(Object, Validation_Object),
         Instance_Objects,
         Validation_Instance_Objects,
         Map_1,
         Map_2),
    mapm([Object,Validation_Object]>>read_write_obj_to_graph_validation_obj(Object, Validation_Object),
         Schema_Objects,
         Validation_Schema_Objects,
         Map_2,
         Map_3),
    mapm([Object,Validation_Object]>>read_write_obj_to_graph_validation_obj(Object, Validation_Object),
         Inference_Objects,
         Validation_Inference_Objects,
         Map_3,
         New_Map),
    (   Commit_Info = Transaction_Object.get(commit_info)
    ->  Validation_Object = Intermediate_Validation_Object_1.put(commit_info, Commit_Info)
    ;   Validation_Object = Intermediate_Validation_Object_1).


commit_validation_object(Validation_Object) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object]
    } :< Validation_Object,
    terminus_descriptor{
    } = Descriptor,
    !,
    terminus_instance_name(Label),
    % This is okay for now, since we don't want the
    % schema to be changed in WOQL queries
    (   Instance_Object.changed = true
    ->  storage(Store),
        safe_open_named_graph(Store, Label, Graph),
        nb_set_head(Graph, Instance_Object.read)
    ;   true).
commit_validation_object(Validation_Object) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object]
    } :< Validation_Object,
   database_descriptor{
        database_name: Database_Name
    } = Descriptor,
    !,
    % super simple case, we just need to set head
    % That is, we don't need to check the schema
    (   Instance_Object.changed = true
    ->  storage(Store),
        % The label is the same as the same as the database_name
        % therefore we can just open it
        safe_open_named_graph(Store, Database_Name, Graph),
        nb_set_head(Graph, Instance_Object.read)
    ;   true).
commit_validation_object(Validation_Object) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object],
        parent: Parent_Transaction
    } :< Validation_Object,
    repository_descriptor{
        database_descriptor: Descriptor,
        repository_name: Repo_Name
    } = Descriptor,
    layer_to_id(Instance_Object.read, Layer_ID),
    (   Instance_Object.changed = true
    ->  once(ask(Parent_Transaction,
                 (   t(URI, repository:repository_name, Repo_Name^^xsd:string),
                     t(URI, repository:repository_head, ExistingRepositoryHead),
                     t(ExistingRepositoryHead, layer:layer_id, LayerIdOfHeadToRemove^^xsd:string),
                     delete(ExistingRepositoryHead, layer:layer_id, LayerIdOfHeadToRemove^^xsd:string),
                     delete(URI, repository:repository_head, ExistingRepositoryHead),
                     idgen(layer:'ShadowLayer', [Layer_ID], NewShadowLayerID),
                     insert(NewShadowLayerID, rdf:type, layer:'ShadowLayer'),
                     insert(NewShadowLayerID, layer:shadow_layer, Layer_ID^^xsd:string)
                 )
                ))
    ;   true).
commit_validation_object(Validation_Object) :-
    /*
    validation_object{
        parent : Parent_Transaction,
        descriptor: Descriptor,
        instance_objects: Instance_Objects,
        schema_objects: Schema_Objects,
        inference_objects: Inference_Objects
    } :< Validation_Object,
*/  
    branch_descriptor{ repository_descriptor: Repository_Descriptor,
                       branch_name : Branch},
    !,
    append([Instance_Objects,Schema_Objects,Inference_Objects],
           Union_Objects),

    atomic_list_concat(['http://terminushub.com/document/'],Branch_Base),
    % TODO: This is where it all went wrong.
    random_uri(ref:Branch_Name,'Commit',Commit_URI),
    (   exists([Obj]>>(Obj.changed = true), Union_Objects)
    ->  once(ask(Parent_Transaction,
                 (   t(Branch_URI, ref:branch_name, Branch_Name^^xsd:string),
                     * idgen(ref:'Commit',[],X)
                 )
                ))
    ;   true
    ).
commit_validation_object(Validation_Object) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object]
    } :< Validation_Object,
    label_descriptor{
        label: Label
    } = Descriptor,
    !,
    % super simple case, we just need to set head
    % That is, assuming anything changed
    (   Instance_Object.changed = true
    ->  storage(Store),
        safe_open_named_graph(Store, Label, Graph),
        nb_set_head(Graph, Instance_Object.read)
    ;   true).



/**
 * pre_test_schema(-Pred:atom) is nondet.
 *
 * List of cycle checks which must occur prior to normal scheme validation
 */
% Required for consistency
pre_test_schema(class_cycle_SC).
pre_test_schema(property_cycle_SC).

/**
 * test_schema(-Pred:atom) is nondet.
 *
 * This predicate gives each available schema constraint.
 */
% Restrictions on schemas
test_schema(no_immediate_class_SC).
test_schema(no_immediate_domain_SC).
test_schema(no_immediate_range_SC).
test_schema(annotation_overload_SC).
% OWL DL constraints
test_schema(orphan_class_SC).
test_schema(orphan_property_SC).
test_schema(invalid_range_SC).
test_schema(invalid_domain_SC).
test_schema(domain_not_subsumed_SC).
test_schema(range_not_subsumed_SC).
test_schema(property_type_overload_SC).
test_schema(invalid_RDFS_property_SC).

/*
 * needs_schema_validation(Validation_Object) is det.
 *
 */
needs_schema_validation(Validation_Object) :-
    validation_object{
        schema_objects: Schema_Objects,
    } :< Validation_Object,
    exists([Schema_Object]>>(Schema_Object.changed = true), Schema_Objects).

/*
 * needs_schema_instance_validation(Validation_Object) is det.
 *
 * Check to see if we need to do a blast-radius calculation
 * on the schema and renew instance checking on possibly impacted
 * triples.
 *
 * Currently just assumes we do if the schema changed.
 *
 */
needs_schema_instance_validation(Validation_Object) :-
    validation_object{
        schema_objects: Schema_Objects,
        instance_objects: Instance_Objects,
    } :< Validation_Object,
    Instance_Objects /= [],
    exists([Schema_Object]>>(Schema_Object.changed = true), Schema_Objects).

/*
 * needs_local_instance_validation(Validation_Object) is det.
 *
 * Checks to see if we need to do some local instance validation.
 *
 */
needs_local_instance_validation(Validation_Object) :-
    validation_object{
        instance_objects: Instance_Objects,
    } :< Validation_Object,
    exists([Instance_Object]>>(Instance_Object.changed = true), Instance_Objects).

/*
 * refute_pre_schema(Validation_Object,Witness) is nondet.
 *
 * Get a witness for each refutation of the pre-schema
 */
refute_pre_schema(Validation_Object,Witness) :-
    pre_test_schema(Pre_Check),
    call(Pre_Check,Validation_Object,Witness).

/*
 * refute_schema(Validation_Object,Witness) is nondet.
 *
 * Get a witness for each refutation of the schema
 */
refute_schema(Validation_Object,Witness) :-
    test_schema(Check),
    call(Check,Validation_Object,Witness).

/*
 * calculate_invalidating_class(Validation_Object, Classes) is det.
 *
 * 1) If we are a deleted class, we need to be checked
 * 2) Anyone subsumption below us needs to be checked.
 * 3) If we are an added class, everyone subsumption below us needs to be checked
 *    for all restrictions *above* us.
 * 4) If we are an added restriction, everyone subsumption below us needs to be checked
 * 5) If we have changed the subsumption hierarchy, everyone below the change
 *    needs to be checked for everyone above.
 */
calculate_invalidating_class(Validation_Object, Class) :-
    Schema = Validation_Object.schema_objects,
    xrdf_deleted(Schema, Super, rdf:type, owl:'Class'),
    subsumption_of(Class, Super, Validation_Object).
calculate_invalidating_class(Validation_Object, Class) :-
    Schema = Validation_Object.schema_objects,
    xrdf_added(Schema, Super, rdf:type, owl:'Class'),
    subsumption_of(Class, Super, Validation_Object).
calculate_invalidating_class(Validation_Object, Class) :-
    Schema = Validation_Object.schema_objects,
    xrdf_added(Schema, _Sub, rdf:subClassOf, Super),
    subsumption_of(Class, Super, Validation_Object).
calculate_invalidating_class(Validation_Object, Class) :-
    Schema = Validation_Object.schema_objects,
    xrdf_added(Schema, _Sub, rdf:unionOf, Super),
    subsumption_of(Class, Super, Validation_Object).
calculate_invalidating_class(Validation_Object, Class) :-
    Schema = Validation_Object.schema_objects,
    xrdf_added(Schema, Super, rdf:instersectionOf, _Sub),
    subsumption_of(Class, Super, Validation_Object).
calculate_invalidating_class(Validation_Object, Class) :-
    Schema = Validation_Object.schema_objects,
    xrdf_added(Schema, Super, rdf:oneOf, _OneOf),
    subsumption_of(Class, Super, Validation_Object).
calculate_invalidating_class(Validation_Object, Class) :-
    Schema = Validation_Object.schema_objects,
    xrdf_deleted(Schema, Sub, rdf:subClassOf, Super),
    subsumption_of(Class, Sub, Validation_Object).
calculate_invalidating_class(Validation_Object, Class) :-
    Schema = Validation_Object.schema_objects,
    xrdf_deleted(Schema, Sub, rdf:unionOf, _Super),
    subsumption_of(Class, Sub, Validation_Object).
calculate_invalidating_class(Validation_Object, Class) :-
    Schema = Validation_Object.schema_objects,
    xrdf_deleted(Schema, _Super, rdf:instersectionOf, Sub),
    subsumption_of(Class, Sub, Validation_Object).
calculate_invalidating_class(Validation_Object, Class) :-
    Schema = Validation_Object.schema_objects,
    xrdf_deleted(Schema, _Super, rdf:oneOf, Sub),
    subsumption_of(Class, Sub, Validation_Object).
calculate_invalidating_class(Validation_Object, Class) :-
    Schema = Validation_Object.schema_objects,
    xrdf_added(Schema, Super, rdf:type, owl:'Restriction'),
    subsumption_of(Class, Super, Validation_Object).

calculate_invalidating_classes(Validation_Object, Classes) :-
    findall(Class,
            calculate_invalidating_class(Validation_Object, Class),
            Unsorted),
    sort(Unsorted, Classes).

/*
 * calculate_invalidating_property(Validation_Object,Property) is det.
 *
 * 1) If we are an added property, everyone subsumption below us in the property
 *    hierarchy needs to be checked if we have any applicable restrictions
 * 2) If we are an added property, everyone subsumption above us in the property
 *    hierarchy needs to be checked if we have any applicable restrictions
 * 3) If we are a deleted property, everyone subsumption below us in the property
 *    hierarchy needs to be checked.
 * 4) If we are a deleted property, everyone subsumption above us in the property
 *    hierarchy needs to be checked.
 */
calculate_invalidating_property(Validation_Object, Prop) :-
    Schema = Validation_Objects.schema_objects,
    xrdf_deleted(Schema, Super, rdf:type, owl:'DatatypeProperty'),
    subsumption_properties_of(Prop, Super, Validation_Object).
calculate_invalidating_property(Validation_Object, Prop) :-
    Schema = Validation_Objects.schema_objects,
    xrdf_deleted(Schema, Sub, rdf:type, owl:'DatatypeProperty'),
    subsumption_properties_of(Sub, Prop, Validation_Object).
calculate_invalidating_property(Validation_Object, Prop) :-
    Schema = Validation_Objects.schema_objects,
    xrdf_added(Schema, Super, rdf:type, owl:'DatatypeProperty'),
    subsumption_properties_of(Prop, Super, Validation_Object).
calculate_invalidating_property(Validation_Object, Prop) :-
    Schema = Validation_Objects.schema_objects,
    xrdf_added(Schema, Sub, rdf:type, owl:'DatatypeProperty'),
    subsumption_properties_of(Sub, Prop, Validation_Object).
calculate_invalidating_property(Validation_Object, Prop) :-
    Schema = Validation_Objects.schema_objects,
    xrdf_deleted(Schema, Super, rdf:type, owl:'ObjectProperty'),
    subsumption_properties_of(Prop, Super, Validation_Object).
calculate_invalidating_property(Validation_Object, Prop) :-
    Schema = Validation_Objects.schema_objects,
    xrdf_deleted(Schema, Sub, rdf:type, owl:'ObjectProperty'),
    subsumption_properties_of(Sub, Prop, Validation_Object).
calculate_invalidating_property(Validation_Object, Prop) :-
    Schema = Validation_Objects.schema_objects,
    xrdf_added(Schema, Super, rdf:type, owl:'ObjectProperty'),
    subsumption_properties_of(Prop, Super, Validation_Object).
calculate_invalidating_property(Validation_Object, Prop) :-
    Schema = Validation_Objects.schema_objects,
    xrdf_added(Schema, Sub, rdf:type, owl:'ObjectProperty'),
    subsumption_properties_of(Sub, Prop, Validation_Object).
calculate_invalidating_property(Validation_Object, Prop) :-
    Schema = Validation_Objects.schema_objects,
    xrdf_deleted(Schema, _Sub, rdfs:subPropertyOf, Super),
    subsumption_properties_of(Prop, Super, Validation_Object).
calculate_invalidating_property(Validation_Object, Prop) :-
    Schema = Validation_Objects.schema_objects,
    xrdf_added(Schema, _Sub, rdfs:subPropertyOf, Super),
    subsumption_properties_of(Prop, Super, Validation_Object).
calculate_invalidating_property(Validation_Object, Prop) :-
    Schema = Validation_Objects.schema_objects,
    xrdf_deleted(Schema, Sub, rdfs:subPropertyOf, _Super),
    subsumption_properties_of(Sub, Prop, Validation_Object).
calculate_invalidating_property(Validation_Object, Prop) :-
    Schema = Validation_Objects.schema_objects,
    xrdf_added(Schema, Sub, rdfs:subPropertyOf, _Super),
    subsumption_properties_of(Sub, Prop, Validation_Object).

calculate_invalidating_properties(Validation_Object, Properties) :-
    findall(Property,
            calculate_invalidating_properties(Validation_Object, Property),
            Unsorted),
    sort(Unsorted, Property).


/*
 * safe_added_classes(Schema) is semidet.
 *
 * True if only new classes are also below us.
 */
safe_added_classes(Schema) :-
    forall(
        xrdf_added(Schema, Class, rdf:type, owl:'Class'),
        (   strict_subsumption_of(Sub, Class, Validation_Object)
        *-> xrdf_added(Schema, Sub, rdf:type, owl:'Class')
        ;   fail
        )
    ).

/*
 * safe_added_properties(Schema) is semidet.
 *
 * True if only new properties are also below us
 */
safe_added_properties(Schema) :-
    forall(
        (   xrdf_added(Schema, Property, rdf:type, owl:'DatatypeProperty')
        ;   xrdf_added(Schema, Property, rdf:type, owl:'Object_Property'))
        (   strict_subsumption_property_of(SubProperty, Property, Validation_Object)
        *-> (   xrdf_added(Schema, Property, rdf:type, owl:'DatatypeProperty')
            ;   xrdf_added(Schema, Property, rdf:type, owl:'Object_Property'))
        ;   fail
        )
    ).


/*
 * safe_added_restrictions(Schema) is semidet.
 *
 * True if only new classes and restrictions are also below us
 */
safe_added_restrictions(Schema) :-
    forall(
        xrdf_added(Schema, Restriction, rdf:type, owl:'Restriction')
        (   strict_subsumption_of(SubClass, Restriction, Validation_Object)
        *-> (   xrdf_added(Schema, SubClass, rdf:type, owl:'Restriction')
            ;   xrdf_added(Schema, Property, rdf:type, owl:'Class'))
        ;   fail
        )
    ).

no_deleted_classes(Schema) :-
    \+ xrdf_deleted(Schema, Class, rdf:type, owl:'Class').

no_deleted_properties(Schema) :-
    \+ xrdf_deleted(Schema, Class, rdf:type, owl:'DatatypeProperty'),
    \+ xrdf_deleted(Schema, Class, rdf:type, owl:'ObjectProperty').

no_deleted_restrictions(Schema) :-
    \+ xrdf_deleted(Schema, Class, rdf:type, owl:'Restriction').

/*
 * empty_blast_radius(Validation_Object) is semidet.
 *
 * If the blast radius is empty, let's punt
 *
 * This is a conservative estimate. It can be made tighter.
 */
empty_blast_radius(Validation_Object) :-
    validation_object{
        schema_objects: Schema_Objects,
    } :< Validation_Object,
    safe_added_classes(Schema_Objects),
    safe_added_properties(Schema_Objects),
    safe_added_restrictions(Post_Database),
    no_deleted_classes(Schema_Objects),
    no_deleted_properties(Schema_Objects),
    no_deleted_restrictions(Schema_Objects).

/*
 * refute_instance_schema(Validation_Object,Witness) is nondet.
 *
 * Get a witness for each refutation of the instance graph
 * which could have been invalidated by changes to the schema.
 *
 * This should calculate blast radius.
 */
refute_instance_schema(Validation_Object,Witness) :-
    validation_object{
        schema_objects: Schema_Objects,
        instance_objects: Instance_Objects,
    } :< Validation_Object,
    empty_blast_radius(Validation_Object),
    !,
    fail.
refute_instance_schema(Validation_Object,Witness) :-
    validation_object{
        schema_objects: Schema_Objects,
        instance_objects: Instance_Objects,
    } :< Validation_Object,
    calculate_invalidating_classes(Validation_Object, Classes),
    member(Class, Classes),
    xrdf(Instance_Objects, X, rdf:type, Class),
    xrdf(Instance_Objects, X, P, Y),
    refute_insertion(Instance_Objects, X, P, Y, Witness).
refute_instance_schema(Validation_Object,Witness) :-
    validation_object{
        schema_objects: Schema_Objects,
        instance_objects: Instance_Objects,
    } :< Validation_Object,
    calculate_invalidating_properties(Validation_Object, Properties),
    member(Property, Properties),
    xrdf(Instance_Objects, X, Property, Y),
    refute_insertion(Instance_Objects, X, Property, Y, Witness).

/*
 * refute_instance(Validation_Object,Witness) is nondet.
 *
 * Get a witness for each refutation of the inserted/deleted
 *
 * This should calculate blast radius.
 */
refute_instance(Validation_Object,Witness) :-
    validation_object{
        schema_objects: Schema_Objects,
        instance_objects: Instance_Objects,
    } :< Validation_Object,
    % TODO: Check changed!
    xrdf_added(Instance_Objects, X, P, Y),
    refute_insertion(Validation_Object,X,P,Y,Witness).
refute_instance(Validation_Object,Witness) :-
    validation_object{
        schema_objects: Schema_Objects,
        instance_objects: Instance_Objects,
    } :< Validation_Object,
    % TODO: Check changed!
    xrdf_deleted(Instance_Objects, X, P, Y),
    refute_deletion(Validation_Object,X,P,Y,Witness).

/*
 * refute_validation_object(+Validation:validation_obj, -Witness) is nondet.
 *
 * We are for the moment going to do validation on all objects
 * regardless of level in the tier of our hierarchy. Since higher level objects
 * are written only by us, and the schemata are never written we can presumably
 * get away with dispensing with schema and instance checking. However in
 * early phases, it's probably best if we leave it in so we can be confident
 * we are not writing nonsense!
 *
 * There is no current way to do inference validation. We are not allowing updates
 * so hopefully this is ok!
 */
refute_validation_object(Validation_Object, Witness) :-
    % Pre Schema
    needs_schema_validation(Validation_Objects),
    refute_pre_schemas(Validation_Object, Witness),
    % Do not proceed if schema has circularities
    !.
refute_validation_object(Validation_Object, Witness) :-
    % Pre Schema
    needs_schema_validation(Validation_Object),
    refute_schema(Validation_Object, Witness),
    % Do not proceed if we have a broken schema
    !.
refute_validation_object(Validation_Object, Witness) :-
    % Pre Schema
    needs_schema_instance_validation(Validation_Object),
    refute_instance_schema(Validation_Object, Witness).
refute_validation_object(Validation_Object, Witness) :-
    needs_instance_validation(Validation_Object),
    refute_instance(Validation_Object,Witness).

/*
 * refute_validation_objects(Validation_Objects, Witness) is nondet.
 *
 * Find all refutations of the given validation object.
 */
refute_validation_objects(Validation_Objects, Witness) :-
    member(Validation_Object, Validation_Objects),
    refute_validation_object(Validation_Object, Witness).


/*
 * turtle_schema_transaction(+Database,-Database,+Schema,+New_Schema_Stream, Witnesses) is det.
 *
 * Updates a schema using a turtle formatted stream.
 *
 * TODO: This predicate is now really quite bogus, however we have to do something similar because we need
 * calculate an intermediate graph for insertion.
 */
turtle_schema_transaction(Database, Schema, New_Schema_Stream, Witnesses) :-

    % make a fresh empty graph against which to diff
    open_memory_store(Store),
    open_write(Store, Builder),

    % write to a temporary builder.
    rdf_process_turtle(
        New_Schema_Stream,
        {Builder}/
        [Triples,_Resource]>>(
            forall(member(T, Triples),
                   (   normalise_triple(T, rdf(X,P,Y)),
                       object_storage(Y,S),
                       nb_add_triple(Builder, X, P, S)
                   ))),
        [encoding(utf8)]),
    % commit this builder to a temporary layer to perform a diff.
    nb_commit(Builder,Layer),

    with_transaction(
        [transaction_record{
             pre_database: Database,
             write_graphs: [Schema],
             update_database: Update_DB,
             post_database: Post_DB},
         witnesses(Witnesses)],
        % Update
        validate:(
            % first write everything into the layer-builder that is in the new
            % file, but not in the db.
            forall(
                (   xrdf([Schema], A_Old, B_Old, C_Old),
                    \+ xrdf_db(Layer,A_Old,B_Old,C_Old)),
                delete(Schema,A_Old,B_Old,C_Old)
            ),
            forall(
                (   xrdf_db(Layer,A_New,B_New,C_New),
                    \+ xrdf([Schema], A_New, B_New, C_New)),
                insert(Schema,A_New,B_New,C_New)
            )
        ),
        % Post conditions
        validate:(
            findall(Pre_Witness,
                    (   pre_test_schema(Pre_Check),
                        call(Pre_Check,Post_DB,Pre_Witness)),
                    Pre_Witnesses),
            (   \+ Pre_Witnesses = []
            % We have witnesses of failure and must bail
            ->  Witnesses = Pre_Witnesses
            % We survived the pre_tests, better check schema constriants
            ;   findall(Schema_Witness,
                        (   test_schema(Check),
                            call(Check,Post_DB,Schema_Witness)),
                        Schema_Witnesses),
                (   \+ Schema_Witnesses = []
                ->  Witnesses = Schema_Witnesses
                    % Winning!
                ;   % TODO: We do not need to perform a global check of instances
                    % Better would be a local check derived from schema delta.
                    (    schema_validation_skippable(Update_DB, Schema, Layer)
                    ->   true
                    ;    findall(Witness,
                              (   database_instance(Post_DB, Instance),
                                  xrdf(Instance,E,F,G),
                                  refute_insertion(Post_DB,E,F,G,Witness)),
                              Witnesses)
                    )

                )
            ),
            % if we got here, I think this might be safe.
            database_module(Database, Module),
            compile_schema_to_module(Post_DB, Module)
        )
    ).
