:- module(api_frame, [api_class_frame/6, api_filled_frame/5]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(document)).

api_class_frame(System_DB, Auth, Path, Class, Frame, Options) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(
        askable_context(Descriptor, System_DB, Auth, Context),
        error(unresolvable_collection(Descriptor),_)),

    assert_read_access(Context),

    (   Class = uri(Class_Uri)
    ->  do_or_die(class_frame(Context,Class_Uri,Frame,Options),
                  error(could_not_create_class_frame(Class_Uri)))
    ;   do_or_die(all_class_frames(Context,Frame),
                  error(could_not_create_class_frames))
    ).

api_filled_frame(System_DB, Auth, Path, Instance_Uri, Filled_Frame) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(
        askable_context(Descriptor, System_DB, Auth, Context),
        error(unresolvable_collection(Descriptor),_)),

    assert_read_access(Context),

    catch(
        prefix_expand(Instance_Uri, Context.prefixes, InstanceEx),
        error(key_has_unknown_prefix(K),_),
        throw(error(instance_uri_has_unknown_prefix(K),_))),

    throw(error(unimplemented,_)),
    do_or_die(
        filled_frame_jsonld(Context,InstanceEx,Filled_Frame),
        error(could_not_create_filled_class_frame(Instance_Uri),_)).
