:- module(layer_entity, [
              has_layer/2,
              layer_id_uri/3,
              insert_layer_object/3
          ]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(document)).

:- use_module(library(plunit)).

has_layer(Askable, Layer_Id) :-
    ask(Askable,
        t(_, layer:identifier, Layer_Id^^xsd:string)).

layer_id_uri(Askable, Layer_Id, Layer_Uri) :-
    once(ask(Askable,
             t(Layer_Uri, layer:identifier, Layer_Id^^xsd:string))).

insert_layer_object(Context, Layer_Id, Layer_Uri) :-
    Document = json{
                   '@type' : "layer:Layer",
                   'layer:identifier' : Layer_Id
               },
    insert_document(Context, Document, Layer_Uri).

:- begin_tests(layer_objects).
:- use_module(core(util/test_utils)).
:- use_module(database).
test(insert_layer_object,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]) :-
    ref_schema_context_from_label_descriptor("testlabel",Descriptor, Context),

    Layer_Id = "f3dfc8d0d103b0be9428938174326e6256ad1beb",
    with_transaction(Context,
                     (   insert_layer_object(Context, Layer_Id, Layer_Uri)),
                     _),

    layer_id_uri(Descriptor, Layer_Id, Layer_Uri).
:- end_tests(layer_objects).
