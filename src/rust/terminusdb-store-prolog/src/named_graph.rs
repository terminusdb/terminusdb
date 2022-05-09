use crate::layer::*;
use crate::store::*;
use std::io::{self, Write};
use swipl::prelude::*;
use terminus_store::store::sync::*;

predicates! {
    pub semidet fn create_named_graph(context, store_term, graph_name_term, graph_term) {
        let store: WrappedStore = store_term.get_ex()?;
        let graph_name: PrologText = graph_name_term.get_ex()?;

        let graph = context.try_or_die(store.create(&graph_name))?;
        graph_term.unify(&WrappedNamedGraph(graph))
    }

    pub semidet fn open_named_graph(context, store_term, graph_name_term, graph_term) {
        let store: WrappedStore = store_term.get_ex()?;
        let graph_name: PrologText = graph_name_term.get_ex()?;

        match context.try_or_die(store.open(&graph_name))? {
            None => Err(PrologError::Failure),
            Some(graph) => graph_term.unify(&WrappedNamedGraph(graph)),
        }
    }

    pub semidet fn delete_named_graph(context, store_term, graph_name_term) {
        let store: WrappedStore = store_term.get_ex()?;
        let graph_name: PrologText = graph_name_term.get_ex()?;

        into_prolog_result(context.try_or_die(store.delete(&graph_name))?)
    }

    #[name("head")]
    pub semidet fn head2(context, graph_term, layer_term) {
        let graph: WrappedNamedGraph = graph_term.get_ex()?;
        match context.try_or_die(graph.head())? {
            None => Err(PrologError::Failure),
            Some(layer) => layer_term.unify(&WrappedLayer(layer)),
        }
    }

    #[name("head")]
    pub semidet fn head3(context, graph_term, layer_term, version_term) {
        let graph: WrappedNamedGraph = graph_term.get_ex()?;
        let (layer_opt, version) = context.try_or_die(graph.head_version())?;
        version_term.unify(version)?;

        if let Some(layer) = layer_opt {
            layer_term.unify(&WrappedLayer(layer))?;
        }

        Ok(())
    }

    pub semidet fn nb_set_head(context, graph_term, layer_term) {
        let graph: WrappedNamedGraph = graph_term.get_ex()?;
        let layer: WrappedLayer = layer_term.get_ex()?;

        into_prolog_result(context.try_or_die(graph.set_head(&layer))?)
    }

    pub semidet fn nb_force_set_head(context, graph_term, layer_term) {
        let graph: WrappedNamedGraph = graph_term.get_ex()?;
        let layer: WrappedLayer = layer_term.get_ex()?;

        context.try_or_die(graph.force_set_head(&layer))?;

        Ok(())
    }

    #[name("nb_force_set_head")]
    pub semidet fn nb_force_set_head_version(context, graph_term, layer_term, version_term) {
        let graph: WrappedNamedGraph = graph_term.get_ex()?;
        let layer: WrappedLayer = layer_term.get_ex()?;

        let version: u64 = version_term.get_ex()?;

        let result = context.try_or_die(graph.force_set_head_version(&layer, version))?;

        into_prolog_result(result)
    }
}

wrapped_clone_blob!("named_graph", pub WrappedNamedGraph, SyncNamedGraph);

impl CloneBlobImpl for WrappedNamedGraph {
    fn write(&self, stream: &mut PrologStream) -> io::Result<()> {
        write!(stream, "<named_graph {}>", self.name())
    }
}
