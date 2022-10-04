pub fn predicate_value_iter<'a>(
    g: &'a SyncStoreLayer,
    property: &'a str,
    object_type: &ObjectType,
    object: &'a str,
) -> Box<dyn Iterator<Item = IdTriple> + 'a> {
    let maybe_property_id = g.predicate_id(property);
    if let Some(property_id) = maybe_property_id {
        let maybe_object_id = match object_type {
            ObjectType::Value => g.object_value_id(object),
            ObjectType::Node => g.object_node_id(object),
        };
        if let Some(object_id) = maybe_object_id {
            Box::new(
                g.triples_o(object_id)
                    .filter(move |t| t.predicate == property_id),
            )
        } else {
            Box::new(std::iter::empty())
        }
    } else {
        Box::new(std::iter::empty())
    }
}

pub fn predicate_value_filter<'a>(
    g: &'a SyncStoreLayer,
    iter: Box<dyn Iterator<Item = IdTriple> + 'a>,
    property: &str,
    object_type: &ObjectType,
    object: &str,
) -> Box<dyn Iterator<Item = IdTriple> + 'a> {
    let maybe_property_id = g.predicate_id(property);
    if let Some(property_id) = maybe_property_id {
        let maybe_object_id = match object_type {
            ObjectType::Value(ty) => g.object_value_id(object),
            ObjectType::Node => g.object_node_id(object),
        };
        if let Some(object_id) = maybe_object_id {
            Box::new(iter.filter(move |t| g.triple_exists(t.subject, property_id, object_id)))
        } else {
            Box::new(std::iter::empty())
        }
    } else {
        Box::new(std::iter::empty())
    }
}

enum ObjectType {
    Node,
    Value,
}

pub fn lookup_field_requests(
    g: &SyncStoreLayer,
    fields: Vec<(String, ObjectType, Option<String>)>,
) -> Vec<u64> {
    let constraints: Vec<(String, ObjectType, String)> = fields
        .into_iter()
        .map(|(p, oty, mv)| match mv {
            Option::Some(v) => Some((p, oty, v)),
            Option::None => None,
        })
        .flatten()
        .collect();
    if constraints.len() == 0 {
        panic!("Somehow there are no constraints on the triples")
    } else {
        let (p, oty, o) = &constraints[0];
        let zero_iter = predicate_value_iter(g, &p, &oty, &o);
        constraints[1..]
            .iter()
            .fold(zero_iter, |iter, (p, oty, o)| {
                predicate_value_filter(g, iter, &p, &oty, &o)
            })
            .collect()
    }
}
