use super::frame::GraphQLName;

pub fn ordering_name(type_name: &GraphQLName) -> GraphQLName<'static> {
    GraphQLName(format!("{type_name}_Ordering").into())
}

pub fn restriction_name(type_name: &GraphQLName) -> GraphQLName<'static> {
    GraphQLName(format!("{type_name}_Restriction").into())
}

pub fn filter_name(type_name: &GraphQLName) -> GraphQLName<'static> {
    GraphQLName(format!("{type_name}_Filter").into())
}

pub fn collection_filter_name(type_name: &GraphQLName) -> GraphQLName<'static> {
    GraphQLName(format!("{type_name}_Collection_Filter").into())
}

pub fn enum_filter_name(type_name: &GraphQLName) -> GraphQLName<'static> {
    GraphQLName(format!("{type_name}_Enum_Filter").into())
}

pub fn path_field_to_class<'a>(field_name: &'a GraphQLName<'a>) -> Option<GraphQLName<'a>> {
    let field_name = field_name.as_str();
    if field_name.starts_with("_path_to_") {
        const PREFIX_LEN: usize = "_path_to_".len();
        let class = &field_name[PREFIX_LEN..];
        Some(GraphQLName(class.into()))
    } else {
        None
    }
}

pub fn inverse_field_name(property: &GraphQLName, class: &GraphQLName) -> GraphQLName<'static> {
    let field = format!("_{property}_of_{class}");
    GraphQLName(field.into())
}

pub fn path_to_class_name(class: &GraphQLName) -> GraphQLName<'static> {
    GraphQLName(format!("_path_to_{class}").into())
}

pub fn update_class_name(class: &GraphQLName) -> GraphQLName<'static> {
    GraphQLName(format!("Update{class}").into())
}

pub fn insert_class_name(class: &GraphQLName) -> GraphQLName<'static> {
    GraphQLName(format!("Insert{class}").into())
}

pub fn delete_class_name(class: &GraphQLName) -> GraphQLName<'static> {
    GraphQLName(format!("Delete{class}").into())
}
