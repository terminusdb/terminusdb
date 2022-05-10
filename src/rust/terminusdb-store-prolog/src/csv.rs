use chardetng::*;
use csv::ReaderBuilder;
use encoding_rs::UTF_8;
use hex;
use sha1::{Digest, Sha1};
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;
use terminus_store::layer::StringTriple;
use terminus_store::store::sync::*;
use urlencoding;

use swipl::prelude::*;

use crate::builder::WrappedBuilder;

predicates! {
    pub semidet fn csv_iri(_context, csv_name_term, prefix_term, iri_term) {
        let csv_name: String = csv_name_term.get::<PrologText>()?.to_string();
        let prefix: String = prefix_term.get::<PrologText>()?.to_string();
        let (_, node) = csv_name_iri(csv_name, prefix);

        iri_term.unify(&node)
    }

    pub semidet fn csv_builder(context,
                               name_term,
                               csv_term,
                               builder_term,
                               data_prefix_term,
                               schema_prefix_term,
                               header_term,
                               skip_header_term
    ) {
        let name: PrologText = name_term.get_ex()?;
        let csv: PrologText = csv_term.get_ex()?;
        let builder: WrappedBuilder = builder_term.get_ex()?;
        let data_prefix: PrologText = data_prefix_term.get_ex()?;
        let schema_prefix: PrologText = schema_prefix_term.get_ex()?;

        // TODO original throws an exception here if the type does not match
        let has_header: bool = header_term.get_ex()?;
        let skip_header: bool = skip_header_term.get_ex()?;

        context.try_or_die_generic(import_csv(name.to_string(),
                                              csv.to_string(),
                                              &builder,
                                              None,
                                              data_prefix.to_string(),
                                              schema_prefix.to_string(),
                                              has_header,
                                              skip_header
        ))
    }

    #[name("csv_builder")]
    pub semidet fn csv_builder_with_schema(context,
                                           name_term,
                                           csv_term,
                                           builder_term,
                                           schema_builder_term,
                                           data_prefix_term,
                                           schema_prefix_term,
                                           header_term,
                                           skip_header_term
    ) {
        let name: PrologText = name_term.get_ex()?;
        let csv: PrologText = csv_term.get_ex()?;
        let builder: WrappedBuilder = builder_term.get_ex()?;
        let schema_builder: WrappedBuilder = schema_builder_term.get_ex()?;
        let data_prefix: PrologText = data_prefix_term.get_ex()?;
        let schema_prefix: PrologText = schema_prefix_term.get_ex()?;

        // TODO original throws an exception here if the type does not match
        let has_header: bool = header_term.get_ex()?;
        let skip_header: bool = skip_header_term.get_ex()?;

        context.try_or_die_generic(import_csv(name.to_string(),
                                              csv.to_string(),
                                              &builder,
                                              Some(&schema_builder),
                                              data_prefix.to_string(),
                                              schema_prefix.to_string(),
                                              has_header,
                                              skip_header
        ))
    }
}

pub fn csv_name_iri(csv_name: String, data_prefix: String) -> (String, String) {
    let csv_name_escaped = urlencoding::encode(&csv_name);
    let csv_node = format!("{}CSV_{}", data_prefix, csv_name_escaped);
    return (csv_name_escaped, csv_node);
}

pub fn csv_row_type_info(
    csv_name: &str,
    column_names: &[String],
    schema_prefix: &str,
) -> (String, String, String) {
    let mut column_hasher = Sha1::new();
    let mut sorted_column_names = column_names.to_vec();
    sorted_column_names.sort();
    column_hasher.update(csv_name.clone());
    for field in sorted_column_names.iter() {
        // create a Sha1 object
        column_hasher.update(field);
    }
    let column_hash = column_hasher.finalize();
    let column_hash_string = hex::encode(column_hash);

    let row_type = format!("{}CSVRow_{}", schema_prefix, column_hash_string);
    // Create Row types
    let row_label = format!("\"CSV Row from {}\"@en", csv_name);
    let sorted_column_string = format!("CSV Row object for columns {:?}", sorted_column_names);
    let row_comment = format!("{:?}@en", sorted_column_string);

    (row_type, row_label, row_comment)
}

fn check_utf8(csv_path: PathBuf) -> Result<bool, csv::Error> {
    let csv_path_clone = csv_path.clone();
    let mut f = File::open(csv_path_clone)?;
    let mut buffer = [0; 2048];
    // read a chunk
    let _n = f.read(&mut buffer);

    let mut enc_detector = EncodingDetector::new();

    enc_detector.feed(&buffer, true);

    let res = enc_detector.guess(None, true);

    if res == UTF_8 {
        return Ok(true);
    } else {
        return Ok(false);
    }
}

pub fn import_csv(
    csv_name: String,
    csv_path: String,
    builder: &SyncStoreLayerBuilder,
    schema_builder_option: Option<&SyncStoreLayerBuilder>,
    data_prefix: String,
    schema_prefix: String,
    has_header: bool,
    skip_header: bool,
) -> Result<(), csv::Error> {
    let pathbuf: PathBuf = csv_path.into();

    if !check_utf8(pathbuf.clone())? {
        return Err(
            io::Error::new(io::ErrorKind::InvalidData, "Could not convert to utf-8").into(),
        );
    }
    let file = File::open(pathbuf)?;

    let mut reader = ReaderBuilder::new()
        .has_headers(has_header && !skip_header)
        .from_reader(file);

    let mut header = Vec::new();
    let mut column_names = Vec::new();

    let headers = reader.headers();
    // if headers.is_error(){
    //     return io::Error::new(io::ErrorKind::UnexpectedEof,"There are no lines in this CSV").into();
    // }
    let (csv_name_escaped, csv_node) = csv_name_iri(csv_name.clone(), data_prefix.clone());
    if !has_header || skip_header {
        let len = headers
            .expect("Expected a Some for headers but headers are empty")
            .len();
        for i in 0..len {
            header.push(format!(
                "{}{}_column_{}",
                schema_prefix, csv_name_escaped, i
            ));
            column_names.push(format!("{}", i));
        }
    } else {
        for field in headers
            .expect("Expected a Some for headers but headers are empty")
            .iter()
        {
            let escaped_field = urlencoding::encode(field);
            column_names.push(String::from(field));
            header.push(format!(
                "{}{}_column_{}",
                schema_prefix, csv_name_escaped, escaped_field
            ));
        }
    }

    // Prefixes
    let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    let rdfs = "http://www.w3.org/2000/01/rdf-schema#";
    let xsd = "http://www.w3.org/2001/XMLSchema#";

    let rdf_type = format!("{}{}", rdf, "type");
    let label = format!("{}{}", rdfs, "label");

    // Create the csv type
    let csv_type = format!("{}CSV", schema_prefix);
    let csv_name_value = format!("{:?}@en", csv_name);

    builder.add_string_triple(StringTriple::new_node(&csv_node, &rdf_type, &csv_type))?;
    builder.add_string_triple(StringTriple::new_value(&csv_node, &label, &csv_name_value))?;

    // Create the ordered column names metadata for the csv
    let mut column_index = 0;
    for field in column_names.iter() {
        let escaped_field = urlencoding::encode(field);
        let column_predicate = format!("{}csv_column", schema_prefix);
        let column_node = format!(
            "{}ColumnObject_{}_{}",
            data_prefix, csv_name_escaped, escaped_field
        );
        let column_type = format!("{}Column", schema_prefix);
        let column_index_predicate = format!("{}csv_column_index", schema_prefix);
        let column_index_value = format!("{}^^'{}{}'", column_index, xsd, "integer");
        let column_name_predicate = format!("{}csv_column_name", schema_prefix);
        let column_name_value = format!("{:?}^^'{}{}'", field, xsd, "string");

        builder.add_string_triple(StringTriple::new_node(
            &csv_node,
            &column_predicate,
            &column_node,
        ))?;

        builder.add_string_triple(StringTriple::new_node(
            &column_node,
            &rdf_type,
            &column_type,
        ))?;

        builder.add_string_triple(StringTriple::new_value(
            &column_node,
            &column_index_predicate,
            &column_index_value,
        ))?;

        builder.add_string_triple(StringTriple::new_value(
            &column_node,
            &column_name_predicate,
            &column_name_value,
        ))?;
        column_index += 1;
    }

    if let Some(schema_builder) = schema_builder_option {
        write_schema(&csv_name, schema_builder, &schema_prefix, &column_names)?;
    }

    let (row_type, _, _) = csv_row_type_info(&csv_name, &column_names, &schema_prefix);

    for result in reader.records() {
        let record = result?;
        // create a Sha1 object
        let mut hasher = Sha1::new();
        // add csv_name to hasher so we don't overlap CSVs
        hasher.update(csv_name.clone());
        // process input message
        for (_, field) in record.iter().enumerate() {
            hasher.update(field);
        }
        let hash = hasher.finalize();
        let hash_string = hex::encode(hash);
        let node = format!("{}CSVRow_{}", data_prefix, hash_string);
        let row_predicate = format!("{}csv_row", schema_prefix);

        // add row type
        builder.add_string_triple(StringTriple::new_node(&node, &rdf_type, &row_type))?;
        // add row predicate
        builder.add_string_triple(StringTriple::new_node(&csv_node, &row_predicate, &node))?;
        for (col, field) in record.iter().enumerate() {
            let value = format!("{:?}^^'{}{}'", field, xsd, "string");
            let column = &header[col];
            builder.add_string_triple(StringTriple::new_value(&node, &column, &value))?;
        }
    }

    return Ok(());
}

fn write_schema(
    csv_name: &str,
    schema_builder: &SyncStoreLayerBuilder,
    schema_prefix: &str,
    column_names: &[String],
) -> Result<(), csv::Error> {
    // Prefixes
    let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    let rdfs = "http://www.w3.org/2000/01/rdf-schema#";
    let xsd = "http://www.w3.org/2001/XMLSchema#";
    let owl = "http://www.w3.org/2002/07/owl#";

    // common predicates
    let rdf_type = format!("{}{}", rdf, "type");
    let label = format!("{}{}", rdfs, "label");
    let comment = format!("{}{}", rdfs, "comment");
    let domain = format!("{}{}", rdfs, "domain");
    let range = format!("{}{}", rdfs, "range");
    let owl_class = format!("{}{}", owl, "Class");
    let datatype_property = format!("{}{}", owl, "DatatypeProperty");
    let object_property = format!("{}{}", owl, "ObjectProperty");
    let sub_class_of = format!("{}{}", rdfs, "subClassOf");

    // common types
    let xsd_string = format!("{}{}", xsd, "string");
    let xsd_integer = format!("{}{}", xsd, "integer");

    // Create the csv object
    let csv_type = format!("{}CSV", schema_prefix);
    let csv_label = "\"CSV\"@en";
    let csv_comment = "\"CSV object\"@en";
    let document = "http://terminusdb.com/schema/system#Document";
    let (row_type, row_label, row_comment) =
        csv_row_type_info(csv_name, column_names, schema_prefix);

    schema_builder.add_string_triple(StringTriple::new_node(&csv_type, &rdf_type, &owl_class))?;
    schema_builder.add_string_triple(StringTriple::new_value(&csv_type, &label, &csv_label))?;
    schema_builder.add_string_triple(StringTriple::new_value(&csv_type, &comment, &csv_comment))?;
    schema_builder.add_string_triple(StringTriple::new_node(
        &csv_type,
        &sub_class_of,
        &document,
    ))?;

    // Create column objects and fields
    let column_predicate = format!("{}csv_column", schema_prefix);
    let column_predicate_label = "\"csv column\"@en";
    let column_predicate_comment = "\"Associates a CSV with a column object\"@en";

    let column_type = format!("{}Column", schema_prefix);
    let column_label = "\"Column\"@en";
    let column_comment = "\"Column information object for a CSV\"@en";
    let column_index_predicate = format!("{}csv_column_index", schema_prefix);
    let column_index_label = "\"csv column index\"@en";
    let column_index_comment = "\"The ordering index for a column in a csv\"@en";
    let column_name_predicate = format!("{}csv_column_name", schema_prefix);
    let column_name_label = "\"csv column name\"@en";
    let column_name_comment = "\"The name of the column as it was verbatim in the CSV\"@en";

    schema_builder.add_string_triple(StringTriple::new_node(
        &column_type,
        &rdf_type,
        &owl_class,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &column_type,
        &label,
        &column_label,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &column_type,
        &comment,
        &column_comment,
    ))?;
    // column
    schema_builder.add_string_triple(StringTriple::new_node(
        &column_predicate,
        &rdf_type,
        &object_property,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &column_predicate,
        &label,
        &column_predicate_label,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &column_predicate,
        &comment,
        &column_predicate_comment,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_node(
        &column_predicate,
        &domain,
        &csv_type,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_node(
        &column_predicate,
        &range,
        &column_type,
    ))?;

    // index
    schema_builder.add_string_triple(StringTriple::new_node(
        &column_index_predicate,
        &rdf_type,
        &datatype_property,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &column_index_predicate,
        &label,
        &column_index_label,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &column_index_predicate,
        &comment,
        &column_index_comment,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_node(
        &column_index_predicate,
        &domain,
        &column_type,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_node(
        &column_index_predicate,
        &range,
        &xsd_integer,
    ))?;

    // name
    schema_builder.add_string_triple(StringTriple::new_node(
        &column_name_predicate,
        &rdf_type,
        &datatype_property,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &column_name_predicate,
        &label,
        &column_name_label,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &column_name_predicate,
        &comment,
        &column_name_comment,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_node(
        &column_name_predicate,
        &domain,
        &column_type,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_node(
        &column_name_predicate,
        &range,
        &xsd_string,
    ))?;

    // Row super class
    let row_super = format!("{}CSVRow", schema_prefix);
    let row_super_label = "\"CSV Row\"@en";
    let row_super_comment = "\"Generic Row of a CSV file\"@en";

    schema_builder.add_string_triple(StringTriple::new_node(&row_super, &rdf_type, &owl_class))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &row_super,
        &label,
        &row_super_label,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &row_super,
        &comment,
        &row_super_comment,
    ))?;

    schema_builder.add_string_triple(StringTriple::new_node(&row_type, &rdf_type, &owl_class))?;
    schema_builder.add_string_triple(StringTriple::new_node(
        &row_type,
        &sub_class_of,
        &row_super,
    ))?;

    let system_prefix = "http://terminusdb.com/schema/system#";
    let csv_name_property = format!("{}csv_name", system_prefix);
    let csv_name_value = format!("{:?}@en", csv_name);
    schema_builder.add_string_triple(StringTriple::new_value(&row_type, &label, &row_label))?;
    schema_builder.add_string_triple(StringTriple::new_value(&row_type, &comment, &row_comment))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &row_type,
        &csv_name_property,
        &csv_name_value,
    ))?;
    // Row predicate
    let row_predicate = format!("{}csv_row", schema_prefix);
    let row_predicate_label = "\"csv row\"@en";
    let row_predicate_comment = "\"Connects a CSV to its rows\"@en";
    let row_super = format!("{}CSVRow", schema_prefix);

    schema_builder.add_string_triple(StringTriple::new_node(
        &row_predicate,
        &rdf_type,
        &object_property,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &row_predicate,
        &label,
        &row_predicate_label,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_value(
        &row_predicate,
        &comment,
        &row_predicate_comment,
    ))?;
    schema_builder.add_string_triple(StringTriple::new_node(&row_predicate, &domain, &csv_type))?;
    schema_builder.add_string_triple(StringTriple::new_node(&row_predicate, &range, &row_super))?;

    let (csv_name_escaped, _) = csv_name_iri(csv_name.to_string(), "".to_string());

    // Create column predicates for each field
    for field in column_names {
        let escaped_field = urlencoding::encode(field);
        let column_p = format!(
            "{}{}_column_{}",
            schema_prefix, csv_name_escaped, escaped_field
        );
        let column_label = format!("\"Column {}\"@en", field);
        let column_comment = format!("\"CSV Column for header name {}\"@en", field);

        schema_builder.add_string_triple(StringTriple::new_node(
            &column_p,
            &rdf_type,
            &datatype_property,
        ))?;
        schema_builder.add_string_triple(StringTriple::new_value(
            &column_p,
            &label,
            &column_label,
        ))?;
        schema_builder.add_string_triple(StringTriple::new_value(
            &column_p,
            &comment,
            &column_comment,
        ))?;
        schema_builder.add_string_triple(StringTriple::new_node(&column_p, &domain, &row_type))?;
        schema_builder.add_string_triple(StringTriple::new_node(&column_p, &range, &xsd_string))?;
    }
    return Ok(());
}
