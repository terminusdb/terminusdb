//! Fast JSON parser using serde_json
//!
//! Provides json_read_string/2 which works like json_read_dict/3
//! Drop-in replacement with better performance.

use swipl::prelude::*;
use serde_json::{Value, Deserializer};
use serde::Deserialize;

/// Convert serde_json::Value to Prolog term
fn json_value_to_prolog_term<'a, C: QueryableContextType>(context: &'a Context<'a, C>, value: &Value) -> PrologResult<Term<'a>> {
    match value {
        Value::Number(n) => {
            // Return same format as library(http/json):json_read_dict
            let term = context.new_term_ref();
            
            // Try integer first (for small integers)
            if let Some(i) = n.as_i64() {
                term.unify(i)?;
            } else if let Some(u) = n.as_u64() {
                // Unsigned integer - convert via string for bigint support
                let num_str = u.to_string();
                term.put(&num_str)?;
            } else if let Some(f) = n.as_f64() {
                // Float
                term.unify(f)?;
            } else {
                // Should not happen with valid JSON
                return Err(PrologError::Failure);
            }
            Ok(term)
        }
        Value::String(s) => {
            let term = context.new_term_ref();
            term.unify(s.as_str())?;
            Ok(term)
        }
        Value::Bool(b) => {
            let atom = if *b { Atom::new("true") } else { Atom::new("false") };
            let term = context.new_term_ref();
            term.unify(atom)?;
            Ok(term)
        }
        Value::Null => {
            let atom = Atom::new("null");
            let term = context.new_term_ref();
            term.unify(atom)?;
            Ok(term)
        }
        Value::Array(arr) => {
            let mut terms = Vec::with_capacity(arr.len());
            for item in arr {
                terms.push(json_value_to_prolog_term(context, item)?);
            }
            let list_term = context.new_term_ref();
            list_term.unify(terms.as_slice())?;
            Ok(list_term)
        }
        Value::Object(map) => {
            // Build Prolog dict using DictBuilder
            let mut builder = DictBuilder::new().tag("json");
            
            for (key, val) in map.iter() {
                let val_term = json_value_to_prolog_term(context, val)?;
                builder = builder.entry(key.as_str(), val_term);
            }
            
            let term = context.new_term_ref();
            term.put(&builder)?;
            Ok(term)
        }
    }
}

predicates! {
    /// Parse JSON from string using fast Rust parser
    /// Original string-based parser kept for backward compatibility
    #[module("$json_preserve")]
    semidet fn json_read_string(context, json_string_term, result_term) {
        let json_str: String = json_string_term.get()?;
        
        // Parse JSON with serde
        let value: Value = serde_json::from_str(&json_str)
            .map_err(|_e| PrologError::Failure)?;
        
        // Convert to Prolog term with regular numbers
        let prolog_term = json_value_to_prolog_term(context, &value)?;
        result_term.unify(&prolog_term)
    }
    
    /// Parse JSON from a Prolog stream
    /// 
    /// Note: This reads the entire stream to handle UTF-8 correctly.
    /// Memory usage is O(stream size), same as if Prolog's read_string were used.
    /// Used as a fallback for streams that can provide Content-Length for read_exact.
    #[module("$json_preserve")]
    semidet fn json_read_stream(context, stream_term, result_term) {
        use std::io::Read;
        
        // Get readable stream from term
        let mut stream: ReadablePrologStream = stream_term.get_ex()?;
        
        // Read entire stream into buffer
        let mut buf = Vec::new();
        match stream.read_to_end(&mut buf) {
            Ok(0) => return Err(PrologError::Failure), // Empty = EOF
            Ok(_) => {},
            Err(_) => return Err(PrologError::Failure),
        }
        
        // Parse from byte slice
        let value: Value = serde_json::from_slice(&buf)
            .map_err(|_| PrologError::Failure)?;
        
        // Convert to Prolog term
        let prolog_term = json_value_to_prolog_term(context, &value)?;
        
        // Unify with result
        result_term.unify(&prolog_term)
    }
}

pub fn register() {
    register_json_read_string();
    register_json_read_stream();
}
