//! Clark-notation expanded names for `xsd:QName` and `xbrl:SQName` literals.
//!
//! Storage form is always Clark notation `{namespace-IRI}localname`.  The
//! lexical edge form is `prefix:localname` (OIM §3.1: first-colon split,
//! NCName prefix, non-whitespace localname; `xsd:QName` additionally requires
//! an NCName localname).  Rendering is `prefix:localname` when the namespace
//! is bound by exact whole-string match in the active prefix map — never
//! longest-prefix substring matching, which corrupts namespaces that merely
//! share a leading substring — and the Clark form verbatim otherwise.

use std::collections::HashMap;

/// Namespace URI of an `xsd:QName` / `xbrl:SQName` Clark value is delimited by
/// the first `}`; everything after it is the localname (which may itself
/// contain `}`, `:` or `/` for SQName).
pub fn split_clark(clark: &str) -> Option<(&str, &str)> {
    let rest = clark.strip_prefix('{')?;
    let end = rest.find('}')?;
    let ns = &rest[..end];
    let local = &rest[end + 1..];
    if ns.is_empty() || local.is_empty() {
        return None;
    }
    Some((ns, local))
}

/// Render a stored Clark value as `prefix:local` by exact-match namespace
/// lookup, or verbatim Clark when unbound.  `uri_to_prefix` comes from
/// `reverse_prefix_map` (or `get_layer_reverse_prefix_map`).
pub fn clark_to_lexical(clark: &str, uri_to_prefix: &HashMap<String, String>) -> String {
    match split_clark(clark) {
        Some((ns, local)) => match uri_to_prefix.get(ns) {
            Some(prefix) => format!("{}:{}", prefix, local),
            None => clark.to_string(),
        },
        None => clark.to_string(),
    }
}

/// Convert a `prefix:local` lexical or `{uri}local` Clark input to canonical
/// Clark storage form.  `strict_local` is true for `xsd:QName` (local must be
/// an NCName) and false for `xbrl:SQName` (any non-whitespace local).
/// Err carries a human-readable reason for the caller's error type.
pub fn lexical_to_clark(
    lex: &str,
    prefix_to_uri: &HashMap<String, String>,
    strict_local: bool,
) -> Result<String, String> {
    if lex.starts_with('{') {
        let (ns, local) = match split_clark(lex) {
            Some(parts) => parts,
            None => return Err(format!("malformed Clark name {}", lex)),
        };
        validate_local(local, lex, strict_local)?;
        let _ = ns;
        Ok(lex.to_string())
    } else {
        let idx = lex
            .find(':')
            .ok_or_else(|| format!("no prefix separator ':' in {}", lex))?;
        let (prefix, local) = (&lex[..idx], &lex[idx + 1..]);
        if prefix.is_empty() {
            return Err(format!("empty prefix in {}", lex));
        }
        if !is_ncname(prefix) {
            return Err(format!("prefix is not an NCName in {}", lex));
        }
        validate_local(local, lex, strict_local)?;
        match prefix_to_uri.get(prefix) {
            Some(uri) => Ok(format!("{{{}}}{}", uri, local)),
            None => Err(format!("unbound prefix in {}", lex)),
        }
    }
}

fn validate_local(local: &str, lex: &str, strict_local: bool) -> Result<(), String> {
    if local.is_empty() {
        return Err(format!("empty local name in {}", lex));
    }
    if local.chars().any(|c| c.is_whitespace()) {
        return Err(format!("whitespace in local name of {}", lex));
    }
    if strict_local && !is_ncname(local) {
        return Err(format!("local name is not an NCName in {}", lex));
    }
    Ok(())
}

/// Approximation of XML NCName: first char alphabetic or '_', remainder
/// alphanumeric or one of '-', '_', '.'.  Matches the byte-scan style of
/// `split_prefixed_name`; the Prolog `xsd_parser` grammar check is
/// authoritative at schema-validation time.
fn is_ncname(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '.')
}

/// Invert a prefix→URI map into URI→prefix for exact-match Clark render.
/// `@`-keys (context keywords such as `@base`, not namespace bindings) are
/// skipped; when two prefixes bind the same URI the lexicographically
/// smallest prefix wins so rendering is deterministic.
pub fn reverse_prefix_map(prefix_to_uri: &HashMap<String, String>) -> HashMap<String, String> {
    let mut rev = HashMap::with_capacity(prefix_to_uri.len());
    for (prefix, uri) in prefix_to_uri {
        if prefix.starts_with('@') {
            continue;
        }
        match rev.entry(uri.clone()) {
            std::collections::hash_map::Entry::Vacant(e) => {
                e.insert(prefix.clone());
            }
            std::collections::hash_map::Entry::Occupied(mut e) => {
                if prefix.as_str() < e.get().as_str() {
                    e.insert(prefix.clone());
                }
            }
        }
    }
    rev
}

#[cfg(test)]
mod tests {
    use super::*;

    fn prefixes() -> HashMap<String, String> {
        HashMap::from([
            ("eg".to_string(), "http://example.org/ns#".to_string()),
            ("xbrl".to_string(), "https://xbrl.org/2021".to_string()),
            ("zz".to_string(), "http://dup.example/ns".to_string()),
            ("aa".to_string(), "http://dup.example/ns".to_string()),
            ("@base".to_string(), "http://base.example/".to_string()),
        ])
    }

    #[test]
    fn clark_to_lexical_exact_match() {
        let rev = reverse_prefix_map(&prefixes());
        assert_eq!(
            "eg:foo",
            clark_to_lexical("{http://example.org/ns#}foo", &rev)
        );
    }

    #[test]
    fn clark_to_lexical_unbound_returns_clark() {
        let rev = reverse_prefix_map(&prefixes());
        assert_eq!(
            "{http://unbound.example/}x",
            clark_to_lexical("{http://unbound.example/}x", &rev)
        );
    }

    #[test]
    fn clark_to_lexical_no_substring_match() {
        // eg -> http://example.org/ns# must NOT contract a namespace that
        // merely starts with that URI.
        let rev = reverse_prefix_map(&prefixes());
        assert_eq!(
            "{http://example.org/ns#sub}foo",
            clark_to_lexical("{http://example.org/ns#sub}foo", &rev)
        );
    }

    #[test]
    fn clark_to_lexical_smallest_prefix_on_tie() {
        let rev = reverse_prefix_map(&prefixes());
        assert_eq!("aa:x", clark_to_lexical("{http://dup.example/ns}x", &rev));
    }

    #[test]
    fn clark_to_lexical_local_with_colons_and_braces() {
        let rev = reverse_prefix_map(&prefixes());
        assert_eq!(
            "xbrl:foo:bar}baz",
            clark_to_lexical("{https://xbrl.org/2021}foo:bar}baz", &rev)
        );
    }

    #[test]
    fn lexical_to_clark_expands_bound_prefix() {
        assert_eq!(
            "{http://example.org/ns#}foo".to_string(),
            lexical_to_clark("eg:foo", &prefixes(), false).unwrap()
        );
    }

    #[test]
    fn lexical_to_clark_first_colon_split_keeps_local_colons() {
        assert_eq!(
            "{http://example.org/ns#}a:b:c".to_string(),
            lexical_to_clark("eg:a:b:c", &prefixes(), false).unwrap()
        );
    }

    #[test]
    fn lexical_to_clark_clark_passthrough() {
        let clark = "{http://unbound.example/}x";
        assert_eq!(
            clark.to_string(),
            lexical_to_clark(clark, &prefixes(), false).unwrap()
        );
    }

    #[test]
    fn lexical_to_clark_rejects_unbound_prefix() {
        assert!(lexical_to_clark("no:foo", &prefixes(), false).is_err());
    }

    #[test]
    fn lexical_to_clark_rejects_malformed_input() {
        for bad in ["nocolon", ":x", "p:", "{ns}", "{}x", "{ns"] {
            assert!(
                lexical_to_clark(bad, &prefixes(), false).is_err(),
                "expected error for {}",
                bad
            );
        }
    }

    #[test]
    fn lexical_to_clark_rejects_whitespace_local() {
        assert!(lexical_to_clark("eg:a b", &prefixes(), false).is_err());
        assert!(lexical_to_clark("{http://example.org/ns#}a b", &prefixes(), false).is_err());
    }

    #[test]
    fn lexical_to_clark_strict_local_requires_ncname() {
        assert!(lexical_to_clark("eg:a:b", &prefixes(), true).is_err());
        assert_eq!(
            "{http://example.org/ns#}a".to_string(),
            lexical_to_clark("eg:a", &prefixes(), true).unwrap()
        );
        assert!(lexical_to_clark("{http://example.org/ns#}a:b", &prefixes(), true).is_err());
    }

    #[test]
    fn reverse_prefix_map_skips_at_keys() {
        let rev = reverse_prefix_map(&prefixes());
        assert!(!rev.contains_key("http://base.example/"));
    }
}
