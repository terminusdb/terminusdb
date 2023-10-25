use aho_corasick::AhoCorasickBuilder;
use regex::Regex;

use super::frame::GraphQLName;

// This should probably use Aho-Corasick instead
const DIACRITICS: &[&str] = &[
    "Ä", "Æ", "Ǽ", "Đ", "Ð", "Ƒ", "Ħ", "I", "Ł", "Ø", "Ǿ", "Ö", "Œ", "ß", "Ŧ", "Ü", "ä", "æ", "ǽ",
    "đ", "ð", "ƒ", "ħ", "i", "ł", "ø", "ǿ", "ö", "œ", "ß", "ŧ", "ü",
];

const DIGRAPHS: &[&str] = &[
    "AE", "AE", "AE", "D", "D", "F", "H", "I", "L", "O", "O", "OE", "OE", "SS", "T", "UE", "ae",
    "ae", "ae", "d", "d", "f", "h", "i", "l", "o", "o", "oe", "oe", "ss", "t", "ue",
];

pub fn graphql_sanitize(string: &str) -> GraphQLName {
    let ac = AhoCorasickBuilder::new().build(DIACRITICS);
    let new_string = ac.replace_all(string, DIGRAPHS);
    let re = Regex::new("^[^_a-zA-Z]|[^[_a-zA-Z0-9]]").unwrap();
    let nobadchars = re.replace_all(&new_string, "_");
    let re = Regex::new("^_+").unwrap();
    GraphQLName(re.replace_all(&nobadchars, "_").into())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn graphql_sanitize_check() {
        assert_eq!(
            graphql_sanitize("Document-TF-IDF"),
            GraphQLName("Document_TF_IDF".into())
        );
        assert_eq!(
            graphql_sanitize("doc:Document"),
            GraphQLName("doc_Document".into())
        );
        assert_eq!(
            graphql_sanitize("SørenLorenson"),
            GraphQLName("SorenLorenson".into())
        );
        assert_eq!(graphql_sanitize("ÖBB"), GraphQLName("OEBB".into()));
    }
}
