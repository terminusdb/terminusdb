use super::consts::*;
use std::borrow::Cow;

fn common_prefix<'a>(s1: &'a [u8], s2: &'a [u8]) -> &'a [u8] {
    for i in 0..s1.len() {
        if s2.len() <= i || s1[i] != s2[i] {
            return &s1[..i];
        }
    }

    s1
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum PrefixContraction<'a> {
    Base,
    Schema,
    Other(&'a str),
    JSON,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Prefix {
    Schema(String),
    Base(String),
    Other(String, String),
    JSON,
}

impl Prefix {
    pub fn base(expansion: &str) -> Prefix {
        Prefix::Base(expansion.to_string())
    }
    pub fn schema(expansion: &str) -> Prefix {
        Prefix::Schema(expansion.to_string())
    }
    pub fn other(contraction: &str, expansion: &str) -> Prefix {
        Prefix::Other(contraction.to_string(), expansion.to_string())
    }

    pub fn contraction(&self) -> PrefixContraction {
        match self {
            Prefix::Schema(_) => PrefixContraction::Schema,
            Prefix::Base(_) => PrefixContraction::Base,
            Prefix::Other(contraction, _) => PrefixContraction::Other(contraction),
            Prefix::JSON => PrefixContraction::JSON,
        }
    }

    pub fn expansion(&self) -> &str {
        match self {
            Prefix::Schema(e) => e,
            Prefix::Base(e) => e,
            Prefix::Other(_, e) => e,
            Prefix::JSON => SYS_JSON_PREFIX,
        }
    }
}

#[derive(PartialEq, Eq)]
struct PrefixContracterTree {
    prefix: Option<Prefix>,
    part: Vec<u8>,
    children: Vec<PrefixContracterTree>,
}

impl std::fmt::Debug for PrefixContracterTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "PrefixContracterTree {{ prefix: {:?}, part: {:?}, children: {:?} }}",
            self.prefix,
            String::from_utf8_lossy(&self.part),
            self.children
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PrefixContracter {
    trees: Vec<PrefixContracterTree>,
}

impl PrefixContracter {
    pub fn new<I: IntoIterator<Item = Prefix>>(prefixes: I) -> PrefixContracter {
        let mut items: Vec<_> = prefixes.into_iter().collect();

        items.push(Prefix::JSON);

        items.sort_by(|p1, p2| p1.expansion().cmp(&p2.expansion()));
        items.dedup();
        items.reverse();

        // TODO verify that we don't have duplicates

        let mut stack: Vec<PrefixContracterTree> = Vec::new();

        let mut intermediate: Vec<PrefixContracterTree> = Vec::new();

        let mut offset = 0;
        let mut matched: Vec<u8> = Vec::with_capacity(0);
        loop {
            if let Some(prefix) = items.pop() {
                if let Some(mut previous) = stack.pop() {
                    let common = common_prefix(&matched, prefix.expansion().as_bytes()).to_vec();
                    if common.len() == matched.len() {
                        // this prefix is a continuation of the previous one. neat.
                        let part = prefix.expansion().as_bytes()[common.len()..].to_vec();

                        let current = PrefixContracterTree {
                            part,
                            prefix: Some(prefix),
                            children: Vec::new(),
                        };
                        offset += previous.part.len();
                        matched.extend_from_slice(current.part.as_slice());
                        stack.push(previous);
                        stack.push(current);
                    } else {
                        // This prefix deviates from previous.
                        // we must unwind until we reach a point where it can be made a continuation.
                        // This is either because we discover a point where it is an exact continuation, or we find a point where we can make a branch, or roll up to termination.

                        loop {
                            if common.len() == offset + previous.part.len() {
                                // we found an exact continuation point. by pushing this back onto the stack we're good.
                                stack.push(previous);

                                matched = common;
                                break;
                            } else if common.len() > offset
                                && common.len() < offset + previous.part.len()
                            {
                                // we found a branch point. We set up a branch tree entry and push it onto the stack. next iteration of the outer loop will find it as something it is able to continue on.
                                let part = common[offset..].to_vec();
                                previous.part = previous.part[part.len()..].to_vec();

                                let current = PrefixContracterTree {
                                    part,
                                    prefix: None,
                                    children: vec![previous],
                                };

                                offset = common.len() - current.part.len();
                                matched = common;

                                stack.push(current);

                                break;
                            } else {
                                if let Some(mut parent) = stack.pop() {
                                    // we're rolling up things here. We know there will be no further extensions until we come in a part where we can branch.
                                    //if let Some(last) = stack.last() {
                                    //    offset -= last.part.len();
                                    //}
                                    //else {
                                    //    offset = 0;
                                    //}
                                    offset -= parent.part.len();

                                    parent.children.push(previous);
                                    previous = parent;
                                } else {
                                    // we've terminated, there are no more parents, this is a result.
                                    intermediate.push(previous);
                                    break;
                                }
                            }
                        }

                        // prefix will be re-popped on next iteration
                        items.push(prefix);
                    }
                } else {
                    // nothing on stack means we're starting a completely new segment. exciting.
                    let part = prefix.expansion().as_bytes().to_vec();
                    let current = PrefixContracterTree {
                        part,
                        prefix: Some(prefix),
                        children: Vec::new(),
                    };

                    offset = 0;
                    matched = current.part.clone();
                    stack.push(current);
                }
            } else {
                break;
                // nothing left on item, all that remains to be done is to roll up the stack (if any) into a final result.
            }
        }

        if !stack.is_empty() {
            let mut previous = stack.pop().unwrap();
            loop {
                if let Some(mut parent) = stack.pop() {
                    // we're rolling up things here. We know there will be no further extensions until we come in a part where we can branch.
                    if let Some(last) = stack.last() {
                        offset -= last.part.len();
                    } else {
                        offset = 0;
                    }

                    parent.children.push(previous);
                    previous = parent;
                } else {
                    // we've terminated, there are no more parents, this is a result.
                    intermediate.push(previous);
                    break;
                }
            }
        }

        Self {
            trees: intermediate,
        }
    }

    pub fn contract<'a>(&self, s: &'a str) -> Option<(PrefixContraction, &'a str)> {
        let mut cur: &[PrefixContracterTree] = &self.trees;
        let slice = s.as_bytes();
        let mut offset = 0;
        let mut prefix = None;
        'outer: loop {
            if cur.is_empty() {
                break;
            }

            // TODO There is a chance that using compare instead of
            // equals may result in faster code. After all, the prefix
            // tree is sorted, so if we can stop looking once we
            // encounter parts ordered after our search slice. But
            // whether or not this actually is faster depends on
            // implementation details of string comparison vs string
            // ordering, and this will require measuring to figure out
            // which one is best.
            for c in cur {
                if c.part.len() <= slice.len() - offset
                    && c.part == slice[offset..offset + c.part.len()]
                {
                    if let Some(p) = &c.prefix {
                        // this is a possible match, though a more specific match may be found after.
                        prefix = Some(p);
                    }

                    offset += c.part.len();
                    cur = &c.children;
                    continue 'outer;
                }
            }

            // if we're here, we didn't find anything to descend in, so let's break.
            break;
        }

        if let Some(p) = prefix {
            // Unsafe justification: The slice originates as an utf8
            // string. The eliminated prefix is a complete utf8
            // sequence. Therefore, the remainder must be a complete
            // utf8 sequence too.
            let remainder = unsafe { std::str::from_utf8_unchecked(&slice[p.expansion().len()..]) };

            Some((p.contraction(), remainder))
        } else {
            None
        }
    }

    pub fn schema_contract<'a>(&self, s: &'a str) -> Cow<'a, str> {
        match self.contract(s) {
            None => Cow::Borrowed(s),
            Some((PrefixContraction::Base, rest)) => Cow::Owned(format!("@base:{}", rest)),
            Some((PrefixContraction::Schema, rest)) => Cow::Borrowed(rest),
            Some((PrefixContraction::Other(p), rest)) => Cow::Owned(format!("{}:{}", p, rest)),
            Some((PrefixContraction::JSON, rest)) => Cow::Borrowed(rest),
        }
    }

    pub fn instance_contract<'a>(&self, s: &'a str) -> Cow<'a, str> {
        match self.contract(s) {
            None => Cow::Borrowed(s),
            Some((PrefixContraction::Base, rest)) => Cow::Borrowed(rest),
            Some((PrefixContraction::Schema, rest)) => Cow::Owned(format!("@schema:{}", rest)),
            Some((PrefixContraction::Other(p), rest)) => Cow::Owned(format!("{}:{}", p, rest)),
            Some((PrefixContraction::JSON, rest)) => Cow::Borrowed(rest),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn contracter_builds_right_thing() {
        let x = vec![
            Prefix::other("y", "http://foo/bar/yetanother/"),
            Prefix::base("http://foo/bar/documents/"),
            Prefix::schema("http://foo/bar#"),
            Prefix::other("c", "http://quux/moo"),
            Prefix::other("b", "http://foooo/x/y/"),
            Prefix::other("a", "http://foooo/x/"),
            Prefix::other("d", "abc://def"),
            Prefix::other("x", "http://foo/bar/other/"),
        ];

        let result = PrefixContracter::new(x);
        let expected = PrefixContracter {
            trees: vec![
                PrefixContracterTree {
                    part: b"abc://def".to_vec(),
                    prefix: Some(Prefix::other("d", "abc://def")),
                    children: vec![],
                },
                PrefixContracterTree {
                    part: b"http://".to_vec(),
                    prefix: None,
                    children: vec![
                        PrefixContracterTree {
                            part: b"foo".to_vec(),
                            prefix: None,
                            children: vec![
                                PrefixContracterTree {
                                    part: b"/bar".to_vec(),
                                    prefix: None,
                                    children: vec![
                                        PrefixContracterTree {
                                            part: b"#".to_vec(),
                                            prefix: Some(Prefix::schema("http://foo/bar#")),
                                            children: vec![],
                                        },
                                        PrefixContracterTree {
                                            part: b"/".to_vec(),
                                            prefix: None,
                                            children: vec![
                                                PrefixContracterTree {
                                                    part: b"documents/".to_vec(),
                                                    prefix: Some(Prefix::base(
                                                        "http://foo/bar/documents/",
                                                    )),
                                                    children: vec![],
                                                },
                                                PrefixContracterTree {
                                                    part: b"other/".to_vec(),
                                                    prefix: Some(Prefix::other(
                                                        "x",
                                                        "http://foo/bar/other/",
                                                    )),
                                                    children: vec![],
                                                },
                                                PrefixContracterTree {
                                                    part: b"yetanother/".to_vec(),
                                                    prefix: Some(Prefix::other(
                                                        "y",
                                                        "http://foo/bar/yetanother/",
                                                    )),
                                                    children: vec![],
                                                },
                                            ],
                                        },
                                    ],
                                },
                                PrefixContracterTree {
                                    part: b"oo/x/".to_vec(),
                                    prefix: Some(Prefix::other("a", "http://foooo/x/")),
                                    children: vec![PrefixContracterTree {
                                        part: b"y/".to_vec(),
                                        prefix: Some(Prefix::other("b", "http://foooo/x/y/")),
                                        children: vec![],
                                    }],
                                },
                            ],
                        },
                        PrefixContracterTree {
                            part: b"quux/moo".to_vec(),
                            prefix: Some(Prefix::other("c", "http://quux/moo")),
                            children: vec![],
                        },

                        // this is the default json prefix
                        PrefixContracterTree {
                            part: b"terminusdb.com/schema/json#".to_vec(),
                            prefix: Some(Prefix::JSON),
                            children: vec![],
                        },
                    ],
                },
            ],
        };

        assert_eq!(expected, result);
    }

    #[test]
    fn contracter_contracts() {
        let x = vec![
            Prefix::other("y", "http://foo/bar/yetanother/"),
            Prefix::base("http://foo/bar/documents/"),
            Prefix::schema("http://foo/bar#"),
            Prefix::other("c", "http://quux/moo"),
            Prefix::other("b", "http://foooo/x/y/"),
            Prefix::other("a", "http://foooo/x/"),
            Prefix::other("d", "abc://def/"),
            Prefix::other("x", "http://foo/bar/other/"),
        ];

        let c = PrefixContracter::new(x);

        assert_eq!(
            Some((PrefixContraction::Base, "moo")),
            c.contract("http://foo/bar/documents/moo")
        );

        assert_eq!(
            Some((PrefixContraction::Schema, "SomeType")),
            c.contract("http://foo/bar#SomeType")
        );

        assert_eq!(
            Some((PrefixContraction::Other("x"), "moo")),
            c.contract("http://foo/bar/other/moo")
        );

        assert_eq!(
            Some((PrefixContraction::Other("d"), "bar")),
            c.contract("abc://def/bar")
        );

        assert_eq!(
            Some((PrefixContraction::Other("b"), "baz")),
            c.contract("http://foooo/x/y/baz")
        );

        assert_eq!(None, c.contract("http://fooo/bar/other/moo"));
    }

    #[test]
    fn contracter_schema_contracts() {
        let x = vec![
            Prefix::other("y", "http://foo/bar/yetanother/"),
            Prefix::base("http://foo/bar/documents/"),
            Prefix::schema("http://foo/bar#"),
            Prefix::other("c", "http://quux/moo"),
            Prefix::other("b", "http://foooo/x/y/"),
            Prefix::other("a", "http://foooo/x/"),
            Prefix::other("d", "abc://def/"),
            Prefix::other("x", "http://foo/bar/other/"),
        ];

        let c = PrefixContracter::new(x);

        assert_eq!(
            "@base:moo",
            c.schema_contract("http://foo/bar/documents/moo")
        );

        assert_eq!("SomeType", c.schema_contract("http://foo/bar#SomeType"));

        assert_eq!("x:moo", c.schema_contract("http://foo/bar/other/moo"));

        assert_eq!("d:bar", c.schema_contract("abc://def/bar"));

        assert_eq!("b:baz", c.schema_contract("http://foooo/x/y/baz"));

        assert_eq!(
            "http://fooo/bar/other/moo",
            c.schema_contract("http://fooo/bar/other/moo")
        );
    }

    #[test]
    fn contracter_instance_contracts() {
        let x = vec![
            Prefix::other("y", "http://foo/bar/yetanother/"),
            Prefix::base("http://foo/bar/documents/"),
            Prefix::schema("http://foo/bar#"),
            Prefix::other("c", "http://quux/moo"),
            Prefix::other("b", "http://foooo/x/y/"),
            Prefix::other("a", "http://foooo/x/"),
            Prefix::other("d", "abc://def/"),
            Prefix::other("x", "http://foo/bar/other/"),
        ];

        let c = PrefixContracter::new(x);

        assert_eq!("moo", c.instance_contract("http://foo/bar/documents/moo"));

        assert_eq!(
            "@schema:SomeType",
            c.instance_contract("http://foo/bar#SomeType")
        );

        assert_eq!("x:moo", c.instance_contract("http://foo/bar/other/moo"));

        assert_eq!("d:bar", c.instance_contract("abc://def/bar"));

        assert_eq!("b:baz", c.instance_contract("http://foooo/x/y/baz"));

        assert_eq!(
            "http://fooo/bar/other/moo",
            c.instance_contract("http://fooo/bar/other/moo")
        );
    }
}
