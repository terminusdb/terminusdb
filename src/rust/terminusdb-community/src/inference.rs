//! Ladder-aware castability + specificity for document type inference.
//!
//! Why: document inference runs the specificity ladder once per field value
//! per candidate type. The Prolog path calls json_value_cast_type/4 and
//! cast_specificity/3 in sequence — the dominant insert cost on typed
//! values. Fusing both into one native predicate removes that per-value
//! Prolog round-trip for the common scalar casts while keeping the Prolog
//! semantics bit-for-bit where they matter (witnesses, failure modes).
//!
//! cast_level/3 fuses what the Prolog inference path did as two steps — the
//! json_value_cast_type/4 gate plus cast_specificity/3 — into one native
//! call for the scalar ladder types:
//!
//!   integer family < decimal < float/double < xdd:json < string family
//!
//! For a handled target type it unifies Level with the interpretation
//! specificity when the cast succeeds, and fails when the cast is
//! impossible (mirroring the casting_error outcome). For target types or
//! value forms outside the ladder it unifies the atom `unhandled` so the
//! caller can run the original Prolog path — dates, intervals, QNames,
//! validated name types, and bare rationals stay there. These casts are
//! cheaper natively than in Prolog too, so dates/intervals/QNames are the
//! next candidates to absorb; only the scalar ladder was needed for the
//! measured win, so they are left to Prolog for now.
//!
//! Typed literals `V^^T` (e.g. wire decimals parsed as `NrD^^xsd:decimal`)
//! get level 0 on an exact type match, level 5 when the inner value provably
//! recasts (integral inner -> integer family, rational/float inner ->
//! decimal/double/string), and `unhandled` otherwise so Prolog keeps the
//! authentic casting witness. Inner-term classification uses `PL_term_type`
//! on the compound's first argument — `PL_RATIONAL` is never integral since
//! SWI normalizes `Nr1` to `N` (a `get_arg::<i64>` probe would silently
//! truncate `3r2`, misclassifying it as integral).
//!
//! Castability mirrors typecast_switch semantics observed in casting.pl:
//! - strings/atoms cast via the string source
//! - integers via the xsd:decimal source (so only targets having a
//!   (T, decimal) clause accept them — e.g. xsd:string but not xsd:token)
//! - floats via their ~w-formatted string ("1.5"^^xsd:string), hence never
//!   castable to the integer family
//! - booleans are atoms and therefore also cast via the string source
//! - xdd:json accepts any string that parses as JSON (incl. scalars)

use serde_json::Value as JsonValue;
use swipl::fli::*;
use swipl::prelude::*;

/// The ladder classes `cast_level` decides natively.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Class {
    /// xsd:integer — strict [+-]?digit+ string parse (the `integer` DCG)
    Integer,
    /// derived integer types — number lexicals plus an is_* range
    SubInt {
        min: Option<i128>,
        max: Option<i128>,
    },
    Decimal,
    Float,
    Bool,
    /// xdd:json — only JSON objects/arrays in lexical strings
    Json,
    /// xsd:string — accepts every scalar form
    Str,
    /// string-subsumed unconditional types: normalizedString, token, anyURI
    /// (validated names such as NCName/Name/NMTOKEN/language stay unhandled)
    StrSub,
    /// xsd:anySimpleType — accepts every form via string forwarding
    Simple,
    Unhandled,
}

/// Local name of an (expanded or prefixed) type URI.
fn local_name(type_uri: &str) -> &str {
    if let Some((_, name)) = type_uri.rsplit_once('#') {
        name
    } else if let Some((_, name)) = type_uri.rsplit_once('/') {
        name
    } else {
        type_uri
            .rsplit_once(':')
            .map(|(_, name)| name)
            .unwrap_or(type_uri)
    }
}

fn classify(type_uri: &str) -> Class {
    match local_name(type_uri) {
        "integer" => Class::Integer,
        "byte" => Class::SubInt {
            min: Some(-128),
            max: Some(127),
        },
        "short" => Class::SubInt {
            min: Some(-32768),
            max: Some(32767),
        },
        "int" => Class::SubInt {
            min: Some(i32::MIN as i128),
            max: Some(i32::MAX as i128),
        },
        "long" => Class::SubInt {
            min: Some(i64::MIN as i128),
            max: Some(i64::MAX as i128),
        },
        "unsignedByte" => Class::SubInt {
            min: Some(0),
            max: Some(255),
        },
        "unsignedShort" => Class::SubInt {
            min: Some(0),
            max: Some(65535),
        },
        "unsignedInt" => Class::SubInt {
            min: Some(0),
            max: Some(u32::MAX as i128),
        },
        "unsignedLong" => Class::SubInt {
            min: Some(0),
            max: Some(u64::MAX as i128),
        },
        "positiveInteger" => Class::SubInt {
            min: Some(1),
            max: None,
        },
        "negativeInteger" => Class::SubInt {
            min: None,
            max: Some(-1),
        },
        "nonNegativeInteger" => Class::SubInt {
            min: Some(0),
            max: None,
        },
        "nonPositiveInteger" => Class::SubInt {
            min: None,
            max: Some(0),
        },
        "decimal" => Class::Decimal,
        "float" | "double" => Class::Float,
        "boolean" => Class::Bool,
        "json" => Class::Json,
        "string" => Class::Str,
        "normalizedString" | "token" | "anyURI" => Class::StrSub,
        "anySimpleType" => Class::Simple,
        _ => Class::Unhandled,
    }
}

/// Parsed integer value. Arbitrary-precision magnitudes saturate: a value
/// that does not fit u128 keeps only its sign (bounded ranges then always
/// reject it, sign-based ranges decide by sign — matching Prolog bignums).
#[derive(Debug, Clone, Copy)]
struct IntMagnitude {
    negative: bool,
    /// magnitude if it fits u128; None = overflow (larger than u128::MAX)
    mag: Option<u128>,
}

impl IntMagnitude {
    fn small(v: i64) -> Self {
        IntMagnitude {
            negative: v < 0,
            mag: Some(v.unsigned_abs() as u128),
        }
    }

    fn in_range(&self, min: Option<i128>, max: Option<i128>) -> bool {
        match self.mag {
            Some(m) => {
                // ±m fits i128 whenever m does, since |i128::MIN| = u128::MAX
                let v = if self.negative {
                    if m > (i128::MAX as u128) + 1 {
                        return min.is_none();
                    }
                    -(m as i128)
                } else if m > i128::MAX as u128 {
                    return max.is_none();
                } else {
                    m as i128
                };
                min.is_none_or(|lo| v >= lo) && max.is_none_or(|hi| v <= hi)
            }
            // overflow magnitude: passes a min bound iff negative (value is
            // -huge), passes a max bound iff positive (value is +huge)
            None => {
                if self.negative {
                    min.is_none()
                } else {
                    max.is_none()
                }
            }
        }
    }
}

/// The lexical forms a scalar can take on the wire.
enum Form {
    Int(IntMagnitude),
    /// Numeric but not a provably-small-or-bignum integer: rational,
    /// float, or integer beyond u64. Only used as the inner form of a
    /// pre-typed literal — bare values of these kinds stay unhandled.
    NumericOther,
    Float,
    Bool,
    Str(String),
    /// Pre-typed literal `V^^T` (e.g. `3r2^^xsd:decimal` emitted by the
    /// JSON reader for decimal-point wire values). The tag is a ladder
    /// hint: exact-type fields get level 0, other castable targets get
    /// level 5 with the value recast to them, and uncastable targets fall
    /// back to Prolog so its witness reports the authentic error.
    Typed {
        vtype: String,
        inner: Box<Form>,
    },
}

fn form_of(term: &Term) -> Option<Form> {
    match term.term_type() {
        TermType::Integer => {
            if let Ok(v) = term.get::<i64>() {
                Some(Form::Int(IntMagnitude::small(v)))
            } else if let Ok(f) = term.get::<f64>() {
                // bignum beyond i64: only the sign is needed for range logic
                Some(Form::Int(IntMagnitude {
                    negative: f.is_sign_negative(),
                    mag: None,
                }))
            } else {
                None
            }
        }
        TermType::Float => Some(Form::Float),
        TermType::Atom => {
            let name = term.get_atom_name(|n| n.map(|s| s.to_string())).ok()??;
            match name.as_str() {
                "true" | "false" => Some(Form::Bool),
                // 'null' casts via atom→string like any other atom in
                // json_value_cast_type (the atom clause precedes the
                // boolean/throw clauses)
                _ => Some(Form::Str(name)),
            }
        }
        TermType::String => term.get::<String>().ok().map(Form::Str),
        TermType::CompoundTerm => {
            // V^^T — only the two-argument ^^ shape with an atomic type tag.
            let functor = term.get::<Functor>().ok()?;
            if functor.arity() != 2 || functor.name().name() != "^^" {
                return None;
            }
            let vtype = term.get_arg::<Atom>(2).ok()?.name();
            // Classify the inner value by its real term type — the i64/u64
            // getables silently convert rationals and floats, so they must
            // be gated on PL_term_type, not tried directly.
            let inner = unsafe {
                let arg_ref = PL_new_term_ref();
                let ok = PL_get_arg(1, term.term_ptr(), arg_ref);
                let f = if ok.is_success() {
                    match PL_term_type(arg_ref) as u32 {
                        PL_INTEGER => {
                            if let Ok(v) = term.get_arg::<i64>(1) {
                                Form::Int(IntMagnitude::small(v))
                            } else if let Ok(v) = term.get_arg::<u64>(1) {
                                Form::Int(IntMagnitude {
                                    negative: false,
                                    mag: Some(v as u128),
                                })
                            } else {
                                // bignum beyond u64: keep the sign only
                                let neg = term
                                    .get_arg::<f64>(1)
                                    .map(|f| f.is_sign_negative())
                                    .unwrap_or(false);
                                Form::Int(IntMagnitude {
                                    negative: neg,
                                    mag: None,
                                })
                            }
                        }
                        // PL_RATIONAL is always non-integral (Nr1 normalizes
                        // to the integer N); PL_FLOAT is an IEEE double.
                        PL_RATIONAL | PL_FLOAT => Form::NumericOther,
                        _ => Form::Str(String::new()),
                    }
                } else {
                    Form::Str(String::new())
                };
                PL_reset_term_refs(arg_ref);
                f
            };
            Some(Form::Typed {
                vtype,
                inner: Box::new(inner),
            })
        }
        _ => None,
    }
}

/// Can a pre-typed `V^^T` value be recast to `class`?  Mirrors the subset
/// of typecast/4 whose success is decidable locally: a provably integral
/// inner reaches integer-family targets only inside their range; any
/// numeric inner reaches decimal/float/string. `None` means "cannot
/// prove locally" — the caller defers to Prolog's full cast check.
fn typed_recastable(inner: &Form, class: Class) -> Option<bool> {
    match (inner, class) {
        (Form::Int(_), Class::Integer) => Some(true),
        (Form::Int(m), Class::SubInt { min, max }) => Some(m.in_range(min, max)),
        (Form::Int(_), Class::Decimal | Class::Float | Class::Str) => Some(true),
        (Form::NumericOther, Class::Decimal | Class::Float | Class::Str) => Some(true),
        _ => None,
    }
}

/// The value Prolog's casting_error/witness path would report for a failed
/// cast: the intermediate representation the failing cast produced.
/// Strings and atoms fail after conversion to a string; floats after
/// conversion to their written atom ("1.0", not "1"); ints stay ints.
fn unify_cast_witness(out: &Term, form: &Form, src: &Term) -> PrologResult<()> {
    match form {
        Form::Float => {
            let f: f64 = src.get()?;
            let mut s = format!("{}", f);
            if !s.contains(['.', 'e', 'E', 'n', 'i']) {
                s.push_str(".0");
            }
            out.unify(Atom::new(&s))
        }
        Form::Bool => {
            let name = src.get_atom_name(|n| n.unwrap_or("").to_string())?;
            out.unify(name.as_str())
        }
        Form::Str(s) => out.unify(s.as_str()),
        Form::Int(_) => out.unify(src),
        // unreachable via ladder_level (NumericOther lives inside Typed
        // which is handled before it; Typed values take their own arm) —
        // keep the value itself as the witness like integers do.
        Form::NumericOther | Form::Typed { .. } => out.unify(src),
    }
}

/// digit run allowing single underscores between digits (number_string)
fn digit_run(s: &str, valid: impl Fn(u8) -> bool) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if valid(bytes[i]) {
            i += 1;
        } else if bytes[i] == b'_' && i + 1 < bytes.len() && valid(bytes[i + 1]) {
            i += 2;
        } else {
            break;
        }
    }
    if i == 0 {
        None
    } else {
        Some(i)
    }
}

/// What number_string would make of the lexical (kind only; the value only
/// matters for range checks, where it saturates like [IntMagnitude]).
enum NumLex {
    Int(IntMagnitude),
    Float,
    Rat,
}

fn digits_value(s: &str, radix: u32, negative: bool) -> IntMagnitude {
    let mut mag: u128 = 0;
    let mut overflow = false;
    for b in s.bytes() {
        if b == b'_' {
            continue;
        }
        let d = (b as char).to_digit(radix).unwrap_or(0) as u128;
        mag = match mag
            .checked_mul(radix as u128)
            .and_then(|m| m.checked_add(d))
        {
            Some(m) => m,
            None => {
                overflow = true;
                0
            }
        };
        if overflow {
            break;
        }
    }
    IntMagnitude {
        negative,
        mag: if overflow { None } else { Some(mag) },
    }
}

fn is_dec_digit(b: u8) -> bool {
    b.is_ascii_digit()
}

/// Mirror of number_string/2's accept set: [+-]? (0x… | 0o… | 0b… |
/// digits [r digits] | digits [. digits] [[eE][+-]?digits]) with single
/// underscores allowed between digits. "5.0"/"1e2" are Float, not Int.
fn number_lexical(s: &str) -> Option<NumLex> {
    let (neg, t) = match s.as_bytes().first() {
        Some(b'-') => (true, &s[1..]),
        Some(b'+') => (false, &s[1..]),
        _ => (false, s),
    };
    if t.len() >= 2 && &t[..2] == "0x" {
        let n = digit_run(&t[2..], |b| b.is_ascii_hexdigit())?;
        return (n == t.len() - 2).then(|| NumLex::Int(digits_value(&t[2..], 16, neg)));
    }
    if t.len() >= 2 && &t[..2] == "0o" {
        let n = digit_run(&t[2..], |b| (b'0'..=b'7').contains(&b))?;
        return (n == t.len() - 2).then(|| NumLex::Int(digits_value(&t[2..], 8, neg)));
    }
    if t.len() >= 2 && &t[..2] == "0b" {
        let n = digit_run(&t[2..], |b| b == b'0' || b == b'1')?;
        return (n == t.len() - 2).then(|| NumLex::Int(digits_value(&t[2..], 2, neg)));
    }
    let n = digit_run(t, is_dec_digit)?;
    let rest = &t[n..];
    if rest.is_empty() {
        return Some(NumLex::Int(digits_value(t, 10, neg)));
    }
    if let Some(frac) = rest.strip_prefix('r') {
        let m = digit_run(frac, is_dec_digit)?;
        return (m == frac.len()).then_some(NumLex::Rat);
    }
    // float: [digits . digits] [[eE] [+-]? digits]
    let mut tail = rest;
    let mut is_float = false;
    if let Some(f) = tail.strip_prefix('.') {
        let m = digit_run(f, is_dec_digit)?;
        tail = &f[m..];
        is_float = true;
    }
    if let Some(e) = tail.strip_prefix(|c| c == 'e' || c == 'E') {
        let e = e.strip_prefix(|c| c == '+' || c == '-').unwrap_or(e);
        let m = digit_run(e, is_dec_digit)?;
        is_float = m == e.len() && m > 0;
        tail = &e[m..];
    }
    (is_float && tail.is_empty()).then_some(NumLex::Float)
}

/// Mirror of string_decimal_to_rational/2: leading '-'s strip recursively,
/// a leading '.' canonicalizes to "0.", at most one '.' is allowed and each
/// side then needs only be number-lexical (frac "1e2" parses, as in Prolog).
fn decimal_lexical(s: &str) -> bool {
    let mut t = s;
    while let Some(r) = t.strip_prefix('-') {
        t = r;
    }
    let t = t
        .strip_prefix('.')
        .map(|r| format!("0.{}", r))
        .unwrap_or_else(|| t.to_string());
    match t.split('.').collect::<Vec<_>>().as_slice() {
        [_] => number_lexical(&t).is_some(),
        [intp, frac] => {
            (number_lexical(intp).is_some() || intp.is_empty())
                && !frac.is_empty()
                && number_lexical(frac).is_some()
        }
        _ => false,
    }
}

/// xsd:integer string parse — the strict `integer` DCG: [+-]? [0-9]+ only.
fn strict_int_lexical(s: &str) -> bool {
    let d = s.strip_prefix(|c| c == '+' || c == '-').unwrap_or(s);
    !d.is_empty() && d.bytes().all(is_dec_digit)
}

/// xdd:json: the string must parse as strict ISO JSON — atom_json_dict/3
/// is deliberately NOT mirrored here: its Prolog-number leniency (0x…,
/// 1_0, 3r4, 007, "5.", "+7") must not cause a string to be *inferred*
/// as JSON, even though the explicit typecast path still accepts them.
fn json_lexical(s: &str) -> bool {
    serde_json::from_str::<JsonValue>(s).is_ok()
}

/// castability + ladder level for a (value form, handled class) pair.
/// None = not castable (the Prolog casting_error outcome).
fn ladder_level(form: &Form, class: Class) -> Option<u64> {
    match (form, class) {
        (Form::Int(_), Class::Integer) => Some(0),
        (Form::Int(v), Class::SubInt { min, max }) => v.in_range(min, max).then_some(0),
        (Form::Int(_), Class::Decimal) => Some(1),
        (Form::Int(_), Class::Float) => Some(2),
        (Form::Int(_), Class::Str) => Some(4),
        (Form::Int(_), Class::Simple) => Some(5),
        (Form::Int(_), _) => None,

        (Form::Float, Class::Decimal) => Some(0),
        (Form::Float, Class::Float) => Some(2),
        (Form::Float, Class::Json) => Some(3),
        (Form::Float, Class::Str | Class::StrSub) => Some(4),
        (Form::Float, Class::Simple) => Some(5),
        (Form::Float, _) => None,

        (Form::Bool, Class::Bool) => Some(0),
        (Form::Bool, Class::Json) => Some(1),
        (Form::Bool, Class::Str | Class::StrSub) => Some(2),
        (Form::Bool, Class::Simple) => Some(3),
        (Form::Bool, _) => None,

        (Form::Str(s), Class::Integer) => strict_int_lexical(s).then_some(1),
        (Form::Str(s), Class::SubInt { min, max }) => match number_lexical(s) {
            Some(NumLex::Int(v)) => v.in_range(min, max).then_some(1),
            _ => None,
        },
        (Form::Str(s), Class::Decimal) => decimal_lexical(s).then_some(2),
        (Form::Str(s), Class::Float) => number_lexical(s).map(|_| 3),
        (Form::Str(s), Class::Bool) => {
            matches!(s.as_str(), "0" | "1" | "true" | "false").then_some(4)
        }
        (Form::Str(s), Class::Json) => json_lexical(s).then_some(5),
        (Form::Str(_), Class::Str | Class::StrSub) => Some(6),
        (Form::Str(_), Class::Simple) => Some(4),
        (_, Class::Unhandled) => None,
        // NumericOther only appears inside Typed (handled above);
        // Typed itself is matched before ladder_level is reached.
        (Form::NumericOther, _) | (Form::Typed { .. }, _) => None,
    }
}

predicates! {
    /// Ladder-aware cast + specificity level for document inference.
    ///
    /// cast_level(+Value, +Type, -Level, -Witness) always succeeds, with
    /// three outcomes:
    ///   Level = integer   — cast to a handled ladder type succeeds at this
    ///                       interpretation-specificity level
    ///   Level = fail      — cast is impossible; Witness is unified with
    ///                       the value a casting_error would carry (the
    ///                       intermediate string/atom form for floats,
    ///                       booleans and atoms, the value itself else)
    ///   Level = unhandled — type or value form outside the ladder; the
    ///                       caller falls back to the Prolog cast gate
    #[module("$inference")]
    semidet fn cast_level(_context, value_term, type_term, level_term, wit_term) {
        let type_uri: String = type_term
            .get::<String>()
            .or_else(|_| type_term.get::<Atom>().map(|a| a.name().to_string()))?;
        let class = classify(&type_uri);
        let form = form_of(value_term);
        match (class, form) {
            (_, None) | (Class::Unhandled, _) => level_term.unify(Atom::new("unhandled")),
            // Pre-typed literal: exact type match is level 0; a provably
            // recastable target is level 5; anything else defers to the
            // Prolog gate so failed casts keep their native witness.
            (class, Some(Form::Typed { vtype, inner })) => {
                if local_name(&vtype) == local_name(&type_uri) && classify(&vtype) == class {
                    level_term.unify(0u64)
                } else {
                    match typed_recastable(&inner, class) {
                        Some(true) => level_term.unify(5u64),
                        _ => level_term.unify(Atom::new("unhandled")),
                    }
                }
            }
            (class, Some(form)) => match ladder_level(&form, class) {
                Some(level) => level_term.unify(level),
                None => {
                    unify_cast_witness(wit_term, &form, value_term)?;
                    level_term.unify(Atom::new("fail"))
                }
            },
        }
    }
}

pub fn register() {
    register_cast_level();
}
