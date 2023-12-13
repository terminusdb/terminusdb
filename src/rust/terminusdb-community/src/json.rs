use std::{cell::Cell, io::Read};

use swipl::{prelude::*, term::de};

enum TranscodeOutput {
    Element,
    Eof,
}

fn translate_transcode_error<C: QueryableContextType>(
    context: &Context<C>,
    result: Result<(), de::Error>,
    non_whitespace_detected: bool,
) -> PrologResult<TranscodeOutput> {
    match result {
        Ok(_) => Ok(TranscodeOutput::Element),
        // annoying to have to match a string like that but we
        // unfortunately do not retain the original error due to how
        // transcoding works
        Err(de::Error::Message(m)) if !non_whitespace_detected && m.starts_with("EOF") => {
            Ok(TranscodeOutput::Eof)
        }
        Err(de::Error::Message(m)) => {
            context.raise_exception(&term! {context: syntax_error(invalid_json(#m))}?)
        }
        Err(de::Error::UnificationFailed) => Err(PrologError::Failure),
        Err(e) => context.try_or_die_generic(Err(e)),
    }
}

struct NonWhitespaceDetector<'a, R> {
    canary: &'a Cell<bool>,
    inner: R,
}

impl<'a, R: Read> Read for NonWhitespaceDetector<'a, R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let result = self.inner.read(buf);

        if !self.canary.get() {
            if let Ok(size) = result.as_ref() {
                // let's see if any of the read data is non-whitespace
                if buf[..*size].iter().any(|c| !c.is_ascii_whitespace()) {
                    self.canary.set(true);
                }
            }
        }

        result
    }
}

predicates! {
    #[module("$util")]
    semidet fn json_read_dict_fast(context, stream_term, doc_term) {
        let stream: ReadablePrologStream = stream_term.get_ex()?;
        let whitespace_canary = Cell::new(false);
        let reader = NonWhitespaceDetector {
            inner: stream.decoding_reader(),
            canary: &whitespace_canary,
        };
        let mut json_deserializer = serde_json::Deserializer::from_reader(reader);
        let config = SerializerConfiguration::new().default_tag(atom!("json")).unit_atom(atom!("null"));
        let serializer = swipl::term::ser::Serializer::new_with_config(context, doc_term.clone(), config);
        if matches!(translate_transcode_error(context, serde_transcode::transcode(&mut json_deserializer, serializer), whitespace_canary.get())?, TranscodeOutput::Eof) {
            doc_term.unify(atom!("eof"))?;
        }

        Ok(())
    }
}

pub fn register() {
    register_json_read_dict_fast();
}
