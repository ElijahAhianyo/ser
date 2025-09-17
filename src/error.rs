use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum Error {
    #[error("[Line {line}] Error {loc}: {message}")]
    Report {
        line: u32,
        loc: String,
        message: String,
    },
}

impl Error {
    pub(crate) fn report(line: u32, loc: String, message: String) -> Self {
        Self::Report { line, loc, message }
    }
}
