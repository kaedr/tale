use std::ops::Range;

pub type TaleResult<T> = Result<T, TaleError>;

pub type TaleResultVec<T> = Result<T, Vec<TaleError>>;

#[derive(Debug)]
pub struct TaleError {
    pub kind: TaleErrorKind,
    pub span: Range<usize>,
    pub msg: String,
}

#[derive(Debug)]
pub enum TaleErrorKind {
    Lexical,
    Parse,
    Analysis,
    Eval,
    System,
}

impl TaleError {
    pub fn system(msg: String) -> Self {
        Self {
            kind: TaleErrorKind::System,
            span: 0..0,
            msg,
        }
    }

    pub fn lexer(span: Range<usize>, msg: String) -> Self {
        Self {
            kind: TaleErrorKind::Lexical,
            span,
            msg,
        }
    }

    pub fn parser(span: Range<usize>, msg: String) -> Self {
        Self {
            kind: TaleErrorKind::Parse,
            span,
            msg,
        }
    }

    pub fn analyzer(span: Range<usize>, msg: String) -> Self {
        Self {
            kind: TaleErrorKind::Analysis,
            span,
            msg,
        }
    }

    pub fn evaluator(span: Range<usize>, msg: String) -> Self {
        Self {
            kind: TaleErrorKind::Eval,
            span,
            msg,
        }
    }
}

impl From<std::io::Error> for TaleError {
    fn from(value: std::io::Error) -> Self {
        Self::system(value.to_string())
    }
}

impl From<TaleError> for Vec<TaleError> {
    fn from(value: TaleError) -> Self {
        vec![value]
    }
}
