use std::ops::Range;

use crate::lexer::Position;

pub type TaleResult<T> = Result<T, TaleError>;

pub type TaleResultVec<T> = Result<T, Vec<TaleError>>;

#[derive(Debug, Clone)]
pub struct TaleError {
    kind: TaleErrorKind,
    span: Range<usize>,
    position: Position,
    msg: String,
}

#[derive(Debug, Clone)]
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
            position: (0, 0),
            msg,
        }
    }

    pub fn lexer(span: Range<usize>, position: Position, msg: String) -> Self {
        Self {
            kind: TaleErrorKind::Lexical,
            span,
            position,
            msg,
        }
    }

    pub fn parser(span: Range<usize>, position: Position, msg: String) -> Self {
        Self {
            kind: TaleErrorKind::Parse,
            span,
            position,
            msg,
        }
    }

    pub fn analyzer(span: Range<usize>, position: Position, msg: String) -> Self {
        Self {
            kind: TaleErrorKind::Analysis,
            span,
            position,
            msg,
        }
    }

    pub fn evaluator(span: Range<usize>, position: Position, msg: String) -> Self {
        Self {
            kind: TaleErrorKind::Eval,
            span,
            position,
            msg,
        }
    }

    pub fn update_span(&mut self, span: Range<usize>) {
        self.span = span;
    }

    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    pub fn start(&self) -> usize {
        self.span.start
    }

    pub fn end(&self) -> usize {
        self.span.end
    }

    pub fn position(&self) -> Position {
        self.position
    }

    pub fn update_position(&mut self, position: Position) {
        self.position = position;
    }

    pub fn append_message(&mut self, add_msg: &str) {
        self.msg.push_str(add_msg);
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
