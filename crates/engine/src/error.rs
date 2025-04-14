use std::ops::Range;

use ariadne::{Color, Label, Report, Source};

use crate::{
    lexer::{Position, Token},
    state::{ParserState, SymbolValue},
};

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
    Evaluation,
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

    pub fn from_parser_vec(errs: Vec<chumsky::error::Rich<'_, Token>>) -> Vec<Self> {
        errs.into_iter()
            .map(|err| {
                Self::parser(
                    err.span().into_range(),
                    Default::default(),
                    err.reason().to_string(),
                )
            })
            .collect::<Vec<Self>>()
    }

    pub fn update_parser_vec_with_state(mut errs: Vec<Self>, state: &ParserState) -> Vec<Self> {
        errs.iter_mut().for_each(|err| {
            err.update_span(state.get_source_span(&err.span()));
            err.update_position(state.get_source_position(&err.span()));
        });
        errs
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
            kind: TaleErrorKind::Evaluation,
            span,
            position,
            msg,
        }
    }

    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    pub fn update_span(&mut self, span: Range<usize>) {
        self.span = span;
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

    pub fn kind(&self) -> &TaleErrorKind {
        &self.kind
    }

    pub fn msg(&self) -> &String {
        &self.msg
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

pub fn render_tale_result_vec(
    prefix: &str,
    source_name: &str,
    source: String,
    trv: TaleResultVec<SymbolValue>,
) -> TaleResultVec<SymbolValue> {
    match trv {
        Ok(value) => {
            value.render(prefix);
            Ok(SymbolValue::Placeholder)
        }
        Err(tev) => render_tale_error_vec(tev, source_name, source),
    }
}

pub fn render_tale_error_vec(
    tev: Vec<TaleError>,
    source_name: &str,
    source: String,
) -> TaleResultVec<SymbolValue> {
    for error in tev {
        Report::build(ariadne::ReportKind::Error, (source_name, error.span()))
            .with_message(format!("{:?} Error: {}", error.kind(), error.msg()))
            .with_label(
                Label::new((source_name, error.span()))
                    .with_message("Problem occurred here.")
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((source_name, Source::from(&source)))
            .map_err(|err| TaleError::from(err))?;
    }
    Ok(SymbolValue::Placeholder)
}
