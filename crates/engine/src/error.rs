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
    #[must_use]
    pub fn system(msg: String) -> Self {
        Self {
            kind: TaleErrorKind::System,
            span: 0..0,
            position: (0, 0),
            msg,
        }
    }

    #[must_use]
    pub fn lexer(span: Range<usize>, position: Position, msg: String) -> Self {
        Self {
            kind: TaleErrorKind::Lexical,
            span,
            position,
            msg,
        }
    }

    #[must_use]
    pub fn parser(span: Range<usize>, position: Position, msg: String) -> Self {
        Self {
            kind: TaleErrorKind::Parse,
            span,
            position,
            msg,
        }
    }

    #[must_use]
    pub fn from_parser_vec(errs: Vec<chumsky::error::Rich<'_, Token>>) -> Vec<Self> {
        errs.into_iter()
            .map(|err| {
                // TODO: Better handling of context nesting
                let context_list = err
                    .contexts()
                    .map(|context| context.0.to_string())
                    .collect::<Vec<_>>()
                    .join(" -> ");
                let msg = if context_list.is_empty() {
                    err.reason().to_string()
                } else {
                    format!("{} In: [{context_list}]", err.reason())
                };
                println!("err span: {:?}", &err.span());
                Self::parser(err.span().into_range(), Default::default(), msg)
            })
            .collect::<Vec<Self>>()
    }

    #[must_use]
    pub fn update_parser_vec_with_state(mut errs: Vec<Self>, state: &ParserState) -> Vec<Self> {
        for err in &mut errs {
            let span = err.span();
            err.update_span(state.get_source_span(&span));
            err.update_position(state.get_source_position(&span));
        }
        errs
    }

    #[must_use]
    pub fn analyzer(span: Range<usize>, position: Position, msg: String) -> Self {
        Self {
            kind: TaleErrorKind::Analysis,
            span,
            position,
            msg,
        }
    }

    #[must_use]
    pub fn evaluator(span: Range<usize>, position: Position, msg: String) -> Self {
        Self {
            kind: TaleErrorKind::Evaluation,
            span,
            position,
            msg,
        }
    }

    #[must_use]
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    pub fn update_span(&mut self, span: Range<usize>) {
        self.span = span;
    }

    #[must_use]
    pub fn start(&self) -> usize {
        self.span.start
    }

    #[must_use]
    pub fn end(&self) -> usize {
        self.span.end
    }

    #[must_use]
    pub fn position(&self) -> Position {
        self.position
    }

    pub fn update_position(&mut self, position: Position) {
        self.position = position;
    }

    pub fn append_message(&mut self, add_msg: &str) {
        self.msg.push_str(add_msg);
    }

    #[must_use]
    pub fn kind(&self) -> &TaleErrorKind {
        &self.kind
    }

    #[must_use]
    pub fn msg(&self) -> &String {
        &self.msg
    }
}

impl From<std::io::Error> for TaleError {
    fn from(value: std::io::Error) -> Self {
        Self::system(value.to_string())
    }
}

impl From<std::num::TryFromIntError> for TaleError {
    fn from(value: std::num::TryFromIntError) -> Self {
        Self::evaluator(0..0, Position::default(), value.to_string())
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
    source: &str,
    tale_result_vec: TaleResultVec<SymbolValue>,
) -> TaleResultVec<SymbolValue> {
    match tale_result_vec {
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
    source: &str,
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
            .eprint((source_name, Source::from(source)))
            .map_err(TaleError::from)?;
    }
    Ok(SymbolValue::Placeholder)
}
