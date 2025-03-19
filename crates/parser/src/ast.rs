use chumsky::Span;

#[derive(Debug, Clone)]
struct MetaData<'src> {
    content: &'src str,
    position: (usize, usize),
    span: SpanInfo<'src>,
}

#[derive(Debug, Clone)]
struct SpanInfo<'src> {
    context: &'src str,
    start: usize,
    end: usize,
}

impl<'src> Span for SpanInfo<'src> {
    type Context = &'src str;

    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            context,
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {
        self.context
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}
