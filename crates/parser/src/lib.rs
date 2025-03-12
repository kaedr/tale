mod expression;
mod statement;
mod types;

pub enum Atom {
    String(String),
    Number(usize),
    Ident(String),
    DieRoll(usize, usize)
}

pub enum AST {

}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
    }
}
