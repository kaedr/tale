use std::rc::Rc;

use crate::Atom;

pub enum Expression {
    Atom(Atom),
    Neg(Rc<Expression>),
    Add(Rc<Expression>, Rc<Expression>),
    Sub(Rc<Expression>, Rc<Expression>),
    Mul(Rc<Expression>, Rc<Expression>),
    Div(Rc<Expression>, Rc<Expression>),
    Mod(Rc<Expression>, Rc<Expression>),
    Pow(Rc<Expression>, Rc<Expression>),
}
