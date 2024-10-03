use crate::parsing::builtins::Builtin;
use bincode::{Decode, Encode};

#[derive(Debug, Clone, Decode, Encode)]
pub enum Expression {
    Bool(bool),
    Integer(i64),
    Float(f64),
    Builtin(Builtin),
    Symbol(String),
    String(String),
    Char(char),
    List(Vec<Expression>),
}

pub type Ast = Vec<Expression>;
