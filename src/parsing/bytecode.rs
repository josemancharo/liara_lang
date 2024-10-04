use crate::parsing::ast::Ast;
use bincode;
use bincode::config::Config;

pub fn to_bytecode(ast: Ast) -> Option<Vec<u8>> {
    todo!();
}

pub fn from_bytecode(ast: Vec<u8>) -> Option<(Ast, usize)> {
    todo!();
}