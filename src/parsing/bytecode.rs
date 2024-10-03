use crate::parsing::ast::Ast;
use bincode;
use bincode::config::Config;

pub fn to_bytecode(ast: Ast) -> Option<Vec<u8>> {
    bincode::encode_to_vec(ast, bincode::config::standard()).ok()
}

pub fn from_bytecode(ast: Vec<u8>) -> Option<(Ast, usize)> {
    bincode::decode_from_slice::<Ast, dyn Config>(ast.as_slice(), bincode::config::standard()).ok()
}