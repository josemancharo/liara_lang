use crate::parsing::ast::{Ast, Expression};
use std::iter::Peekable;
use crate::parsing::builtins::evaluate_builtin;
use crate::parsing::tokenizer::tokenize;

pub fn parse(input: String) -> Result<Ast, String> {
    let tokens = tokenize(input)?;
    let mut tokens_iter = tokens.into_iter().peekable();
    let mut ast = vec![];

    while let Some(expr) = parse_expression(&mut tokens_iter)? {
        ast.push(expr);
    }

    Ok(ast)
}

fn parse_expression<I>(tokens: &mut Peekable<I>) -> Result<Option<Expression>, String>
where
    I: Iterator<Item = String>,
{
    match tokens.next() {
        Some(token) => match token.as_str() {
            "(" => {
                let mut list = vec![];
                while let Some(peek) = tokens.peek() {
                    if peek == ")" {
                        tokens.next(); // Consume ")"
                        break;
                    }
                    if let Some(expr) = parse_expression(tokens)? {
                        list.push(expr);
                    }
                }
                Ok(Some(Expression::List(list)))
            }
            ")" => Err("Unexpected ')'".to_string()),
            _ => {
                if token == "true" {
                    Ok(Some(Expression::Bool(true)))
                } else if token == "false" {
                    Ok(Some(Expression::Bool(false)))
                }
                else if token.starts_with("\"") && token.ends_with("\"") {
                    let content = &token[1..token.len() - 1];
                    Ok(Some(Expression::String(content.to_string())))
                }
                else if token.starts_with('\'') && token.ends_with('\'') && token.len() >= 3 {
                    let char_content = &token[1..token.len() - 1];
                    let mut chars_iter = char_content.chars();
                    if let Some(c) = chars_iter.next() {
                        if chars_iter.next().is_some() {
                            return Err(format!("Invalid char literal: '{}'", char_content));
                        }
                        Ok(Some(Expression::Char(c)))
                    } else {
                        Err(format!("Empty char literal: '{}'", char_content))
                    }
                }
                else if let Some(int_val) = parse_integer(&token) {
                    Ok(Some(Expression::Integer(int_val)))
                }
                else if let Ok(float_val) = token.parse::<f64>() {
                    Ok(Some(Expression::Float(float_val)))
                }
                else if let Some(builtin) = evaluate_builtin(&token){
                    Ok(Some(Expression::Builtin(builtin)))
                }
                else {
                    Ok(Some(Expression::Symbol(token)))
                }
            }
        },
        None => Ok(None),
    }
}

fn parse_integer(token: &String) -> Option<i64> {
    if token.starts_with("0x") || token.starts_with("0X") {
        i64::from_str_radix(&token[2..], 16).ok()
    }
    else if token.starts_with("0b") || token.starts_with("0B") {
        i64::from_str_radix(&token[2..], 2).ok()
    }
    else if token.starts_with("0o") || token.starts_with("0O") {
        i64::from_str_radix(&token[2..], 8).ok()
    }
    else if token.starts_with("0q") || token.starts_with("0Q") {
        i64::from_str_radix(&token[2..], 36).ok()
    }
    else {
        token.parse::<i64>().ok()
    }
}