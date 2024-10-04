use crate::parsing::ast::{Ast, Expression};
use std::iter::Peekable;
use crate::parsing::builtins::evaluate_builtin;
use crate::parsing::tokenizer::tokenize;
use snailquote::unescape;

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
    I: Iterator<Item=String>,
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
                } else if token.starts_with("\"") && token.ends_with("\"") {
                    let content_unescaped = unescape(&token).expect(format!("Invalid string literal: '{}'", &token).as_str());
                    Ok(Some(Expression::String(content_unescaped)))
                } else if token.starts_with('\'') && token.ends_with('\'') && token.len() >= 3 {
                    let content_unescaped = unescape(&token).expect(format!("Invalid char literal: '{}'", &token).as_str());
                    let mut chars_iter = content_unescaped.chars();
                    if let Some(c) = chars_iter.next() {
                        if chars_iter.next().is_some() {
                            return Err(format!("Invalid char literal: '{}'", &token));
                        }
                        Ok(Some(Expression::Char(c)))
                    } else {
                        Err(format!("Empty char literal: '{}'", &token))
                    }
                } else if let Some(int_val) = parse_integer(&token) {
                    Ok(Some(Expression::Integer(int_val)))
                } else if let Ok(float_val) = token.parse::<f64>() {
                    Ok(Some(Expression::Float(float_val)))
                } else if let Some(builtin) = evaluate_builtin(&token) {
                    Ok(Some(Expression::Builtin(builtin)))
                } else {
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
    } else if token.starts_with("0b") || token.starts_with("0B") {
        i64::from_str_radix(&token[2..], 2).ok()
    } else if token.starts_with("0o") || token.starts_with("0O") {
        i64::from_str_radix(&token[2..], 8).ok()
    } else if token.starts_with("0q") || token.starts_with("0Q") {
        i64::from_str_radix(&token[2..], 36).ok()
    } else {
        token.parse::<i64>().ok()
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::ast::Expression;
    use crate::parsing::builtins::Builtin;
    use crate::parsing::parser::{parse, parse_expression, parse_integer};

    #[test]
    fn test_parse_integer() {
        assert_eq!(parse_integer(&"0".to_string()), Some(0i64));
        assert_eq!(parse_integer(&"0xFF".to_string()), Some(0xFFi64));
        assert_eq!(parse_integer(&"0b11".to_string()), Some(3i64));
        assert_eq!(parse_integer(&"0o77".to_string()), Some(63i64));
        assert_eq!(parse_integer(&"0qZ".to_string()), Some(35i64));
    }

    #[test]
    fn test_parse_string() {
        let raw = "\"Your \\nmom\"";
        let parsed = parse(raw.to_string()).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed.first(), Some(&Expression::String("Your \nmom".to_string())));
    }

    #[test]
    fn test_parse_char() {
        let raw = "'\t'";
        let parsed = parse(raw.to_string()).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed.first(), Some(&Expression::Char('\t')));
    }

    #[test]
    fn test_parse_s_expression() {
        let raw = "(* 1 0.4)";
        let parsed = parse(raw.to_string()).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed.first(), Some(&Expression::List(
            vec![
                Expression::Builtin(Builtin::Mul),
                Expression::Integer(1i64),
                Expression::Float(0.4f64)
            ]
        )));
    }

    #[test]
    fn test_parse_s_expressions() {
        let raw = "(mod my_module (note \"Is super cool\"))\t  (* (+ 4 (- 8 5)) 0.4)\n(+ 1 @)\n\n";
        let parsed = parse(raw.to_string()).unwrap();
        assert_eq!(parsed.len(), 3);
    }
}