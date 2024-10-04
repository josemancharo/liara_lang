pub fn tokenize(input: String) -> Result<Vec<String>, String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    let mut current_token = String::new();

    while let Some(&ch) = chars.peek() {
        match ch {
            ';' => {
                chars.next();
                while let Some(&c) = chars.peek() {
                    if c == '\n' {
                        break;
                    }
                    chars.next();
                }
            }
            '(' | ')' => {
                chars.next();
                tokens.push(ch.to_string());
            }
            '"' => {
                chars.next();
                let mut string_literal = String::new();
                while let Some(&c) = chars.peek() {
                    match c {
                        '\\' => {
                            string_literal.push('\\');
                            chars.next();
                            if let Some(escaped_char) = chars.next() {
                                match escaped_char {
                                    'n' | 't' | '"' | '\\' | '\'' | 'r' | '0' => string_literal.push(escaped_char),
                                    other => {
                                        return Err(format!("Unknown escape sequence: \\{}", other));
                                    }
                                }
                            } else {
                                return Err("Unterminated escape sequence in string".to_string());
                            }
                        }
                        '"' => {
                            chars.next();
                            break;
                        }
                        _ => {
                            string_literal.push(c);
                            chars.next();
                        }
                    }
                }
                tokens.push(format!("\"{}\"", string_literal));
            }
            '\'' => {
                chars.next();
                let mut char_literal = String::new();
                let mut escaped = false;

                if let Some(&c) = chars.peek() {
                    if c == '\\' {
                        chars.next();
                        if let Some(escaped_char) = chars.next() {
                            match escaped_char {
                                'n' | 't' | '"' | '\\' | '\'' | 'r' | '0' => char_literal.push(escaped_char),
                                other => {
                                    return Err(format!("Unknown escape sequence: \\{}", other));
                                }
                            }
                        } else {
                            return Err("Unterminated escape sequence in char".to_string());
                        }
                    } else {
                        char_literal.push(c);
                        chars.next();
                    }
                } else {
                    return Err("Unterminated char literal".to_string());
                }

                if let Some(&c) = chars.peek() {
                    if c == '\'' {
                        chars.next();
                    } else {
                        return Err("Expected closing single quote for char literal".to_string());
                    }
                } else {
                    return Err("Unterminated char literal".to_string());
                }

                if char_literal.chars().count() != 1 {
                    return Err(format!("Invalid char literal: '{}'", char_literal));
                }

                if escaped {
                    tokens.push(format!("'\\{}'", char_literal));
                }
                else {
                    tokens.push(format!("'{}'", char_literal));
                }
            }
            c if c.is_whitespace() => {
                chars.next();
            }
            _ => {
                current_token.push(ch);
                chars.next();
                while let Some(&c) = chars.peek() {
                    if c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == '\'' {
                        break;
                    }
                    current_token.push(c);
                    chars.next();
                }
                tokens.push(current_token.clone());
                current_token.clear();
            }
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use crate::parsing::tokenizer::tokenize;

    #[test]
    fn test_tokenize() {
        let tokens = tokenize("(* 3 5)".to_string());
        assert!(tokens.is_ok());
        assert_eq!(tokens.unwrap(), vec!["(", "*", "3", "5", ")"]);
    }

    #[test]
    fn test_char() {
        let tokens = tokenize("(print 'b')".to_string());
        assert!(tokens.is_ok());
        assert_eq!(tokens.unwrap(), vec!["(", "print", "'b'", ")"]);
        let tokens = tokenize("(print '\\'')".to_string());
        assert!(tokens.is_ok());
        assert_eq!(tokens.unwrap(), vec!["(", "print", "'\''", ")"]);
    }

    #[test]
    fn test_str() {
        let tokens = tokenize("(print \"\\\"Hello\\n World\\\"\")".to_string());
        assert!(tokens.is_ok());
        assert_eq!(tokens.unwrap(), vec!["(", "print", "\"\\\"Hello\\n World\\\"\"", ")"]);
    }
}