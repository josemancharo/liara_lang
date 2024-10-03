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
                            chars.next();
                            if let Some(escaped_char) = chars.next() {
                                match escaped_char {
                                    'n' => string_literal.push('\n'),
                                    't' => string_literal.push('\t'),
                                    '"' => string_literal.push('"'),
                                    '\\' => string_literal.push('\\'),
                                    '\'' => string_literal.push('\''),
                                    'r' => string_literal.push('\r'),
                                    '0' => string_literal.push('\0'),
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
                        escaped = true;
                        chars.next();
                        if let Some(escaped_char) = chars.next() {
                            match escaped_char {
                                'n' => char_literal.push('\n'),
                                't' => char_literal.push('\t'),
                                '\\' => char_literal.push('\\'),
                                '\'' => char_literal.push('\''),
                                'r' => char_literal.push('\r'),
                                '0' => char_literal.push('\0'),
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

                tokens.push(format!("'{}'", char_literal));
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


