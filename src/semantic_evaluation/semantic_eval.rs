use std::collections::HashMap;
use crate::parsing::ast::Ast;
use crate::semantic_evaluation::semantic_ast::*;
// Assume the following enums and structs are defined as per the Semantic AST definitions above

pub fn evaluate_semantics(ast: &Ast) -> Result<SemanticAst, String> {
    let mut semantic_ast = SemanticAst { nodes: Vec::new() };
    let mut environment = Environment::new();

    for expr in &ast {
        let node = evaluate_expression(expr, &mut environment)?;
        semantic_ast.nodes.push(node);
    }

    Ok(semantic_ast)
}

// Represents the environment during semantic analysis, tracking defined entities
struct Environment {
    modules: HashMap<String, Module>,
    structs: HashMap<String, Struct>,
    functions: HashMap<String, Function>,
    generators: HashMap<String, Generator>,
    variables: HashMap<String, Type>,
    constants: HashMap<String, Type>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            modules: HashMap::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
            generators: HashMap::new(),
            variables: HashMap::new(),
            constants: HashMap::new(),
        }
    }

    // Add more environment management functions as needed
}

// Evaluates an individual expression and returns a SemanticAstNode
fn evaluate_expression(expr: &Expression, env: &mut Environment) -> Result<SemanticAstNode, String> {
    match expr {
        Expression::List(list) => {
            if list.is_empty() {
                return Err("Empty list expression".to_string());
            }

            // The first element determines the type of expression
            let first = &list[0];
            match first {
                Expression::Symbol(symbol) => {
                    match symbol.as_str() {
                        "mod" => evaluate_module(&list[1..], env),
                        "fn" => evaluate_function(&list[1..], env),
                        "gen" => evaluate_generator(&list[1..], env),
                        "struct" => evaluate_struct(&list[1..], env),
                        "val" => evaluate_variable_declaration(&list[1..], env),
                        "const" => evaluate_constant_declaration(&list[1..], env),
                        _ => evaluate_builtin_expression(expr, env),
                    }
                }
                _ => evaluate_builtin_expression(expr, env),
            }
        }
        _ => Err(format!("Unexpected top-level expression: {:#?}", expr)),
    }
}

// Evaluates module definitions
fn evaluate_module(list: &[Expression], env: &mut Environment) -> Result<SemanticAstNode, String> {
    if list.is_empty() {
        return Err("Module definition missing name".to_string());
    }

    // Module name
    let name_expr = &list[0];
    let module_name = match name_expr {
        Expression::Symbol(name) => name.clone(),
        _ => return Err(format!("Module name must be a symbol. Found: {:#?}", name_expr)),
    };

    // Module contents
    let mut module = Module {
        name: module_name.clone(),
        contents: Vec::new(),
    };

    for expr in &list[1..] {
        let node = evaluate_expression(expr, env)?;
        module.contents.push(node);
    }

    // Add module to environment
    if env.modules.contains_key(&module_name) {
        return Err(format!("Module '{}' is already defined", module_name));
    }
    env.modules.insert(module_name.clone(), module.clone());

    Ok(SemanticAstNode::Module(module))
}

// Evaluates function definitions
fn evaluate_function(list: &[Expression], env: &mut Environment) -> Result<SemanticAstNode, String> {
    if list.len() < 3 {
        return Err("Function definition must include name, parameters, and body".to_string());
    }

    // Function name
    let name_expr = &list[0];
    let func_name = match name_expr {
        Expression::Symbol(name) => name.clone(),
        _ => return Err(format!("Function name must be a symbol. Found: {:#?}", name_expr)),
    };

    // Parameters
    let params_expr = &list[1];
    let parameters = match params_expr {
        Expression::List(param_list) => {
            let mut params = Vec::new();
            for param in param_list {
                match param {
                    Expression::List(param_def) => {
                        if param_def.len() != 2 {
                            return Err(format!(
                                "Parameter definition must have exactly two elements (name and type). Found: {:#?}",
                                param_def
                            ));
                        }
                        let param_name = match &param_def[0] {
                            Expression::Symbol(name) => name.clone(),
                            _ => {
                                return Err(format!(
                                    "Parameter name must be a symbol. Found: {:#?}",
                                    param_def[0]
                                ))
                            }
                        };
                        let param_type = match &param_def[1] {
                            Expression::Symbol(type_name) => parse_type(type_name)?,
                            Expression::List(type_list) => parse_complex_type(type_list)?,
                            _ => {
                                return Err(format!(
                                    "Parameter type must be a symbol or a list. Found: {:#?}",
                                    param_def[1]
                                ))
                            }
                        };
                        params.push(Parameter {
                            name: param_name,
                            ty: param_type,
                        });
                    }
                    _ => {
                        return Err(format!(
                            "Parameter definition must be a list. Found: {:#?}",
                            param
                        ))
                    }
                }
            }
            params
        }
        _ => return Err(format!("Function parameters must be a list. Found: {:#?}", params_expr)),
    };

    // Body
    let body_expr = &list[2];
    let body = match body_expr {
        Expression::List(body_list) => {
            let mut body_nodes = Vec::new();
            for expr in body_list {
                let node = evaluate_expression(expr, env)?;
                body_nodes.push(node);
            }
            body_nodes
        }
        _ => return Err(format!("Function body must be a list. Found: {:#?}", body_expr)),
    };

    // Optional return type
    let return_type = if list.len() > 3 {
        let return_expr = &list[3];
        match return_expr {
            Expression::List(ret_list) => {
                if ret_list.len() != 2 || ret_list[0] != Expression::Symbol("returns".to_string()) {
                    return Err(format!(
                        "Return type specification must be in the form (returns Type). Found: {:#?}",
                        return_expr
                    ));
                }
                match &ret_list[1] {
                    Expression::Symbol(type_name) => Some(parse_type(type_name)?),
                    Expression::List(type_list) => Some(parse_complex_type(type_list)?),
                    _ => {
                        return Err(format!(
                            "Return type must be a symbol or a list. Found: {:#?}",
                            ret_list[1]
                        ))
                    }
                }
            }
            _ => return Err(format!(
                "Return type specification must be a list. Found: {:#?}",
                return_expr
            )),
        }
    } else {
        None
    };

    // Create Function struct
    let function = Function {
        name: func_name.clone(),
        parameters: parameters.clone(),
        body: body.clone(),
        return_type: return_type.clone(),
    };

    // Add function to environment
    if env.functions.contains_key(&func_name) {
        return Err(format!("Function '{}' is already defined", func_name));
    }
    env.functions.insert(func_name.clone(), function.clone());

    Ok(SemanticAstNode::Function(function))
}

// Evaluates generator function definitions
fn evaluate_generator(list: &[Expression], env: &mut Environment) -> Result<SemanticAstNode, String> {
    if list.len() < 3 {
        return Err("Generator definition must include name, parameters, and body".to_string());
    }

    // Generator name
    let name_expr = &list[0];
    let gen_name = match name_expr {
        Expression::Symbol(name) => name.clone(),
        _ => return Err(format!("Generator name must be a symbol. Found: {:#?}", name_expr)),
    };

    // Parameters
    let params_expr = &list[1];
    let parameters = match params_expr {
        Expression::List(param_list) => {
            let mut params = Vec::new();
            for param in param_list {
                match param {
                    Expression::List(param_def) => {
                        if param_def.len() != 2 {
                            return Err(format!(
                                "Parameter definition must have exactly two elements (name and type). Found: {:#?}",
                                param_def
                            ));
                        }
                        let param_name = match &param_def[0] {
                            Expression::Symbol(name) => name.clone(),
                            _ => {
                                return Err(format!(
                                    "Parameter name must be a symbol. Found: {:#?}",
                                    param_def[0]
                                ))
                            }
                        };
                        let param_type = match &param_def[1] {
                            Expression::Symbol(type_name) => parse_type(type_name)?,
                            Expression::List(type_list) => parse_complex_type(type_list)?,
                            _ => {
                                return Err(format!(
                                    "Parameter type must be a symbol or a list. Found: {:#?}",
                                    param_def[1]
                                ))
                            }
                        };
                        params.push(Parameter {
                            name: param_name,
                            ty: param_type,
                        });
                    }
                    _ => {
                        return Err(format!(
                            "Parameter definition must be a list. Found: {:#?}",
                            param
                        ))
                    }
                }
            }
            params
        }
        _ => return Err(format!("Generator parameters must be a list. Found: {:#?}", params_expr)),
    };

    // Body
    let body_expr = &list[2];
    let body = match body_expr {
        Expression::List(body_list) => {
            let mut body_nodes = Vec::new();
            for expr in body_list {
                let node = evaluate_expression(expr, env)?;
                body_nodes.push(node);
            }
            body_nodes
        }
        _ => return Err(format!("Generator body must be a list. Found: {:#?}", body_expr)),
    };

    // Create Generator struct
    let generator = Generator {
        name: gen_name.clone(),
        parameters: parameters.clone(),
        body: body.clone(),
    };

    // Add generator to environment
    if env.generators.contains_key(&gen_name) {
        return Err(format!("Generator '{}' is already defined", gen_name));
    }
    env.generators.insert(gen_name.clone(), generator.clone());

    Ok(SemanticAstNode::Generator(generator))
}

// Evaluates struct definitions
fn evaluate_struct(list: &[Expression], env: &mut Environment) -> Result<SemanticAstNode, String> {
    if list.is_empty() {
        return Err("Struct definition missing name".to_string());
    }

    // Struct name
    let name_expr = &list[0];
    let struct_name = match name_expr {
        Expression::Symbol(name) => name.clone(),
        _ => return Err(format!("Struct name must be a symbol. Found: {:#?}", name_expr)),
    };

    // Struct fields
    let mut fields = Vec::new();
    for field_expr in &list[1..] {
        match field_expr {
            Expression::List(field_def) => {
                if field_def.is_empty() {
                    return Err("Empty field definition".to_string());
                }
                if field_def.len() < 2 {
                    return Err(format!(
                        "Field definition must have at least two elements (name and type). Found: {:#?}",
                        field_def
                    ));
                }

                // Field name
                let field_name = match &field_def[0] {
                    Expression::Symbol(name) => name.clone(),
                    _ => {
                        return Err(format!(
                            "Field name must be a symbol. Found: {:#?}",
                            field_def[0]
                        ))
                    }
                };

                // Field type
                let field_type = match &field_def[1] {
                    Expression::Symbol(type_name) => parse_type(type_name)?,
                    Expression::List(type_list) => parse_complex_type(type_list)?,
                    _ => {
                        return Err(format!(
                            "Field type must be a symbol or a list. Found: {:#?}",
                            field_def[1]
                        ))
                    }
                };

                // Optional default value
                let default = if field_def.len() > 2 {
                    let default_expr = &field_def[2];
                    Some(default_expr.clone())
                } else {
                    None
                };

                fields.push(StructField {
                    name: field_name,
                    ty: field_type,
                    default,
                });
            }
            _ => {
                return Err(format!(
                    "Field definition must be a list. Found: {:#?}",
                    field_expr
                ))
            }
        }
    }

    // Create Struct struct
    let struct_def = Struct {
        name: struct_name.clone(),
        fields: fields.clone(),
    };

    // Add struct to environment
    if env.structs.contains_key(&struct_name) {
        return Err(format!("Struct '{}' is already defined", struct_name));
    }
    env.structs.insert(struct_name.clone(), struct_def.clone());

    Ok(SemanticAstNode::Struct(struct_def))
}

// Evaluates variable declarations
fn evaluate_variable_declaration(list: &[Expression], env: &mut Environment) -> Result<SemanticAstNode, String> {
    if list.len() != 2 {
        return Err("Variable declaration must have exactly two elements (name and value)".to_string());
    }

    // Variable name
    let name_expr = &list[0];
    let var_name = match name_expr {
        Expression::Symbol(name) => name.clone(),
        _ => return Err(format!("Variable name must be a symbol. Found: {:#?}", name_expr)),
    };

    // Variable value
    let value_expr = &list[1];
    let value = value_expr.clone(); // We'll process expressions later as needed

    // Infer type from value expression or require explicit type annotations
    // For simplicity, we'll infer types here
    let var_type = infer_type(&value, env)?;

    // Check if variable is already defined
    if env.variables.contains_key(&var_name) || env.constants.contains_key(&var_name) {
        return Err(format!("Variable '{}' is already defined", var_name));
    }

    // Add variable to environment
    env.variables.insert(var_name.clone(), var_type.clone());

    Ok(SemanticAstNode::VariableDeclaration(VariableDeclaration {
        name: var_name,
        value,
    }))
}

// Evaluates constant declarations
fn evaluate_constant_declaration(list: &[Expression], env: &mut Environment) -> Result<SemanticAstNode, String> {
    if list.len() != 2 {
        return Err("Constant declaration must have exactly two elements (name and value)".to_string());
    }

    // Constant name
    let name_expr = &list[0];
    let const_name = match name_expr {
        Expression::Symbol(name) => name.clone(),
        _ => return Err(format!("Constant name must be a symbol. Found: {:#?}", name_expr)),
    };

    // Constant value
    let value_expr = &list[1];
    let value = value_expr.clone(); // We'll process expressions later as needed

    // Infer type from value expression or require explicit type annotations
    // For simplicity, we'll infer types here
    let const_type = infer_type(&value, env)?;

    // Check if constant is already defined
    if env.variables.contains_key(&const_name) || env.constants.contains_key(&const_name) {
        return Err(format!("Constant '{}' is already defined", const_name));
    }

    // Add constant to environment
    env.constants.insert(const_name.clone(), const_type.clone());

    Ok(SemanticAstNode::ConstantDeclaration(ConstantDeclaration {
        name: const_name,
        value,
    }))
}

// Evaluates built-in expressions
fn evaluate_builtin_expression(expr: &Expression, env: &mut Environment) -> Result<SemanticAstNode, String> {
    // For this example, we'll handle builtins that are function-like
    match expr {
        Expression::List(list) => {
            if list.is_empty() {
                return Err("Empty list expression".to_string());
            }

            let first = &list[0];
            let builtin_name = match first {
                Expression::Symbol(name) => name.clone(),
                _ => return Err(format!("Builtin name must be a symbol. Found: {:#?}", first)),
            };

            // Map the builtin name to BuiltinKind
            let builtin_kind = match BuiltinKind::from_str(&builtin_name) {
                Some(kind) => kind,
                None => return Err(format!("Unknown builtin: '{}'", builtin_name)),
            };

            // Collect arguments
            let args = list[1..].to_vec();

            // Perform semantic checks based on builtin kind
            // For simplicity, we'll assume all builtins take specific argument counts/types
            // This can be expanded as needed
            match builtin_kind {
                BuiltinKind::Add | BuiltinKind::Sub | BuiltinKind::Mul | BuiltinKind::Div | BuiltinKind::Mod => {
                    if args.len() != 2 {
                        return Err(format!("'{}' expects exactly two arguments", builtin_name));
                    }
                    // Further type checks can be added here
                }
                BuiltinKind::Branch => {
                    if args.len() != 3 {
                        return Err("'branch' expects exactly three arguments (condition, true_case, false_case)".to_string());
                    }
                    // Check that condition is a boolean expression
                    let cond_type = infer_type(&args[0], env)?;
                    if cond_type != Type::Bool {
                        return Err("Condition in 'branch' must be of type bool".to_string());
                    }
                }
                BuiltinKind::Guard => {
                    if args.len() < 2 {
                        return Err("'guard' expects at least two arguments (expression and branches)".to_string());
                    }
                    // First argument should be an expression
                    // Remaining should be branches
                    // This requires parsing branches, which can be complex
                }
                BuiltinKind::For => {
                    if args.len() != 2 {
                        return Err("'for' expects exactly two arguments (iterable, body)".to_string());
                    }
                    // Check that first argument is an iterable (range or generator)
                    let iter_type = infer_type(&args[0], env)?;
                    match iter_type {
                        Type::Struct(name) => {
                            if name != "Generator" {
                                return Err("'for' expects an iterable (range or generator)".to_string());
                            }
                        }
                        Type::Option(_) | Type::Result(_, _) => {
                            return Err("'for' expects an iterable (range or generator)".to_string());
                        }
                        _ => {}
                    }
                    // Body should be a list of expressions
                }
                BuiltinKind::Yield => {
                    // 'yield' should only appear within generator functions
                    // This requires context awareness, which isn't handled here
                }
                BuiltinKind::Next => {
                    if args.len() != 1 {
                        return Err("'next' expects exactly one argument (generator)".to_string());
                    }
                    // Check that argument is a generator
                    let gen_type = infer_type(&args[0], env)?;
                    if !matches!(gen_type, Type::Struct(name) if name == "Generator") {
                        return Err("'next' expects a generator as its argument".to_string());
                    }
                }
                // Handle other builtins similarly
                _ => {
                    // For unhandled builtins, no specific checks
                }
            }

            // Return as an expression node
            Ok(SemanticAstNode::Expression(Expression::Builtin(
                builtin_kind,
                args,
            )))
        }
        _ => Err(format!("Unhandled builtin expression: {:#?}", expr)),
    }
}

// Parses a type from a symbol name
fn parse_type(type_name: &str) -> Result<Type, String> {
    match type_name {
        "int" => Ok(Type::Int),
        "float" => Ok(Type::Float),
        "string" => Ok(Type::String),
        "bool" => Ok(Type::Bool),
        "void" => Ok(Type::Void),
        _ => {
            // Assume it's a struct type
            Ok(Type::Struct(type_name.to_string()))
        }
    }
}

// Parses complex types, such as option and result types
fn parse_complex_type(type_list: &[Expression]) -> Result<Type, String> {
    if type_list.is_empty() {
        return Err("Empty type list".to_string());
    }

    let type_head = &type_list[0];
    match type_head {
        Expression::Symbol(type_name) => {
            match type_name.as_str() {
                "option" => {
                    if type_list.len() != 2 {
                        return Err("Option type must have exactly one type parameter".to_string());
                    }
                    let inner_type = match &type_list[1] {
                        Expression::Symbol(name) => parse_type(name)?,
                        Expression::List(inner_list) => parse_complex_type(inner_list)?,
                        _ => return Err(format!("Invalid type parameter in option: {:#?}", type_list[1])),
                    };
                    Ok(Type::Option(Box::new(inner_type)))
                }
                "result" => {
                    if type_list.len() != 3 {
                        return Err("Result type must have exactly two type parameters".to_string());
                    }
                    let ok_type = match &type_list[1] {
                        Expression::Symbol(name) => parse_type(name)?,
                        Expression::List(inner_list) => parse_complex_type(inner_list)?,
                        _ => return Err(format!("Invalid ok type in result: {:#?}", type_list[1])),
                    };
                    let err_type = match &type_list[2] {
                        Expression::Symbol(name) => parse_type(name)?,
                        Expression::List(inner_list) => parse_complex_type(inner_list)?,
                        _ => return Err(format!("Invalid err type in result: {:#?}", type_list[2])),
                    };
                    Ok(Type::Result(Box::new(ok_type), Box::new(err_type)))
                }
                _ => Err(format!("Unknown complex type: '{}'", type_name)),
            }
        }
        _ => Err(format!("Invalid type specification: {:#?}", type_head)),
    }
}

// Infers the type of an expression
fn infer_type(expr: &Expression, env: &Environment) -> Result<Type, String> {
    match expr {
        Expression::Literal(lit) => match lit {
            Literal::Int(_) => Ok(Type::Int),
            Literal::Float(_) => Ok(Type::Float),
            Literal::String(_) => Ok(Type::String),
            Literal::Bool(_) => Ok(Type::Bool),
            Literal::None => Ok(Type::Option(Box::new(Type::Void))),
            Literal::Some(inner) => {
                let inner_type = infer_type(inner, env)?;
                Ok(Type::Option(Box::new(inner_type)))
            }
            Literal::Ok(inner) => {
                let inner_type = infer_type(inner, env)?;
                // Assuming result type is Result<InnerType, Void>
                Ok(Type::Result(Box::new(inner_type), Box::new(Type::Void)))
            }
            Literal::Err(inner) => {
                let inner_type = infer_type(inner, env)?;
                // Assuming result type is Result<Void, InnerType>
                Ok(Type::Result(Box::new(Type::Void), Box::new(inner_type)))
            }
        },
        Expression::Variable(name) => {
            if let Some(ty) = env.variables.get(name) {
                Ok(ty.clone())
            } else if let Some(ty) = env.constants.get(name) {
                Ok(ty.clone())
            } else {
                Err(format!("Undefined variable or constant '{}'", name))
            }
        }
        Expression::Builtin(kind, args) => {
            // Infer type based on builtin kind
            match kind {
                BuiltinKind::Add | BuiltinKind::Sub | BuiltinKind::Mul | BuiltinKind::Div | BuiltinKind::Mod => {
                    // Assuming arithmetic operations return Int or Float based on operands
                    // For simplicity, return Int
                    Ok(Type::Int)
                }
                BuiltinKind::Some => {
                    if args.len() != 1 {
                        return Err("'some' expects exactly one argument".to_string());
                    }
                    let inner_type = infer_type(&args[0], env)?;
                    Ok(Type::Option(Box::new(inner_type)))
                }
                BuiltinKind::None => Ok(Type::Option(Box::new(Type::Void))),
                BuiltinKind::OkOf => {
                    if args.len() != 1 {
                        return Err("'ok' expects exactly one argument".to_string());
                    }
                    let inner_type = infer_type(&args[0], env)?;
                    Ok(Type::Result(Box::new(inner_type), Box::new(Type::Void)))
                }
                BuiltinKind::ErrorOf => {
                    if args.len() != 1 {
                        return Err("'err' expects exactly one argument".to_string());
                    }
                    let inner_type = infer_type(&args[0], env)?;
                    Ok(Type::Result(Box::new(Type::Void), Box::new(inner_type)))
                }
                BuiltinKind::CastInt => Ok(Type::Int),
                BuiltinKind::CastFloat => Ok(Type::Float),
                BuiltinKind::StringFrom => Ok(Type::String),
                BuiltinKind::Branch => {
                    // Branch returns void or some type based on context
                    Ok(Type::Void)
                }
                BuiltinKind::Guard => Ok(Type::Void),
                BuiltinKind::For => Ok(Type::Void),
                BuiltinKind::Print => Ok(Type::Void),
                BuiltinKind::Yield => Ok(Type::Void),
                BuiltinKind::Next => {
                    // Assuming 'next' returns Option<YieldType>
                    Ok(Type::Option(Box::new(Type::Void))) // Replace Type::Void with actual yield type
                }
                _ => Ok(Type::Void), // Defaulting to Void for unhandled builtins
            }
        }
        Expression::FunctionCall(name, args) => {
            if let Some(func) = env.functions.get(name) {
                if func.parameters.len() != args.len() {
                    return Err(format!(
                        "Function '{}' expects {} arguments but got {}",
                        name,
                        func.parameters.len(),
                        args.len()
                    ));
                }
                // Check argument types
                for (arg_expr, param) in args.iter().zip(&func.parameters) {
                    let arg_type = infer_type(arg_expr, env)?;
                    if &arg_type != &param.ty {
                        return Err(format!(
                            "Type mismatch in function '{}': expected {:?}, got {:?}",
                            name, param.ty, arg_type
                        ));
                    }
                }
                // Return type of the function
                match &func.return_type {
                    Some(ty) => Ok(ty.clone()),
                    None => Ok(Type::Void),
                }
            } else {
                Err(format!("Undefined function '{}'", name))
            }
        }
        Expression::GeneratorCall(name, args) => {
            if let Some(gen) = env.generators.get(name) {
                if gen.parameters.len() != args.len() {
                    return Err(format!(
                        "Generator '{}' expects {} arguments but got {}",
                        name,
                        gen.parameters.len(),
                        args.len()
                    ));
                }
                // Check argument types
                for (arg_expr, param) in args.iter().zip(&gen.parameters) {
                    let arg_type = infer_type(arg_expr, env)?;
                    if &arg_type != &param.ty {
                        return Err(format!(
                            "Type mismatch in generator '{}': expected {:?}, got {:?}",
                            name, param.ty, arg_type
                        ));
                    }
                }
                // Generators return a Generator type
                Ok(Type::Struct("Generator".to_string()))
            } else {
                Err(format!("Undefined generator '{}'", name))
            }
        }
        Expression::Reference(name, ref_type) => {
            if let Some(var_type) = env.variables.get(name) {
                match ref_type {
                    ReferenceType::Weak => Ok(Type::Struct("WeakRef".to_string())),
                    ReferenceType::Strong => Ok(Type::Struct("StrongRef".to_string())),
                }
            } else {
                Err(format!("Undefined variable '{}'", name))
            }
        }
        Expression::Dereference(name) => {
            // Assuming dereferencing a reference returns the original type
            // This requires tracking the type of references in the environment
            // For simplicity, we'll return Void
            Ok(Type::Void)
        }
        Expression::CopyWith(name, modifications) => {
            // Ensure the variable exists
            if !env.variables.contains_key(name) && !env.structs.contains_key(name) {
                return Err(format!("Undefined variable or struct '{}'", name));
            }
            // Ensure that modifications correspond to struct fields
            // This requires fetching the struct definition
            // For simplicity, we'll assume it's valid
            Ok(Type::Void)
        }
        Expression::Guard(_) => {
            // Guard expressions return void
            Ok(Type::Void)
        }
    }
}

// Parses a semantic AST node into a SemanticAstNode
// (Implementation details are handled within evaluate_expression)

// Parses a guard expression
fn evaluate_guard_expression(expr: &Expression, env: &mut Environment) -> Result<SemanticAstNode, String> {
    match expr {
        Expression::List(list) => {
            if list.is_empty() {
                return Err("Empty guard expression".to_string());
            }

            // 'guard' expression
            let guard_expr = &list[0];
            let branches_expr = &list[1..];

            // Parse the expression to guard
            if branches_expr.is_empty() {
                return Err("Guard expression requires at least one branch".to_string());
            }

            let expression = match &list[0] {
                Expression::List(inner_list) => {
                    // The first element should be the expression to guard
                    if inner_list.is_empty() {
                        return Err("Guard expression missing target expression".to_string());
                    }
                    // Extract the target expression
                    // For simplicity, assume it's a variable
                    if inner_list.len() != 1 {
                        return Err("Guard target expression must have exactly one element".to_string());
                    }
                    let target = &inner_list[0];
                    Box::new(target.clone())
                }
                _ => return Err("Guard target must be a list expression".to_string()),
            };

            // Parse branches
            let mut branches = Vec::new();
            for branch_expr in &list[1..] {
                match branch_expr {
                    Expression::List(branch_list) => {
                        if branch_list.is_empty() {
                            return Err("Empty guard branch".to_string());
                        }
                        let pattern = match &branch_list[0] {
                            Expression::List(pattern_list) => {
                                if pattern_list.is_empty() {
                                    return Err("Empty pattern in guard branch".to_string());
                                }
                                match &pattern_list[0] {
                                    Expression::Symbol(sym) => {
                                        match sym.as_str() {
                                            "some" => {
                                                if pattern_list.len() != 2 {
                                                    return Err("Pattern 'some' requires one binding".to_string());
                                                }
                                                match &pattern_list[1] {
                                                    Expression::Symbol(var) => Pattern::Some(var.clone()),
                                                    _ => return Err("Binding in 'some' pattern must be a symbol".to_string()),
                                                }
                                            }
                                            "none" => {
                                                if pattern_list.len() != 1 {
                                                    return Err("Pattern 'none' takes no bindings".to_string());
                                                }
                                                Pattern::None
                                            }
                                            "ok" => {
                                                if pattern_list.len() != 2 {
                                                    return Err("Pattern 'ok' requires one binding".to_string());
                                                }
                                                match &pattern_list[1] {
                                                    Expression::Symbol(var) => Pattern::Ok(var.clone()),
                                                    _ => return Err("Binding in 'ok' pattern must be a symbol".to_string()),
                                                }
                                            }
                                            "err" => {
                                                if pattern_list.len() != 2 {
                                                    return Err("Pattern 'err' requires one binding".to_string());
                                                }
                                                match &pattern_list[1] {
                                                    Expression::Symbol(var) => Pattern::Err(var.clone()),
                                                    _ => return Err("Binding in 'err' pattern must be a symbol".to_string()),
                                                }
                                            }
                                            "_" => Pattern::Wildcard,
                                            _ => {
                                                return Err(format!("Unknown pattern symbol '{}'", sym));
                                            }
                                        }
                                    }
                                    _ => return Err("Pattern must start with a symbol".to_string()),
                                }
                            }
                            _ => return Err("Pattern must be a list expression".to_string()),
                        };

                        // Body expressions
                        let body = match &branch_list[1..] {
                            [] => Vec::new(),
                            exprs => exprs.to_vec(),
                        };

                        branches.push(GuardBranch { pattern, body });
                    }
                    _ => {
                        return Err(format!(
                            "Guard branch must be a list. Found: {:#?}",
                            branch_expr
                        ))
                    }
                }
            }

            Ok(SemanticAstNode::Expression(Expression::Guard(GuardExpression {
                expression,
                branches,
            })))
        }
        _ => Err(format!("Invalid guard expression: {:#?}", expr)),
    }
}

// Evaluates builtins that require special handling (e.g., guard)
fn evaluate_builtin_expression_extended(expr: &Expression, env: &mut Environment) -> Result<SemanticAstNode, String> {
    match expr {
        Expression::List(list) => {
            if list.is_empty() {
                return Err("Empty list expression".to_string());
            }

            let first = &list[0];
            match first {
                Expression::Symbol(symbol) => match symbol.as_str() {
                    "guard" => evaluate_guard_expression(expr, env),
                    _ => evaluate_builtin_expression(expr, env),
                },
                _ => evaluate_builtin_expression(expr, env),
            }
        }
        _ => evaluate_builtin_expression(expr, env),
    }
}
