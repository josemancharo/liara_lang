// Define the various types in Liara
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Option(Box<Type>),
    Result(Box<Type>, Box<Type>),
    Struct(String), // Reference to a struct by name
    Void,           // Represents no return value
    // Add more types as needed
}
// Represents a function parameter with a name and type
#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
}

// Represents a field within a struct, including optional default values
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
    pub default: Option<Expression>, // Default value, if any
}
// Represents different kinds of expressions in Liara
#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    Builtin(BuiltinKind, Vec<Expression>), // Builtin operation with operands
    FunctionCall(String, Vec<Expression>),
    GeneratorCall(String, Vec<Expression>),
    Reference(String, ReferenceType),
    Dereference(String),
    CopyWith(String, Vec<(String, Expression)>),
    Guard(GuardExpression),
    // Add more expression types as needed
}

// Represents literal values
#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    None,
    Some(Box<Expression>),
    Ok(Box<Expression>),
    Err(Box<Expression>),
    // Add more literals as needed
}
// Enumerates the types of references
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReferenceType {
    Weak,
    Strong,
}
// Represents a single pattern match branch
#[derive(Debug, Clone)]
pub enum Pattern {
    Some(String),    // Matches `some(value)` and binds `value` to the given variable name
    None,            // Matches `none`
    Ok(String),      // Matches `ok(value)` and binds `value`
    Err(String),     // Matches `err(error)` and binds `error`
    Wildcard,        // Matches anything (`_`)
}

// Represents a guard expression with multiple pattern branches
#[derive(Debug, Clone)]
pub struct GuardExpression {
    pub expression: Box<Expression>,        // The expression to match against
    pub branches: Vec<GuardBranch>,         // The pattern branches
}

// Represents a single branch in a guard expression
#[derive(Debug, Clone)]
pub struct GuardBranch {
    pub pattern: Pattern,
    pub body: Vec<Expression>,               // The body to execute if the pattern matches
}
// Represents the entire program as a list of semantic AST nodes
#[derive(Debug, Clone)]
pub struct SemanticAst {
    pub nodes: Vec<SemanticAstNode>,
}

// Enumerates all possible semantic AST nodes
#[derive(Debug, Clone)]
pub enum SemanticAstNode {
    Module(Module),
    Function(Function),
    Generator(Generator),
    Struct(Struct),
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
    ConstantDeclaration(ConstantDeclaration),
    // Add more nodes as needed
}

// Represents a module with a name and contents
#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub contents: Vec<SemanticAstNode>,
}

// Represents a function definition
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Vec<SemanticAstNode>,
    pub return_type: Option<Type>, // None implies void or inferred type
}

// Represents a generator function definition
#[derive(Debug, Clone)]
pub struct Generator {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Vec<SemanticAstNode>,
}

// Represents a struct definition
#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

// Represents a variable declaration
#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub value: Expression,
}

// Represents a constant declaration
#[derive(Debug, Clone)]
pub struct ConstantDeclaration {
    pub name: String,
    pub value: Expression,
}
// Enumerates all built-in operations and constructs
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Gt,
    Eq,
    Neq,
    LtEq,
    GtEq,
    And,
    Or,
    Not,
    BitAnd,
    BitOr,
    BitXor,
    BitInv,
    BitShiftLeft,
    BitShiftRight,
    CastInt,
    CastFloat,
    StringFrom,
    Some,
    None,
    Ok,
    Err,
    Branch,
    Guard,
    Import,
    PeekStack,
    For,
    Break,
    Continue,
    Exit,
    Const,
    Do,
    Deref,
    CopyWith,
    Yield,
    Next,
    // Add more builtins as needed
}
