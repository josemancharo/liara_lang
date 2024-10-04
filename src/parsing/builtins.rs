use std::rc::Weak;
use crate::parsing::builtins::Builtin::*;
use bincode::{Decode, Encode};

#[derive(Debug, Clone, Decode, Encode)]
pub enum Builtin {
    Val,                   // Declare immutable variable
    Fn,                    // Define a function
    Gen,                   // Define a generator function
    Mul,                   // Multiplication (*)
    Div,                   // Division (/)
    Sub,                   // Subtraction (-)
    Add,                   // Addition (+)
    Mod,                   // Modulus (%)
    Branch,                // Conditional branching
    Lt,                    // Less than (<)
    Gt,                    // Greater than (>)
    Eq,                    // Equal to (==)
    Neq,                   // Not equal to (<>)
    LtEq,                  // Less than or equal to (<=)
    GtEq,                  // Greater than or equal to (>=)
    Or,                    // Logical OR
    And,                   // Logical AND
    Not,                   // Logical NOT
    BitOr,                 // Bitwise OR (&)
    BitAnd,                // Bitwise AND (&)
    BitXor,                // Bitwise XOR (^)
    BitInv,                // Bitwise Inversion (~)
    BitShiftLeft,          // Bitwise shift left (<<)
    BitShiftRight,         // Bitwise shift right (>>)
    CastInt,               // Cast to integer
    CastFloat,             // Cast to float
    StringFrom,            // Create string from value
    SomeOf,                // Create some(value) option
    NoneOf,                // Create none option
    Guard,                 // Branching based on option's presence
    Nop,                   // No operation (_)
    Import,                // Import modules
    PeekStack,             // Peek the top of the stack (@)
    For,                   // For loop
    Break,                 // Break loop
    Continue,              // Continue loop
    Exit,                  // Exit program
    Const,                 // Define a constant
    Struct,                // Define a struct
    PublicField,           // Define a public field in a struct
    WeakRef,               // Create a weak reference
    StrongRef,             // Create a strong reference
    Module,                // Define a module
    Do,                    // Begin a do block
    Deref,                 // Dereference a pointer
    CopyWith,              // Copy with modifications (#)
    OkOf,                  // Create an ok(result)
    ErrorOf,               // Create an err(error)
    Range,                 // Create a range for looping
}

pub fn evaluate_builtin(name: &String) -> Option<Builtin>
{
    match name.as_str() {
        "*" => Some(Mul),
        "+" => Some(Add),
        "-" => Some(Sub),
        "%" => Some(Mod),
        "<" => Some(Lt),
        ">" => Some(Gt),
        "==" => Some(Eq),
        "<>" => Some(Neq),
        "<=" => Some(LtEq),
        ">=" => Some(GtEq),
        "not" => Some(Not),
        "&" => Some(BitAnd),
        "@" => Some(PeekStack),
        "|" => Some(BitOr),
        "and" => Some(And),
        "or" => Some(Or),
        "/" => Some(Div),
        "^" => Some(BitXor),
        "<<" => Some(BitShiftLeft),
        ">>" => Some(BitShiftRight),
        "int" => Some(CastInt),
        "float" => Some(CastFloat),
        "string" => Some(StringFrom),
        "some" => Some(SomeOf),
        "none" => Some(NoneOf),
        "branch" => Some(Branch),
        "guard" => Some(Guard),
        "_" => Some(Nop),
        "~" => Some(BitInv),
        "fn" => Some(Fn),
        "val" => Some(Val),
        "use" => Some(Import),
        "for" => Some(For),
        "break" => Some(Break),
        "continue" => Some(Continue),
        "const" => Some(Const),
        "exit" => Some(Exit),
        "ref" => Some(StrongRef),
        "maybe" => Some(WeakRef),
        "struct" => Some(Struct),
        "field" => Some(PublicField),
        "mod" => Some(Module),
        "do" => Some(Do),
        "deref" => Some(Deref),
        "#" => Some(CopyWith),
        "ok" => Some(OkOf),
        "err" => Some(ErrorOf),
        "range" => Some(Range),
        _ => None
    }
}
