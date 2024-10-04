use crate::parsing::parser::parse;
mod parsing;
mod semantic_evaluation;

fn main() {
    let input = "
(use sys print)
(val x 0b01)
(val b 'b')
(val c 0qZZ)
(val y 2)
(+ x y)
    (* @ 2.)
    (+ @ 0x03)
    (- 0 @)
(print @) ; prints -9.0
(print 'b')

".to_string();
    match parse(input) {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => eprintln!("Error: {}", e),
    }
}
