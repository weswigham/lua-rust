#[macro_use]
extern crate nom;

mod parser;
mod printer;

fn main() {
    match parser::parse("function foo() end") {
        Ok(r) => print!("{}", printer::pretty_print(r)),
        Err(err) => print!("{}", err), 
    }

}
