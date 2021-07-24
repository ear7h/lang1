use lang1::*;
use std::io::{self, Read};

fn main() {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();
    let expr = parser::parse(&buf).unwrap();
    let globals = Default::default();
    let mut env = env::Env::new(&globals);
    let res = expr::reduce(&expr, &mut env).unwrap();
    println!("{:?}", res);
}
