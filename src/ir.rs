#![allow(dead_code)]

pub struct Function {
    pub name :   String,
    pub public : bool,

    /// function params
    pub params : Vec<(String, u64)>, // Vec<(name, size)>
    /// local vars
    pub locals : Vec<(String, u64)>, // Vec<(name, size)>

    /// return vals
    pub rets : Vec<(String, u64)>, // Vec<(name, size)>

    /// the operations making up the body of the function
    pub body : Vec<Operation>,
}

impl Function {
    pub fn var_size(&self, name : &str) -> u64 {
        for vars in vec![&self.params, &self.locals, &self.rets] {
            for (var, size) in vars.iter() {
                if var == name {
                    return *size
                }
            }
        }

        panic!("variable does not exist: {}", name);
    }
}

pub enum Operation {
    Call(Val, Vec<Val>),
    Ret,

    /// move data from one place to another (dst, src, sign, size)
    Mov(Val, Val, OpSize),

    /// Arithmetic
    Add(Val, Val, Val, Sign, OpSize),
    Sub(Val, Val, Val, Sign, OpSize),
    Mul(Val, Val, Val, Sign, OpSize),
    Div(Val, Val, Val, Sign, OpSize),

    /// bits
    And(Val, Val, Val, OpSize),
    Orr(Val, Val, Val, OpSize),
    Xor(Val, Val, Val, OpSize),
    Shr(Val, Val, Val, Sign, OpSize),
    Shl(Val, Val, Val, OpSize),
}

/// Describes the data for some variable
pub enum Val {
    /// Gets encoded along with the instruction in the text section
    Imm(u64),
    /// Stored on the stack or registers
    Local(String),
    ///
    Global(String),
}

// in "memory units"
pub enum OpSize {
    Units1,
    Units2,
    Units4,
    Units8,
}

pub enum Sign {
    Signed,
    Unsigned,
}
