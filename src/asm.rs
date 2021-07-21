#![allow(dead_code)]

#[derive(Default)]
pub(crate) struct Function {
    name :   String,
    export : bool,

    // local space that is being allocated,
    // note that this is only measured as space and individual
    // items are not listed, the calling convention and alignment
    // rules are handled in the transformation from the ast ir
    // to this ir
    arg_size :   Addr,
    local_size : Addr,
    ret_size :   Addr,

    // body of the function
    body : Vec<Instr>,
}

/// a multiple of the address resolution unit
type Addr = u64;

pub enum Instr {
    /// realive jump in units of *instructions*
    RelJump(i64),

    ///
    Jump(String),

    /// return from function
    Ret,

    /// conditionally execute the next .3 instructions
    Cond(Cond, Val, u64),

    /// move data from one place to another (dst, src, sign, size)
    Mov(Val, Val, Sign, MemUnit),

    /// Arithmetic
    Add(Val, Val, Val, Sign, MemUnit),
    Sub(Val, Val, Val, Sign, MemUnit),
    Mul(Val, Val, Val, Sign, MemUnit),
    Div(Val, Val, Val, Sign, MemUnit),

    /// bits
    And(Val, Val, Val, MemUnit),
    Orr(Val, Val, Val, MemUnit),
    Xor(Val, Val, Val, MemUnit),
    Shr(Val, Val, Val, Sign, MemUnit),
    Shl(Val, Val, Val, MemUnit),
}

/// various ways of addressing values
pub enum Val {
    /// immediate value
    Imm(u64),

    /// address of some section + offset
    Addr(Section, i64),

    /// value at some section + offset
    Deref(Section, i64),
}

pub enum Section {
    // function local pseudo sections
    Arg,
    Local,
    Ret,

    // conventional global sections
    Global(String),    // RODATA, BSS
    MutGlobal(String), // DATA
    Function(String),  /* TEXT */

                       /* TODO(ear7h):
                        * Context, like odin?
                        * ThreadLocal ?
                        * Label(String), */
}

pub enum Sign {
    Signed,
    Unsigned,
}

/// size of the instruction operands in
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum MemUnit {
    Exp1,
    Exp2,
    Exp3,
    Exp4,
}

#[derive(PartialEq, Eq)]
pub enum Cond {
    /// equal
    EQ,
    /// not equal
    NEQ,
    /// less-than or equal
    LE,
    /// less-than
    LT,
    /// greater-than or equal
    GE,
    /// greater-than
    GT,
    /// zero
    ZE,
    /// not zero
    NZ,
    /// negative
    NE,
    /// postive
    PO,
    /// overflow
    OF,
}
