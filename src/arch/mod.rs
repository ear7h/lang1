// reexports
pub mod x86_64;

use crate::{
    ir,
    instruction_writer::InstructionWriter,
};

/// a translator from the high level assembly represention
/// into an architecture specific object representation
///
/// function generation, calling, and returning have default
/// implementations, **HOWEVER** it's still up to the implementer
/// of push_operation to use them (given Call and Ret variants of
/// ir::Operation).
pub trait Arch<C : CallConv> {
    fn push_func(obj : &mut ObjectBuilder, func : &ir::Function) {
        C::push_prologue(obj, func);

        for operation in func.body.iter() {
            Self::push_operation(obj, operation);
        }

        // implicit return
        Self::push_ret(obj, func)
    }

    fn push_call(obj : &mut ObjectBuilder,
                 caller : &ir::Function,
                 callee : &ir::Val,
                 args : &[ir::Val]) {
        C::push_call(obj, caller, callee, args)
    }

    fn push_ret(obj : &mut ObjectBuilder, func : &ir::Function) {
        C::push_return(obj, func);
    }

    fn push_operation(obj: &mut ObjectBuilder, instr : &ir::Operation);
}

/// Represents an architecture's calling convention.
///
/// Note to self: this was originally writting with the idea that the calling
/// convention could be written in a different package, that is without
/// the knowledge of instructions and registers that Arch has.
pub trait CallConv {
    /// push the instructions to call another function:
    /// * move args to the top of the stack
    /// * allocate stack space for return value
    /// * remove args from stack
    /// * stores some register values on the stack
    fn push_call(obj : &mut ObjectBuilder,
                 caller : &ir::Function,
                 calee : &ir::Val,
                 args : &[ir::Val]);

    /// push the instructions to return from a fucntion:
    /// * deallocate local var stack space
    /// * deallocate arg stack space?
    fn push_return(obj: &mut ObjectBuilder,
                   func : &ir::Function);

    /// push the instructions in the beginning of a called function
    /// * allocates stack space for local vars
    /// * stores some registers on the stack
    fn push_prologue(obj: &mut ObjectBuilder,
                     func : &ir::Function);
}

pub type ObjectBuilder = InstructionWriter;

/*
pub trait Scope {
    pub fn addr_of(&self, sym: String) -> ScopedAddress;
}

pub type ScopedAddress = (ScopeBase, u64);

pub enum ScopeBase {
    /// the address of the bottom of the current stack frame
    FramePtr,
    /// the address of the current instruction
    InstructionPtr,
    /// the text section
    Text,
    /// the data section
    Data,
}
*/


/*

   register allocation notes:


    /// Register allocation table, each index of the first dimension
    /// represents a register and the second diminsion is the variables
    /// which can be mapped to the register.
    ///
    /// This table actually has a stronger meaning than just
    /// "register allocation", it actually implies that
    /// no two variables mapped to the same register are
    /// used at the same time. So, this table can
    /// also be used for stack memory allocation.
    ///
    /// Ideally the entries are also sorted by usage (the sum of the
    /// accesses of all variables for a specific register).
    ///
    /// Return values could be
    ///
    /// All fields in this structure are exposed for ease of use,
    /// but this field can be genederated using
    /// `set_default_register_allocation` or
    /// `set_optimized_register_allocation`. If `register_allocation` is
    /// `None`, then the register related methods will return results
    /// equivalent to `set_default_register_allocation`, though with a
    /// performance hit since the allocation table needs to be generated
    /// each call.
    pub register_allocation : Option<Vec<Vec<String>>>,
    */


/// A register allocation is a mapping of variables "registers"
/// such that no two varibles mapped to the same register
/// are used at the same time. "registers" is in quotes because
/// it could also be a memory location. The main idea is that
/// the variables are never used at the same time.
///
/// Notes:
///     * the return values might be better off in the stack the whole time?
trait RegisterAllocation {
    fn register_pressure(&self) -> usize;
    fn register_for(name: &str) -> usize;
}

