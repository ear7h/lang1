use std::{
    str::FromStr,
    fs::OpenOptions,
};

use lang1::{
    arch::x86_64::*,
    instruction_writer::*,
};

use faerie;

use target_lexicon::triple;

fn main() {
    let obj = faerie::ArtifactBuilder::new(triple!("x86_64-unknown-unknown-unknown-macho"))
        .name("simple_exit.o".to_owned())
        .finish();

    let mut obj = ObjectWriter::new(obj);

    obj.func("main".to_owned(), |f| {
        Instr{
            mnemonic : Mn::XOR,
            operands : Opr::RM(Reg::RAX, (Reg::RAX, AddrMod::Direct)),
        }.encode(f);

        Instr{
            mnemonic : Mn::ADD,
            operands : Opr::I(Imm::Bits8(42)),
        }.encode(f);

        Instr{
            mnemonic : Mn::RETN,
            operands : Opr::ZO,
        }.encode(f);
    });

    let file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open("simple_exit.o")
        .unwrap();

    obj.write(file).unwrap();
}
