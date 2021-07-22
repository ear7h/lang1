#![cfg(test)]

use std::fs::File;
use std::io::Write;
use std::panic;

use crate::arch::x86_64::*;
use crate::instruction_writer::{
    mock_function_writer,
    FunctionWriter,
    LittleEndian,
    Reloc,
};

#[test]
fn test_mnemonic() {
    struct Tcase {
        name :  &'static str,
        instr : Instr,
        bin :   &'static [u8],
    }

    let tcases = vec![
        Tcase {
            name :  "add    eax,0xbeef",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::I(Imm::Bits32(0xdeadbeef)),
            },
            bin :   &[0x05, 0xef, 0xbe, 0xad, 0xde],
        },
        Tcase {
            name :  "add    ax,0xbbbb",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::I(Imm::Bits16(0xbeef)),
            },
            bin :   &[0x66, 0x05, 0xef, 0xbe],
        },
        Tcase {
            name :  "add    al,0xbb",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::I(Imm::Bits8(0xbb)),
            },
            bin :   &[0x04, 0xbb],
        },
        Tcase {
            name :  "add    al,0xbb", // dual
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MI(
                    (Reg::AL, AddrMod::Direct),
                    Imm::Bits8(0xbb),
                ),
            },
            bin :   &[0x80, 0xc0, 0xbb],
        },
        Tcase {
            name :  "add    ax,0xbb", // dual
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MI(
                    (Reg::AX, AddrMod::Direct),
                    Imm::Bits16(0xbb),
                ),
            },
            bin :   &[0x66, 0x81, 0xc0, 0xbb, 0x00],
        },
        Tcase {
            name :  "add    eax,0xbb", //imm32 dual
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MI(
                    (Reg::EAX, AddrMod::Direct),
                    Imm::Bits32(0xbb),
                ),
            },
            bin :   &[0x81, 0xc0, 0xbb, 0x00, 0x00, 0x00],
        },
        /*
        Tcase{
            name: "add    BYTE PTR [ax], 0xbb",
            instr: Instr{
                mnemonic: Mn::ADD,
                operands: Opr::MI(
                    (Reg::AX, AddrMod::Direct),
                    Imm::Bits8(0xbb)),
            },
            bin: &[0x83, 0xc0, 0xbb],
        },
        */
        Tcase {
            name :  "add    eax,0xffffffbb",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MI(
                    // the size of the reg doesn't really matter here
                    (Reg::EAX, AddrMod::Direct),
                    Imm::Bits8(0xbb),
                ),
            },
            bin :   &[0x83, 0xc0, 0xbb],
        },
        Tcase {
            name :  "add    DWORD PTR [rax],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::RAX, AddrMod::Indirect(None)),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x08],
        },
        Tcase {
            name :  "add    ecx,DWORD PTR [rax]",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::RM(
                    Reg::ECX,
                    (Reg::RAX, AddrMod::Indirect(None)),
                ),
            },
            bin :   &[0x03, 0x08],
        },
        Tcase {
            name :  "add    QWORD PTR [rax],rcx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::RAX, AddrMod::Indirect(None)),
                    Reg::RCX,
                ),
            },
            bin :   &[0x48, 0x01, 0x08],
        },
        Tcase {
            name :  "add    QWORD PTR [eax],rcx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::EAX, AddrMod::Indirect(None)),
                    Reg::RCX,
                ),
            },
            bin :   &[0x67, 0x48, 0x01, 0x08],
        },
        Tcase {
            name :  "add    WORD PTR [eax],cx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::EAX, AddrMod::Indirect(None)),
                    Reg::CX,
                ),
            },
            bin :   &[0x67, 0x66, 0x01, 0x08],
        },
        // various addressing modes

        // SIB only
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*1],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::Indirect(Some((SibScale::Scale1, Reg::RDX))),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x0c, 0x10],
        },
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*2],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::Indirect(Some((SibScale::Scale2, Reg::RDX))),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x0c, 0x50],
        },
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*4],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::Indirect(Some((SibScale::Scale4, Reg::RDX))),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x0c, 0x90],
        },
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*8],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::Indirect(Some((SibScale::Scale8, Reg::RDX))),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x0c, 0xD0],
        },
        // disp only
        Tcase {
            name :  "add    DWORD PTR [rax+0x1],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::RAX, AddrMod::IndirectDisp8(1, None)),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x48, 0x01],
        },
        Tcase {
            name :  "add    DWORD PTR [rax+0xbeef],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::RAX, AddrMod::IndirectDisp32(0xbeef, None)),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x88, 0xef, 0xbe, 0x00, 0x00],
        },
        // disp8 and  SIB
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*1+0x1],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::IndirectDisp8(
                            1,
                            Some((SibScale::Scale1, Reg::RDX)),
                        ),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x4C, 0x10, 0x01],
        },
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*2+0x1],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::IndirectDisp8(
                            1,
                            Some((SibScale::Scale2, Reg::RDX)),
                        ),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x4C, 0x50, 0x01],
        },
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*2+0x1],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::IndirectDisp8(
                            1,
                            Some((SibScale::Scale2, Reg::RDX)),
                        ),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x4C, 0x50, 0x01],
        },
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*4+0x1],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::IndirectDisp8(
                            1,
                            Some((SibScale::Scale4, Reg::RDX)),
                        ),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x4C, 0x90, 0x01],
        },
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*8+0x1],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::IndirectDisp8(
                            1,
                            Some((SibScale::Scale8, Reg::RDX)),
                        ),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x4C, 0xd0, 0x01],
        },
        // disp32 and SIB
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*1+0xbeef],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::IndirectDisp32(
                            0xbeef,
                            Some((SibScale::Scale1, Reg::RDX)),
                        ),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x8C, 0x10, 0xef, 0xbe, 0x00, 0x00],
        },
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*2+0xbeef],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::IndirectDisp32(
                            0xbeef,
                            Some((SibScale::Scale2, Reg::RDX)),
                        ),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x8C, 0x50, 0xef, 0xbe, 0x00, 0x00],
        },
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*4+0xbeef],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::IndirectDisp32(
                            0xbeef,
                            Some((SibScale::Scale4, Reg::RDX)),
                        ),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x8C, 0x90, 0xef, 0xbe, 0x00, 0x00],
        },
        Tcase {
            name :  "add    DWORD PTR [rax+rdx*8+0xbeef],ecx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::IndirectDisp32(
                            0xbeef,
                            Some((SibScale::Scale8, Reg::RDX)),
                        ),
                    ),
                    Reg::ECX,
                ),
            },
            bin :   &[0x01, 0x8C, 0xd0, 0xef, 0xbe, 0x00, 0x00],
        },
        // qword ptr disp32 and SIB
        Tcase {
            name :  "add    QWORD PTR [rax+rdx*8+0xbeef],rcx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RAX,
                        AddrMod::IndirectDisp32(
                            0xbeef,
                            Some((SibScale::Scale8, Reg::RDX)),
                        ),
                    ),
                    Reg::RCX,
                ),
            },
            bin :   &[0x48, 0x01, 0x8C, 0xd0, 0xef, 0xbe, 0x00, 0x00],
        },
        // addr32 disp8 and SIB
        Tcase {
            name :  "add    QWORD PTR [eax+edx*8+0x1],rcx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::EAX,
                        AddrMod::IndirectDisp8(
                            0x01,
                            Some((SibScale::Scale8, Reg::EDX)),
                        ),
                    ),
                    Reg::RCX,
                ),
            },
            bin :   &[0x67, 0x48, 0x01, 0x4C, 0xd0, 0x01],
        },
        // addr32 disp32 and SIB
        Tcase {
            name :  "add    QWORD PTR [eax+edx*8+0xbeef],rcx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::EAX,
                        AddrMod::IndirectDisp32(
                            0xbeef,
                            Some((SibScale::Scale8, Reg::EDX)),
                        ),
                    ),
                    Reg::RCX,
                ),
            },
            bin :   &[0x67, 0x48, 0x01, 0x8C, 0xd0, 0xef, 0xbe, 0x00, 0x00],
        },
        // rm addr32 disp32 and SIB
        Tcase {
            name :  "add    rcx,QWORD PTR [eax+edx*8+0xbeef]",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::RM(
                    Reg::RCX,
                    (
                        Reg::EAX,
                        AddrMod::IndirectDisp32(
                            0xbeef,
                            Some((SibScale::Scale8, Reg::EDX)),
                        ),
                    ),
                ),
            },
            bin :   &[0x67, 0x48, 0x03, 0x8C, 0xd0, 0xef, 0xbe, 0x00, 0x00],
        },
        // addr32 disp32 and SIB, reg32
        Tcase {
            name :  "add    WORD PTR [eax+edx*8+0xbeef],cx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::EAX,
                        AddrMod::IndirectDisp32(
                            0xbeef,
                            Some((SibScale::Scale8, Reg::EDX)),
                        ),
                    ),
                    Reg::CX,
                ),
            },
            bin :   &[0x67, 0x66, 0x01, 0x8C, 0xd0, 0xef, 0xbe, 0x00, 0x00],
        },
        // addr32 disp32 and SIB, reg8
        Tcase {
            name :  "add    BYTE PTR [eax+edx*8+0xbeef],cl",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::EAX,
                        AddrMod::IndirectDisp32(
                            0xbeef,
                            Some((SibScale::Scale8, Reg::EDX)),
                        ),
                    ),
                    Reg::CL,
                ),
            },
            bin :   &[0x67, 0x00, 0x8c, 0xd0, 0xef, 0xbe, 0x00, 0x00],
        },
        // mem edge cases

        // dword ptr [rip]
        Tcase {
            name :  "add    DWORD PTR [rip+0x0],edx        # 0xa2",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::RIP, AddrMod::Indirect(None)),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x15, 0x00, 0x00, 0x00, 0x00],
        },
        // dword ptr [rip + disp8] -> disp32
        Tcase {
            name :  "add    DWORD PTR [rip+0x1],edx        # 0xa9",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::RIP, AddrMod::IndirectDisp8(1, None)),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x15, 0x01, 0x00, 0x00, 0x00],
        },
        // dword ptr [rip + disp32]
        Tcase {
            name :  "add    DWORD PTR [rip+0xbeef],edx        # 0xbf9d",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::RIP, AddrMod::IndirectDisp32(0xbeef, None)),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x15, 0xef, 0xbe, 0x00, 0x00],
        },
        // [rbp] -> [rbp + disp8]
        Tcase {
            name :  "add    DWORD PTR [rbp+0x0],edx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::RBP, AddrMod::Indirect(None)),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x55, 0x00],
        },
        // [rbp + disp8] normal
        Tcase {
            name :  "add    DWORD PTR [rbp+0x1],edx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::RBP, AddrMod::IndirectDisp8(1, None)),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x55, 0x01],
        },
        // [rbp + disp32] normal
        Tcase {
            name :  "add    DWORD PTR [rbp+0xbeef],edx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::RBP, AddrMod::IndirectDisp32(0xbeef, None)),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x95, 0xef, 0xbe, 0x00, 0x00],
        },
        // [rbp + rax*1] -> [rbp + rax*1 + 0]
        Tcase {
            name :  "add    DWORD PTR [rbp+rax*1+0x0],edx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RBP,
                        AddrMod::Indirect(Some((SibScale::Scale1, Reg::RAX))),
                    ),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x54, 0x05, 0x00],
        },
        // [rbp + rax*2] -> [rbp + rax*2 + 0]
        Tcase {
            name :  "add    DWORD PTR [rbp+rax*2+0x0],edx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RBP,
                        AddrMod::Indirect(Some((SibScale::Scale2, Reg::RAX))),
                    ),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x54, 0x45, 0x00],
        },
        // [rbp + rax*4] -> [rbp + rax*4 + 0]
        Tcase {
            name :  "add    DWORD PTR [rbp+rax*4+0x0],edx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RBP,
                        AddrMod::Indirect(Some((SibScale::Scale4, Reg::RAX))),
                    ),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x54, 0x85, 0x00],
        },
        // [rbp + rax*8] -> [rbp + rax*8 + 0]
        Tcase {
            name :  "add    DWORD PTR [rbp+rax*8+0x0],edx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RBP,
                        AddrMod::Indirect(Some((SibScale::Scale8, Reg::RAX))),
                    ),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x54, 0xC5, 0x00],
        },
        // [rbp + rax*8 + disp8] normal
        Tcase {
            name :  "add    DWORD PTR [rbp+rax*8+0x1],edx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RBP,
                        AddrMod::IndirectDisp8(
                            1,
                            Some((SibScale::Scale8, Reg::RAX)),
                        ),
                    ),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x54, 0xC5, 0x01],
        },
        // [rbp + rax*8 + disp32] normal
        Tcase {
            name :  "add    DWORD PTR [rbp+rax*8+0xbeef],edx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RBP,
                        AddrMod::IndirectDisp32(
                            0xbeef,
                            Some((SibScale::Scale8, Reg::RAX)),
                        ),
                    ),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x94, 0xC5, 0xef, 0xbe, 0x00, 0x00],
        },
        // [rbp + rax*8 + disp32] normal
        Tcase {
            name :  "add    DWORD PTR [rbp+rax*8+0xbeef],edx",
            instr : Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RBP,
                        AddrMod::IndirectDisp32(
                            0xbeef,
                            Some((SibScale::Scale8, Reg::RAX)),
                        ),
                    ),
                    Reg::EDX,
                ),
            },
            bin :   &[0x01, 0x94, 0xC5, 0xef, 0xbe, 0x00, 0x00],
        },
    ];

    /*
    TODO: write all test results to single file for integration tests
    set_test_marker(&mut s, &mut w);
    */

    for tc in tcases {
        let mut w = mock_function_writer();

        let res = panic::catch_unwind(panic::AssertUnwindSafe(|| {
            tc.instr.clone().encode(&mut w);
            // tc.instr.clone().encode(&mut ww);
        }));

        let (instrs, _) = w.parts();

        match res {
            Err(e) => {
                println!("FAIL {}", tc.name);
                panic!("{:?}", e);
            },
            Ok(()) => {
                assert_eq!(&instrs, tc.bin, "\nfor test: {}\n", tc.name);
            },
        }
    }

    /*
    TODO: write all test results to single file for integration tests
    set_test_marker(&mut s, &mut w);
    write_test_artifacts("add", s, w);
    */
}

fn set_test_marker(
    asm : &mut String,
    bin : &mut FunctionWriter<LittleEndian>,
) {
    asm.push_str("nop\n");
    asm.push_str("nop\n");
    asm.push_str("nop\n");
    asm.push_str("nop\n");

    bin.push8(0x90);
    bin.push8(0x90);
    bin.push8(0x90);
    bin.push8(0x90);
}

fn write_test_artifacts(name : &str, asm : String, bin : &[u8]) {
    File::create(format!("test_{}.asm", name))
        .unwrap()
        .write_all(&asm.as_bytes())
        .unwrap();
    File::create(format!("test_{}.bin", name))
        .unwrap()
        .write_all(&bin)
        .unwrap();
    println!("files written");
}

#[test]
fn mnemonic_rip_rel() {
    struct Tcase {
        name :   &'static str,
        instr :  Instr,
        bin :    &'static [u8],
        relsym : Option<Reloc>,
    }

    let tcases = vec![
        // RIP rel
        Tcase {
            name :   "add    DWORD PTR [rip+0x1],ecx        # 0xb",
            instr :  Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (Reg::RIP, AddrMod::RIPRel(Some(1), None)),
                    Reg::ECX,
                ),
            },
            bin :    &[0x01, 0x0d, 0x01, 0x00, 0x00, 0x00],
            relsym : None,
        },
        Tcase {
            name :   "add    DWORD PTR [rip+0x0],ecx        # 0x10",
            instr :  Instr {
                mnemonic : Mn::ADD,
                operands : Opr::MR(
                    (
                        Reg::RIP,
                        AddrMod::RIPRel(None, Some("label".to_string())),
                    ),
                    Reg::ECX,
                ),
            },
            bin :    &[0x01, 0x0d, 0x00, 0x00, 0x00, 0x00],
            relsym : Some(Reloc {
                at : 2,
                to : "label".to_string(),
            }),
        },
    ];

    // set_test_marker(&mut s, &mut w);

    for tc in tcases {
        let mut w = mock_function_writer();

        let res =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                tc.instr.clone().encode(&mut w);
            }));

        let (instrs, relocs) = w.parts();
        match res {
            Err(e) => {
                println!("FAIL {}", tc.name);
                panic!("{:?}", e);
            },
            Ok(()) => {
                assert_eq!(instrs, tc.bin, "\nfor test: {}\n", tc.name);
                assert_eq!(
                    relocs.get(0),
                    tc.relsym.as_ref(),
                    "\nfor test: {}\n",
                    tc.name
                );
            },
        }
    }
    /*
    set_test_marker(&mut s, &mut w);
    write_test_artifacts("rip_rel", s, w);
    */
}

fn encode(instr : Instr) {
    let mut w = mock_function_writer();

    instr.encode(&mut w);
}

#[test]
#[should_panic]
fn rip_sib_fail() {
    // "add [rip+rdx*2], edx",
    encode(Instr {
        mnemonic : Mn::ADD,
        operands : Opr::MR(
            (
                Reg::RIP,
                AddrMod::Indirect(Some((SibScale::Scale2, Reg::RDX))),
            ),
            Reg::EDX,
        ),
    })
}

#[test]
#[should_panic]
fn rip_sib_fail2() {
    // "add [rip+0xbeef], edx",
    encode(Instr {
        mnemonic : Mn::ADD,
        operands : Opr::MR(
            (
                Reg::RIP,
                AddrMod::IndirectDisp32(
                    0xbeef,
                    Some((SibScale::Scale8, Reg::RDX)),
                ),
            ),
            Reg::EDX,
        ),
    })
}
