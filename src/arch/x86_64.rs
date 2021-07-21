#![allow(unused_imports)]
#![allow(dead_code)]

use std::io;

use crate::instruction_writer::{FunctionWriter, LittleEndian, Reloc};

const SIB_RM_BITS : u8 = 0b100;
const PREFIX_ADDR_SIZE : u8 = 0x67;
const PREFIX_OP_SIZE : u8 = 0x66;

#[derive(Debug, Clone, Copy)]
pub enum Mn {
    // basic ops
    ADD,
    ADC,
    AND,
    OR,
    SBB,
    SUB,
    XOR,
    CMP,

    // shifts
    SHR,
    SHL,
    SAR,
    ROR,
    ROL,

    LEA,
    MOV,

    RETN,
    RETF,

    JMP,
    JL,  // signed
    JGE, // signed
    JB,  // unsigned
    JAE, // unsigned
    JZ,  // aka JE
    JNZ, // aka JNE
}

#[derive(Debug, Clone, Copy)]
pub enum Imm {
    Bits8(u8),
    Bits16(u16),
    Bits32(u32),
}

type Mem = (Reg, AddrMod);

#[derive(Debug, Clone)]
pub enum Opr {
    RM(Reg, Mem),
    MR(Mem, Reg),
    MI(Mem, Imm),
    I(Imm), // implicit AL/AX/EAX/RAX
    ZO,
}

#[derive(Debug, Clone)]
pub struct Instr {
    pub mnemonic : Mn,
    pub operands : Opr,
}

impl Instr {
    pub fn encode(self, w : &mut FunctionWriter<LittleEndian>) {
        // ADD-like
        if let Some(op_code) = match self.mnemonic {
            Mn::ADD => Some(0x00),
            Mn::ADC => Some(0x10),
            Mn::AND => Some(0x20),
            Mn::OR  => Some(0x08),
            Mn::SBB => Some(0x18),
            Mn::SUB => Some(0x28),
            Mn::XOR => Some(0x30),
            Mn::CMP => Some(0x38),
            _ => None,
        } {
            return match self.operands {
                Opr::I(Imm::Bits8(op2)) => {
                    w.push8(op_code | 0b100);
                    w.push8(op2);
                },
                Opr::I(Imm::Bits16(op2)) => {
                    w.push8(PREFIX_OP_SIZE);
                    w.push8(op_code | 0b101);
                    w.push16(op2);
                },
                Opr::I(Imm::Bits32(op2)) => {
                    w.push8(op_code | 0b101);
                    w.push32(op2);
                },
                Opr::MI((rm_reg, imm), Imm::Bits8(op2)) => {
                    let flag = if rm_reg.size() > RegSize::Bits8 {
                        0b10
                    } else {
                        0
                    };

                    enc_add_reg_rm(
                        w,
                        op_code | flag,
                        RegOrExt::Ext(0),
                        (rm_reg, imm),
                    );
                    w.push8(op2);
                },
                Opr::MI((rm_reg, imm), Imm::Bits16(op2)) => {
                    assert!(rm_reg.size() >= RegSize::Bits16);

                    enc_add_reg_rm(w, op_code, RegOrExt::Ext(0), (rm_reg, imm));
                    w.push16(op2);
                },
                Opr::MI((rm_reg, imm), Imm::Bits32(op2)) => {
                    assert!(rm_reg.size() >= RegSize::Bits32);

                    enc_add_reg_rm(w, op_code, RegOrExt::Ext(0), (rm_reg, imm));
                    w.push32(op2);
                },
                Opr::RM(reg, mem) => {
                    enc_add_reg_rm(w, op_code | 0b10, RegOrExt::Reg(reg), mem);
                },
                Opr::MR(mem, reg) => {
                    enc_add_reg_rm(w, op_code, RegOrExt::Reg(reg), mem);
                },
                _ => panic!("invalid operand for operation {:?}", self)
            }
        }


        // RET
        if let Some(op_code) = match self.mnemonic {
            Mn::RETN => Some(0xC2),
            Mn::RETF => Some(0xCA),
            _ => None,
        } {
            return match self.operands {
                Opr::ZO => {
                    w.push8(op_code | 1)
                },
                Opr::I(Imm::Bits16(pop_size)) => {
                    w.push8(op_code);
                    w.push16(pop_size);
                },
                _ => panic!("invalid operand for operation {:?}", self)
            }
        }

        panic!("not implemented")
    }
}

#[derive(Debug)]
pub enum RegOrExt {
    Reg(Reg),
    Ext(u8),
}

impl RegOrExt {
    fn try_reg(&self) -> Option<&Reg> {
        match self {
            Self::Reg(r) => Some(r),
            Self::Ext(_) => None,
        }
    }

    fn bits(&self) -> u8 {
        match self {
            Self::Reg(r) => r.reg_bits(),
            Self::Ext(n) => *n,
        }
    }

    fn is_reg(&self) -> bool {
        matches!(self, RegOrExt::Reg(_))
    }

    fn is_ext(&self) -> bool {
        matches!(self, RegOrExt::Ext(_))
    }
}

fn enc_add_reg_rm(
    // dst: &mut [u8;16],
    dst : &mut FunctionWriter<'_, LittleEndian>,
    op_code : u8,
    // TODO(ear7h) Option<Reg> for SIB addressing without a base
    reg : RegOrExt, // reg or op code extentsion
    rm : (Reg, AddrMod),
) {
    // | 0x80
    let original_state = dst.state_save();

    let (rm_reg, addr_mod) = rm;

    if addr_mod != AddrMod::Direct {
        assert!(
            rm_reg.size() >= RegSize::Bits32,
            "cannot use reg smaller than 32 bits as addr"
        );

        if rm_reg.size() == RegSize::Bits32 {
            dst.push8(PREFIX_ADDR_SIZE);
        }
    }

    println!("reg: {:?}", reg);
    if reg.try_reg().map_or(rm.0.size(), |r| r.size()) == RegSize::Bits16 {
        dst.push8(PREFIX_OP_SIZE);
    }

    // rex byte
    if reg.try_reg().map_or(false, |r| {
        r.size() == RegSize::Bits64
            || r.rex_bit()
            || matches!(r, Reg::SPL | Reg::BPL | Reg::SIL | Reg::DIL)
    }) || rm_reg.rex_bit()
        || addr_mod.reg().map_or(false, |r| r.rex_bit())
        || matches!(rm_reg, Reg::SPL | Reg::BPL | Reg::SIL | Reg::DIL)
    {
        assert!(
            !matches!(rm_reg, Reg::AH | Reg::CH | Reg::BH | Reg::DH),
            "cannot use high byte register {:?} with REX prefix",
            rm_reg
        );

        reg.try_reg().map(|r| {
            assert!(
                !matches!(r, Reg::AH | Reg::CH | Reg::BH | Reg::DH),
                "cannot use high byte register {:?} with REX prefix",
                r
            );
        });

        dst.push8(
            Rex {
                w : reg
                    .try_reg()
                    .map_or(false, |r| r.size() == RegSize::Bits64),
                r : reg.try_reg().map_or(false, |r| r.rex_bit()),
                x : addr_mod.reg().as_mut().map_or(false, |r| r.rex_bit()),
                b : rm_reg.rex_bit(),
            }
            .into(),
        );
    }

    // decide op code
    println!("reg {:?}", rm_reg);
    println!("reg {:?}", rm_reg.size());
    let op_code1 = match reg.try_reg().map_or(rm_reg.size(), |r| r.size()) {
        RegSize::Bits8 => op_code & (!1),
        RegSize::Bits16 | RegSize::Bits32 | RegSize::Bits64 => {
            println!("got op code");
            op_code | 1
        },
    } | reg.try_reg().map_or(0x80, |_| 0);

    println!("op_code1 {}", op_code1);
    dst.push8(op_code1);

    /*
    7 6 5 4 3 2 1 0
    | | | | | | | |
    | | | | |  r/m
    | |  reg
    mod
    */
    match addr_mod {
        AddrMod::Indirect(None) => {
            assert_ne!(
                rm_reg.reg_bits(),
                SIB_RM_BITS,
                "register number is used for SIB"
            );

            if matches!(rm_reg, Reg::RIP | Reg::EIP) {
                dst.state_load(&original_state);
                return enc_add_reg_rm(
                    dst,
                    op_code,
                    reg,
                    (rm_reg, AddrMod::IndirectDisp32(0, None)),
                )
            }

            if rm_reg.reg_bits() == Reg::RBP.reg_bits() {
                dst.state_load(&original_state);
                return enc_add_reg_rm(
                    dst,
                    op_code,
                    reg,
                    (rm_reg, AddrMod::IndirectDisp8(0, None)),
                )
            }

            dst.push8((reg.bits() << 3) | rm_reg.reg_bits());
        },
        AddrMod::Indirect(Some((scale, index))) => {
            dst.push8((0b00 << 6) | (reg.bits() << 3) | SIB_RM_BITS);

            // if base is RBP we need to encode it with
            // with a 0 displacement
            if rm_reg.reg_bits() == Reg::RBP.reg_bits() {
                dst.state_load(&original_state);
                return enc_add_reg_rm(
                    dst,
                    op_code,
                    reg,
                    (rm_reg, AddrMod::IndirectDisp8(0, Some((scale, index)))),
                )
            }

            dst.push8(enc_sib_byte(scale, Some(index), Some(rm_reg)));
        },
        AddrMod::IndirectDisp8(disp, None) => {
            assert_ne!(
                rm_reg.reg_bits(),
                SIB_RM_BITS,
                "register number is used for SIB"
            );

            if rm_reg == Reg::RIP || rm_reg == Reg::EIP {
                dst.state_load(&original_state);
                return enc_add_reg_rm(
                    dst,
                    op_code,
                    reg,
                    (rm_reg, AddrMod::IndirectDisp32(disp as u32, None)),
                )
            }

            dst.push8((0b01 << 6) | (reg.bits() << 3) | rm_reg.reg_bits());

            dst.push8(disp);
        },
        AddrMod::IndirectDisp8(disp, Some((scale, index))) => {
            dst.push8((0b01 << 6) | (reg.bits() << 3) | SIB_RM_BITS);

            dst.push8(enc_sib_byte(scale, Some(index), Some(rm_reg)));

            dst.push8(disp);
        },
        AddrMod::IndirectDisp32(disp, None) => {
            assert_ne!(
                rm_reg.reg_bits(),
                SIB_RM_BITS,
                "register number is used for SIB"
            );

            dst.push8(
                if matches!(rm_reg, Reg::RIP | Reg::EIP) {
                    0b00 << 6
                } else {
                    0b10 << 6
                } | (reg.bits() << 3)
                    | rm_reg.reg_bits(),
            );

            dst.push32(disp);
        },
        AddrMod::IndirectDisp32(disp, Some((scale, index))) => {
            dst.push8((0b10 << 6) | (reg.bits() << 3) | SIB_RM_BITS);

            dst.push8(enc_sib_byte(scale, Some(index), Some(rm_reg)));

            dst.push32(disp);
        },
        AddrMod::RIPRel(disp_opt, label_opt) => {
            assert!(matches!(rm_reg, Reg::RIP | Reg::EIP));

            dst.push8((0b00 << 6) | (reg.bits() << 3) | Reg::RIP.reg_bits());

            if let Some(label) = label_opt {
                dst.push_reloc(label);
            }

            dst.push32(disp_opt.unwrap_or(0));
        },
        AddrMod::Direct => {
            dst.push8((0b11 << 6) | (reg.bits() << 3) | rm_reg.reg_bits());
        },
    };
}

fn enc_sib_byte(
    scale : SibScale,
    index_opt : Option<Reg>,
    base_opt : Option<Reg>, /* TODO(ear7h) Option<Reg> for SIB addressing
                             * without a base */
) -> u8 {
    assert_eq!(
        index_opt.map(|r| r.size()),
        base_opt.map(|r| r.size()),
        "sib index and base must have same size"
    );

    index_opt.map(|index| {
        assert!(
            !matches!(index, Reg::RSP | Reg::ESP | Reg::RIP | Reg::EIP),
            "cannot use RSP/ESP or RIP/EIP as index register"
        );
    });

    base_opt.map(|base| {
        assert!(
            base.size() >= RegSize::Bits32,
            "sib index and base registers must be >= 32 bits"
        );
        assert!(
            !matches!(base, Reg::RIP | Reg::EIP),
            "cannot use RIP/EIP as base register"
        );
    });

    let scale_bits = match scale {
        SibScale::Scale1 => 0b00,
        SibScale::Scale2 => 0b01,
        SibScale::Scale4 => 0b10,
        SibScale::Scale8 => 0b11,
    };

    let index_bits = index_opt.map_or(0b100, |index| index.reg_bits());
    let base_bits = base_opt.map_or(0b101, |base| base.reg_bits());

    (scale_bits << 6) | (index_bits << 3) | base_bits
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum AddrMod {
    Indirect(Option<(SibScale, Reg)>),
    IndirectDisp8(u8, Option<(SibScale, Reg)>),
    IndirectDisp32(u32, Option<(SibScale, Reg)>),
    RIPRel(Option<u32>, Option<String>),
    Direct,
}

impl AddrMod {
    fn reg(&self) -> Option<Reg> {
        match self {
            Self::Indirect(Some((_, index))) => Some(*index),
            Self::IndirectDisp8(_, Some((_, index))) => Some(*index),
            Self::IndirectDisp32(_, Some((_, index))) => Some(*index),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum SibScale {
    Scale1,
    Scale2,
    Scale4,
    Scale8,
}

#[rustfmt::skip]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Reg {
    RAX, EAX, AX, AH, AL, // accumulator
    RBX, EBX, BX, BH, BL, // base
    RCX, ECX, CX, CH, CL, // counter
    RDX, EDX, DX, DH, DL, // data

    RBP, EBP, BP, BPL, // base pointer
    RSI, ESI, SI, SIL, // source index
    RDI, EDI, DI, DIL, // destination index
    RSP, ESP, SP, SPL, // stack pointer

    RIP, EIP, IP, // instruction pointer

    // general purpose regs added in x86-64
    R8,  R8D,  R8W,  R8B,
    R9,  R9D,  R9W,  R9B,
    R10, R10D, R10W, R10B,
    R11, R11D, R11W, R11B,
    R12, R12D, R12W, R12B,
    R13, R13D, R13W, R13B,
    R14, R14D, R14W, R14B,
    R15, R15D, R15W, R15B,
}

impl Reg {
    #[rustfmt::skip]
    pub fn size(&self) -> RegSize {
        match self {
            Self::RAX | Self::RBX | Self::RCX | Self::RDX |
            Self::RBP | Self::RSI | Self::RDI | Self::RSP |
            Self::R8  | Self::R9  | Self::R10 | Self::R11 |
            Self::R12 | Self::R13 | Self::R14 | Self::R15 |
            Self::RIP => RegSize::Bits64,

            Self::EAX  | Self::EBX  | Self::ECX  | Self::EDX  |
            Self::EBP  | Self::ESI  | Self::EDI  | Self::ESP  |
            Self::R8D  | Self::R9D  | Self::R10D | Self::R11D |
            Self::R12D | Self::R13D | Self::R14D | Self::R15D |
            Self::EIP => RegSize::Bits32,

            Self::AX   | Self::BX   | Self::CX   | Self::DX   |
            Self::BP   | Self::SI   | Self::DI   | Self::SP   |
            Self::R8W  | Self::R9W  | Self::R10W | Self::R11W |
            Self::R12W | Self::R13W | Self::R14W | Self::R15W |
            Self::IP => RegSize::Bits16,

            Self::AH   | Self::AL   | Self::BH   | Self::BL   |
            Self::CH   | Self::CL   | Self::DH   | Self::DL   |
            Self::BPL  | Self::SIL  | Self::DIL  | Self::SPL  |
            Self::R8B  | Self::R9B  | Self::R10B | Self::R11B |
            Self::R12B | Self::R13B | Self::R14B | Self::R15B => RegSize::Bits8,
        }
    }

    // only uses bottom 3 bits
    #[rustfmt::skip]
    pub fn rex_bit(&self) -> bool {
        match self {
            Self::R9  | Self::R9D  | Self::R9W  | Self::R9B  |
            Self::R10 | Self::R10D | Self::R10W | Self::R10B |
            Self::R11 | Self::R11D | Self::R11W | Self::R11B |
            Self::R12 | Self::R12D | Self::R12W | Self::R12B |
            Self::R13 | Self::R13D | Self::R13W | Self::R13B |
            Self::R14 | Self::R14D | Self::R14W | Self::R14B |
            Self::R15 | Self::R15D | Self::R15W | Self::R15B => true,
            _ => false,
        }
    }

    #[rustfmt::skip]
    pub fn reg_bits(&self) -> u8 {
        match self {
            Self::RAX | Self::EAX | Self::AX  | Self::AL  |
            Self::R8  | Self::R8D | Self::R8W | Self::R8B => 0,

            Self::RCX | Self::ECX | Self::CX  | Self::CL  |
            Self::R9  | Self::R9D | Self::R9W | Self::R9B => 1,

            Self::RDX | Self::EDX  | Self::DX   | Self::DL   |
            Self::R10 | Self::R10D | Self::R10W | Self::R10B => 2,

            Self::RBX | Self::EBX  | Self::BX   | Self::BL   |
            Self::R11 | Self::R11D | Self::R11W | Self::R11B => 3,

            Self::AH  |
            Self::RSP | Self::ESP  | Self::SP   | Self::SPL |
            Self::R12 | Self::R12D | Self::R12W | Self::R12B => 4,

            Self::CH |
            Self::RBP | Self::EBP  | Self::BP   | Self::BPL |
            Self::RIP | Self::EIP  | Self::IP   |
            Self::R13 | Self::R13D | Self::R13W | Self::R13B => 5,

            Self::DH  |
            Self::RSI | Self::ESI  | Self::SI   | Self::SIL  |
            Self::R14 | Self::R14D | Self::R14W | Self::R14B => 6,

            Self::BH  |
            Self::RDI | Self::EDI  | Self::DI   | Self::DIL  |
            Self::R15 | Self::R15D | Self::R15W | Self::R15B => 7,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum RegSize {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
}

pub struct Rex {
    pub w : bool, // when 1, a 64-bit operand sized is used, else (ususally) 32
    pub r : bool, // 1 bit extension to MODRM.reg
    pub x : bool, // 1 bit extension to SIB.index
    pub b : bool, // 1 bit extension to MODRM.rm or SIB.base
}

impl Into<u8> for Rex {
    fn into(self) -> u8 {
        let mut ret : u8 = 0b0100_0000;

        if self.w {
            ret |= 0b1000
        }
        if self.r {
            ret |= 0b100
        }
        if self.x {
            ret |= 0b10
        }
        if self.b {
            ret |= 0b1
        }

        ret
    }
}
