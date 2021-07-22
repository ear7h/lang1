#![allow(unused_variables)]
#![allow(dead_code)]

use std::fs::File;
use std::cell::Cell;

use faerie;

pub struct RelocChunk {
    from :   String,
    relocs : Vec<Reloc>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Reloc {
    pub at : usize,
    pub to : String,
}


pub trait Endian {
    fn from_le(le : &mut [u8]) -> &mut [u8];
}

pub struct LittleEndian;

impl Endian for LittleEndian {
    fn from_le(x : &mut [u8]) -> &mut [u8] {
        x
    }
}

pub struct BigEndian;

impl Endian for BigEndian {
    fn from_le(x : &mut [u8]) -> &mut [u8] {
        x.reverse();
        x
    }
}

#[cfg(test)]
pub fn mock_function_writer<E : Endian>() -> FunctionWriter<E> {
    FunctionWriter::<E> {
        instrs : Vec::new(),
        relocs : Vec::new(),
        _endian : Default::default(),
    }
}

pub struct FunctionWriter<E> {
    instrs :  Vec<u8>,
    relocs :  Vec<Reloc>,
    _endian : std::marker::PhantomData<E>,
}

pub struct FunctionWriterState {
    instrs : usize,
    relocs : usize,
}

impl<E : Endian> FunctionWriter<E> {

    /// (<>)
    pub fn extend(&mut self, other : &mut Self) {
        let l = self.instrs.len();
        self.instrs.append(&mut other.instrs);

        for mut reloc in other.relocs.drain(..) {
            reloc.at += l;
            self.relocs.push(reloc)
        }
    }

    pub fn clear(&mut self) {
        self.instrs.clear();
        self.relocs.clear();
    }

    pub fn push_bytes(&mut self, b : &[u8]) {
        for v in b.iter().copied() {
            self.instrs.push(v)
        }
    }

    pub fn push8(&mut self, x : u8) {
        self.instrs.push(x);
    }

    pub fn push16(&mut self, x : u16) {
        self.instrs
            .extend_from_slice(E::from_le(&mut x.to_le_bytes()))
    }

    pub fn push32(&mut self, x : u32) {
        self.instrs
            .extend_from_slice(E::from_le(&mut x.to_le_bytes()))
    }

    pub fn push64(&mut self, x : u64) {
        self.instrs
            .extend_from_slice(E::from_le(&mut x.to_le_bytes()))
    }


    /// assumes 4 byte address
    pub fn push_reloc(&mut self, to : String) {
        self.relocs.push(Reloc {
            to,
            at : self.instrs.len(),
        })
    }

    pub fn state_save(&self) -> FunctionWriterState {
        FunctionWriterState {
            instrs : self.instrs.len(),
            relocs : self.relocs.len(),
        }
    }

    pub fn state_load(&mut self, state : &FunctionWriterState) {
        self.instrs.truncate(state.instrs);
        self.relocs.truncate(state.relocs);
    }

    pub fn parts(self) -> (Vec<u8>, Vec<Reloc>) {
        (self.instrs, self.relocs)
    }
}

pub struct ObjectWriter {
    artifact : faerie::Artifact,
    relocs :   Vec<Reloc>,
    relocs_buf :   Cell<Vec<Reloc>>,
}

impl ObjectWriter {
    pub fn new(artifact : faerie::Artifact) -> Self {
        Self{
            relocs : Default::default(),
            relocs_buf : Default::default(),
            artifact,
        }
    }

    pub fn write(&self, f : File) -> Result<(), faerie::ArtifactError> {
        // TODO: write relocs
        self.artifact.write(f)
    }

    pub fn func<F, T>(&mut self, name : String, f : F) -> T
    where
        F : FnOnce(&mut FunctionWriter<LittleEndian>) -> T,
    {


        let mut fw = FunctionWriter {
            instrs :  Vec::new(),
            relocs :  self.relocs_buf.take(),
            _endian : Default::default(),
        };

        let ret = f(&mut fw);

        self.relocs_buf.set(fw.relocs);

        self.relocs.append(self.relocs_buf.get_mut());

        self.artifact
            .declare_with(&name, faerie::Decl::function().global(), fw.instrs)
            .unwrap();

        ret
    }
}
