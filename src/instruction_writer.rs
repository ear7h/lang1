#![allow(unused_variables)]
#![allow(dead_code)]

use std::{
    collections::HashMap,
    io::self,
};

pub struct InstructionWriter {
    saved_section: usize,
    saved_byte: usize,

    sections : Vec<(Section, Vec<u8>)>,

    /// symbol table
    symtab: HashMap<String, SymRow>,

    /// relocation table
    reltab: Vec<(String, RelRow)>,
}

impl InstructionWriter {
    pub fn new(first_section: Section) -> Self {
        Self {
            saved_section: 1,
            saved_byte: 0,
            sections: vec![(first_section, vec![])],
            symtab: HashMap::new(),
            reltab: Vec::new(),
        }
    }

    pub fn write_to<W : io::Write>(&self, w : &mut W) -> io::Result<()> {
        for (_, s) in &self.sections {
            w.write(s.as_slice())?;
        }

        Ok(())
    }

    pub fn reltab(&self) -> &Vec<(String, RelRow)> {
        &self.reltab
    }

    pub fn push_section(&mut self, section : Section) {
        self.sections.push((section, vec![]));
    }

    pub fn state_save(&mut self) {
        self.saved_section = self.sections.len();
        self.saved_byte = self.sections.last().unwrap().1.len();
    }

    pub fn state_load(&mut self) {
        while self.saved_section != self.sections.len() {
            self.sections.pop();
            self.saved_byte = self.sections.last().unwrap().1.len();
        }

        while self.saved_byte != self.sections.last().unwrap().1.len() {
            self.sections.last_mut().unwrap().1.pop();
        }

        self.state_save();
    }

    pub fn push_8(&mut self, d: u8) {
        self.sections.last_mut().unwrap().1.push(d);
    }

    pub fn push_32(&mut self, d: u32) {
        self.push_8(d as u8);
        self.push_8((d >> 8)  as u8);
        self.push_8((d >> 16)  as u8);
        self.push_8((d >> 24)  as u8);
    }

    pub fn push_symbol(&mut self, name: String) -> Result<(),()> {
        let old = self.symtab.insert(name, SymRow{
            section_idx: self.sections.len()-1,
            idx: self.sections.last().unwrap().1.len(),
        });

        if old.is_some() {
            Err(())
        } else {
            Ok(())
        }
    }

    // len is in bytes
    pub fn push_reloc(&mut self, label: String, len: u64) {
        self.reltab.push((label, RelRow{
            section_idx: self.sections.len() - 1,
            idx: self.sections.last_mut().unwrap().1.len(),
            len: len,
        }));
    }

    #[cfg(test)]
    pub fn first_section_slice(&self) -> &[u8] {
        assert_eq!(self.sections.len(), 1);
        &self.sections[0].1
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct RelRow {
    pub section_idx: usize,
    pub idx: usize,
    pub len: u64,
}

pub struct SymRow {
    section_idx: usize,
    idx: usize,
}

// TODO(ear7h) put this in a object.rs
pub enum Section {
    Text,
    Data,
}
