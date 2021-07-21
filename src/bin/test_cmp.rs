use std::env::args;
use std::fs::File;
use std::io::Read;
use std::process::Command;

fn main() {
    let n = args().nth(1).unwrap();

    println!(
        "{:?}",
        Command::new("nasm")
            .arg(format!("test_{}.asm", n))
            .args(vec!["-f", "macho64", "-O0", "-o"])
            .arg(format!("test_{}_nasm.bin", n))
            //.stdout(Stdio::null())
            .output()
            .unwrap()
    );

    let mut nasm_bin = Vec::new();
    File::open(format!("test_{}_nasm.bin", n))
        .unwrap()
        .read_to_end(&mut nasm_bin)
        .unwrap();
    let (nasm_start, nasm_end) = get_start_end(&nasm_bin);
    let nasm_len = nasm_end - nasm_start;

    let mut lang_bin = Vec::new();
    File::open(format!("test_{}.bin", n))
        .unwrap()
        .read_to_end(&mut lang_bin)
        .unwrap();
    let (lang_start, lang_end) = get_start_end(&lang_bin);
    let lang_len = lang_end - lang_start;

    for i in 0..nasm_len.min(lang_len) {
        assert_eq!(
            nasm_bin[nasm_start + i],
            lang_bin[lang_start + i],
            "at byte {}",
            i
        );
    }

    assert_eq!(nasm_end - nasm_start, lang_end - lang_start);
}

fn get_start_end(bin : &Vec<u8>) -> (usize, usize) {
    let mut idx_start = 0;
    let mut nop_count = 0;

    for b in bin {
        if *b == 0x90 {
            nop_count += 1;
        } else {
            nop_count = 0;
        }
        idx_start += 1;

        if nop_count == 4 {
            break
        }
    }

    let mut idx_end = 0;
    nop_count = 0;
    for b in &bin[idx_start..] {
        if *b == 0x90 {
            nop_count += 1;
        } else {
            nop_count = 0;
        }
        idx_end += 1;

        if nop_count == 4 {
            break
        }
    }

    (idx_start, idx_end)
}
