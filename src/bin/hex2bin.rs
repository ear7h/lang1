use std::env;
use std::fs::File;
use std::io::{self, BufRead, Write};

fn main() -> io::Result<()> {
    let mut args = env::args();
    if args.len() != 2 {
        panic!("must have only 1 arg");
    }

    let fname = args.nth(1).unwrap();

    let lines = io::BufReader::new(File::open(fname)?).lines();

    let mut out = io::stdout();

    for rline in lines {
        let mut line = rline?;

        line.truncate(line.find("#").unwrap_or(line.len()));

        let hexes = line.matches(|c| {
            (c >= '0' && c <= '9')
                || (c >= 'a' && c <= 'f')
                || (c >= 'A' && c <= 'F')
        });

        let mut buf : [u8; 1] = [0];
        let mut parity = false;

        for hex in hexes {
            match parity {
                false => buf[0] = hex2nibble(hex.as_bytes()[0]),
                true => {
                    buf[0] = (buf[0] << 4) | hex2nibble(hex.as_bytes()[0]);
                    out.write(&buf)?;
                },
            }
            parity = !parity;
        }

        if parity {
            buf[0] = 0;
            out.write(&buf)?;
        }

        out.flush()?;
    }

    Ok(())
}

fn hex2nibble(b : u8) -> u8 {
    return match b as char {
        '0' => 0,
        '1' => 1,
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 8,
        '9' => 9,
        'A' => 10,
        'B' => 11,
        'C' => 12,
        'D' => 13,
        'E' => 14,
        'F' => 15,
        'a' => 10,
        'b' => 11,
        'c' => 12,
        'd' => 13,
        'e' => 14,
        'f' => 15,
        _ => 0,
    }
}
