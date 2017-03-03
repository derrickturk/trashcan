use std::env;
use std::fs::File;

use std::io;
use std::io::Read;
use std::io::Write;

extern crate nom;

extern crate trashcan;
use trashcan::parser;
use trashcan::codegen::Emit;

fn main() {
    let args = env::args_os().collect::<Vec<_>>();
    if args.len() <= 1 {
        return;
    }

    let mut dumpsters = vec![];
    for f in &args[1..] {
        let mut file = File::open(f).expect(
            &format!("Unable to open {}.", f.to_string_lossy()));
        let mut contents = vec![];
        let _ = file.read_to_end(&mut contents).expect(
            &format!("Unable to read {}.", f.to_string_lossy()));
        let strip = parser::strip_comments(&contents);
        match parser::dumpster(&strip) {
            nom::IResult::Done(rest, dumpster) => {
                if !rest.is_empty() {
                    panic!("Invalid trailing content in {}",
                           f.to_string_lossy());
                }
                dumpsters.push(dumpster);
            },

            nom::IResult::Error(err) => {
                println!("{}", String::from_utf8_lossy(&strip));
                panic!("Parse error: {:?}", err);
            },

            res => {
                panic!("Something weird: {:?}", res);
            },
        }
    }

    let mut stdout = io::LineWriter::new(io::stdout());
    for d in dumpsters {
        for m in d.modules {
            m.emit(&mut stdout, (), 0).unwrap();
        }
    }
}
