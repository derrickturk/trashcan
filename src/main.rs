use std::env;
use std::fs::File;
use std::io::Read;

extern crate nom;

extern crate trashcan;
use trashcan::parser;

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
        if let nom::IResult::Done(rest, dumpster) =
          parser::dumpster(&contents) {
            if !rest.is_empty() {
                panic!("Invalid trailing content in {}", f.to_string_lossy());
            }
            dumpsters.push(dumpster);
        } else {
            panic!("Invalid dumpster: {}", f.to_string_lossy());
        }
    }

    for d in dumpsters {
        println!("{:?}", d);
    }
}
