use std::env;
use std::fs::File;

use std::io;
use std::io::Read;

extern crate nom;

extern crate trashcan;
use trashcan::parser;
use trashcan::analysis;
use trashcan::transform;
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

    // pre-processing / rename passes
    let dumpster = transform::merge_dumpsters(dumpsters);

    // order matters here!
    let dumpster = transform::for_loop_var_gensym(dumpster);
    let dumpster = transform::vb_keyword_gensym(dumpster);
    let dumpster = transform::fn_name_local_gensym(dumpster);
    let mut dumpster = transform::case_folding_duplicate_gensym(dumpster);

    // symbol table generation & deferred type resolution
    let mut symtab = analysis::SymbolTable::build(&mut dumpster)
        .expect("symtab/resolve error");

    // typecheck
    analysis::typecheck(&dumpster, &symtab)
        .expect("typeck error");

    // post-processing / semantics-preserving passes
    //   (these need symbols and access to typing)
    //   (they also may emit new symbols etc)
    // order matters here!
    let dumpster = transform::cast_rewrite(dumpster, &mut symtab);
    let dumpster = transform::short_circuit_logicals(dumpster, &mut symtab);
    let dumpster = transform::array_loop_rewrite(dumpster, &mut symtab);
    let dumpster = transform::along_loop_rewrite(dumpster);
    let dumpster = transform::alloc_along_rewrite(dumpster, &mut symtab);

    // codegen pass
    for m in dumpster.modules.iter() {
        let file = m.filename();
        let mut file = File::create(&file).expect(
            &format!("Unable to open {}.", file));
        m.emit(&mut file, &symtab, (), 0).unwrap();
    }

    let mut stdout = io::LineWriter::new(io::stdout());
    symtab.dump(&mut stdout, 0).unwrap();
}
