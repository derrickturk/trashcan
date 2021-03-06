use std::env;
use std::fs::File;

use std::io::Read;

extern crate trashcan;
use trashcan::parser;
use trashcan::analysis;
use trashcan::transform;
use trashcan::codegen::Emit;

fn main() {
    let args = env::args_os().collect::<Vec<_>>();
    if args.len() <= 1 {
        println!("tcc 0.1.0\nThe trashcan compiler\n\nUSAGE:\n    tcc source.tc [source2.tc ...]\n");
        return;
    }

    let mut dumpsters = vec![];
    for f in &args[1..] {
        let mut file = File::open(f).expect(
            &format!("Unable to open {}.", f.to_string_lossy()));
        let mut contents = vec![];
        let _ = file.read_to_end(&mut contents).expect(
            &format!("Unable to read {}.", f.to_string_lossy()));

        match parser::parse_dumpster(&f.to_string_lossy(), &contents) {
            Ok(d) => dumpsters.push(d),
            Err(err) => {
                println!("syntax error: {}", err);
                return;
            }
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
    let mut symtab = match analysis::SymbolTable::build(&mut dumpster) {
        Ok(symtab) => symtab,
        Err(errs) => {
            for err in errs {
                println!("{}", err);
            }
            return;
        }
    };

    // typecheck
    match analysis::typecheck(&mut dumpster, &symtab) {
        Ok(_) => { },
        Err(errs) => {
            for err in errs {
                println!("{}", err);
            }
            return;
        }
    };

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
}
