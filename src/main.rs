use std::fs::read_to_string;
use std::os::linux::raw::stat;

use eyre::Result;

use sea::parser;
use sea::parser::debug::print_statement;
use sea::parser::lexer;

fn call(command_name: &str, args: &[&str]) -> Result<String> {
    let output = std::process::Command::new(command_name)
        .args(args)
        .stdin(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .stderr(std::process::Stdio::inherit())
        .output()?;

    if output.status.success() {
        Ok(String::from_utf8(output.stdout)?)
    } else {
        Err(eyre::eyre!(String::from_utf8(output.stderr)?))
    }
}

fn main() {
    let input = &read_to_string("test.rv").expect("Failed to read file");

    let tokens = lexer::Lexer::new(input).lex();

    let (statements, errors) = parser::Parser::new(input, tokens).parse();

    for error in errors.iter() {
        let spanned = &input[error.start..error.end];
        println!("{:?} => {:?}", error, spanned);
    }

    // for statement in statements.iter() {
    //     print_statement(statement, input, 0)
    // }

    let mut ircompiler = sea::compiler::ir::IRCompiler::new();

    for stat in statements.iter() {
        if let Err(e) = ircompiler.compile_statement(stat, None) {
            ircompiler.print();
            panic!("{:?}", e);
        }
    }

    ircompiler.print();

    // let compiler = sea::compiler::Compiler::new("user".to_string());

    // let code = compiler.compile(&statements).expect("Failed to compile");

    // std::fs::write("output.c", code).expect("Failed to write file");

    // call("clang-format", &["-i", "output.c"]).expect("Failed to format");

    // call("gcc", &["output.c", "-O3", "-o", "output"]).expect("Failed to compile");

    // println!("\x1b[36m{}\x1b[0m", "-------------- Running --------------");

    // let start = std::time::Instant::now();
    // call("./output", &[]).expect("Failed to run");
    // let end = start.elapsed();

    // println!(
    //     "\n\x1b[36m{}\x1b[0m",
    //     "-------------------------------------"
    // );

    // println!("\x1b[32m{}\x1b[0m", format!("Time: {:?}", end));
}
