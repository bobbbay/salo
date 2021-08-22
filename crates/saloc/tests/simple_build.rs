use saloc::compiler::compiler::*;

#[test]
/// Test the full workflow of the typestate compiler.
fn typestate_workflow_test() {
    let parser = Parser::<Source>::new("<stdin>", "a : Str;");

    let ast = match parser.parse() {
        MaybeAST::AST(ast) => ast,
        MaybeAST::Error(error) => {
            error.report();
        }
    };

    let output = match ast.evaluate() {
        MaybeOutput::Output(output) => output,
        MaybeOutput::Error(error) => error.report(),
    };

    output.export();
}
