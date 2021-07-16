use std::{
	io::{
		self,
		BufReader,
		Read
	},
	fs::File
};
use source_span::Position;
use utf8_decode::UnsafeDecoder;
use chom::gen;
use chom_ir::eval;

#[test]
fn simple() -> io::Result<()> {
	let file = File::open("tests/simple.chom")?;
	let input = BufReader::new(file);
	let metrics = source_span::DefaultMetrics::with_tab_stop(4);
	let utf8_input = UnsafeDecoder::new(input.bytes());
	let buffer = source_span::SourceBuffer::new(utf8_input, Position::default(), metrics);
	
	match chom::parse_and_compile(&buffer, metrics) {
		Ok(grammar) => {
			let config = chom::gen::Config { locate: true };
			match gen::context_from_grammar(config, grammar.mono(), &["glue"], &["ast"], &["lexer"], &["parser"]) {
				Ok(context) => {
					use chom_ir::PrettyPrint;
					eprintln!("{}", context.ir_context().root_module().pretty_print(context.ir_context(), "  "));

					let phrase = "1 + 2";
					let target_ty = "expr";
					let expected = "expr.add(expr.term(term.integer(1)), expr.term(term.integer(2)))";
					
					let mut source = phrase.chars().map(|c| Ok(c));
					let mut eval = eval::Evaluator::new(context.ir_context());
					
					let target_ty_expr = eval.context().parse_ty_expr(&target_ty).expect("invalid target type expr").expect("target type not found");
					let lexer = eval.instanciate_lexer(&mut source).expect("no lexer");
					let parser = eval.context().parsers_for(&target_ty_expr).next().expect("no parser defined for target type");

					match eval.parse(parser, lexer) {
						Ok(result) => {
							match result {
								Ok(value) => {
									match eval.debug_format(value) {
										Ok(result) => {
											assert_eq!(result, expected);
											Ok(())
										}
										Err(e) => panic!("failed while formatting with: {}", e)
									}
								},
								Err(e) => panic!("failed with: {}", e.as_ref().name())
							}
						},
						Err(e) => {
							panic!("fatal error during parsing: {}", e)
						}
					}
				},
				Err(e) => {
					panic!("error")
				}
			}
		},
		Err(e) => {
			panic!("error")
		}
	}
}