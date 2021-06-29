use std::path::{Path,PathBuf};
use proc_macro2::TokenStream;
use quote::quote;
use crate::{mono,gen::{Parser,pseudo}};
use super::{
	Target,
	Generate
};

/// Rust target.
pub struct Rust;

impl Target for Rust {
	type Output = TokenStream;

	fn module_filename<P: AsRef<Path>>(&self, root: P, module_path: pseudo::module::Path) -> PathBuf {
		let mut file_path = root.as_ref().to_path_buf();

		for id in module_path {
			if let pseudo::module::Id::Named(name) = id {
				file_path.push(crate::util::to_snake_case(&name));
			}
		}

		file_path.set_extension("rs");
		file_path
	}
}

impl Generate<pseudo::module::Id> for Rust {
	fn generate(&self, context: &pseudo::Context, id: &pseudo::module::Id) -> TokenStream {
		match id {
			pseudo::module::Id::Root => quote! { crate },
			pseudo::module::Id::Named(name) => {
				let id = quote::format_ident!("{}", crate::util::to_snake_case(&name));
				quote! { #id }
			}
		}
	}
}

impl Rust {
	fn module_path(&self, path: pseudo::module::Path) -> TokenStream {
		let mut tokens = TokenStream::new();

		tokens
	}

	fn generate_lexer_definition(&self) -> TokenStream {
		quote! {
			pub struct Lexer<I: ::std::iter::Itrator, M> {
				chars: ::std::iter::Peekable<I>,
				buffer: ::std::string::String,
				metrics: M,
				span: ::source_span::Span
			}
		}
	}

	fn generate_lexer_impl(&self, context: &pseudo::Context, expr: &pseudo::Expr) -> TokenStream {
		let extern_module_path = self.module_path(context.extern_module_path());
		let body = self.generate(context, expr);
	
		quote! {
			impl<
				E: ::std::convert::Into<#extern_module_path::Error>,
				I: ::std::iter::Iterator<Item = ::std::result::Result<char, E>>,
				M: ::source_span::Metrics,
			> Lexer<I, M> {
				fn peek_char(
					&mut self
				) -> ::std::result::Result<
					::std::option::Option<char>,
					::source_span::Loc<#extern_module_path::Error>,
				> {
					match self.chars.peek() {
						Some(Ok(c)) => Ok(Some(*c)),
						Some(Err(_)) => Err(self.consume_char().unwrap_err()),
						None => Ok(None),
					}
				}
	
				fn consume_char(
					&mut self,
				) -> ::std::result::Result<(), ::source_span::Loc<#extern_module_path::Error>> {
					match self.chars.next() {
						Some(Ok(c)) => {
							self.buffer.push(c);
							self.span.push(c, &self.metrics);
							Ok(())
						}
						Some(Err(e)) => Err(::source_span::Loc::new(e.into(), self.span.end().into())),
						None => Ok(()),
					}
				}
	
				fn next_token(
					&mut self,
				) -> ::std::result::Result<
					::std::option::Option<::source_span::Loc<Token>>,
					::source_span::Loc<#extern_module_path::Error>,
				> {
					#body
				}
			}
		}
	}

	fn generate_parser(&self, context: &pseudo::Context, index: mono::Index, parser: &Parser) -> TokenStream {
		let ty = context.grammar().ty(index).unwrap();
		let id = format!("parse_{}", ty.id().as_defined().to_caml_case());
	
		match parser {
			Parser::LR0(expr) => {
				let body = self.generate(context, expr);
				quote! {
					fn #id<
						L: ::std::iter::Iterator<
							Item = ::std::result::Result<
								::source_span::Loc<crate::test::lexer::Token>,
								::source_span::Loc<Error>,
							>,
						>,
					>(
						lexer: &mut L,
					) -> {
						#body
					}
				}
			},
			Parser::LALR1(expr) => {
				unimplemented!()
			}
		}
	}
}

impl Generate<pseudo::Module> for Rust {
	fn generate(&self, context: &pseudo::Context, module: &pseudo::Module) -> TokenStream {
		use pseudo::module::Role;
		use pseudo::Routine;

		let roles = module.roles().map(|r| match r {
			Role::Lexer => self.generate_lexer_definition(),
			_ => TokenStream::new()
		});

		let routines = module.routines().map(|r| match r {
			Routine::Lexer(expr) => self.generate_lexer_impl(context, expr),
			Routine::Parser(index, parser) => self.generate_parser(context, *index, parser)
		});
		
		let submodules = module.modules().map(|index| {
			let m = context.module(index).unwrap();
			let id = self.generate(context, m.id());
			let inner = self.generate(context, m);
			quote! {
				mod #id {
					#inner
				}
			}
		});
		
		quote! {
			#(#roles)*
			#(#routines)*
			#(#submodules)*
		}
	}
}

impl Generate<Box<pseudo::Expr>> for Rust {
	fn generate(&self, context: &pseudo::Context, e: &Box<pseudo::Expr>) -> TokenStream {
		self.generate(context, e.as_ref())
	}
}

impl Generate<pseudo::Expr> for Rust {
	fn generate(&self, context: &pseudo::Context, e: &pseudo::Expr) -> TokenStream {
		use pseudo::Expr;
		match e {
			Expr::Constant(c) => self.generate(context, c),
			Expr::Get(id) => self.generate(context, id),
			Expr::Set(id, value, next) => {
				let id = self.generate(context, id);
				let value = self.generate(context, value);
				let next = self.generate(context, next);
				quote! {
					let #id = #value;
					#next
				}
			},
			Expr::Lexer(op) => self.generate(context, op),
			Expr::Parser(op) => self.generate(context, op),
			// Expr::New(ty, args) => {
				
			// }
			_ => unimplemented!()
		}
	}
}

impl Generate<pseudo::expr::LexerOperation> for Rust {
	fn generate(&self, context: &pseudo::Context, op: &pseudo::expr::LexerOperation) -> TokenStream {
		use pseudo::expr::LexerOperation as Operation;
		match op {
			Operation::SourcePeek => quote! { self.peek()? },
			Operation::SourceConsume(next) => {
				let next = self.generate(context, next);
				quote! { self.consume()?; #next }
			},
			Operation::BufferPush(next) => {
				let next = self.generate(context, next);
				quote! { self.buffer.push(c); #next }
			},
			Operation::BufferClear(next) => {
				let next = self.generate(context, next);
				quote! { self.buffer.clear(); #next }
			},
			Operation::BufferParse(index) => {
				let terminal = context.grammar().terminal(*index).unwrap();
				let ident = terminal.id(context.grammar().poly()).unwrap();
				let id = quote::format_ident!("{}", ident.to_snake_case());
				let extern_module_path = self.module_path(context.extern_module_path());
				quote! { #extern_module_path::#id(self.buffer.as_str()) }
			},
			Operation::BufferIter => quote! { self.buffer.chars() },
			Operation::BufferCharsNext(next) => {
				let next = self.generate(context, next);
				quote! { let char_opt = chars.next(); #next }
			}
		}
	}
}

impl Generate<pseudo::expr::ParserOperation> for Rust {
	fn generate(&self, context: &pseudo::Context, op: &pseudo::expr::ParserOperation) -> TokenStream {
		use pseudo::expr::ParserOperation as Operation;
		match op {
			Operation::StackPush(value, next) => {
				let value = self.generate(context, value);
				let next = self.generate(context, next);
				quote! { self.stack.push(#value); #next }
			},
			Operation::StackPop(patterns, next) => {
				let pop = patterns.iter().rev().map(|pattern| {
					quote! { let item = self.stack.pop(); }
				});
				let next = self.generate(context, next);
				quote! {
					#(#pop)*
					#next
				}
			},
			Operation::LexerNext(next) => {
				let next = self.generate(context, next);
				quote! { let any_token_opt = lexer.next()?; #next }
			}
		}
	}
}

impl Generate<pseudo::Constant> for Rust {
	fn generate(&self, _: &pseudo::Context, c: &pseudo::Constant) -> TokenStream {
		use pseudo::Constant;
		match c {
			Constant::Int(n) => quote! { #n },
			Constant::Char(c) => quote! { #c },
			Constant::CharRange(a, b) => quote! { #a..=#b }
		}
	}
}

impl Generate<pseudo::Id> for Rust {
	fn generate(&self, _: &pseudo::Context, id: &pseudo::Id) -> TokenStream {
		use pseudo::Id;
		match id {
			Id::Source => quote! { self.source },
			Id::CharOpt => quote! { c_opt },
			Id::State => quote! { self.state },
			Id::Buffer => quote! { self.buffer },
			Id::BufferChars => quote! { chars },
			Id::SubToken => quote! { sub_token },
			Id::Lexer => quote! { self.lexer },
			Id::AnyTokenOpt => quote! { any_token_opt },
			Id::Token => quote! { token },
			Id::Stack => quote! { self.stack },
			Id::AnyNodeOpt => quote! { self.any_node_opt },
			Id::AnyNode => quote! { any_node },
			Id::Node => quote! { node },
			Id::Item(i) => {
				let id = quote::format_ident!("item{}", i);
				quote! { #id }
			},
			Id::SavedState => quote! { saved_state },
			Id::Unexpected => quote! { unexpected },
			Id::Result => quote! { result }
		}
	}
}