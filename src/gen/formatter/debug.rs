use std::collections::HashMap;
use chom_ir::{
	function,
	ty,
	expr::{
		Var,
		MatchCase
	},
	Constant
};
use super::super::{
	Namespace,
	Id,
	id,
	Function,
	FunctionId,
	Expr,
	Pattern,
	TypeExpr
};

pub fn function<'c, 'a: 'c, 'p>(context: &'c chom_ir::Context<Namespace<'a, 'p>>, ty_ref: ty::Ref) -> Function<'a, 'p> {
	Function::new(
		function::Owner::Type(ty_ref),
		FunctionId::DebugFormat,
		function::Signature::debug_format(
			Id::Format(id::Format::Output)
		),
		Some(generate(context, ty_ref))
	)
}

pub fn generate_args_fmt<'c, 'a: 'c, 'p, A>(args: A) -> Expr<'a, 'p> where A: IntoIterator<Item=&'c TypeExpr<'a, 'p>>, A::IntoIter: ExactSizeIterator + DoubleEndedIterator {
	let args = args.into_iter();
	if args.len() == 0 {
		Expr::Literal(Constant::Unit)
	} else {
		let mut expr = Expr::Write(
			Id::Format(id::Format::Output),
			")".to_string(),
			Box::new(Expr::Literal(Constant::Unit))
		);

		for (i, _) in args.enumerate().rev() {
			expr = Expr::DebugFormat(
				Id::Format(id::Format::Output),
				Var::Defined(Id::Format(id::Format::Arg(i as u32))),
				Box::new(expr)
			);

			if i > 0 {
				expr = Expr::Write(
					Id::Format(id::Format::Output),
					", ".to_string(),
					Box::new(expr)
				)
			}
		}

		Expr::Write(
			Id::Format(id::Format::Output),
			"(".to_string(),
			Box::new(expr)
		)
	}
}

/// Generate the debug formatter for the given type.
///
/// The output of the generated code follows a specific output that can
/// be used to debug the generated formatter.
pub fn generate<'c, 'a: 'c, 'p>(context: &'c chom_ir::Context<Namespace<'a, 'p>>, ty_ref: ty::Ref) -> Expr<'a, 'p> {
	use chom_ir::Namespace;
	use ty::Desc;
	let ty = context.ty(ty_ref).unwrap();

	match ty.desc() {
		Desc::Opaque | Desc::Lexer => Expr::Unreachable,
		Desc::Enum(enm) => {
			let cases = enm
				.variants()
				.iter()
				.enumerate()
				.map(|(v, variant)| {
					use ty::Variant;
					let v = v as u32;
					match variant {
						Variant::Native(_) => panic!("cannot format native variant"),
						Variant::Defined(id, desc) => {
							use ty::VariantDesc;

							let (args, len) = match desc {
								VariantDesc::Tuple(args) => (generate_args_fmt(args), args.len()),
								VariantDesc::Struct(strct) => (generate_args_fmt(strct.fields().iter().map(|f| &f.ty)), strct.fields().len())
							};

							let expr = Expr::Write(
								Id::Format(id::Format::Output),
								format!(
									"{}.{}",
									ty.id().ident(context.id()),
									context.id().variant_ident(*id)
								),
								Box::new(args)
							);

							MatchCase {
								pattern: Pattern::Cons(
									ty_ref,
									v,
									(0..len).map(|i| {
										Pattern::Bind(
											Id::Format(id::Format::Arg(i as u32))
										)
									}).collect()
								),
								expr,
							}
						}
					}
				})
				.collect();

			Expr::Match {
				expr: Box::new(Expr::Get(Var::This)),
				cases,
			}
		}
		Desc::Struct(strct) => {
			let args = generate_args_fmt(strct.fields().iter().map(|f| &f.ty));
			Expr::Write(
				Id::Format(id::Format::Output),
				ty.id().ident(context.id()).to_snake_case().to_string(),
				Box::new(args)
			)
		},
		Desc::TupleStruct(args) => {
			let args = generate_args_fmt(args);
			Expr::Write(
				Id::Format(id::Format::Output),
				ty.id().ident(context.id()).to_snake_case().to_string(),
				Box::new(args)
			)
		},
	}
}