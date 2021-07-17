use super::super::{id, Expr, Function, FunctionId, Id, Namespace, Pattern, TypeExpr};
use chom_ir::{expr::MatchCase, function, ty};

pub fn function<'c, 'a: 'c, 'p>(
	context: &'c chom_ir::Context<Namespace<'a, 'p>>,
	ty_ref: ty::Ref,
) -> Function<'a, 'p> {
	let ty = context.ty(ty_ref).unwrap();
	Function::new(
		function::Owner::Type(ty_ref),
		FunctionId::DebugFormat,
		function::Signature::debug_format(
			Id::Format(id::Format::This),
			ty::Expr::Instance(
				ty_ref,
				ty.parameters().iter().map(|x| ty::Expr::Var(*x)).collect(),
			),
			Id::Format(id::Format::Output),
		),
		Some(generate(context, ty_ref)),
	)
}

pub fn generate_args_fmt<'c, 'a: 'c, 'p, A>(args: A, fields: bool) -> Expr<'a, 'p>
where
	A: IntoIterator<Item = &'c TypeExpr<'a, 'p>>,
	A::IntoIter: ExactSizeIterator + DoubleEndedIterator,
{
	let args = args.into_iter();
	if args.len() == 0 {
		Expr::Get(Id::Format(id::Format::Output))
	} else {
		let mut expr = Expr::Write(
			Id::Format(id::Format::Output),
			")".to_string(),
			Box::new(Expr::Get(Id::Format(id::Format::Output))),
		);

		for (i, _) in args.enumerate().rev() {
			expr = Expr::DebugFormat(
				Id::Format(id::Format::Output),
				Box::new(if fields {
					Expr::RefField(Id::Format(id::Format::This), i as u32)
				} else {
					Expr::Get(Id::Format(id::Format::Arg(i as u32)))
				}),
				Box::new(expr),
			);

			if i > 0 {
				expr = Expr::Write(
					Id::Format(id::Format::Output),
					", ".to_string(),
					Box::new(expr),
				)
			}
		}

		Expr::Write(
			Id::Format(id::Format::Output),
			"(".to_string(),
			Box::new(expr),
		)
	}
}

/// Generate the debug formatter for the given type.
///
/// The output of the generated code follows a specific output that can
/// be used to debug the generated formatter.
pub fn generate<'c, 'a: 'c, 'p>(
	context: &'c chom_ir::Context<Namespace<'a, 'p>>,
	ty_ref: ty::Ref,
) -> Expr<'a, 'p> {
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
								VariantDesc::Tuple(args) => {
									(generate_args_fmt(args, false), args.len())
								}
								VariantDesc::Struct(strct) => (
									generate_args_fmt(strct.fields().iter().map(|f| &f.ty), false),
									strct.fields().len(),
								),
							};

							let expr = Expr::Write(
								Id::Format(id::Format::Output),
								format!(
									"{}.{}",
									ty.id().ident(context.id()),
									context.id().variant_ident(*id)
								),
								Box::new(args),
							);

							MatchCase {
								pattern: Pattern::Cons(
									ty_ref,
									v,
									(0..len)
										.map(|i| {
											Pattern::Bind(Id::Format(id::Format::Arg(i as u32)))
										})
										.collect(),
								),
								expr,
							}
						}
					}
				})
				.collect();

			Expr::MatchRef {
				expr: Box::new(Expr::Get(Id::Format(id::Format::This))),
				cases,
			}
		}
		Desc::Struct(strct) => {
			let args = generate_args_fmt(strct.fields().iter().map(|f| &f.ty), true);
			Expr::Write(
				Id::Format(id::Format::Output),
				ty.id().ident(context.id()).to_snake_case().to_string(),
				Box::new(args),
			)
		}
		Desc::TupleStruct(args) => {
			let args = generate_args_fmt(args, true);
			Expr::Write(
				Id::Format(id::Format::Output),
				ty.id().ident(context.id()).to_snake_case().to_string(),
				Box::new(args),
			)
		}
	}
}
