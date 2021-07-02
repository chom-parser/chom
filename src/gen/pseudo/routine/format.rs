use crate::gen::pseudo::{
	self,
	expr::{FormatOperation as Operation, MatchCase},
	id, pattern, Context, Expr, Id, Pattern,
};

/// Generate the debug formatter for the given type.
///
/// The output of the generated code follows a specific output that can
/// be used to debug the generated formatter.
pub fn generate_debug(context: &Context, ty_ref: pseudo::ty::Ref) -> Expr {
	use pseudo::ty::Desc;
	let ty = context.ty(ty_ref).unwrap();
	match ty.desc() {
		Desc::Opaque => Expr::Unreachable,
		Desc::Enum(enm) => {
			let cases = enm
				.variants()
				.iter()
				.enumerate()
				.map(|(v, variant)| {
					use pseudo::ty::Variant;
					let v = v as u32;
					match variant {
						Variant::BuiltIn(_) => panic!("cannot format built-in variant"),
						Variant::Defined(id, desc) => {
							use pseudo::ty::VariantDesc;

							let expr = Expr::Format(Operation::Write(
								format!(
									"{}.{}",
									ty.id().ident().to_snake_case(),
									id.to_snake_case()
								),
								(0..desc.len())
									.map(|i| Expr::Get(Id::Format(id::Format::Arg(i))))
									.collect(),
							));

							let args = match desc {
								VariantDesc::Tuple(args) => pattern::ConsArgs::Tuple(
									(0..args.len())
										.map(|i| {
											Pattern::Bind(Id::Format(id::Format::Arg(i as u32)))
										})
										.collect(),
								),
								VariantDesc::Struct(strct) => pattern::ConsArgs::Struct(
									strct
										.fields()
										.iter()
										.enumerate()
										.map(|(i, f)| pattern::Binding {
											name: f.id.clone(),
											pattern: Pattern::Bind(Id::Format(id::Format::Arg(
												i as u32,
											))),
										})
										.collect(),
								),
							};

							MatchCase {
								pattern: Pattern::Cons(ty_ref, v, args),
								expr,
							}
						}
					}
				})
				.collect();

			Expr::Match {
				expr: Box::new(Expr::Get(Id::Format(id::Format::Itself))),
				cases,
			}
		}
		Desc::Struct(strct) => Expr::Format(Operation::Write(
			ty.id().ident().to_snake_case(),
			strct
				.fields()
				.iter()
				.map(|f| {
					Expr::GetField(
						Box::new(Expr::Get(Id::Format(id::Format::Itself))),
						f.id.clone(),
					)
				})
				.collect(),
		)),
		Desc::TupleStruct(args) => Expr::Format(Operation::Write(
			ty.id().ident().to_snake_case(),
			(0..args.len())
				.map(|i| {
					Expr::GetTupleField(
						Box::new(Expr::Get(Id::Format(id::Format::Itself))),
						i as u32,
					)
				})
				.collect(),
		)),
	}
}
