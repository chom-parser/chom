use crate::{out, poly::Grammar};

#[derive(Debug)]
pub enum Error {
	AmbiguousTerminals(u32, u32, TerminalAmbiguity),
}

#[derive(Debug)]
pub enum TerminalAmbiguity {
	// ShiftReduce {
	// 	prefix: String,
	// 	next: btree_range_map::AnyRange<char>,
	// },
	ReduceReduce { token: String },
}

impl Error {
	pub fn title(&self) -> String {
		match self {
			Error::AmbiguousTerminals(_, _, _) => format!("ambiguous terminals"),
		}
	}

	pub fn fill_block(&self, grammar: &Grammar, block: &mut out::Block) {
		use yansi::Paint;

		match self {
			Error::AmbiguousTerminals(id, other_id, ambiguity) => {
				let terminal = &grammar.terminals()[*id as usize].0;
				let other_terminal = &grammar.terminals()[*other_id as usize].0;
				let other_span = grammar.terminals()[*other_id as usize]
					.1
					.iter()
					.next()
					.unwrap()
					.span();

				block.highlights_mut().add(
					other_span,
					Some("ambiguous with this terminal".to_string()),
					out::WARNING,
				);

				match ambiguity {
					// TerminalAmbiguity::ShiftReduce { prefix, next } => {
					// 	block.add_note(
					// 		out::NoteType::Note,
					// 		format!(
					// 			"the following sequence of character is ambiguous:\n\n    {}{}\n\n",
					// 			Paint::new(prefix).bold(),
					// 			Paint::new(next.first().unwrap()).bold()
					// 		),
					// 	);
					// 	block.add_note(
					// 		out::NoteType::Note,
					// 		format!(
					// 			"this could be the {} {}, followed by some {} starting with {}",
					// 			terminal.format(grammar),
					// 			Paint::new(prefix).bold(),
					// 			other_terminal.format(grammar),
					// 			Paint::new(next.first().unwrap()).bold()
					// 		),
					// 	);
					// 	block.add_note(
					// 		out::NoteType::Note,
					// 		format!(
					// 			"this could also be some {} starting with {}{}",
					// 			other_terminal.format(grammar),
					// 			Paint::new(prefix).bold(),
					// 			Paint::new(next.first().unwrap()).bold()
					// 		),
					// 	);
					// }
					TerminalAmbiguity::ReduceReduce { token } => {
						block.add_note(
							out::NoteType::Note,
							format!(
								"the following sequence of character is ambiguous:\n\n    {}\n\n",
								Paint::new(token).bold()
							),
						);
						block.add_note(
							out::NoteType::Note,
							format!("this could be some {}", terminal.format(grammar)),
						);
						block.add_note(
							out::NoteType::Note,
							format!("this could also be some {}", other_terminal.format(grammar)),
						);
					}
				}
			} // _ => ()
		}
	}
}
