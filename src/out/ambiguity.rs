use source_span::{Position, Span};
use yansi::Paint;

const LINE_CHAR: char = '-';
const INTER_CHAR: char = '+';

pub fn format_ambiguity_example<A: AsRef<str>, B: AsRef<str>>(
	content: &str,
	a_span: Span,
	a_label: A,
	b_span: Span,
	b_label: B,
) -> String {
	let a_label = a_label.as_ref();
	let b_label = b_label.as_ref();

	let mut pos = Position::default();

	let mut down = String::new();
	let mut up = String::new();

	let mut a_before = true;
	let mut b_before = true;
	let mut offset = 0;

	let intersection = a_span.inter(b_span);

	for c in content.chars() {
		let in_both = pos >= intersection.start() && pos < intersection.end();

		if pos < a_span.start() {
			down.push(' ')
		} else if pos < a_span.end() {
			if pos == a_span.start() && a_before {
				let len = a_label.len() + 1;
				down.clear();
				if len >= pos.column {
					offset = len - pos.column;
				} else {
					for _ in 0..(pos.column - len) {
						down.push(' ')
					}
				}
				down.push_str(a_label);
				down.push(' ');
				b_before = false;
			}

			if in_both {
				down.push(INTER_CHAR)
			} else {
				down.push(LINE_CHAR)
			}

			if pos == a_span.last() && !a_before {
				down.push(' ');
				down.push_str(a_label)
			}
		}

		if pos < b_span.start() {
			up.push(' ')
		} else if pos < b_span.end() {
			if pos == b_span.start() && b_before {
				let len = b_label.len() + 1;
				up.clear();
				if len >= pos.column {
					offset = len - pos.column;
				} else {
					for _ in 0..(pos.column - len) {
						up.push(' ')
					}
				}
				up.push_str(b_label);
				up.push(' ');
				a_before = false;
			}

			if in_both {
				up.push(INTER_CHAR)
			} else {
				up.push(LINE_CHAR)
			}

			if pos == b_span.last() && !b_before {
				up.push(' ');
				up.push_str(b_label)
			}
		}

		pos = pos.next(c, &source_span::DEFAULT_METRICS);
	}

	let mut margin_str = String::new();
	for _ in 0..offset {
		margin_str.push(' ');
	}

	let a_margin = if a_before { "" } else { margin_str.as_str() };

	let b_margin = if b_before { "" } else { margin_str.as_str() };

	format!(
		"{}{}\n{}{}\n{}{}",
		b_margin,
		Paint::yellow(up).bold(),
		margin_str,
		content,
		a_margin,
		Paint::yellow(down).bold()
	)
}
