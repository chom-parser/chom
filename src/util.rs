pub fn to_caml_case(s: &str) -> String {
	let mut id = String::new();
	let mut up = true;

	for c in s.chars() {
		match c {
			'_' | ' ' | '-' => {
				up = true;
			}
			c if up => {
				up = false;
				id.push(c.to_uppercase().next().unwrap());
			}
			c => {
				id.push(c);
			}
		}
	}

	id
}

pub fn upcase_to_caml_case(s: &str) -> String {
	let mut id = String::new();
	let mut up = true;

	for c in s.chars() {
		match c {
			'_' | ' ' | '-' => {
				up = true;
			}
			c if up => {
				up = false;
				id.push(c.to_uppercase().next().unwrap());
			}
			c => {
				id.push(c.to_lowercase().next().unwrap());
			}
		}
	}

	id
}

pub fn to_snake_case(s: &str) -> String {
	let mut id = String::new();
	for c in s.chars() {
		match c {
			' ' | '-' => {
				if !id.is_empty() {
					id.push('_');
				}
			}
			c if c.is_uppercase() => {
				if !id.is_empty() {
					id.push('_');
				}

				id.push(c.to_lowercase().next().unwrap());
			}
			c => id.push(c),
		}
	}

	id
}

pub fn upcase_to_snake_case(s: &str) -> String {
	let mut id = String::new();
	for c in s.chars() {
		match c {
			' ' | '-' => {
				if !id.is_empty() {
					id.push('_');
				}
			}
			c => id.push(c.to_lowercase().next().unwrap()),
		}
	}

	id
}
