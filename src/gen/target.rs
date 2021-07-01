use super::pseudo;
use std::path::{Path, PathBuf};

mod rust;

pub use rust::Rust;

pub trait Target {
	type Output;

	fn module_filename<P: AsRef<Path>>(&self, root: P, path: pseudo::module::Path) -> PathBuf;
}

pub trait Generate<T>: Target {
	/// Generate the given component.
	fn generate(&self, context: &pseudo::Context, t: &T) -> <Self as Target>::Output;
}
