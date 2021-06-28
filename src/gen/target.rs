use super::pseudo;

pub mod rust;

pub trait Target {
	type Output;
}

pub trait Generate<T>: Target {
	/// Generate the given component.
	fn generate(&self, context: &pseudo::Context, t: &T) -> <Self as Target>::Output;
}