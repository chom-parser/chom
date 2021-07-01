mod error;
mod first_follow;
pub mod table;

pub use error::Error;
pub use first_follow::FirstAndFollowGraph;
pub use table::Table;

pub enum Algorithm {
	LR0,
	LALR1,
}
