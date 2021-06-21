pub enum Error {
	LL0Ambiguity(LL0Ambiguity),
}

/// LL0 ambiguity.
pub enum LL0Ambiguity {
	ShiftReduce,
	ReduceReduce,
}
