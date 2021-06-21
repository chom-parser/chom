use super::table::{Item, Rule, Symbol};
use crate::mono::{ty, Grammar, Type};
use std::collections::{BTreeSet, HashMap, HashSet};

fn type_locations(grammar: &Grammar) -> ty::Map<Vec<Item>> {
	let mut locations = ty::Map::new(grammar, |_| Vec::new());

	for (f_index, f) in grammar.enumerate_functions() {
		for (offset, symbol) in f.arguments().iter().enumerate() {
			match symbol {
				ty::Expr::Type(ty) => locations.get_mut(*ty).unwrap().push(Item {
					rule: Rule::Function(f_index),
					offset: offset as u32,
				}),
				_ => (),
			}
		}
	}

	locations
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Vertex {
	Terminal(u32),
	First((u32, ty::Instance)),
	Follow((u32, ty::Instance)),
	EndOfStream,
}

impl Vertex {
	// pub fn is_terminal(&self) -> bool {
	// 	match self {
	// 		Self::Terminal(_) => true,
	// 		_ => false
	// 	}
	// }

	pub fn is_symbol(&self) -> bool {
		match self {
			Self::Terminal(_) => true,
			Self::EndOfStream => true,
			_ => false,
		}
	}

	// pub fn as_terminal(&self) -> Option<u32> {
	// 	match self {
	// 		Self::Terminal(t) => Some(*t),
	// 		_ => None
	// 	}
	// }

	pub fn as_symbol(&self) -> Option<Option<u32>> {
		match self {
			Self::Terminal(t) => Some(Some(*t)),
			Self::EndOfStream => Some(None),
			_ => None,
		}
	}
}

fn compute_first_set_dependencies(grammar: &Grammar, ty: &Type) -> HashSet<Vertex> {
	let mut set = HashSet::new();

	for f_index in ty.constructors() {
		let f = grammar.function(f_index).unwrap();

		let info = match f.argument(0) {
			Some(symbol) => match symbol {
				ty::Expr::Terminal(t) => Vertex::Terminal(t),
				ty::Expr::Type(nt) => Vertex::First(nt),
			},
			None => Vertex::Follow(f.return_ty()),
		};

		set.insert(info);
	}

	set
}

fn compute_first_sets_dependencies(grammar: &Grammar) -> ty::Map<HashSet<Vertex>> {
	ty::Map::new(grammar, |(_, ty)| {
		compute_first_set_dependencies(grammar, ty)
	})
}

fn compute_follow_set_dependencies(
	grammar: &Grammar,
	locations: &ty::Map<Vec<Item>>,
	ty_index: (u32, ty::Instance),
) -> HashSet<Vertex> {
	let mut set = HashSet::new();

	for item in locations.get(ty_index).unwrap() {
		// let f = grammar.function(*function).unwrap();
		let info = match item.rule.symbol(grammar, item.offset + 1) {
			Some(symbol) => match symbol {
				Symbol::Expr(ty::Expr::Terminal(t)) => Vertex::Terminal(t),
				Symbol::Expr(ty::Expr::Type(nt)) => Vertex::First(nt),
				Symbol::EndOfStream => Vertex::EndOfStream,
			},
			None => Vertex::Follow(item.rule.return_ty(grammar)),
		};

		set.insert(info);
	}

	set
}

fn compute_follow_sets_dependencies(grammar: &Grammar) -> ty::Map<HashSet<Vertex>> {
	let locations = type_locations(grammar);
	ty::Map::new(grammar, |(i, _)| {
		compute_follow_set_dependencies(grammar, &locations, i)
	})
}

struct DependencyGraph {
	firsts: ty::Map<HashSet<Vertex>>,
	follows: ty::Map<HashSet<Vertex>>,
}

impl DependencyGraph {
	fn new(grammar: &Grammar) -> Self {
		Self {
			firsts: compute_first_sets_dependencies(grammar),
			follows: compute_follow_sets_dependencies(grammar),
		}
	}

	/// Returns an iterator over the vertices of the graph, except the terminals.
	fn vertices(&self) -> impl '_ + Iterator<Item = Vertex> {
		self.firsts
			.enumerate()
			.map(|(i, _)| Vertex::First(i))
			.chain(self.follows.enumerate().map(|(i, _)| Vertex::Follow(i)))
	}

	fn successors(&self, v: Vertex) -> impl '_ + Iterator<Item = Vertex> {
		match v {
			Vertex::First(i) => Some(self.firsts.get(i).unwrap().iter().cloned())
				.into_iter()
				.flatten(),
			Vertex::Follow(i) => Some(self.follows.get(i).unwrap().iter().cloned())
				.into_iter()
				.flatten(),
			_ => None.into_iter().flatten(),
		}
	}
}

struct Component {
	vertices: Vec<Vertex>,
	terminals: BTreeSet<Option<u32>>,
}

pub struct FirstAndFollowGraph {
	map: ty::Map<(usize, usize)>,
	components: Vec<Component>,
}

impl FirstAndFollowGraph {
	pub fn new(grammar: &Grammar) -> Self {
		// Solve dependencies using Tarjan's SCC algorithm.
		struct Data {
			index: u32,
			lowlink: u32,
			on_stack: bool,
		}

		fn strong_connect(
			deps: &DependencyGraph,
			v: Vertex,
			stack: &mut Vec<Vertex>,
			map: &mut HashMap<Vertex, Data>,
			graph: &mut FirstAndFollowGraph,
		) -> u32 {
			let index = map.len() as u32;
			stack.push(v);
			map.insert(
				v,
				Data {
					index,
					lowlink: index,
					on_stack: true,
				},
			);

			// Consider successors of v
			for w in deps.successors(v) {
				let new_v_lowlink = match map.get(&w) {
					None => {
						// Successor w has not yet been visited; recurse on it
						let w_lowlink = strong_connect(deps, w, stack, map, graph);
						Some(std::cmp::min(map[&v].lowlink, w_lowlink))
					}
					Some(w_data) => {
						if w_data.on_stack {
							// Successor w is in stack S and hence in the current SCC
							// If w is not on stack, then (v, w) is an edge pointing to an SCC already found and must be ignored
							// Note: The next line may look odd - but is correct.
							// It says w.index not w.lowlink; that is deliberate and from the original paper
							Some(std::cmp::min(map[&v].lowlink, w_data.index))
						} else {
							None
						}
					}
				};

				if let Some(new_v_lowlink) = new_v_lowlink {
					map.get_mut(&v).unwrap().lowlink = new_v_lowlink;
				}
			}

			let lowlink = map[&v].lowlink;

			// If v is a root node, pop the stack and generate an SCC
			if lowlink == map[&v].index {
				// Start a new strongly connected component
				let mut component = Vec::new();

				loop {
					let w = stack.pop().unwrap();
					map.get_mut(&w).unwrap().on_stack = false;

					// Add w to current strongly connected component
					component.push(w);

					if w == v {
						break;
					}
				}

				// Output the current strongly connected component
				graph.insert(component)
			}

			lowlink
		}

		let deps = DependencyGraph::new(grammar);
		let mut map: HashMap<Vertex, Data> = HashMap::new();
		let mut stack = Vec::new();
		let mut graph = Self::empty(&deps);

		for v in deps.vertices() {
			strong_connect(&deps, v, &mut stack, &mut map, &mut graph);
		}

		graph.compute_terminals(&deps);
		graph
	}

	fn empty(deps: &DependencyGraph) -> Self {
		let mut map = deps.firsts.map(|_| (0, 0));

		Self {
			map,
			components: Vec::new(),
		}
	}

	fn insert(&mut self, component: Vec<Vertex>) {
		let c = self.components.len();

		for v in &component {
			match v {
				Vertex::First(i) => self.map.get_mut(*i).unwrap().0 = c,
				Vertex::Follow(i) => self.map.get_mut(*i).unwrap().1 = c,
				_ => (),
			}
		}

		self.components.push(Component {
			vertices: component,
			terminals: BTreeSet::new(),
		})
	}

	fn first_component(&self, i: (u32, ty::Instance)) -> usize {
		self.map.get(i).unwrap().0
	}

	fn follow_component(&self, i: (u32, ty::Instance)) -> usize {
		self.map.get(i).unwrap().1
	}

	fn compute_terminals_of_component(&mut self, deps: &DependencyGraph, c: usize) {
		if self.components[c].terminals.is_empty() {
			let set = if self.components[c].vertices.iter().all(|v| v.is_symbol()) {
				self.components[c]
					.vertices
					.iter()
					.map(|v| v.as_symbol().unwrap())
					.collect()
			} else {
				let mut set = BTreeSet::new();

				let dependencies: HashSet<_> = self.components[c]
					.vertices
					.iter()
					.map(|v| deps.successors(*v))
					.flatten()
					.map(|dep| match dep {
						Vertex::First(i) => Some(self.first_component(i)),
						Vertex::Follow(i) => Some(self.follow_component(i)),
						_ => None,
					})
					.collect();

				for d in dependencies {
					if let Some(d) = d {
						self.compute_terminals_of_component(deps, d)
					}
				}

				for dep in self.components[c]
					.vertices
					.iter()
					.map(|v| deps.successors(*v))
					.flatten()
				{
					match dep {
						Vertex::Terminal(t) => {
							set.insert(Some(t));
						}
						Vertex::First(i) => {
							let d = self.first_component(i);
							set.extend(self.components[d].terminals.iter().cloned())
						}
						Vertex::Follow(i) => {
							let d = self.follow_component(i);
							set.extend(self.components[d].terminals.iter().cloned())
						}
						Vertex::EndOfStream => {
							set.insert(None);
						}
					}
				}

				set
			};

			self.components[c].terminals = set
		}
	}

	fn compute_terminals(&mut self, deps: &DependencyGraph) {
		for c in 0..self.components.len() {
			self.compute_terminals_of_component(deps, c)
		}
	}

	pub fn first(&self, ty: (u32, ty::Instance)) -> &BTreeSet<Option<u32>> {
		let c = self.first_component(ty);
		&self.components[c].terminals
	}

	pub fn follow(&self, ty: (u32, ty::Instance)) -> &BTreeSet<Option<u32>> {
		let c = self.follow_component(ty);
		&self.components[c].terminals
	}
}
