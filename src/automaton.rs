struct Transitions<Q: Hash + Eq, Label> {
    table: HashMap<Label, HashSet<Q>>
}

struct Automaton<Q: Hash + Eq, Label> {
    start: Q,
    end: Q,
    delta: HashMap<Q, Transitions<Q>>
}
