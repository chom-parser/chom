use crate::CharSet;

pub type Range = crate::charset::Range;

pub trait Union: Sized {
    fn union(&self, other: &Self) -> Self;
}

struct LinkedRange<T> {
    range: Range,
    value: T,
    tail: Option<Box<LinkedRange<T>>>
}

impl<T> LinkedRange<T> {
    pub fn new(range: Range, value: T, tail: Option<Box<LinkedRange<T>>>) -> LinkedRange<T> {
        LinkedRange {
            range: range,
            value: value,
            tail: tail
        }
    }

    pub fn tail(&self) -> Option<&LinkedRange<T>> {
        match &self.tail {
            Some(tail) => Some(&*tail),
            None => None
        }
    }
}

impl<T: Clone + Union> LinkedRange<T> {
    fn add_before(&mut self, range: Range, value: T) {
        let mut new_tail = LinkedRange::new(self.range.clone(), self.value.clone(), None);
        std::mem::swap(&mut new_tail.tail, &mut self.tail);
        self.range = range;
        self.tail = Some(Box::new(new_tail));
    }

    fn add_after(&mut self, range: Range, value: T) {
        match &mut self.tail {
            Some(tail) => tail.set(range, value),
            None => {
                self.tail = Some(Box::new(LinkedRange::new(range, value, None)))
            }
        }
    }

    pub fn set(&mut self, range: Range, value: T) {
        if !range.is_empty() {
            let (before, left_before, inter, left_after, after) = self.range.inter_split(&range);

            // "after" comes before "before" because `add_before` changes `self` radicaly.
            if let Some(range) = after {
                self.add_after(range, value.clone());
            } else if let Some(range) = left_after {
                self.add_after(range, self.value.clone());
            }

            if let Some(range) = inter {
                self.range = range;
                self.value = self.value.union(&value);
            }

            // "before" comes after "after" because `add_before` changes `self` radicaly.
            if let Some(range) = left_before {
                self.add_before(range, self.value.clone());
            } else if let Some(range) = before {
                self.add_before(range, value.clone());
            }
        }
    }
}

pub struct CharMap<T> {
    head: Option<LinkedRange<T>>
}

impl<T: Clone + PartialEq + Union> CharMap<T> {
    fn set_range(&mut self, range: Range, value: T) {
        match &mut self.head {
            Some(head) => head.set(range, value),
            None => {
                self.head = Some(LinkedRange::new(range, value, None))
            }
        }
    }

    fn set(&mut self, key: CharSet, value: T) {
        for range in key.iter() {
            self.set_range(range, value.clone())
        }
    }
}
