use arrayvec::ArrayVec;

#[derive(Debug, Clone)]
pub struct ArraySet<T, const N: usize> {
    data: ArrayVec<T, N>,
}
impl<T: PartialEq, const N: usize> ArraySet<T, N> {
    pub fn new() -> Self {
        ArraySet {
            data: ArrayVec::new(),
        }
    }

    pub fn insert(&mut self, value: T) -> bool {
        if self.data.len() < N && !self.data.contains(&value) {
            self.data.push(value);
            true
        } else {
            false
        }
    }

    pub fn contains(&self, value: &T) -> bool {
        self.data.contains(value)
    }

    pub fn remove(&mut self, value: &T) -> bool {
        if let Some(pos) = self.data.iter().position(|x| x == value) {
            self.data.remove(pos);
            true
        } else {
            false
        }
    }
}

impl<T: PartialEq, const N: usize> PartialEq for ArraySet<T, N> {
    fn eq(&self, other: &Self) -> bool {
        if self.data.len() != other.data.len() {
            return false;
        }

        self.data.iter().all(|item| other.contains(item))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_array_set() {
        let mut set = ArraySet::<i32, 5>::new();
        assert!(set.insert(1));
        assert!(set.insert(2));
        assert!(!set.insert(1)); // Duplicate

        assert_eq!(set.data.len(), 2);
        assert!(set.contains(&1));
        assert!(!set.contains(&3));
        assert!(set.remove(&1));
        assert!(!set.contains(&1));
        assert_eq!(set.data.len(), 1);
    }

    #[test]
    fn test_array_set_eq() {
        let mut set1 = ArraySet::<i32, 5>::new();
        let mut set2 = ArraySet::<i32, 5>::new();

        assert!(set1.insert(1));
        assert!(set1.insert(2));

        assert!(set2.insert(2));
        assert!(set2.insert(1));

        assert_eq!(set1, set2);

        set2.remove(&2);

        assert_ne!(set1, set2);
    }
}
