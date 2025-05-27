use std::env;
use std::collections::{HashSet, LinkedList, BTreeSet};
use firv2_example::ArraySet;

fn main() {
    println!("Hello, world! {}", test0());
    println!("Hello, world! {}", test1());
    println!("HashSet {:?}", test2());
    println!("LinkedList! {:?}", test3());
    println!("BTreeSet! {:?}", test4());
    println!("ArraySet! {:?}", test5());
}

#[rust_firv2::harden_fn(i32::eq)]
#[unsafe(no_mangle)]
fn test0() -> i32 {
    2
}

#[rust_firv2::harden_fn(i32::eq)]
#[unsafe(no_mangle)]
fn test1() -> i32 {
    env::args()
        .skip(1)
        .map(|arg| arg.parse::<i32>().unwrap())
        .sum()
}


#[rust_firv2::harden_fn(HashSet::eq)]
#[unsafe(no_mangle)]
fn test2() -> HashSet<i32> {
    env::args()
        .skip(1)
        .map(|arg| arg.parse::<i32>().unwrap())
        .collect()
}
#[rust_firv2::harden_fn(LinkedList::eq)]
#[unsafe(no_mangle)]
fn test3() -> LinkedList<i32> {
    env::args()
        .skip(1)
        .map(|arg| arg.parse::<i32>().unwrap())
        .collect()
}

#[rust_firv2::harden_fn(BTreeSet::eq)]
#[unsafe(no_mangle)]
fn test4() -> BTreeSet<i32> {
    env::args()
        .skip(1)
        .map(|arg| arg.parse::<i32>().unwrap())
        .collect()
}

#[rust_firv2::harden_fn(ArraySet::eq)]
#[unsafe(no_mangle)]
fn test5() -> ArraySet<i32, 5> {
    let mut array_set = ArraySet::<i32, 5>::new();
    env::args()
        .skip(1)
        .take(5)
        .for_each(|arg| {
            let value = arg.parse::<i32>().unwrap();
            array_set.insert(value);
        });
    array_set
}
