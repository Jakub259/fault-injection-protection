use cfip_example::ArraySet;
use std::collections::{BTreeSet, HashSet, LinkedList};
use std::env;

fn main() {
    println!("Hello, world! {}", test0());
    println!("Hello, world! {}", test1());
    println!("HashSet {:?}", test2());
    println!("LinkedList! {:?}", test3());
    println!("BTreeSet! {:?}", test4());
    println!("ArraySet! {:?}", test5());
}

#[cfip::harden_vars_fn(var)]
fn test0() -> i32 {
    let var : i32 = 2;
    var
}

#[cfip::harden_vars_fn(var)]
fn test1() -> i32 {
    let var : std::env::Args = env::args();
    var.skip(1).map(|arg| arg.parse::<i32>().unwrap()).sum()
}

#[cfip::harden_vars_fn(var)]
fn test2() -> HashSet<i32> {
    let var : std::env::Args = env::args();
    var.skip(1).map(|arg| arg.parse::<i32>().unwrap()).collect()
}

#[cfip::harden_vars_fn(var)]
fn test3() -> LinkedList<i32> {
    let var : std::env::Args = env::args();
    var.skip(1).map(|arg| arg.parse::<i32>().unwrap()).collect()
}

#[cfip::harden_vars_fn(var)]
fn test4() -> BTreeSet<i32> {
    let var: std::env::Args = env::args();
    var.skip(1).map(|arg| arg.parse::<i32>().unwrap()).collect()
}

#[cfip::harden_vars_fn(var)]
fn test5() -> ArraySet<i32, 5> {
    let var: std::env::Args = env::args();
    let mut array_set = ArraySet::<i32, 5>::new();
    var.skip(1).take(5).for_each(|arg| {
        let value = arg.parse::<i32>().unwrap();
        array_set.insert(value);
    });
    array_set
}
