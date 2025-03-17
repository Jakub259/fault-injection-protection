pub fn foo(x: &i32, y: &i32) -> bool {
    return x == y;
}

#[rust_firv2::harden_vars_fn(foo)]
pub fn test1() -> i32 {
    let x = 5;
    let y = 6;
    let z = x + y;
    println!("{}", z);
    1
}

#[rust_firv2::harden_vars_fn(i32::eq)]
pub fn test2<'a>(x: &'a i32) -> &'a i32 {
    x
}
