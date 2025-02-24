# Fault Injection protection

TODO: description

## Build

### Dependencies

* llvm developer package 19.*
* cmake >=3.20
* rust toolchain with LLVM 19.* backend

### llvm plugin

```sh
mkdir build; cd build
cmake ..
cmake --build .
cd -
```

### rust macro

```sh
cd rust_attribute
cargo b
```
