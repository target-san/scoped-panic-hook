# Convenient panic catcher and scoped panic hooks infrastructure

This crate provides convenient API for obtaining panic info after unwinding

```rust
    let result = catch_panic(|| {
        panic!("Something bad happened!");
    });

    if let Err(panic) = result {
        eprintln!("{}", panic.display_with_backtrace());
    }
```

This will either produce normal result of closure or or conveniently gathered panic info,
which includes panic location, message, raw payload and backtrace

## Useful modules

* `hook` - raw scoped hook APIs
* `panic` - all types and functions relevant to capturing panics, including some advanced features

# Development

Project uses [`cargo-xtask` pattern](https://github.com/matklad/cargo-xtask).
Run `cargo xtask` to see available commands

# TODO

Test no-unwind cases
