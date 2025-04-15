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

Crate also exposes raw APIs for scoped panic hooks, see `hook` module
