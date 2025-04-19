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

## Integration tests note

They're located in a separate unpublished subcrate. This is because most of them need to execute
certain test case binary as a separate process and then analyze its output

# Nightly features

Crate doesn't have separate feature `nightly`. Instead, it uses toolchain detection to automatically determine
whether to use nightly features or not.

While API remains the same, some internal behaviors are changed
* Global hook used to support scoped hooks is installed atomically,
  using `update_hook` instead of `take_hook`+`set_hook`
* No-unwind panics are handled more correctly. In particular,
  if no-unwind panic happens as the first one, you'll see panic details anyway.
* Backtrace print style used inside hook in no-unwind case is taken from `std::panic::get_backtrace_style`
  and is not hardcoded to short

These tweaks will be hopefully moved to stable version when related features are stabilized
