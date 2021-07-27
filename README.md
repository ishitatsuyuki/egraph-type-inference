# Type inference for Simply-Typed Lambda Calculus

---

This is a type inference engine for simply-typed lambda calculus. It uses a modified version of e-graph to solve the type equations in a convenient and efficient way. See `src/infer.rs` for details.

## Usage

Install [Rust](https://www.rust-lang.org/) on your machine, and run `cargo run` to launch the interactive type checker. Input lambda expressions like `lambda x: T.expr` and a type-annotated version will be printed out.

## License

Apache v2 or MIT License, at your option