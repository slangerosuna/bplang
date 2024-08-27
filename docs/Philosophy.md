# Design Philosophy
bplang is a multi-paradigm, statically typed, compiled programming language designed with a focus on clarity, maintainability, and efficiency inspired by Rust, Zig, Jai, Haskell, WGSL, Metal, and D. It strives to strike a balance between programmer productivity and performance as well as between popular paradigms like oop, array programming, and functional programming.
## Core Principles
- Readability
	bplang's syntax is designed to be clear and concise with a focus on human-readable high-level abstractions whenever their impact on performance is negligible
- Tool-chain Standardization
	TODO
- Safety
	Like Rust, bplang has a borrow checker, making bplang type safe, memory safe, and thread safe. If the rules of the borrow checker are followed, the only possible causes of panics are the `panic` keyword, the `assert` keyword, and calling functions that panic, like `(?_)::unwrap` or `f64::divide`.
- Compile-time evaluation
    Just as in Jai, every function that can be evaluated at compile-time *is* evaluated at compile-time, regardless of whether or not it is preceded with a `const`. You may ask, then, why does the `const` keyword still exist in bplang, then? The answer is simple, it is there to *require* compile-time evaluation; removing the `const` keyword from a const function has no effect, but adding it to a function that cannot be evaluated at compile-time results in a compiler error.
- Performance
	Though high-level wherever possible within perfomance constraints, bplang, due to having only abstracting where performance hits are neglible, maintains single-threaded performance on-par with C/C++. On top of the equivalent single-threaded performance, thread safety guarantees also encourage the use of concurrency, a tool currently massively underutilized in games. Because it is designed primarily for game development, where performance is often deeply important, this is considered critical to bplang's vision.
* Design by Contract
	TODO
- Polymorphism
	bplang, in its attempt to be as polymorphic as possible implements many methods for polymorphism: ad hoc, parametric, row, and rank polymorphism along with macros
	For ad hoc polymorphism, parameter pattern matching
	There are two approaches to parametric/row polymorphism included in bplang: [[Templating Parameters]] and [[Trait Objects]].
    for rank polymorphism, you can both use [[Templating Parameters]] or the fact that the compiler will automatically implement an overload of `fn(I) -> O` in the form of `fn(impl Iter<I>) -> impl Iter<O>` as well as having first class arrays
	For details on how macros work in bplang, go to the file on [[Macros]]
## Features
* Closures / Anonymous Functions
* First-class and higher-order functions
* Implicit returns
* Algebraic types
* Templating
* Expression-oriented control flow
* Type safety
* Ranges
* Iterators
* Design by contract
	* side effects of functions are recorded by the compiler
	* preconditions and post-conditions can be declared with assertions
* [[Side Effect Negation]]
	* 
* Invariant enforcement
* Modules
* First-class arrays
* Array slicing
* Nested functions
* Limited type inference

## Planned Features
* Variadic function arguments
* Better type inference
* const parameter inference
* Lazy evaluation
* Trait Objects
* Actor model concurrency
* Async/Await
* Standard build system and package manager
	* All written in bplang, no MAKEFILE
* Borrow checker: gives warning rather than error
	* Thread safety
	* Memory safety
* Declarative, Procedural, and Variadic macros
* In-line assembly
	* Will likely only be supported for `x86` and `x64` targets, maybe eventually `arm64`
* Byte Code and JITC
	* Uses '\\n' as a common delimiter for interaction with git for version control of closed-source binaries
	* Used for iterative compilation
	* Used for linking closed-source libraries
* Compatibility with C/C++
* Full compile-time execution
* Memoization
* Shaders that can call all `shader_compatible` functions
	* Compiled to SPIR-V or translated to WGSL
