## Imperative and declarative programming

## Object-oriented
Though it currently does not support inheritance, there are still many aspects of object-oriented programming in bplang, roughly in-line with what is supported within rust.
bplang has:
* traits
* methods
* trait objects
* virtual methods
* method overloading
On top of this, it is currently being considered whether or not to implement inheritance, most likely in the form of Java-style single-inheritance, but, like C++, defaulting to non-virtual methods 

## Functional Programming
One of the main paradigms of bplang is the functional paradigm. With the compiler recognizing all pure functions and giving them special privileges and abilities, like the use of the `-memoize` macro for easy optimization, like we see with the following factorial function
```bplang
fac :: (T: Integer) => (n: T) -> (*str)!T =
    if n >= 0 { fac_unchecked<T>(n) }
    else { !"cannot take factorial of negative number" }
fac_unchecked :: -memoize (T: Integer) => (n: T) -> T =
    match n { 0, 1 => 1, _ => n * fac_unchecked(n-1), }
```
On top of this, bplang also has
* First-class & higher-order functions
* Closures
* Algebraic data types
* Optionally immutable variables
## Procedural Programming
As with most languages, procedures exist in bplang as functions with no return type, rather than as their own dedicated syntax. But, with the ability to mutate state in procedures using pointers and global state, they do have use, unlike their lack of use in functional languages.

Because of this, programming in a procedural manner is trivial in bplang.
## Array Programming
