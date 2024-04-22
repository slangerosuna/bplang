## Imperative and declarative programming

## Object-oriented

## Functional Programming
```bplang
fac :: (T: Integer) => (n: T) -> (*str)!T =
    if n >= 0 { fac_unchecked(n) }
    else { !"cannot take factorial of negative number" }
fac_unchecked :: -memoize (T: Integer) => (n: T) -> T =
    match n { 0, 1 => 1, _ => n * fac_unchecked(n-1), }
```
## Procedural Programming
