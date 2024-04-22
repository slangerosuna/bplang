Conceptually, these are the same as `comptime` parameters in Zig, where the inspiration comes from, than generics as are not just restricted to types. An example of this would be the following:
```bplang
map_slice :: (SIZE: usize, I: type, O: type) =>
	(input: [I; SIZE], f: fn(I) -> O) -> [O; SIZE] = {
		output: [O; SIZE];
		for i in 0..SIZE { output[i] = f(input[i]); }
		output
	}
```
where we take in the generics SIZE, I, and O, of which only I and O are types. By allowing for the generic SIZE to be an integer, we are able to create a function that cannot exist in languages like Rust without macros, allowing for easier to write, more human readable code.

these templating parameters are also not restricted to just functions, they can also be used for any other definition. Examples include:
Structs:
```bplang
SliceWrapper :: (SIZE: usize, I: type) => struct([I; SIZE])
```
Enums:
```bplang
Result :: (O: type, E: type) => enum { ok: O, err: E }
```
Non-tagged Unions:
```bplang
OptionalPtr :: (T: type) => union { *T, null }
```

like regular parameters, templating parameters can also be overloaded
```bplang
map_slice :: (SIZE: usize, I: type, O: type) =>
	(input: [I; SIZE], f: fn(I) -> O) -> [O; SIZE] = {
		output: [O; SIZE];
		for i in 0..SIZE { output[i] = f(input[i]); }
		output
	}

map_slice :: (I:type, O: type) =>
	(input: *[I], f: fn(I) -> O) -> *[O] = {
		output: *[O] = alloc sizeof(O) * input.len;
		for i in 0..input.len { output[i] = f(input[i]); }
		output
	},

```