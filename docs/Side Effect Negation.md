Though it won't always make a function pure, the negate_side_effects macros in the std library will reverse all changes made to state within a function

examples:
`negate_side_effects!`
```bplang
static state: u32 = 0;
foo :: () -> u32 {
	state++;
	state
}

main :: () {
	let i = negate_side_effects!foo();
	# this expands to:
	# ```bp
	# let i = {
    #     a := state;
    #     b := foo();
    #     state = a;
    #     b
	# }
	# ```
	
	# since foo returned 1, you still get 1 as the answer
	assert i == 1;
	# the side effect of mutating state was reversed, keeping state constant
	assert state == 0;
}
```

`-negate_side_effects`
```bplang
static state: u32 = 0;
foo :: -negate_side_effects () -> u32 {
	state++;
	state
}
# 
# this expands to:
# ```
# foo :: () -> u32 {
#     a := state;
#     defer state = a;
#     {
#         state++;
#         state
#     }
# }
# ```

main :: () {
	i := foo();
	# since foo returned 1, you still get 1 as the answer
	assert i == 1;
	# the side effect of mutating state was reversed, keeping state constant
	assert state == 0;
}
```