 # lang1

More than my first attempt at writing a programming language, but the first to
be generate code, though it cannot (yet) parse anything.

This was largely written to write down a/my mental model for compilers. Though also a
majority of the time spent so fars was encoding x86\_64 instructions from an embedded
DSL. The x86\_64 encoder is incomplete, only handling `ADD`-like instructions.

## Testing

### `arch/x86_64.rs`

There is a good amount of hardcoded tests that compare encoded instructions
to a slice of bytes. However this can a be a little error prone, so there is
also some "integration testing" that compares the instruction encoding here
with that of `objdump`.

Test cases also have an associated string name which is actually the intel
syntax version of the instruction. The name and encodings are written to files
`test_xxx.asm` and `test_xxx.bin` respectively. Then the output of
`objdump ... test_xxx.bin` can be `diffed` with that of `test_xxx.asm`


## Roadmap

* [x] - parser
	* [x] - basic types
	* [x] - tuples
	* [ ] - abstract data types
	* [x] - abstraction
	* [x] - application
	* [x] - let binding
	* [x] - patterns (missing ADTs)
	* [x] - types (missing ADTs)
	* [x] - multiple lets
	* [ ] - pattern lets
* [ ] - string interning
* [x] - interpreter
	* [x] - values
	* [x] - beta reduction (function calling)
	* [x] - pattern matching
	* [x] - let bindings
	* [ ] - multiple lets
	* [ ] - pattern lets
* [ ] - type check
	* [ ] - basic types
	* [ ] - patterns
	* [ ] - sizing
	* [ ] - match arm consitency
	* [ ] - match exhaustiveness
	* [ ] - polymorphism
	* [ ] - type inference
* [ ] - pointers?
	* [ ] - requires some level of polymorphism
* [ ] - optimizer
	* [ ] - transform local shadowing into mutability
	* [ ] - automatically demote args to references

* [x] - x86\_64 instruction encoding
	* [x] - add-like
	* [x] - ret
	* [ ] - call
	* [ ] - jmp
	* [ ] - syscall
* [ ] - codegen
	* [ ] - everything on the stack
	* [ ] - use regs
	* [ ] - basic types prelude
		* [ ] - non-builtin string
	* [ ] - debug info
* [ ] - more arches
	* [ ] - Z80/Gameboy
	* [ ] - RISC-V
	* [ ] - arm

