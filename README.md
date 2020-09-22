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


## TODO

* Clean up test artifacts.
    - a new `test_artifact_dir` rust function for use in tests that returns
        `$(git rev-parse --show-toplevel)/test_artifacts` or something (and add
        test_artifacts to .gitignore)
    - some functions for naming the test files
* How to do polyfills, for example a 64 bit add in a 32 bit architecture?
    - As a transformation pass before Arch?
        * this will be complicated the Arch needs to have and interface
            to communicate its op limits
    - In Arch
        * this is easier but may not be general enough
* generating object files
    - mach-O
    - ELF
* A dang parser!
    - the language parsed would be very simple with only the capabilities
        of C. And slowly expanded, largely through sugaring, to have some more modern
        features like interfaces and closures.
    - parameterized types, type classes, type inference would be awesome but I don't
        think it would be in scope of this project, not before more architectures are
        supported.
* Next architectures/backends (in order)
    - Z80/Gameboy
    - RISC-V
    - arm
    - others:
        * brainfuck
        * C
        * llvm ir
* debug information in the object file
* A virtual machine for the langugage
* Userspace simulators from the architecture DSLs?
    - like [rv8](https://web.archive.org/web/20191216112852/https://rv8.io/),
        it emulates the instruction set and forwards syscalls to the host
        operating system.
