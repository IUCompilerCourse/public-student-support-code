# p423-public-code
Utility code, test suites, etc. for the compiler course.

This code will be described in the Appendix of the book.

The `runtime.c` file needs to be compiled and linked with the assembly
code that your compiler produces. To compile `runtime.c`, do the
following
```
   gcc -c -g -std=c99 runtime.c
```
This will produce a file named `runtime.o`. The -g flag is to tell the
compiler to produce debug information that you may need to use
the gdb (or lldb) debugger.

Next, suppose your compiler has translated the Racket program in file
`foo.rkt` into the x86 assembly program in file `foo.s` (The .s filename
extension is the standard one for assembly programs.) To produce
an executable program, you can then do
```
  gcc -g runtime.o foo.s
```
which will produce the executable program named a.out.

There is an example "compiler" in the file `compiler.rkt`.  That
file defines two passes that translate R_0 programs to R_0 programs
and tests them using the `interp-tests` function from `utilities.rkt`. It
tests the passes on the three example programs in the tests
subdirectory. You may find it amusing (I did!) to insert bugs in the
compiler and see the errors reported. Note that `interp-tests` does not
test the final output assembly code; you need to use `compiler-tests`
for that purpose. The usage of `compiler-tests` is quite similar to
`interp-tests`.

