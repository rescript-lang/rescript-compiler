.\"***********************************************************************
.\"*                                                                     *
.\"*                                OCaml                                *
.\"*                                                                     *
.\"*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *
.\"*                                                                     *
.\"*  Copyright 1996 Institut National de Recherche en Informatique et   *
.\"*  en Automatique.  All rights reserved.  This file is distributed    *
.\"*  under the terms of the Q Public License version 1.0.               *
.\"*                                                                     *
.\"***********************************************************************
.\"
.TH OCAMLOPT 1

.SH NAME

ocamlopt \- The OCaml native-code compiler

.SH SYNOPSIS

.B ocamlopt
[
.I options
]
.IR filename \ ...

.B ocamlopt.opt
(same options)

.SH DESCRIPTION

The OCaml high-performance
native-code compiler
.BR ocamlopt (1)
compiles OCaml source files to native code object files and link these
object files to produce standalone executables.

The
.BR ocamlopt (1)
command has a command-line interface very close to that
of
.BR ocamlc (1).
It accepts the same types of arguments and processes them
sequentially:

Arguments ending in .mli are taken to be source files for
compilation unit interfaces. Interfaces specify the names exported by
compilation units: they declare value names with their types, define
public data types, declare abstract data types, and so on. From the
file
.IR x .mli,
the
.BR ocamlopt (1)
compiler produces a compiled interface
in the file
.IR x .cmi.
The interface produced is identical to that
produced by the bytecode compiler
.BR ocamlc (1).

Arguments ending in .ml are taken to be source files for compilation
unit implementations. Implementations provide definitions for the
names exported by the unit, and also contain expressions to be
evaluated for their side-effects.  From the file
.IR x .ml,
the
.BR ocamlopt (1)
compiler produces two files:
.IR x .o,
containing native object code, and
.IR x .cmx,
containing extra information for linking and
optimization of the clients of the unit. The compiled implementation
should always be referred to under the name
.IR x .cmx
(when given a .o file,
.BR ocamlopt (1)
assumes that it contains code compiled from C, not from OCaml).

The implementation is checked against the interface file
.IR x .mli
(if it exists) as described in the manual for
.BR ocamlc (1).

Arguments ending in .cmx are taken to be compiled object code.  These
files are linked together, along with the object files obtained
by compiling .ml arguments (if any), and the OCaml standard
library, to produce a native-code executable program. The order in
which .cmx and .ml arguments are presented on the command line is
relevant: compilation units are initialized in that order at
run-time, and it is a link-time error to use a component of a unit
before having initialized it. Hence, a given
.IR x .cmx
file must come
before all .cmx files that refer to the unit
.IR x .

Arguments ending in .cmxa are taken to be libraries of object code.
Such a library packs in two files
.IR lib .cmxa
and
.IR lib .a
a set of object files (.cmx/.o files). Libraries are build with
.B ocamlopt \-a
(see the description of the
.B \-a
option below). The object
files contained in the library are linked as regular .cmx files (see
above), in the order specified when the library was built. The only
difference is that if an object file contained in a library is not
referenced anywhere in the program, then it is not linked in.

Arguments ending in .c are passed to the C compiler, which generates
a .o object file. This object file is linked with the program.

Arguments ending in .o or .a are assumed to be C object files and
libraries. They are linked with the program.

The output of the linking phase is a regular Unix executable file. It
does not need
.BR ocamlrun (1)
to run.

.B ocamlopt.opt
is the same compiler as
.BR ocamlopt ,
but compiled with itself instead of with the bytecode compiler
.BR ocamlc (1).
Thus, it behaves exactly like
.BR ocamlopt ,
but compiles faster.
.B ocamlopt.opt
is not available in all installations of OCaml.

.SH OPTIONS

The following command-line options are recognized by
.BR ocamlopt (1).
.TP
.B \-a
Build a library (.cmxa/.a file) with the object files (.cmx/.o
files) given on the command line, instead of linking them into an
executable file. The name of the library must be set with the
.B \-o
option.

If
.BR \-cclib \ or \ \-ccopt
options are passed on the command
line, these options are stored in the resulting .cmxa library.  Then,
linking with this library automatically adds back the
.BR \-cclib \ and \ \-ccopt
options as if they had been provided on the
command line, unless the
.B \-noautolink
option is given. Additionally, a substring
.B $CAMLORIGIN
inside a
.BR \ \-ccopt
options will be replaced by the full path to the .cma library,
excluding the filename.
.TP
.B \-absname
Show absolute filenames in error messages.
.TP
.B \-annot
Dump detailed information about the compilation (types, bindings,
tail-calls, etc).  The information for file
.IR src .ml
is put into file
.IR src .annot.
In case of a type error, dump all the information inferred by the
type-checker before the error. The
.IR src .annot
file can be used with the emacs commands given in
.B emacs/caml\-types.el
to display types and other annotations interactively.
.TP
.B \-bin\-annot
Dump detailed information about the compilation (types, bindings,
tail-calls, etc) in binary format. The information for file
.IR src .ml
is put into file
.IR src .cmt.
In case of a type error, dump
all the information inferred by the type-checker before the error.
The annotation files produced by
.B \-bin\-annot
contain more information
and are much more compact than the files produced by
.BR \-annot .
.TP
.B \-c
Compile only. Suppress the linking phase of the
compilation. Source code files are turned into compiled files, but no
executable file is produced. This option is useful to
compile modules separately.
.TP
.BI \-cc \ ccomp
Use
.I ccomp
as the C linker called to build the final executable and as the C
compiler for compiling .c source files.
.TP
.BI \-cclib\ \-l libname
Pass the
.BI \-l libname
option to the linker. This causes the given C library to be linked
with the program.
.TP
.BI \-ccopt \ option
Pass the given option to the C compiler and linker. For instance,
.BI \-ccopt\ \-L dir
causes the C linker to search for C libraries in
directory
.IR dir .
.TP
.B \-compact
Optimize the produced code for space rather than for time. This
results in smaller but slightly slower programs. The default is to
optimize for speed.
.TP
.B \-config
Print the version number of
.BR ocamlopt (1)
and a detailed summary of its configuration, then exit.
.TP
.BI \-for\-pack \ module\-path
Generate an object file (.cmx and .o files) that can later be included
as a sub-module (with the given access path) of a compilation unit
constructed with
.BR \-pack .
For instance,
.B ocamlopt\ \-for\-pack\ P\ \-c\ A.ml
will generate a.cmx and a.o files that can later be used with
.BR "ocamlopt -pack -o P.cmx a.cmx" .
.TP
.B \-g
Add debugging information while compiling and linking. This option is
required in order to produce stack backtraces when
the program terminates on an uncaught exception (see
.BR ocamlrun (1)).
.TP
.B \-i
Cause the compiler to print all defined names (with their inferred
types or their definitions) when compiling an implementation (.ml
file). No compiled files (.cmo and .cmi files) are produced.
This can be useful to check the types inferred by the
compiler. Also, since the output follows the syntax of interfaces, it
can help in writing an explicit interface (.mli file) for a file:
just redirect the standard output of the compiler to a .mli file,
and edit that file to remove all declarations of unexported names.
.TP
.BI \-I \ directory
Add the given directory to the list of directories searched for
compiled interface files (.cmi), compiled object code files (.cmx),
and libraries (.cmxa). By default, the current directory is searched
first, then the standard library directory. Directories added with \-I
are searched after the current directory, in the order in which they
were given on the command line, but before the standard library
directory. See also option
.BR \-nostdlib .

If the given directory starts with
.BR + ,
it is taken relative to the
standard library directory. For instance,
.B \-I\ +compiler-libs
adds the subdirectory
.B compiler-libs
of the standard library to the search path.
.TP
.BI \-impl \ filename
Compile the file
.I filename
as an implementation file, even if its extension is not .ml.
.TP
.BI \-inline \ n
Set aggressiveness of inlining to
.IR n ,
where
.I n
is a positive
integer. Specifying
.B \-inline 0
prevents all functions from being
inlined, except those whose body is smaller than the call site. Thus,
inlining causes no expansion in code size. The default aggressiveness,
.BR \-inline\ 1 ,
allows slightly larger functions to be inlined, resulting
in a slight expansion in code size. Higher values for the
.B \-inline
option cause larger and larger functions to become candidate for
inlining, but can result in a serious increase in code size.
.TP
.BI \-intf \ filename
Compile the file
.I filename
as an interface file, even if its extension is not .mli.
.TP
.BI \-intf\-suffix \ string
Recognize file names ending with
.I string
as interface files (instead of the default .mli).
.TP
.B \-keep-locs
Keep documentation strings in generated .cmi files.
.TP
.B \-keep-locs
Keep locations in generated .cmi files.
.TP
.B \-labels
Labels are not ignored in types, labels may be used in applications,
and labelled parameters can be given in any order.  This is the default.
.TP
.B \-linkall
Force all modules contained in libraries to be linked in. If this
flag is not given, unreferenced modules are not linked in. When
building a library
.RB ( \-a
flag), setting the
.B \-linkall
flag forces all
subsequent links of programs involving that library to link all the
modules contained in the library.
.TP
.B \-no-alias-deps
Do not record dependencies for module aliases.
.TP
.B \-no\-app\-funct
Deactivates the applicative behaviour of functors. With this option,
each functor application generates new types in its result and
applying the same functor twice to the same argument yields two
incompatible structures.
.TP
.B \-noassert
Do not compile assertion checks.  Note that the special form
.B assert\ false
is always compiled because it is typed specially.
This flag has no effect when linking already-compiled files.
.TP
.B \-noautolink
When linking .cmxa libraries, ignore
.BR \-cclib \ and \ \-ccopt
options potentially contained in the libraries (if these options were
given when building the libraries).  This can be useful if a library
contains incorrect specifications of C libraries or C options; in this
case, during linking, set
.B -noautolink
and pass the correct C libraries and options on the command line.
.TP
.B \-nodynlink
Allow the compiler to use some optimizations that are valid only for code
that is never dynlinked.
.TP
.B -nostdlib
Do not automatically add the standard library directory the list of
directories searched for compiled interface files (.cmi), compiled
object code files (.cmx), and libraries (.cmxa). See also option
.BR \-I .
.TP
.B \-nolabels
Ignore non-optional labels in types. Labels cannot be used in
applications, and parameter order becomes strict.
.TP
.BI \-o \ exec\-file
Specify the name of the output file produced by the linker. The
default output name is a.out, in keeping with the Unix tradition. If the
.B \-a
option is given, specify the name of the library produced. If the
.B \-pack
option is given, specify the name of the packed object file produced.
If the
.B \-output\-obj
option is given, specify the name of the output file produced. If the
.B \-shared
option is given, specify the name of plugin file produced.
This can also be used when compiling an interface or implementation
file, without linking, in which case it sets the name of the cmi or
cmo file, and also sets the module name to the file name up to the
first dot.
.TP
.BI \-open \ module
Opens the given module before processing the interface or
implementation files. If several
.B \-open
options are given, they are processed in order, just as if
the statements open! module1;; ... open! moduleN;; were added
at the top of each file.
.TP
.B \-output\-obj
Cause the linker to produce a C object file instead of an executable
file. This is useful to wrap OCaml code as a C library,
callable from any C program. The name of the output object file
must be set with the
.B \-o
option.
This option can also be used to produce a compiled shared/dynamic
library (.so extension).
.TP
.B \-p
Generate extra code to write profile information when the program is
executed.  The profile information can then be examined with the
analysis program
.BR gprof (1).
The
.B \-p
option must be given both at
compile-time and at link-time.  Linking object files not compiled with
.B \-p
is possible, but results in less precise profiling.

See the
.BR gprof (1)
man page for more information about the profiles.

Full support for
.BR gprof (1)
is only available for certain platforms
(currently: Intel x86/Linux and Alpha/Digital Unix).
On other platforms, the
.B \-p
option will result in a less precise
profile (no call graph information, only a time profile).
.TP
.B \-pack
Build an object file (.cmx and .o files) and its associated compiled
interface (.cmi) that combines the .cmx object
files given on the command line, making them appear as sub-modules of
the output .cmx file.  The name of the output .cmx file must be
given with the
.B \-o
option.  For instance,
.B ocamlopt\ -pack\ -o\ P.cmx\ A.cmx\ B.cmx\ C.cmx
generates compiled files P.cmx, P.o and P.cmi describing a
compilation unit having three sub-modules A, B and C,
corresponding to the contents of the object files A.cmx, B.cmx and
C.cmx.  These contents can be referenced as P.A, P.B and P.C
in the remainder of the program.

The .cmx object files being combined must have been compiled with
the appropriate
.B \-for\-pack
option.  In the example above,
A.cmx, B.cmx and C.cmx must have been compiled with
.BR ocamlopt\ \-for\-pack\ P .

Multiple levels of packing can be achieved by combining
.B \-pack
with
.BR \-for\-pack .
See
.IR "The OCaml user's manual" ,
chapter "Native-code compilation" for more details.
.TP
.BI \-pp \ command
Cause the compiler to call the given
.I command
as a preprocessor for each source file. The output of
.I command
is redirected to
an intermediate file, which is compiled. If there are no compilation
errors, the intermediate file is deleted afterwards.
.TP
.BI \-ppx \ command
After parsing, pipe the abstract syntax tree through the preprocessor
.IR command .
The module
.BR Ast_mapper (3)
implements the external interface of a preprocessor.
.TP
.B \-principal
Check information path during type-checking, to make sure that all
types are derived in a principal way. All programs accepted in
.B \-principal
mode are also accepted in default mode with equivalent
types, but different binary signatures.
.TP
.B \-rectypes
Allow arbitrary recursive types during type-checking.  By default,
only recursive types where the recursion goes through an object type
are supported. Note that once you have created an interface using this
flag, you must use it again for all dependencies.
.TP
.BI \-runtime\-variant \ suffix
Add
.I suffix
to the name of the runtime library that will be used by the program.
If OCaml was configured with option
.BR \-with\-debug\-runtime ,
then the
.B d
suffix is supported and gives a debug version of the runtime.
.TP
.B \-S
Keep the assembly code produced during the compilation. The assembly
code for the source file
.IR x .ml
is saved in the file
.IR x .s.
.TP
.B \-safe\-string
Enforce the separation between types
.BR string \ and\  bytes ,
thereby making strings read-only. This will become the default in
a future version of OCaml.
.TP
.B \-shared
Build a plugin (usually .cmxs) that can be dynamically loaded with
the
.B Dynlink
module. The name of the plugin must be
set with the
.B \-o
option. A plugin can include a number of OCaml
modules and libraries, and extra native objects (.o, .a files).
Building native plugins is only supported for some
operating system. Under some systems (currently,
only Linux AMD 64), all the OCaml code linked in a plugin must have
been compiled without the
.B \-nodynlink
flag. Some constraints might also
apply to the way the extra native objects have been compiled (under
Linux AMD 64, they must contain only position-independent code).
.TP
.B \-short\-paths
When a type is visible under several module-paths, use the shortest
one when printing the type's name in inferred interfaces and error and
warning messages.
.TP
.B \-strict\-sequence
The left-hand part of a sequence must have type unit.
.TP
.B \-thread
Compile or link multithreaded programs, in combination with the
system threads library described in
.IR "The OCaml user's manual" .
.TP
.B \-unsafe
Turn bound checking off for array and string accesses (the
.BR v.(i) and s.[i]
constructs). Programs compiled with
.B \-unsafe
are therefore
faster, but unsafe: anything can happen if the program accesses an
array or string outside of its bounds. Additionally, turn off the
check for zero divisor in integer division and modulus operations.
With
.BR \-unsafe ,
an integer division (or modulus) by zero can halt the
program or continue with an unspecified result instead of raising a
.B Division_by_zero
exception.
.TP
.B \-unsafe\-string
Identify the types
.BR string \ and\  bytes ,
thereby making strings writable. For reasons of backward compatibility,
this is the default setting for the moment, but this will change in a future
version of OCaml.
.TP
.B \-v
Print the version number of the compiler and the location of the
standard library directory, then exit.
.TP
.B \-verbose
Print all external commands before they are executed, in particular
invocations of the assembler, C compiler, and linker.
.TP
.BR \-version \ or\  \-vnum
Print the version number of the compiler in short form (e.g. "3.11.0"),
then exit.
.TP
.BI \-w \ warning\-list
Enable, disable, or mark as fatal the warnings specified by the argument
.IR warning\-list .
See
.BR ocamlc (1)
for the syntax of
.IR warning-list .
.TP
.BI \-warn\-error \ warning\-list
Mark as fatal the warnings specified in the argument
.IR warning\-list .
The compiler will stop with an error when one of these
warnings is emitted.  The
.I warning\-list
has the same meaning as for
the
.B \-w
option: a
.B +
sign (or an uppercase letter) marks the corresponding warnings as fatal, a
.B \-
sign (or a lowercase letter) turns them back into non-fatal warnings, and a
.B @
sign both enables and marks as fatal the corresponding warnings.

Note: it is not recommended to use the
.B \-warn\-error
option in production code, because it will almost certainly prevent
compiling your program with later versions of OCaml when they add new
warnings or modify existing warnings.

The default setting is
.B \-warn\-error \-a
(all warnings are non-fatal).
.TP
.B \-warn\-help
Show the description of all available warning numbers.
.TP
.B \-where
Print the location of the standard library, then exit.
.TP
.BI \- \ file
Process
.I file
as a file name, even if it starts with a dash (-) character.
.TP
.BR \-help \ or \ \-\-help
Display a short usage summary and exit.

.SH OPTIONS FOR THE IA32 ARCHITECTURE

The IA32 code generator (Intel Pentium, AMD Athlon) supports the
following additional option:
.TP
.B \-ffast\-math
Use the IA32 instructions to compute
trigonometric and exponential functions, instead of calling the
corresponding library routines.  The functions affected are:
.BR atan ,
.BR atan2 ,
.BR cos ,
.BR log ,
.BR log10 ,
.BR sin ,
.B sqrt
and
.BR tan .
The resulting code runs faster, but the range of supported arguments
and the precision of the result can be reduced.  In particular,
trigonometric operations
.BR cos ,
.BR sin ,
.B tan
have their range reduced to [\-2^64, 2^64].

.SH OPTIONS FOR THE AMD64 ARCHITECTURE

The AMD64 code generator (64-bit versions of Intel Pentium and AMD
Athlon) supports the following additional options:
.TP
.B \-fPIC
Generate position-independent machine code.  This is the default.
.TP
.B \-fno\-PIC
Generate position-dependent machine code.

.SH OPTIONS FOR THE SPARC ARCHITECTURE
The Sparc code generator supports the following additional options:
.TP
.B \-march=v8
Generate SPARC version 8 code.
.TP
.B \-march=v9
Generate SPARC version 9 code.
.P
The default is to generate code for SPARC version 7, which runs on all
SPARC processors.

.SH OPTIONS FOR THE ARM ARCHITECTURE
The ARM code generator supports the following additional options:
.TP
.B \-farch=armv4|armv5|armv5te|armv6|armv6t2|armv7
Select the ARM target architecture
.TP
.B \-ffpu=soft|vfpv2|vfpv3\-d16|vfpv3
Select the floating-point hardware
.TP
.B \-fPIC
Generate position-independent machine code.
.TP
.B \-fno\-PIC
Generate position-dependent machine code.  This is the default.
.TP
.B \-fthumb
Enable Thumb/Thumb-2 code generation
.TP
.B \-fno\-thumb
Disable Thumb/Thumb-2 code generation
.P
The default values for target architecture, floating-point hardware
and thumb usage were selected at configure-time when building
.B ocamlopt
itself. This configuration can be inspected using
.BR ocamlopt\ \-config .
Target architecture depends on the "model" setting, while
floating-point hardware and thumb support are determined from the ABI
setting in "system" (
.BR linux_eabi or linux_eabihf ).

.SH SEE ALSO
.BR ocamlc (1).
.br
.IR "The OCaml user's manual" ,
chapter "Native-code compilation".
