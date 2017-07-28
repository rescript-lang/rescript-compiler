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
.TH OCAMLRUN 1

.SH NAME
ocamlrun \- The OCaml bytecode interpreter

.SH SYNOPSIS
.B ocamlrun
[
.I options
]
.I filename argument ...

.SH DESCRIPTION
The
.BR ocamlrun (1)
command executes bytecode files produced by the
linking phase of the
.BR ocamlc (1)
command.

The first non-option argument is taken to be the name of the file
containing the executable bytecode. (That file is searched in the
executable path as well as in the current directory.) The remaining
arguments are passed to the OCaml program, in the string array
.BR Sys.argv .
Element 0 of this array is the name of the
bytecode executable file; elements 1 to
.I n
are the remaining arguments.

In most cases, the bytecode
executable files produced by the
.BR ocamlc (1)
command are self-executable,
and manage to launch the
.BR ocamlrun (1)
command on themselves automatically.

.SH OPTIONS

The following command-line options are recognized by
.BR ocamlrun (1).
.TP
.B \-b
When the program aborts due to an uncaught exception, print a detailed
"back trace" of the execution, showing where the exception was
raised and which function calls were outstanding at this point.  The
back trace is printed only if the bytecode executable contains
debugging information, i.e. was compiled and linked with the
.B \-g
option to
.BR ocamlc (1)
set.  This option is equivalent to setting the
.B b
flag in the OCAMLRUNPARAM environment variable (see below).
.TP
.BI \-I \ dir
Search the directory
.I dir
for dynamically-loaded libraries, in addition to the standard search path.
.TP
.B \-p
Print the names of the primitives known to this version of
.BR ocamlrun (1)
and exit.
.TP
.B \-v
Direct the memory manager to print verbose messages on standard error.
This is equivalent to setting
.B v=63
in the OCAMLRUNPARAM environment variable (see below).
.TP
.B \-version
Print version string and exit.
.TP
.B \-vnum
Print short version number and exit.

.SH ENVIRONMENT VARIABLES

The following environment variable are also consulted:
.TP
.B CAML_LD_LIBRARY_PATH
Additional directories to search for dynamically-loaded libraries.
.TP
.B OCAMLLIB
The directory containing the OCaml standard
library.  (If
.B OCAMLLIB
is not set,
.B CAMLLIB
will be used instead.) Used to locate the ld.conf configuration file for
dynamic loading.  If not set,
default to the library directory specified when compiling OCaml.
.TP
.B OCAMLRUNPARAM
Set the runtime system options and garbage collection parameters.
(If OCAMLRUNPARAM is not set, CAMLRUNPARAM will be used instead.)
This variable must be a sequence of parameter specifications.
A parameter specification is an option letter followed by an =
sign, a decimal number (or a hexadecimal number prefixed by
.BR 0x ),
and an optional multiplier.  The options are documented below; the
last six correspond to the fields of the
.B control
record documented in
.IR "The OCaml user's manual",
chapter "Standard Library", section "Gc".
.TP
.B b
Trigger the printing of a stack backtrace
when an uncaught exception aborts the program.
This option takes no argument.
.TP
.B p
Turn on debugging support for
.BR ocamlyacc -generated
parsers.  When this option is on,
the pushdown automaton that executes the parsers prints a
trace of its actions.  This option takes no argument.
.TP
.BR R
Turn on randomization of all hash tables by default (see the
.B Hashtbl
module of the standard library). This option takes no
argument.
.TP
.BR h
The initial size of the major heap (in words).
.TP
.BR a \ (allocation_policy)
The policy used for allocating in the OCaml heap.  Possible values
are 0 for the next-fit policy, and 1 for the first-fit
policy.  Next-fit is usually faster, but first-fit is better for
avoiding fragmentation and the associated heap compactions.
.TP
.BR s \ (minor_heap_size)
The size of the minor heap (in words).
.TP
.BR i \ (major_heap_increment)
The default size increment for the major heap (in words).
.TP
.BR o \ (space_overhead)
The major GC speed setting.
.TP
.BR O \ (max_overhead)
The heap compaction trigger setting.
.TP
.BR l \ (stack_limit)
The limit (in words) of the stack size.
.TP
.BR v \ (verbose)
What GC messages to print to stderr.  This is a sum of values selected
from the following:

.B 0x001
Start of major GC cycle.

.B 0x002
Minor collection and major GC slice.

.B 0x004
Growing and shrinking of the heap.

.B 0x008
Resizing of stacks and memory manager tables.

.B 0x010
Heap compaction.

.BR 0x020
Change of GC parameters.

.BR 0x040
Computation of major GC slice size.

.BR 0x080
Calling of finalisation functions.

.BR 0x100
Startup messages (loading the bytecode executable file, resolving
shared libraries).

.BR 0x200
Computation of compaction-triggering condition.

The multiplier is
.BR k ,
.BR M ,\ or
.BR G ,
for multiplication by 2^10, 2^20, and 2^30 respectively.
For example, on a 32-bit machine under bash, the command
.B export OCAMLRUNPARAM='s=256k,v=1'
tells a subsequent
.B ocamlrun
to set its initial minor heap size to 1 megabyte and to print
a message at the start of each major GC cycle.
.TP
.B CAMLRUNPARAM
If OCAMLRUNPARAM is not found in the environment, then CAMLRUNPARAM
will be used instead.  If CAMLRUNPARAM is not found, then the default
values will be used.
.TP
.B PATH
List of directories searched to find the bytecode executable file.

.SH SEE ALSO
.BR ocamlc (1).
.br
.IR "The OCaml user's manual" ,
chapter "Runtime system".
