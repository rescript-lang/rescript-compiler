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
.TH OCAMLDEP 1

.SH NAME
ocamldep \- Dependency generator for OCaml

.SH SYNOPSIS
.B ocamldep
[
.I options
]
.I filename ...

.SH DESCRIPTION

The
.BR ocamldep (1)
command scans a set of OCaml source files
(.ml and .mli files) for references to external compilation units,
and outputs dependency lines in a format suitable for the
.BR make (1)
utility. This ensures that make will compile the source files in the
correct order, and recompile those files that need to when a source
file is modified.

The typical usage is:
.P
ocamldep
.I options
*.mli *.ml > .depend
.P
where .depend is the file that should contain the
dependencies.

Dependencies are generated both for compiling with the bytecode
compiler
.BR ocamlc (1)
and with the native-code compiler
.BR ocamlopt (1).

.SH OPTIONS

The following command-line options are recognized by
.BR ocamldep (1).
.TP
.B \-absname
Show absolute filenames in error messages.
.TP
.BI \-I \ directory
Add the given directory to the list of directories searched for
source files. If a source file foo.ml mentions an external
compilation unit Bar, a dependency on that unit's interface
bar.cmi is generated only if the source for bar is found in the
current directory or in one of the directories specified with
.BR \-I .
Otherwise, Bar is assumed to be a module from the standard library,
and no dependencies are generated. For programs that span multiple
directories, it is recommended to pass
.BR ocamldep (1)
the same
.B \-I
options that are passed to the compiler.
.TP
.BI \-ml\-synonym \ .ext
Consider the given extension (with leading dot) to be a synonym for .ml.
.TP
.BI \-mli\-synonym \ .ext
Consider the given extension (with leading dot) to be a synonym for .mli.
.TP
.B \-modules
Output raw dependencies of the form
.IR filename : \ Module1\ Module2 \ ... \ ModuleN
where
.IR Module1 ,\ ..., \ ModuleN
are the names of the compilation
units referenced within the file
.IR filename ,
but these names are not
resolved to source file names.  Such raw dependencies cannot be used
by
.BR make (1),
but can be post-processed by other tools such as
.BR Omake (1).
.TP
.BI \-native
Generate dependencies for a pure native-code program (no bytecode
version).  When an implementation file (.ml file) has no explicit
interface file (.mli file),
.BR ocamldep (1)
generates dependencies on the
bytecode compiled file (.cmo file) to reflect interface changes.
This can cause unnecessary bytecode recompilations for programs that
are compiled to native-code only.  The flag
.B \-native
causes dependencies on native compiled files (.cmx) to be generated instead
of on .cmo files.  (This flag makes no difference if all source files
have explicit .mli interface files.)
.TP
.BI \-pp \ command
Cause
.BR ocamldep (1)
to call the given
.I command
as a preprocessor for each source file.
.TP
.BI \-ppx \ command
Pipe abstract syntax tree through preprocessor
.IR command .
.TP
.B \-slash
Under Unix, this option does nothing.
.TP
.B \-version
Print version string and exit.
.TP
.B \-vnum
Print short version number and exit.
.TP
.BR \-help \ or \ \-\-help
Display a short usage summary and exit.

.SH SEE ALSO
.BR ocamlc (1),
.BR ocamlopt (1).
.br
.IR The\ OCaml\ user's\ manual ,
chapter "Dependency generator".
