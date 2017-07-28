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
.TH "OCAMLCP" 1

.SH NAME
ocamlcp, ocamloptp \- The OCaml profiling compilers

.SH SYNOPSIS
.B ocamlcp
[
.I ocamlc options
]
[
.BI \-P \ flags
]
.I filename ...

.B ocamloptp
[
.I ocamlopt options
]
[
.BI \-P \ flags
]
.I filename ...

.SH DESCRIPTION
The
.B ocamlcp
and
.B ocamloptp
commands are front-ends to
.BR ocamlc (1)
and
.BR ocamlopt (1)
that instrument the source code, adding code to record how many times
functions are called, branches of conditionals are taken, etc.
Execution of instrumented code produces an execution profile in the
file ocamlprof.dump, which can be read using
.BR ocamlprof (1).

.B ocamlcp
accepts the same arguments and options as
.BR ocamlc (1)
and
.B ocamloptp
accepts the same arguments and options as
.BR ocamlopt (1).
There is only one exception: in both cases, the
.B \-pp
option is not supported.  If you need to preprocess your source files,
you will have to do it separately before calling
.B ocamlcp
or
.BR ocamloptp .

.SH OPTIONS

In addition to the
.BR ocamlc (1)
or
.BR ocamlopt (1)
options,
.B ocamlcp
and
.B ocamloptp
accept one option to control the kind of profiling information, the
.BI \-P \ letters
option. The
.I letters
indicate which parts of the program should be profiled:
.TP
.B a
all options
.TP
.B f
function calls : a count point is set at the beginning of each function body
.TP
.B i
.BR if \ ... \ then \ ... \ else :
count points are set in both
.BR then \ and \ else
branches
.TP
.B l
.BR while , \ for
loops: a count point is set at the beginning of the loop body
.TP
.B m
.B match
branches: a count point is set at the beginning of the
body of each branch of a pattern-matching
.TP
.B t
.BR try \ ... \ with
branches: a count point is set at the beginning of the body of each
branch of an exception catcher

.PP
For instance, compiling with
.B ocamlcp \-P film
profiles function calls,
.BR if \ ... \ then \ ... \ else \ ...,
loops, and pattern matching.

Calling
.BR ocamlcp (1)
or
.BR ocamloptp (1)
without the
.B \-P
option defaults to
.BR \-P\ fm ,
meaning that only function calls and pattern matching are profiled.

Note: for compatibility with previous versions,
.BR ocamlcp (1)
also accepts the option
.B \-p
with the same argument and meaning as
.BR \-P .

.SH SEE ALSO
.BR ocamlc (1),
.BR ocamlopt (1),
.BR ocamlprof (1).
.br
.IR "The OCaml user's manual" ,
chapter "Profiling".
