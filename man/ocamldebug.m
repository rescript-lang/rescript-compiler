.\"***********************************************************************
.\"*                                                                     *
.\"*                                OCaml                                *
.\"*                                                                     *
.\"*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *
.\"*                                                                     *
.\"*  Copyright 2001 Institut National de Recherche en Informatique et   *
.\"*  en Automatique.  All rights reserved.  This file is distributed    *
.\"*  under the terms of the Q Public License version 1.0.               *
.\"*                                                                     *
.\"***********************************************************************
.\"
.TH OCAMLDEBUG 1

.SH NAME
ocamldebug \- the OCaml source-level replay debugger.
.SH SYNOPSIS
.B ocamldebug
.RI [\  options \ ]\  program \ [\  arguments \ ]
.SH DESCRIPTION
.B ocamldebug
is the OCaml source-level replay debugger.

Before the debugger can be used, the program must be compiled and
linked with the
.B \-g
option: all .cmo and .cma files that are part
of the program should have been created with
.BR ocamlc\ \-g ,
and they must be linked together with
.BR ocamlc\ \-g .

Compiling with
.B \-g
entails no penalty on the running time of
programs: object files and bytecode executable files are bigger and
take longer to produce, but the executable files run at
exactly the same speed as if they had been compiled without
.BR \-g .

.SH OPTIONS
A summary of options are included below.
For a complete description, see the html documentation in the ocaml-doc
package.
.TP
.BI \-c \ count
Set the maximum number of simultaneously live checkpoints to
.IR count .
.TP
.BI \-cd \ dir
Run the debugger program from the working directory
.IR dir ,
instead of the current working directory. (See also the
.B cd
command.)
.TP
.B \-emacs
Tell the debugger it is executed under Emacs.  (See
.I "The OCaml user's manual"
for information on how to run the debugger under Emacs.)
Implies
.BR \-machine-readable .
.TP
.BI \-I \ directory
Add
.I directory
to the list of directories searched for source files and
compiled files.  (See also the
.B directory
command.)
.TP
.BI -machine-readable
Print information in a format more suitable for machines instead of human
operators where applicable. For example, when describing a location in a
program, such as when printing a backtrace, print the program counter and
character offset in a file instead of the filename, line number, and character
offset in that line.
.TP
.BI \-s \ socket
Use
.I socket
for communicating with the debugged program. See the description
of the command
.B set\ socket
in
.I "The OCaml user's manual"
for the format of
.IR socket .
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
.BR ocamlc (1)
.br
.IR "The OCaml user's manual" ,
chapter "The debugger".
.SH AUTHOR
This manual page was written by Sven LUTHER <luther@debian.org>,
for the Debian GNU/Linux system (but may be used by others).
