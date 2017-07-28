OVERVIEW:

OCaml is an implementation of the ML language, based on the Caml Light
dialect extended with a complete class-based object system and a
powerful module system in the style of Standard ML.

OCaml comprises two compilers. One generates bytecode which is then
interpreted by a C program. This compiler runs quickly, generates
compact code with moderate memory requirements, and is portable to
essentially any 32 or 64 bit Unix platform. Performance of generated
programs is quite good for a bytecoded implementation.  This compiler
can be used either as a standalone, batch-oriented compiler that
produces standalone programs, or as an interactive, toplevel-based
system.

The other compiler generates high-performance native code for a number
of processors. Compilation takes longer and generates bigger code, but
the generated programs deliver excellent performance, while retaining
the moderate memory requirements of the bytecode compiler. The
native-code compiler currently runs on the following platforms:

Tier 1 (actively used and maintained by the core OCaml team):

    AMD64 (Opteron)    Linux, MacOS X, MS Windows
    IA32 (Pentium)     Linux, FreeBSD, MacOS X, MS Windows
    PowerPC            Linux, MacOS X
    ARM                Linux

Tier 2 (maintained when possible, with help from users):

    AMD64              FreeBSD, OpenBSD
    IA32 (Pentium)     NetBSD, OpenBSD, Solaris 9
    PowerPC            NetBSD
    SPARC              Solaris, Linux, NetBSD

Other operating systems for the processors above have not been tested,
but the compiler may work under other operating systems with little work.

Before the introduction of objects, OCaml was known as Caml Special
Light. OCaml is almost upwards compatible with Caml Special Light,
except for a few additional reserved keywords that have forced some
renaming of standard library functions.

CONTENTS:

  Changes               what's new with each release
  INSTALL               instructions for installation
  LICENSE               license and copyright notice
  Makefile              main Makefile
  README                this file
  README.win32          infos on the MS Windows ports of OCaml
  asmcomp/              native-code compiler and linker
  asmrun/               native-code runtime library
  boot/                 bootstrap compiler
  bytecomp/             bytecode compiler and linker
  byterun/              bytecode interpreter and runtime system
  config/               autoconfiguration stuff
  debugger/             source-level replay debugger
  driver/               driver code for the compilers
  emacs/                OCaml editing mode and debugger interface for GNU Emacs
  lex/                  lexer generator
  maccaml/              the Macintosh GUI
  ocamldoc/             documentation generator
  otherlibs/            several external libraries
  parsing/              syntax analysis
  stdlib/               standard library
  tools/                various utilities
  toplevel/             interactive system
  typing/               typechecking
  utils/                utility libraries
  yacc/                 parser generator

COPYRIGHT:

All files marked "Copyright INRIA" in this distribution are copyright
1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
2007, 2008, 2009, 2010, 2011, 2012 Institut National de Recherche en
Informatique et en Automatique (INRIA) and distributed under the
conditions stated in file LICENSE.

INSTALLATION:

See the file INSTALL for installation instructions on Unix, Linux and
MacOS X machines.  For MS Windows, see README.win32.

DOCUMENTATION:

The OCaml manual is distributed in HTML, PDF, Postscript, DVI, and
Emacs Info files.  It is available on the World Wide Web, at

        http://caml.inria.fr/

AVAILABILITY:

The complete OCaml distribution can be accessed at

        http://caml.inria.fr/

KEEPING IN TOUCH WITH THE CAML COMMUNITY:

There exists a mailing list of users of the OCaml implementations
developed at INRIA. The purpose of this list is to share
experience, exchange ideas (and even code), and report on applications
of the OCaml language. Messages can be written in English or in
French. The list has more than 1000 subscribers.

Messages to the list should be sent to:

              caml-list@inria.fr

You can subscribe to this list via the Web interface at

        https://sympa-roc.inria.fr/wws/info/caml-list

Archives of the list are available on the Web site above.

The Usenet news groups comp.lang.ml and comp.lang.functional
also contains discussions about the ML family of programming languages,
including OCaml.

BUG REPORTS AND USER FEEDBACK:

Please report bugs using the Web interface to the bug-tracking system
at http://caml.inria.fr/bin/caml-bugs

To be effective, bug reports should include a complete program
(preferably small) that exhibits the unexpected behavior, and the
configuration you are using (machine type, etc).

You can also contact the implementors directly at caml@inria.fr.
