OCAML DOCUMENTATION
===================

Prerequisites
-------------

- Any prerequisites required to build OCaml from sources.

- The Unix editor 'ed', no longer installed by default on some systems.

- A LaTeX installation.

- The HeVeA LaTeX-to-HTML convertor (available in OPAM):
  <http://hevea.inria.fr/>

Note that you must make sure `hevea.sty` is installed into TeX properly. Your
package manager may not do this for you. Run `kpsewhich hevea.sty` to check.


Building
--------

0. Install the OCaml distribution.

1. Run `make` in the manual.

NB: If you already set `LD_LIBRARY_PATH` (OS X: `DYLD_LIBRARY_PATH`)
 in your environment don't forget to add
 `otherlibs/unix:otherlibs/str` to it in an absolute way.

Outputs
-------

In the manual:

- The HTML Manual is in directory `htmlman`. The main file is `index.html`.

- The plain text manual is in directory `textman` as file `manual.txt`.

- The Info manual is in directory `infoman`.

- The DVI manual is in directory `texstuff` as file `manual.dvi`.

- The PDF manual is in directory `texstuff` as file `pdfmanual.pdf`.

Source files
------------
The manual is written in an extended dialect of latex and is split in many
source files. During the build process, the sources files are converted into
classical latex file using the tools available in `tools`. These files are
then converted to the different output formats using either latex or hevea.

Each part of the manual corresponds to a specific directory, and each distinct
chapters (or sometimes sections) are mapped to a distinct `.etex` file:

- Part I, Introduction to OCaml: `tutorials`
  - The core language: `coreexamples.etex`
  - The module system: `moduleexamples.etex`
  - Objects in OCaml: `objectexamples.etex`
  - Labels and variants: `lablexamples.etex`
  - Advanced examples with classes and modules: `advexamples.etex`

- Part II, The OCaml language: `refman`
  This part is separated in two very distinct  chapters; the
  `OCaml language` chapter and the `Language extensions` chapter.

  - The OCaml language: `refman.etex`
    This chapter consists in a technical description of the OCaml language.
    Each section of this chapter is mapped to a separated latex file:
     - `lex.etex`, `values.etex`, `names.etex`, `types.etex`, `const.etex`,
     `patterns.etex`, `expr.etex`, `typedecl.etex`, `classes.etex`,
     `modtypes.etex`, `compunit.etex`

  - Language extensions: `exten.etex`
  This chapter contains a description of all recent features of the OCaml
  language.

- Part III, The OCaml tools: 'cmds'
    - Batch compilation (ocamlc): `comp.etex`
    - The toplevel system (ocaml): `top.etex`
    - The runtime system (ocamlrun): `runtime.etex`
    - Native-code compilation (ocamlopt): `native.etex`
    - Lexer and parser generators (ocamllex, ocamlyacc): `lexyacc.etex`
    - Dependency generator (ocamldep): `ocamldep.etex`
    - The browser/editor (ocamlbrowser): `browser.etex`
    - The documentation generator (ocamldoc): `ocamldoc.etex`
    - The debugger (ocamldebug): `debugger.etex`
    - Profiling (ocamlprof): `profil.etex`
    - The ocamlbuild compilation manager: `ocamlbuild.etex`
    - Interfacing C with OCaml: `intf-c.etex`
    - Optimisation with Flambda: `flambda.etex`
    - Memory profiling with Spacetime: `spacetime.etex`
    - Fuzzing with afl-fuzz: `afl-fuzz.etex`

Note that ocamlc,ocamlopt and the toplevel options overlap a lot.
Consequently, these options are described together in the file
`unified-options.etex` and then included from `comp.etex`, `native.etex`,
and `top.etex`. If you need to update this list of options, the top comment
of `unified-options.etex` contains the relevant information.

- Part IV, The OCaml library: 'libref'
 This parts contains an brief presentation of all libraries bundled with the
 compilers and the api documentation generated for these libraries.
    - The core library: `core.etex`
    - The standard library: `stdlib.etex`
    - The compiler front-end: `compilerlibs.etex`
    - The unix library: Unix system calls: `libunix.etex`
    - The legacy num library: this library has been removed from the core
      distribution, see `libnum.etex`
    - The str library: regular expressions and string processing: `libstr.etex`
    - The threads library: `libthreads.etex`
    - The graphics library: `libgraph.etex`
    - The dynlink library: dynamic loading and linking of object files:
      `libdynlink.etex`
    - The bigarray library: `libbigarray.etex`

Latex extensions
----------------

###Caml environments
The tool `tool/caml-tex2` is used to generate the latex code for the examples
in the introduction and language extension parts of the manual. It implements
two pseudo-environments: `caml_example` and `caml_eval`.

The pseudo-environment `caml_example` evaluates its contents using an ocaml
interpreter and then translates both the input code and the interpreter output
to latex code, e.g.
```latex
\begin{caml_example}{toplevel}
let f x = x;;
\end{caml_example}
```
Note that the toplevel output can be suppressed by using a `*` suffix:
```latex
\begin{caml_example*}{verbatim}
let f x = x
\end{caml_example*}
```

The `{verbatim}` or `{toplevel}` argument of the environment corresponds
to the the mode of the example, two modes are available `toplevel` and
`verbatim`.
The `toplevel` mode mimics the appearance and behavior of the toplevel.
In particular, toplevel examples must end with a double semi-colon `;;`,
otherwise an error would be raised.
The `verbatim` does not require a final `;;` and is intended to be
a lighter mode for code examples.

By default, `caml_tex2` raises an error and stops if the output of one
the `caml_example` environment contains an unexpected error or warning.
If such an error or warning is, in fact, expected, it is necessary to
indicate the expected output status to `caml_tex2` by adding either
an option to the `caml_example` environment:
```latex
\begin{caml_example}{toplevel}[error]
1 + 2. ;;
\end{caml_example}
 or for warning
\begin{caml_example}[warning=8]
let f None = None;;
\end{caml_example}
```
or an annotation to the concerned phrase:

```latex
\begin{caml_example}{toplevel}
1 + 2. [@@expect error] ;;
let f None = None [@@expect warning 8];;
3 + 4 [@@expect ok];;
\end{caml_example}
```

The `caml_eval` environment is a companion environment to `caml_example`
and can be used to evaluate OCaml expressions in the toplevel without
printing anything:
```latex
\begin{caml_eval}
let pi = 4. *. atan 1.;;
\end{caml_eval}
\begin{caml_example}{toplevel}
let f x = x +. pi;;
\end{caml_example}
```
Beware that the detection code for these pseudo-environments is quite brittle
and the environments must start and end at the beginning of the line.

###Quoting
The tool `tools/texquote2` provides support for verbatim-like quotes using
`\"` delimiters. More precisely, outside of caml environments and verbatim
environments, `texquote2` translates double quotes `"text"` to
`\machine{escaped_text}`.

###BNF grammar notation
The tool `tools/transf` provides support for BNF grammar notations and special
quotes for non-terminal. When transf is used, the environment `syntax` can
be used to describe grammars using BNF notation:
```latex
\begin{syntax}
expr:
    value-path
  | constant
  | '(' expr ')'
  | 'begin' expr 'end'
  | '(' expr ':' typexpr ')'
  | expr {{',' expr}}
  | constr expr
  | "`"tag-name expr
  | expr '::' expr
  | '[' expr { ';' expr } [';'] ']'
  | '[|' expr { ';' expr } [';'] '|]'
  | '{' field [':' typexpr] '=' expr%
    { ';' field [':' typexpr] '=' expr } [';'] '}'
\end{syntax}
```
Notice that terminal symbols are quoted using `'` delimiters.
Moreover, outside of the syntax environment, `@`-quotes can be used
to introduce fragment of grammar: `@'(' module-expr ')'@`. As a consequence,
when this extension is used `@` characters must be escaped as `\@`.
This extension is used mainly in the language reference part of the manual.
and a more complete description of the notation used is available in the
first subsection of `refman/refman.etex`.
