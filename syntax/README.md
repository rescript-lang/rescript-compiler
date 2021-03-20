# ReScript Syntax ![Tests](https://github.com/rescript-lang/syntax/workflows/CI/badge.svg)

Documentation: https://rescript-lang.org/docs/manual/latest/overview

This repo is the source of truth for the ReScript parser & printer. Issues go here.

**You don't need this repo to use the ReScript syntax**. This comes with ReScript >=8.1. This repo is for syntax developers.

## Contribute

### Why

A detailed discussion by Jonathan Blow and Casey Muratori on why you would hand-roll a parser for a production quality programming language
[Discussion: Making Programming Language Parsers, etc](https://youtu.be/MnctEW1oL-E)

"One reason why I switched off these parser tools is that the promises didn't really materialize.
The amount of work that I had to do change a yacc script from one language to a variant of that language
was more than if I hand wrote the code myself.
"
J. Blow.

### Setup & Usage (For Repo Devs Only)

Required:
- [NodeJS](https://nodejs.org/)
- Ocaml 4.06.1
- OS: Mac

```sh
opam switch create 4.06.1 && eval $(opam env)
git clone https://github.com/rescript-lang/syntax.git
cd syntax
npm install
make # or: make -j9 for faster build
```

This will produce the final binary `lib/rescript.exe` used for testing.

First build is super slow because we're also building our vendored `refmt` (only used for the conversion tool). Subsequent builds should be <2s. If not, please file an issue (build speed is a priority).

We only build production binary, even in dev mode. No need for a separate dev binary when the build is fast enough. Plus, this encourages proper benchmarking of the (production) binary each diff.

After you make a change:
```sh
make
```

Run the core tests:
```sh
make test
```

Run the extended tests:
```sh
make roundtrip-test
```

Update jest snapshots:
```sh
./node_modules/.bin/jest -u
```

Debug a file:
```sh
# write code in test.res
./lib/rescript.exe test.res # test printer
./lib/rescript.exe -print ast test.res # print ast
./lib/rescript.exe -print ml test.res # show ocaml code
./lib/rescript.exe -print res -width 80 test.res # test printer and change default print width
```

Benchmark:
```sh
make bench
```

Enable stack trace:
```sh
# Before you run the binary
export OCAMLRUNPARAM="b"
```

This is likely a known knowledge: add the above line into your shell rc file so that every shell startup you have OCaml stack trace enabled.

### Development Docs

`src/syntax` contains all the source code. Don't change folder structure without notice; ReScript uses this repo as a submodule and assumes `src/syntax`.

#### Error Reporting Logic

Right now, ReScript's compiler's error reporting mechanism, for architectural reasons, is independent from this syntax repo's error reporting mechanism. However, we do want a unified look when they report the errors in the terminal. This is currently achieved by (carefully...) duplicating the error report logic from the compiler repo to here (or vice-versa; either way, just keep them in sync). The files to sync are the compiler repo's [super_location.ml](https://github.com/rescript-lang/rescript-compiler/blob/fcb21790dfb0592f609818df7790192061360631/jscomp/super_errors/super_location.ml) and [super_code_frame.ml](https://github.com/rescript-lang/rescript-compiler/blob/fcb21790dfb0592f609818df7790192061360631/jscomp/super_errors/super_code_frame.ml), into this repo's [res_diagnostics_printing_utils.ml](https://github.com/rescript-lang/syntax/blob/ec5cefb23b659b0a7be170ae0ad26f3fe8a05456/src/res_diagnostics_printing_utils.ml). A few notes:

- Some lines are lightly changed to fit this repo's needs; they're documented in the latter file.
- Please keep these files lightweight and as dependency-less as possible, for easier syncing.
- The syntax logic currently doesn't have warnings, only errors, and potentially more than one.
- In the future, ideally, the error reporting logic would also be unified with GenType and Reanalyze's. It'd be painful to copy paste around all this reporting logic.
- The errors are reported by the parser [here](https://github.com/rescript-lang/syntax/blob/ec5cefb23b659b0a7be170ae0ad26f3fe8a05456/src/res_diagnostics.ml#L146).
- Our editor plugin parses the error report from the compiler and from the syntax [here](https://github.com/rescript-lang/rescript-vscode/blob/0dbf2eb9cdb0bd6d95be1aee88b73830feecb5cc/server/src/utils.ts#L129-L329).

### Example File Conversion

In a random project of yours:

```sh
node_modules/.bin/bsrefmt --print=binary myFile.re | your/path/to/rescript.exe -parse reasonBinary -print ns > myFile.res
node_modules/.bin/bsrefmt --print=binary --interface=true myFile.rei | your/path/to/rescript.exe -parse reasonBinary -print ns -interface > myFile.resi
mv myFile.re myFile.re.backup # random backup name. Could be anything
```

### Example API usage

```
module Parser = ResCore.Parser
module Diagnostics = ResCore.Diagnostics

let filename = "foo.res"
let src = FS.readFile filename

let p =
  (* intended for ocaml compiler *)
  let mode = Parser.ParseForTypeChecker in
  (* if you want to target the printer use: let mode = Parser.Default in*)
  Parser.make ~mode src filename

let structure = ResParser.parseImplementation p
let signature = ResParser.parseInterface p

let () = match p.Parser.diagnostics with
| [] -> () (* no problems *)
| diagnostics -> (* parser contains problems *)
  prerr_string (
    Diagnostics.stringOfReport
      ~style:Diagnostics.Pretty
      diagnostics src
  )
```
