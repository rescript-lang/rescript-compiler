# ReScript Syntax ![Tests](https://github.com/BuckleScript/syntax/workflows/CI/badge.svg)

Blog post: https://reasonml.org/blog/bucklescript-8-1-new-syntax

Documentation: https://reasonml.org/docs/reason-compiler/latest/new-bucklescript-syntax

This repo is the source of truth for the ReScript parser & printer. Issues go here.

**You don't need this repo to use the ReScript syntax**. This comes with BuckleScript 8.1. This repo is for syntax contributors.

## Contribute

### Why

A detailed discussion by Jonathan Blow and Casey Muratori on why you would hand-roll a parser for a production quality programming language
[Discussion: Making Programming Language Parsers, etc](https://youtu.be/MnctEW1oL-E)

"One reason why I switched off these parser tools is that the promises didn't really materialize.
The amount of work that I had to do change a yacc script from one language to a variant of that language
was more than if I hand wrote the code myself.
"
J. Blow.

### Setup & Usage

Currently for devs only.

Required:
- [NodeJS](https://nodejs.org/)
- Ocaml 4.06.1
- OS: Mac

```sh
opam switch create 4.06.1 && eval $(opam env)
git clone https://github.com/bucklescript/syntax.git
cd syntax
npm install
make # or: make -j9 for faster build
```

This will produce the final binary `lib/rescript.exe` used for testing

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
# write code in test.js
./lib/rescript.exe test.js # test printer
./lib/rescript.exe -print ast test.js # print ast
./lib/rescript.exe -print ml test.js # show ocaml code
./lib/rescript.exe -print res -width 80 test.js # test printer and change default print width
```

Benchmark:
```sh
make bench
```

### Development Docs

`src/syntax` contains all the source code. Don't change folder structure without notice; BuckleScript uses this repo as a submodule and assumes `src/syntax`.

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
