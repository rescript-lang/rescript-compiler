# Analysis Library and Binary

This subfolder builds a private command line binary used by the plugin to power a few functionalities such as jump to definition, hover and autocomplete.

The binary reads the `.cmt` and `.cmti` files and analyses them.

For installation & build instructions, see the main CONTRIBUTING.md.

## Overview

See main CONTRIBUTING.md's repo structure. Additionally, `examples/` is a convenience debugging repo. Check out `test.sh` (invoked through `make test`) to see the snapshots testing workflow stored in `tests/`.

## Usage

At root:
```sh
./rescript-editor-analysis.exe --help

# or

dune exec -- rescript-editor-analysis --help
```

## History

This project is based on a fork of [Reason Language Server](https://github.com/jaredly/reason-language-server).
