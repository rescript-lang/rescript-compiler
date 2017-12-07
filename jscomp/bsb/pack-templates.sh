#!/bin/sh

# This is the implementation logic for the scaffolding of a bsb template. See the README in this dir for overview.
# Usually, to naively copy a template somewhere else, you'd find the bsb/templates directory, then use `cp` to recursively copy the files onto the destination. However, this procedure isn't 100% reliable, cross-platform, fast, etc.
# In order to be more resilient to failures, we actually pack up every directory into a single ocaml file using [ocamlres](https://github.com/OCamlPro/ocp-ocamlres), through this script.
# At build time (not at install nor runtime!), ocamlres reads the `templates` directory and its content, and bundles it all up in a file called `bsb_templates.ml` (go check its content!)
# Then, when e.g. `bsb -init my-dir -theme basic-reason` is called, it calls `Bsb_theme_init.init_sample_project`, which goes through `bsb_templates.ml` and writes out the relevant boilerplate files.

set -e
git clean -dfx templates
ocp-ocamlres templates -o bsb_templates.ml
