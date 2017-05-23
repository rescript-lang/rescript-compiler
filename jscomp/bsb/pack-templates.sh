#!/bin/sh
set -e
git clean -dfx templates
ocp-ocamlres templates -o bsb_templates.ml