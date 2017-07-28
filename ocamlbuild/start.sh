#!/bin/sh

#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#       Nicolas Pouillard, projet Gallium, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2007 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################



set -e
set -x
rm -rf _start
mkdir _start
cp *.ml* _start
cd _start
cat >> ocamlbuild_Myocamlbuild_config.ml <<EOF
let bindir = "<start>";;
let libdir = bindir;;
let a = "a";;
let o = "o";;
let so = "so";;
let exe = "";;
EOF
ocamlc -c signatures.mli
ocamlc -c tags.mli
ocamlc -c ocamlbuild_Myocamlbuild_config.ml
ocamlc -c ocamlbuild_where.mli
ocamlc -c ocamlbuild_where.ml
ocamlc -c my_unix.mli
ocamlc -c my_std.mli
ocamlc -c display.mli
ocamlc -c shell.mli
ocamlc -c log.mli
ocamlc -c bool.mli
ocamlc -c glob_ast.mli
ocamlc -c glob_lexer.mli
ocamlc -c glob.mli
ocamlc -c lexers.mli
ocamlc -c slurp.mli
ocamlc -c pathname.mli
ocamlc -c discard_printf.mli
ocamlc -c command.mli
ocamlc -c digest_cache.mli
ocamlc -c resource.mli
ocamlc -c rule.mli
ocamlc -c hygiene.mli
ocamlc -c options.mli
ocamlc -c tools.mli
ocamlc -c exit_codes.mli
ocamlc -c main.mli
ocamlc -c ocaml_utils.mli
ocamlc -c ocaml_tools.mli
ocamlc -c ocaml_compiler.mli
ocamlc -c ocaml_dependencies.mli
ocamlc -c hooks.mli
ocamlc -c ocaml_specific.mli
ocamlc -c configuration.mli
ocamlc -c flags.mli
ocamlc -c ocaml_arch.mli
ocamlc -c solver.mli
ocamlc -c report.mli
ocamlc -c ocamlbuild_where.ml
ocamlc -c fda.mli
ocamlc -c fda.ml
ocamlc -c tools.ml
ocamlc -c plugin.mli
ocamlc -c plugin.ml
ocamlc -c ocaml_dependencies.ml
ocamlc -c exit_codes.ml
ocamllex lexers.mll
ocamlc -c lexers.ml
ocamlc -c param_tags.mli
ocamlc -c param_tags.ml
ocamlc -c main.ml
ocamlc -c findlib.mli
ocamlc -c findlib.ml
ocamlc -c ocaml_specific.ml
ocamlc -c display.ml
ocamlc -c command.ml
ocamlc -c -rectypes discard_printf.ml
ocamlc -c my_std.ml
ocamlc -c shell.ml
ocamlc -c my_unix.ml
ocamlc -c log.ml
ocamlc -c pathname.ml
ocamlc -c options.ml
ocamlc -c slurp.ml
ocamlc -c ocaml_utils.ml
ocamlc -c ocaml_tools.ml
ocamlc -c ocaml_compiler.ml
ocamlc -c hooks.ml
ocamllex glob_lexer.mll
ocamlc -c glob_lexer.ml
ocamlc -c bool.ml
ocamlc -c glob_ast.ml
ocamlc -c glob.ml
ocamlc -c tags.ml
ocamlc -c configuration.ml
ocamlc -c flags.ml
ocamlc -c hygiene.ml
ocamlc -c ocaml_arch.ml
ocamlc -c digest_cache.ml
ocamlc -c resource.ml
ocamlc -c rule.ml
ocamlc -c report.ml
ocamlc -c solver.ml
ocamlc -c ocamlbuildlight.mli
ocamlc -pack ocamlbuild_Myocamlbuild_config.cmo discard_printf.cmo my_std.cmo bool.cmo glob_ast.cmo glob_lexer.cmo glob.cmo lexers.cmo my_unix.cmo tags.cmo display.cmo log.cmo param_tags.cmo shell.cmo slurp.cmo ocamlbuild_where.cmo command.cmo options.cmo pathname.cmo digest_cache.cmo resource.cmo rule.cmo flags.cmo solver.cmo report.cmo ocaml_arch.cmo hygiene.cmo configuration.cmo tools.cmo fda.cmo plugin.cmo ocaml_utils.cmo ocaml_dependencies.cmo ocaml_compiler.cmo ocaml_tools.cmo hooks.cmo findlib.cmo ocaml_specific.cmo exit_codes.cmo main.cmo -o ocamlbuild_pack.cmo
ocamlc -c ocamlbuildlight.ml
ocamlc ocamlbuild_pack.cmo ocamlbuildlight.cmo -o ../ocamlbuild.byte.start
cd ..
rm -rf _start
echo ocamlbuild.byte.start: Sucessfully built.
