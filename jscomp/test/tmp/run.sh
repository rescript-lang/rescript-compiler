#!/bin/sh
set -e
ocaml gen.ml
bscc  -w -49 -no-alias-deps -c -impl Tmp.mlast 
bscc  -w -24 -open Tmp -o Tmp-Hey.cmi -c hey.ml 
bscc  -w -24 -open Tmp -o Tmp-Hi.cmi -c hi.ml 

# can we enforce all stdlib comes from
# Std.List? (seems no)


# can we create module alias for sub dirs
# module SubDir = Tmp-SubDir 

# Tmp-SubDir.mlast 

# module N1 = SubDir-N1

# vs

# module N1 = Tmp-SubDir-N1
