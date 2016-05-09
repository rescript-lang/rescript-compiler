
NATIVE=ocamlopt.opt
BYTE=ocamlc.opt

CAMLP4OF=camlp4of

js_map.ml:js_map.mlp j.ml
	@echo "Regenrating j_map.ml"
	$(CAMLP4OF) -filter map -filter trash -impl $< -printer o > $@

js_fold.ml:js_fold.mlp j.ml
	@echo "Regenrating j_fold.ml"
	$(CAMLP4OF) -filter fold  -filter trash -impl $< -printer o > $@

lam_map.ml: lambda_map.mlp lambda.mlp 
	@echo "Regenrating lambda_map.ml"
	$(CAMLP4OF) -filter map -filter trash -impl $< -printer o > $@

lam_fold.ml: lambda_fold.mlp lambda.mlp 
	@echo "Regenrating lambda_map.ml"
	$(CAMLP4OF) -filter map -filter trash -impl $< -printer o > $@

./bin/bsc: _build/compiler.cmxa
	echo "Linking"
	$(NATIVE) -g -linkall -o $@ -I +compiler-libs ocamlcommon.cmxa  $^  2>>build.compile
# Note: we can remove main.cmx since we have js_main.cmx which use [js_implementation.implementation],
# The motivation is to save compilation time and in the future, more freedom to add new
# compilation flags (since we don't need bytecode anymore)
# Another solution to save compilation time is have a flag like '-i' to stop the compiler
# continuing, in this case the user may occationally want bytecode/nativecode backend

release:snapshot 
	make releasebuild
# Note: this target also built bin/bsc which is used in release mode

# TODO: should be done after we do the build
# Carefully check the stored data if it is platform independent

./bin/ocaml_pack: ./bin/ocaml_pack.mli ./bin/ocaml_pack.ml 
	ocamlopt.opt -w -a -I bin -I +compiler-libs ocamlcommon.cmxa $^ -o $@

snapshot: ./bin/ocaml_pack snapshotcmj
	@echo "Snapshot"
	$< compiler.mllib > bin/compiler.ml

snapshotcmj:
	@echo "Collecting cmj files"
	ocamlbuild -cflags $(OCAMLBUILD_CFLAGS)	 js_pack.native --


releasebuild:
	@echo "Make release compiler"
	$(NATIVE) -g -inline 100 -linkall  -w -a -I +compiler-libs -I bin ocamlcommon.cmxa  bin/compiler.mli bin/compiler.ml -o bin/bsc

stdlib:
	cd stdlib && ./build.sh




# Example:
# MODULE_FLAGS='-js-module amdjs' make world
# MODULE_FLAGS='-js-module commonjs' make world
# MODULE_FLAGS='-js-module goog:buckle' make world

world:
	@echo "Making compiler"		
	ocamlopt.opt -g -inline 100 -linkall  -w -a -I +compiler-libs -I bin ocamlcommon.cmxa  bin/compiler.mli bin/compiler.ml -o bin/bsc
	@echo "Making compiler finished"		

	@echo "Making runtime"
	cd runtime && make all
	@echo "Making runtime finished"

	@echo "Making stdlib"
	cd lib && make all
	@echo "Making stdlib finished"

	@echo "Making lib"
	cd stdlib && make all	
	@echo "Making lib finished"

TMP_OCAMLLIB=$(shell ocamlopt.opt -where)

big-world:bin/big_compiler.ml bin/big_compiler.mli
	@echo "Making compiler"		
	ocamlopt.opt -g -inline 100 -linkall  -w -a  -I bin  bin/big_compiler.mli bin/big_compiler.ml -o bin/bsc
	@echo "Making compiler finished"		

	@echo "Making runtime"
	cd runtime && OCAMLLIB=$(TMP_OCAMLLIB) make all
	@echo "Making runtime finished"

	@echo "Making stdlib"
	cd lib && make all
	@echo "Making stdlib finished"

	@echo "Making lib"
	cd stdlib && make all	
	@echo "Making lib finished"


world-test:
	@echo "Making compiler"		
	ocamlopt.opt -g -inline 100 -w -a -linkall  -I +compiler-libs -I bin ocamlcommon.cmxa  bin/compiler.mli bin/compiler.ml -o bin/bsc
	@echo "Making compiler finished"		

	@echo "Making runtime"
	cd runtime && make all
	@echo "Making runtime finished"

	@echo "Making stdlib"
	cd lib && make all
	@echo "Making stdlib finished"

	@echo "Making lib"
	cd stdlib && make all	
	@echo "Making lib finished"

	@echo "Making test"
	cd test && make all
	@echo "Making test finsihed"

travis-world-test:./bin/ocaml_pack
	@echo "Generating the compiler"
	rm -f bin/compiler.ml 	
	./bin/ocaml_pack ./compiler.mllib > bin/compiler.ml
	@echo "Generating the compiler finished"	
	make world-test

# no depend on ./bin/ocaml_pack ./bin/bsc
# since in npm mode, they are generated from a single file
install:
	cp ./bin/ocaml_pack ./bin/bsc ../bin
	cp ./runtime/*.cmt* ../lib/ocaml
	cp ./stdlib/*.cm* ../lib/ocaml

.PHONY: stdlib
.PHONY: test  quicktest release snapshot snapshotcmj






