
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

./bin/bsc: _build/ext/ext.cmxa _build/common/common.cmxa _build/syntax/syntax.cmxa _build/core.cmxa
	echo "Linking"
	$(NATIVE) -g -linkall -o $@ -I +compiler-libs ocamlcommon.cmxa  $^  2>>build.compile
# Note: we can remove main.cmx since we have js_main.cmx which use [js_implementation.implementation],
# The motivation is to save compilation time and in the future, more freedom to add new
# compilation flags (since we don't need bytecode anymore)
# Another solution to save compilation time is have a flag like '-i' to stop the compiler
# continuing, in this case the user may occationally want bytecode/nativecode backend

release:snapshot 
	$(MAKE) releasebuild
# Note: this target also built bin/bsc which is used in release mode

# TODO: should be done after we do the build
# Carefully check the stored data if it is platform independent

./bin/ocaml_pack: ./bin/ocaml_pack.mli ./bin/ocaml_pack.ml 
	$(NATIVE) -w -a -I bin -I +compiler-libs ocamlcommon.cmxa unix.cmxa  ./bin/ocaml_pack.mli ./bin/ocaml_pack.ml -o $@

snapshot: ./bin/ocaml_pack snapshotcmj
	$(MAKE) snapshotml

snapshotml:./bin/ocaml_pack ./bin/compiler.mllib
	@echo "Snapshot ml"
	$< bin/compiler.mllib > bin/compiler.ml
snapshotcmj:
	@echo "Collecting cmj files"
	ocamlbuild -cflags $(OCAMLBUILD_CFLAGS)	 js_pack.native --


releasebuild:
	@echo "Make release compiler"
	$(NATIVE) -g -inline 1000 -linkall  -w -a -I +compiler-libs -I bin ocamlcommon.cmxa  bin/compiler.mli bin/compiler.ml -o bin/bsc

stdlib:
	cd stdlib && ./build.sh




# Example:
# MODULE_FLAGS='-bs-module amdjs' make world
# MODULE_FLAGS='-bs-module commonjs' make world
# MODULE_FLAGS='-bs-module goog:buckle' make world

world:
	@echo "Making compiler"		
	$(NATIVE) -g -inline 1000 -linkall  -w -a -I +compiler-libs -I bin ocamlcommon.cmxa  bin/compiler.mli bin/compiler.ml -o bin/bsc
	@echo "Making compiler finished"

	@echo "Making stdlib cmis"
	cd stdlib && $(MAKE) allcmis
	@echo "Making stdlib finished"

	@echo "Making runtime"
	cd runtime && $(MAKE) all
	@echo "Making runtime finished"


	@echo "Making stdlib"
	cd stdlib && $(MAKE) all
	@echo "Making stdlib finished"

world-test:
	$(MAKE) world
	@echo "Making test"
	cd test && $(MAKE) all
	@echo "Making test finsihed"

# no depend on ./bin/ocaml_pack ./bin/bsc
# since in npm mode, they are generated from a single file
install:
	cp  ./bin/bsc ../bin/
	cp ./runtime/*.cmt* ../lib/ocaml/
	cp ./runtime/*.cmj* ../lib/ocaml/
	cp ./runtime/js.cmi  ../lib/ocaml/	
	cp ./stdlib/*.cm* ../lib/ocaml/

TMP_OCAMLLIB=$(shell ocamlopt.opt -where)


big-world:bin/big_compiler.ml bin/big_compiler.mli
	@echo "Making compiler"		
	ocamlopt.opt -g -inline 1000 -linkall  -w -a  -I bin  bin/big_compiler.mli bin/big_compiler.ml -o bin/bsc
	@echo "Making compiler finished"		

	@echo "Making runtime"
	cd runtime && OCAMLLIB=$(TMP_OCAMLLIB) $(MAKE) all
	@echo "Making runtime finished"

	@echo "Making stdlib"
	cd stdlib && $(MAKE) all
	@echo "Making stdlib finished"


travis-world-test:./bin/ocaml_pack
	@echo "Generating the compiler"
	rm -f bin/compiler.ml 	
	./bin/ocaml_pack ./bin/compiler.mllib > bin/compiler.ml
	@echo "Generating the compiler finished"	
	$(MAKE) world-test


.PHONY: stdlib
.PHONY: test  quicktest release snapshot snapshotcmj






