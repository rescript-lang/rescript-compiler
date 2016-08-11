
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

./bin/bsc: _build/ext/ext.cmxa _build/common/common.cmxa _build/depends/depends.cmxa _build/syntax/syntax.cmxa _build/core.cmxa
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

./bin/ocamlpack: ./bin/ocamlpack.mli ./bin/ocamlpack.ml 
	$(NATIVE) -w -a -I bin -I +compiler-libs ocamlcommon.cmxa unix.cmxa  $^ -o $@

## order matters
_build/ocamlpack: _build/ext/ext.cmxa _build/common/common.cmxa  _build/depends/depends.cmxa  _build/depends/ocaml_pack_main.cmx
	$(NATIVE) -I +compiler-libs ocamlcommon.cmxa unix.cmxa $^ -o $@

snapshot: ./bin/ocamlpack snapshotcmj
	$(MAKE) snapshotml

snapshotml:./bin/ocamlpack ./bin/compiler.mllib
	@echo "Snapshot ml"
	$< bin/compiler.mllib > bin/compiler.ml
	$< bin/bs_ppx.mllib   > bin/bs_ppx.ml
snapshotcmj:
	@echo "Collecting cmj files"
	ocamlbuild -cflags $(OCAMLBUILD_CFLAGS)	 -lflags -I,+compiler-libs,ocamlcommon.cmxa js_pack.native --

bootocamlpack: ./bin/ocamlpack
	@echo "Bootstrap ocamlpack"
	./bin/ocamlpack bin/ocamlpack.mllib > ./bin/ocamlpack.ml
	$(NATIVE) -w -40 -I +compiler-libs  unix.cmxa ocamlcommon.cmxa -I bin bin/ocamlpack.mli bin/ocamlpack.ml -o bin/ocamlpack
	@echo "Using the bootstrapped ocamlpack to genreate new ocamlpack.ml: `date`" 	
	./bin/ocamlpack bin/ocamlpack.mllib > ./bin/ocamlpack.ml
	@echo "Bootstrap seems finished, please check diffs in ocamlpack"
	git diff bin/ocamlpack.ml

releasebuild:
	@echo "Make release compiler"
	$(NATIVE) -g -inline 1000 -linkall  -w -a -I +compiler-libs -I bin ocamlcommon.cmxa  bin/compiler.mli bin/compiler.ml -o bin/bsc

stdlib:
	cd stdlib && ./build.sh

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

	@echo "Making others"
	cd others && $(MAKE) all
	@echo "Making others finished"

world-test:
	$(MAKE) world
	@echo "Making test"
	cd test && $(MAKE) all
	@echo "Making test finsihed"

travis-world-test:./bin/ocamlpack
	@echo "Generating the compiler"
	rm -f bin/compiler.ml 	
	./bin/ocamlpack ./bin/compiler.mllib > bin/compiler.ml
	@echo "Generating the compiler finished"	
	$(MAKE) world-test

# no depend on ./bin/ocamlpack ./bin/bsc
# since in npm mode, they are generated from a single file
install:
	cp  ./bin/bsc ../bin/
	cp ./runtime/*.cmt* ../lib/ocaml/
	cp ./runtime/*.cmj* ../lib/ocaml/
	cp ./runtime/js.cmi ./runtime/js_array.cmi ./runtime/js_string.cmi ./runtime/js_re.cmi ./runtime/js_unsafe.cmi ../lib/ocaml/	
	cp ./stdlib/*.cm* ../lib/ocaml/
	cp ./others/*.cm* ../lib/ocaml/
TMP_OCAMLLIB=$(shell ocamlopt.opt -where)


# big-world:bin/big_compiler.ml bin/big_compiler.mli
# 	@echo "Making compiler"		
# 	ocamlopt.opt -g -inline 1000 -linkall  -w -a  -I bin  bin/big_compiler.mli bin/big_compiler.ml -o bin/bsc
# 	@echo "Making compiler finished"		

# 	@echo "Making runtime"
# 	cd runtime && OCAMLLIB=$(TMP_OCAMLLIB) $(MAKE) all
# 	@echo "Making runtime finished"

# 	@echo "Making stdlib"
# 	cd stdlib && $(MAKE) all
# 	@echo "Making stdlib finished"




.PHONY: stdlib
.PHONY: test  quicktest release snapshot snapshotcmj






