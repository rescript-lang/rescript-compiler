
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
	# ocamlbuild -no-ocamlfind -lflags -no-check-prims -use-ocamlfind -I bin -no-hygiene -pkgs compiler-libs.common -no-links bin/reason.byte
	# js_of_ocaml -I +compiler-libs   --toplevel +weak.js +toplevel.js  ./polyfill.js _build/bin/reason.byte -I ./runtime/ --file js.cmi:/cmis/js.cmi --file js_unsafe.cmi:/cmis/js_unsafe.cmi --file js_re.cmi:/cmis/js_re.cmi   -o bin/compiler.js

#TODO: seems dependency on jsoo and reason should not block our release


# Note: this target also built bin/bsc which is used in release mode

# TODO: should be done after we do the build
# Carefully check the stored data if it is platform independent

./bin/bspack: ./bin/bspack.mli ./bin/bspack.ml 
	$(NATIVE) -w -a -I bin  unix.cmxa  $^ -o $@

./bin/whole_compiler.ml:./bin/bspack
	@echo "Generating whole_compiler"
	$< -bs-mllib bin/whole_compiler.mllib -o bin/whole_compiler.ml -bs-main js_main.ml -I ../ocaml/utils/ -I ../ocaml/parsing/ -I ../ocaml/typing/ -I ../ocaml/bytecomp/ -I ext -I syntax -I depends -I common
./bin/bsc.exe: ./bin/whole_compiler.mli ./bin/whole_compiler.ml 
	$(NATIVE) -w -a -I bin $^ -o $@

./bin/bsppx: ./bin/bs_ppx.mli ./bin/bs_ppx.ml 
	$(NATIVE) -g -inline 1000 -linkall -w -a -I bin -I +compiler-libs ocamlcommon.cmxa   $^ -o $@


snapshot: ./bin/bspack snapshotcmj
	$(MAKE) snapshotml

snapshotml:./bin/bspack ./bin/compiler.mllib 
	@echo "Snapshot ml"
	$< -bs-mllib bin/compiler.mllib -o bin/compiler.ml
	$< -I common -I ext -I syntax -bs-main bs_ppx_main.ml -o bin/bs_ppx.ml
snapshotcmj:
	@echo "Collecting cmj files"
	ocamlbuild -cflags $(OCAMLBUILD_CFLAGS)	 -lflags -I,+compiler-libs,ocamlcommon.cmxa js_pack.native --

# git diff bin/bspack.ml
# bootbspack requires ../ocaml has parser, lexer processed by ocamlyacc and ocamllex
bootbspack: ./bin/bspack
	@echo "Bootstrap bspack"
	./bin/bspack -bs-log-mllib bin/bspack.mllib -I ../ocaml/parsing/ -I ../ocaml/utils/ -I ext -I common -I depends -bs-main bspack_main.ml -o bin/bspack.ml
	$(NATIVE) -w -40  unix.cmxa  -I bin bin/bspack.mli bin/bspack.ml -o bin/bspack
	@echo "Using the bootstrapped bspack to genreate new bspack.ml: `date`"
	./bin/bspack -bs-log-mllib bin/bspack.mllib -I ../ocaml/parsing/ -I ../ocaml/utils/ -I ext -I common -I depends -bs-main bspack_main.ml -o bin/bspack.ml
	@echo "Bootstrap seems finished, please check diffs in bspack"
	@echo "Rebuilding bspack"
	make -B ./bin/bspack
	@echo "Rebuilding bspack finsihed"


releasebuild:./bin/bspack
	@echo "Make release compiler"
	$(NATIVE) -g -inline 1000 -linkall  -w -a -I +compiler-libs -I bin ocamlcommon.cmxa  bin/compiler.mli bin/compiler.ml -o bin/bsc

	@echo "Make release ppx"
	$(NATIVE) -g -inline 1000 -linkall  -w -a -I +compiler-libs -I bin ocamlcommon.cmxa  bin/bs_ppx.mli bin/bs_ppx.ml -o bin/bsppx


stdlib:
	cd stdlib && ./build.sh

world:
	@echo "Making compiler"		
	$(MAKE) releasebuild
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

travis-world-test:./bin/bspack
	@echo "Generating the compiler"
	rm -f bin/compiler.ml bin/bs_ppx.ml
	$(MAKE) snapshotml	
	@echo "Generating the compiler finished"	
	$(MAKE) world-test

# no depend on ./bin/bspack ./bin/bsc
# since in npm mode, they are generated from a single file
install:
	cp  ./bin/bsc ./bin/bsppx  ./bin/bspack ../bin/
	cp ./runtime/*.cmt* ./runtime/*.cmj* ./stdlib/*.cm* ./others/*.ml ./others/*.mli  ./others/*.cm* ../lib/ocaml/
	cp ./runtime/js.ml  ./runtime/js.cmi ./runtime/js_unsafe.cmi ./runtime/js_null.ml ./runtime/js_null.cmi \
	./runtime/js_undefined.ml ./runtime/js_undefined.cmi ./runtime/js_null_undefined.ml ./runtime/js_null_undefined.cmi  ../lib/ocaml/	




# TMP_OCAMLLIB=$(shell ocamlopt.opt -where)
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
.PHONY: test  quicktest release snapshot snapshotcmj releasebuild






