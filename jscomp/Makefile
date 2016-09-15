ifdef WIN32
	EXE := .exe
endif
ifndef EXE
	EXE := # empty
endif
NATIVE=ocamlopt.opt$(EXE)
BYTE=ocamlc.opt$(EXE)
CAMLP4OF=camlp4of.opt$(EXE)
## Beging Rules only make sense in dev mode
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
## End Rules

release:snapshot
	$(MAKE) releasebuild

# TODO: should be done after we do the build
# Carefully check the stored data if it is platform independent

./bin/bspack$(EXE): ./bin/bspack.mli ./bin/bspack.ml
	$(NATIVE) -w -a -I bin  unix.cmxa  $^ -o $@

# For faster compilation when can cache ast file instead for Linux/MacOS users
# For windows user, we just ship the binary
./bin/bsc$(EXE): _build/ext/ext.cmxa _build/common/common.cmxa _build/depends/depends.cmxa _build/syntax/syntax.cmxa _build/core.cmxa
	echo "Linking"
	$(NATIVE) -g -linkall -o $@ -I +compiler-libs ocamlcommon.cmxa  $^  2>>build.compile

./bin/bsppx$(EXE): ./bin/bsppx.mli ./bin/bsppx.ml
	$(NATIVE) -g -inline 1000 -linkall -w -a -I bin -I +compiler-libs ocamlcommon.cmxa   $^ -o $@

releasebuild:./bin/bspack$(EXE)
	@echo "Make release compiler"
	$(NATIVE) -g -inline 1000 -linkall  -w -a -I +compiler-libs -I bin ocamlcommon.cmxa  bin/compiler.mli bin/compiler.ml -o bin/bsc$(EXE)
	@echo "Make release ppx"
	$(NATIVE) -g -inline 1000 -linkall  -w -a -I +compiler-libs -I bin ocamlcommon.cmxa  bin/bsppx.mli bin/bsppx.ml -o bin/bsppx$(EXE)

snapshot: ./bin/bspack snapshotcmj
	$(MAKE) snapshotml

snapshotml:./bin/bspack ./bin/compiler.mllib
	@echo "Snapshot ml"
	$< -bs-mllib bin/compiler.mllib -o bin/compiler.ml
	$< -I common -I ext -I syntax -bs-main bsppx_main.ml -o bin/bsppx.ml
	$< -bs-log-mllib bin/whole_compiler.mllib -bs-exclude-I config -o bin/whole_compiler.ml -bs-main js_main.ml -I ../ocaml/utils/ -I ../ocaml/parsing/ -I ../ocaml/typing/ -I ../ocaml/bytecomp/ -I ../ocaml/driver/ -I ext -I syntax -I depends -I common

snapshotcmj:
	@echo "Collecting cmj files"
	ocamlbuild -cflags $(OCAMLBUILD_CFLAGS)	 -lflags -I,+compiler-libs,ocamlcommon.cmxa js_pack.native --

# git diff bin/bspack.ml
# bootbspack requires ../ocaml has parser, lexer processed by ocamlyacc and ocamllex
bootbspack: ./bin/bspack$(EXE)
	@echo "Bootstrap bspack"
	./bin/bspack$(EXE) -bs-log-mllib bin/bspack.mllib -prelude bin/bspack_config.ml -bs-exclude-I clflags -bs-exclude-I config  -I ../ocaml/parsing/ -I ../ocaml/utils/ -I ext -I common -I depends -bs-main bspack_main.ml -o bin/bspack.ml
	$(NATIVE) -w -40  unix.cmxa  -I bin bin/bspack.mli bin/bspack.ml -o bin/bspack$(EXE)
	@echo "Using the bootstrapped bspack to genreate new bspack.ml: `date`"
	./bin/bspack$(EXE) -bs-log-mllib bin/bspack.mllib -prelude bin/bspack_config.ml -bs-exclude-I clflags -bs-exclude-I config  -I ../ocaml/parsing/ -I ../ocaml/utils/ -I ext -I common -I depends -bs-main bspack_main.ml -o bin/bspack.ml
	@echo "Bootstrap seems finished, please check diffs in bspack"
	@echo "Rebuilding bspack"
	make -B ./bin/bspack$(EXE)
	@echo "Rebuilding bspack finsihed"

libs:
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

world:
	@echo "Making compiler"
	$(MAKE) releasebuild
	@echo "Making compiler finished"
	$(MAKE) libs
windows-world:
	@echo "Making compiler"
	$(NATIVE) -w -a -I bin ./bin/config.mli ./bin/config.ml  ./bin/whole_compiler.mli ./bin/whole_compiler.ml -o bin/bsc$(EXE)
	$(MAKE) bin/bspack$(EXE)
	$(MAKE) bin/bsppx$(EXE)
	$(MAKE) libs

world-test:
	$(MAKE) world
	@echo "Making test"
	cd test && $(MAKE) all
	@echo "Making test finsihed"

travis-world-test:./bin/bspack
	@echo "Generating the compiler"
	rm -f bin/compiler.ml bin/bsppx.ml
	$(MAKE) snapshotml
	@echo "Generating the compiler finished"
	$(MAKE) world-test

install:
	cp  ./bin/bsc$(EXE) ./bin/bsppx$(EXE)  ./bin/bspack$(EXE) ../bin/
	cp ./runtime/*.cmt* ./runtime/*.cmj* ./stdlib/*.cm* ./others/*.ml ./others/*.mli  ./others/*.cm* ../lib/ocaml/
	cp ./runtime/js.ml  ./runtime/js.cmi ./runtime/js_unsafe.cmi ./runtime/js_null.ml ./runtime/js_null.cmi \
	./runtime/js_undefined.ml ./runtime/js_undefined.cmi ./runtime/js_null_undefined.ml ./runtime/js_null_undefined.cmi  ../lib/ocaml/


.PHONY: test  quicktest release snapshot snapshotcmj releasebuild libs
