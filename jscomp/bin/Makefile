ifdef WIN32
	EXE := .exe
endif
ifndef EXE
	EXE := # empty
endif
NATIVE=ocamlopt.opt$(EXE)
BSC_FLAGS= -w -40-30 -I ../runtime -I ../stdlib -bs-files
BSC=../../lib/bsc.exe
OCAMLOPT_FLAGS= -g -w -40-30 ../stubs/ext_basic_hash_stubs.c
# -inline 1000 makes size too large
# TODO: make sure it can be bootstrapped, at least is a very good
# test case of our optimizations

bspack.exe: bspack.mli bspack.ml
	$(NATIVE) $(OCAMLOPT_FLAGS) unix.cmxa  $^ -o $@

bspack.js:	bspack.mli bspack.ml
	$(BSC) $(BSC_FLAGS) $^

test.exe:all_ounit_tests.mli all_ounit_tests.ml
	$(NATIVE) $(OCAMLOPT_FLAGS) -I +compiler-libs ocamlcommon.cmxa unix.cmxa str.cmxa $^ -o $@

test:test.exe
	./test.exe


#  bspp.exe bsdep.exe
# bspack.exe
# bsppx.exe is needed for merlin
# reactjs_jsx_ppx_3.exe
.PHONY: test
