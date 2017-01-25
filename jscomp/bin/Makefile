ifdef WIN32
	EXE := .exe
endif
ifndef EXE
	EXE := # empty
endif
NATIVE=ocamlopt.opt$(EXE)

OCAMLOPT_FLAGS= -inline 1000 -g -w -a ../stubs/ext_basic_hash_stubs.c

bsppx.exe: config_bsppx.mli config_bsppx.ml bsppx.mli bsppx.ml
	$(NATIVE) $(OCAMLOPT_FLAGS) $^ -o $@

bspp.exe:  bspp.mli bspp.ml
	$(NATIVE) $(OCAMLOPT_FLAGS) $^ -o $@

bsdep.exe: config_bsdep.mli config_bsdep.ml bsdep.mli bsdep.ml
	$(NATIVE) $(OCAMLOPT_FLAGS) $^ -o $@

bsb.exe:  bsb.mli bsb.ml
	$(NATIVE) $(OCAMLOPT_FLAGS) unix.cmxa str.cmxa $^ -o $@

bsb_helper.exe:  bsb_helper.mli bsb_helper.ml
	$(NATIVE) $(OCAMLOPT_FLAGS) unix.cmxa str.cmxa -w -a $^ -o $@

config_whole_compiler.ml:config_whole_compiler.mlp ../config.js
	node ../config.js
bsc.exe: config_whole_compiler.mli config_whole_compiler.ml  whole_compiler.mli whole_compiler.ml
	$(NATIVE) $(OCAMLOPT_FLAGS) $^  -o $@

bspack.exe: config_bspack.mli config_bspack.ml bspack.mli bspack.ml
	$(NATIVE) $(OCAMLOPT_FLAGS) unix.cmxa  $^ -o $@

bsdoc.exe: config_bsdep.mli config_bsdep.ml  bsdoc.mli bsdoc.ml
	$(NATIVE) -g -w -a unix.cmxa dynlink.cmxa str.cmxa $^ -o $@

all: bsppx.exe bspp.exe bsdep.exe bsc.exe bspack.exe bsb.exe bsb_helper.exe bsdoc.exe
