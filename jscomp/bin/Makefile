ifdef WIN32
	EXE := .exe
endif
ifndef EXE
	EXE := # empty
endif
NATIVE=ocamlopt.opt$(EXE)

bsppx.exe: config_bsppx.mli config_bsppx.ml bsppx.mli bsppx.ml
	$(NATIVE) -g -inline 1000 -linkall -w -a $^ -o $@

bspp.exe:  bspp.mli bspp.ml
	$(NATIVE) -g -inline 1000 -linkall -w -a $^ -o $@

bsdep.exe: config_bsdep.mli config_bsdep.ml bsdep.mli bsdep.ml
	$(NATIVE) -g -inline 1000 -linkall -w -a $^ -o $@

bsc.exe: config_whole_compiler.mli config_whole_compiler.ml  whole_compiler.mli whole_compiler.ml
	$(NATIVE) -w -a $^  -o $@

bspack.exe: config_bspack.mli config_bspack.ml bspack.mli bspack.ml
	$(NATIVE) -w -a unix.cmxa  $^ -o $@

all: bsppx.exe bspp.exe bsdep.exe
