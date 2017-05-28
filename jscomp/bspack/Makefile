ifdef WIN32
	EXE := .exe
endif
ifndef EXE
	EXE := # empty
endif
NATIVE=ocamlopt.opt$(EXE)
OCAMLOPT_FLAGS= -g -w -40-30 ext_basic_hash_stubs.c

all: bspack.exe

bspack.exe: bspack.mli bspack.ml
	$(NATIVE) $(OCAMLOPT_FLAGS) unix.cmxa $^ -o $@

clean:
	rm -f *.cmi *.cmx *.o *.exe
