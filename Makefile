# export BS_RELEASE_BUILD=1
# so that amdjs is also set

# Note we should not do snapshotml here
# since in relese build, user may not have the sources available
# -B is not necessary in CI, however, when dir is not clean..

NPROCS := 8
KERNEL := $(shell uname -s)

ifeq ($(KERNEL),Linux)
  NPROCS := $(shell grep -c '^processor' /proc/cpuinfo)
endif

DEST=lib/ocaml
RUNTIME=jscomp/runtime
STDLIB=jscomp/stdlib-402
OTHERS=jscomp/others
BELT_BYTE=jscomp/belt_byte
BELT_NATIVE=jscomp/belt_native
OCAML=vendor/ocaml

world:
	@echo "Making compiler"
	$(MAKE) -B -C lib -j $(NPROCS) all
	$(MAKE) libs

libs:
	@echo "Making compiler finished"
	$(MAKE) -C jscomp/runtime -j $(NPROCS) all
	$(MAKE) -C jscomp/others -j $(NPROCS) all
	$(MAKE) -C $(STDLIB) -j $(NPROCS) all

world-native:
	@echo "Making compiler"
	BS_NATIVE=true $(MAKE) -B -C lib -j $(NPROCS) all
	BS_NATIVE=true $(MAKE) libs

libs-native:
	@echo "Making compiler finished"
	$(MAKE) -C jscomp/stdlib -j $(NPROCS) allcmis
	$(MAKE) -C jscomp/runtime -j $(NPROCS) all
	$(MAKE) -C jscomp/others -j $(NPROCS) all
	$(MAKE) -C jscomp/stdlib -j $(NPROCS) all
	
	BS_NATIVE=true $(MAKE) -C jscomp/belt_bytecode -j $(NPROCS) files
	BS_NATIVE=true $(MAKE) -C jscomp/belt_bytecode -j $(NPROCS)
	BS_NATIVE=true $(MAKE) -C jscomp/belt_native -j $(NPROCS) files
	BS_NATIVE=true $(MAKE) -C jscomp/belt_native -j $(NPROCS)


# TODO: sync up with
# scripts/build_uitil.js
# function install
# scripts/build_util.install

# ATTENTION: syncup build_util.install for windows
install:
	@echo "Installation"
	cp $(RUNTIME)/*.cmt* $(RUNTIME)/*.cmj $(RUNTIME)/js.ml  $(RUNTIME)/js.cmi $(RUNTIME)/js.cmj \
	$(STDLIB)/*.cm* $(STDLIB)/*.ml $(STDLIB)/*.mli \
	$(OTHERS)/*.ml $(OTHERS)/*.mli  $(OTHERS)/*.cm* $(DEST)
	mkdir -p $(DEST)/bytecode $(DEST)/native
	cp $(BELT_BYTE)/*.ml $(BELT_BYTE)/*mli $(BELT_BYTE)/*.o $(BELT_BYTE)/*.cm* $(DEST)/bytecode
	cp $(BELT_NATIVE)/*.ml $(BELT_NATIVE)/*mli $(BELT_NATIVE)/*.o $(BELT_NATIVE)/*.cm* $(BELT_NATIVE)/*.a $(DEST)/native
	cp -r $(OCAML)/lib/ocaml/caml $(DEST)

.PHONY: libs world
