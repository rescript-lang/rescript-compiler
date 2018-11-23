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

world:
	@echo "Making compiler"
	$(MAKE) -B -C lib -j $(NPROCS) all
	$(MAKE) libs

libs:
	@echo "Making compiler finished"
	$(MAKE) -C jscomp/runtime -j $(NPROCS) all
	$(MAKE) -C jscomp/others -j $(NPROCS) all
	$(MAKE) -C $(STDLIB) -j $(NPROCS) all


# TODO: sync up with
# scripts/build_uitil.js
# function install
# scripts/build_util.install

# ATTENTION: syncup build_util.install for windows
install:
	@echo "Installation"
	cp $(RUNTIME)/*.cmt* $(RUNTIME)/*.cmj* $(RUNTIME)/js.ml  $(RUNTIME)/js.cmi \
	$(RUNTIME)/js_unsafe.cmi $(RUNTIME)/js_internal.cmi \
	$(RUNTIME)/js_exn.ml $(RUNTIME)/js_exn.mli $(RUNTIME)/js_exn.cmi \
	$(STDLIB)/*.cm* $(STDLIB)/*.ml $(STDLIB)/*.mli \
	$(OTHERS)/*.ml $(OTHERS)/*.mli  $(OTHERS)/*.cm* $(DEST)

.PHONY: libs world
