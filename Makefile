# export BS_RELEASE_BUILD=1
# so that amdjs is also set

# Note we should not do snapshotml here
# since in relese build, user may not have the sources available
# -B is not necessary in CI, however, when dir is not clean..

world:
	@echo "Making compiler"
	$(MAKE) -B -C lib -j 6 all
	$(MAKE) libs

libs:
	@echo "Making compiler finished"
	$(MAKE) -C jscomp/stdlib -j8 allcmis
	$(MAKE) -C jscomp/runtime -j8 all
	$(MAKE) -C jscomp/others -j8 all
	$(MAKE) -C jscomp/stdlib -j8 all


DEST=lib/ocaml
RUNTIME=jscomp/runtime
STDLIB=jscomp/stdlib
OTHERS=jscomp/others
# TODO: sync up with
# scripts/build_uitil.js
# function install
# scripts/build_util.install

# ATTENTION: syncup build_util.install for windows
install:
	@echo "Installation"
	cp $(RUNTIME)/*.cmt* $(RUNTIME)/*.cmj* $(RUNTIME)/js.ml  $(RUNTIME)/js.cmi \
	$(RUNTIME)/js_unsafe.cmi $(RUNTIME)/js_internal.cmi \
	$(RUNTIME)/caml_exceptions.mli $(RUNTIME)/caml_exceptions.ml $(RUNTIME)/caml_exceptions.cmi \
	$(RUNTIME)/js_null.ml $(RUNTIME)/js_null.cmi \
	$(RUNTIME)/js_undefined.ml $(RUNTIME)/js_undefined.cmi \
	$(RUNTIME)/js_exn.ml $(RUNTIME)/js_exn.mli $(RUNTIME)/js_exn.cmi \
	$(RUNTIME)/js_int.ml $(RUNTIME)/js_int.cmi \
	$(RUNTIME)/js_float.ml $(RUNTIME)/js_float.cmi \
	$(RUNTIME)/js_typed_array.ml $(RUNTIME)/js_typed_array.cmi  \
	$(STDLIB)/*.cm* $(STDLIB)/*.ml $(STDLIB)/*.mli \
	$(OTHERS)/*.ml $(OTHERS)/*.mli  $(OTHERS)/*.cm* $(DEST)

.PHONY: libs world
