
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

./bin/ocamlscript: _build/compiler.cmxa
	echo "Linking"
	$(NATIVE) -g -linkall -o $@ -I +compiler-libs ocamlcommon.cmxa ocamlbytecomp.cmxa $^ main.cmx 2>>build.compile
# Note: we can remove main.cmx since we have js_main.cmx which use [js_implementation.implementation],
# The motivation is to save compilation time and in the future, more freedom to add new
# compilation flags (since we don't need bytecode anymore)
# Another solution to save compilation time is have a flag like '-i' to stop the compiler
# continuing, in this case the user may occationally want bytecode/nativecode backend

release:snapshot
	@echo "Make release compiler"
	$(NATIVE) -I +compiler-libs -I bin -c bin/compiler.mli bin/compiler.ml
	$(NATIVE) -g -linkall -o bin/ocamlscript -I +compiler-libs ocamlcommon.cmxa ocamlbytecomp.cmxa  bin/compiler.cmx main.cmx
	@echo "Collecting cmj files"
	ocamlbuild -cflags $(OCAMLBUILD_CFLAGS)	 js_pack.native --
# Note: this target also built bin/ocamlscript which is used in release mode

# TODO: should be done after we do the build
# Carefully check the stored data if it is platform independent

snapshot:
	@echo "Snapshot"
	cd tools && ./pack.sh && cd ..
	./tools/ocaml_pack compiler.mllib > bin/compiler.ml


stdlib:
	cd stdlib && ./build.sh

.PHONY: stdlib
.PHONY: test  quicktest release snapshot






