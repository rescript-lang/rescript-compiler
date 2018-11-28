include ../Makefile.shared

COMPILER=../../lib/bsc.exe

OTHERS= caml_array caml_string caml_bytes\
	caml_obj caml_int64 \
	caml_exceptions	caml_js_exceptions caml_utils caml_sys caml_io\
	caml_float caml_lexer caml_parser \
	caml_format caml_md5 caml_queue \
	caml_hash_primitive\
	caml_hash caml_weak\
	caml_int32 caml_gc \
	js_primitive caml_basic caml_oo curry caml_oo_curry caml_module \
	caml_missing_polyfill\
	bs_string  js_nativeint js_int64 \

SOURCE_LIST= $(OTHERS) bs_obj caml_builtin_exceptions  block  js   caml_primitive


caml_oo.cmj : caml_array.cmj
js.cmj: js.cmi
caml_format.cmj caml_io.cmj caml_oo_curry.cmj: curry.cmj
caml_format.cmj: caml_int64.cmj caml_int32.cmj caml_utils.cmj caml_string.cmj
caml_weak.cmj caml_module.cmj: caml_obj.cmj js_primitive.cmj
caml_builtin_exceptions.cmj: caml_builtin_exceptions.cmi js.cmi
block.cmj: block.cmi js.cmi 
caml_primitive.cmj: caml_primitive.cmi
caml_int64.cmj : caml_obj.cmj
bs_obj.cmj: js.cmi
# or we can do a post-processing to add missing cmj dependency manually
caml_js_exceptions.cmj : caml_exceptions.cmj
$(addsuffix .cmj, $(OTHERS)): caml_builtin_exceptions.cmj  block.cmj js.cmj  caml_primitive.cmj

## since we use ppx
$(addsuffix .cmi, $(OTHERS)): js.cmi js.cmj



RUNTIME := $(addsuffix .cmj, $(SOURCE_LIST)) $(addsuffix .cmi, $(SOURCE_LIST))

BS_COMMON_FLAGS= -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform

BS_FLAGS=  $(BS_COMMON_FLAGS) $(BS_PKG_FLAGS)

COMPFLAGS += $(BS_FLAGS)  -nostdlib -nopervasives  -unsafe -warn-error A -w -40-49-103 -bin-annot 


$(RUNTIME): $(COMPILER) bs_stdlib_mini.cmi

all: $(RUNTIME)

clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f *.rawlambda *.lam *.lambda *.map

# Note since Makefile rules does not support multiple output
# To avoid bugs, we should always have a .mli file for each .ml when using suffix rules
# otherwise Make can not know how to generate .cmi when only .ml exists
.mli.cmi:
	$(COMPILER) -open Bs_stdlib_mini $(INCLUDES) $(COMPFLAGS)  -c $<
.ml.cmj:
	$(COMPILER) -open Bs_stdlib_mini $(INCLUDES) $(COMPFLAGS)  -c $<

bs_stdlib_mini.cmi:	bs_stdlib_mini.mli
	$(COMPILER) $(COMPFLAGS) -c $<

-include .extradepend
-include .depend

ML_SOURCES=$(addsuffix .ml, $(OTHERS)) block.ml caml_builtin_exceptions.ml
MLI_SOURCES=$(addsuffix .mli, $(OTHERS)) block.mli caml_builtin_exceptions.mli

depend:
	$(CAMLDEP) -native $(INCLUDES) $(ML_SOURCES) $(MLI_SOURCES) | sed -e 's/\.cmx/.cmj/g' >.depend
