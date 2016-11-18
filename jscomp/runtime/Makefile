include ../Makefile.shared

COMPILER=../bin/bsc.exe

OTHERS= caml_array caml_string caml_bytes\
	caml_obj caml_int64 \
	caml_exceptions	caml_utils caml_sys caml_io\
	caml_float caml_lexer caml_parser caml_primitive\
	caml_format caml_md5 caml_queue caml_hash caml_weak\
	caml_backtrace caml_int32 caml_gc typed_array \
	js_primitive caml_basic caml_oo curry caml_module \
	bs_string  js_float bs_obj js_nativeint js_int js_null

SOURCE_LIST= $(OTHERS) caml_builtin_exceptions  block  js js_unsafe

caml_oo.cmj : caml_array.cmj
caml_format.cmj caml_io.cmj: curry.cmj
caml_builtin_exceptions.cmj: caml_builtin_exceptions.cmi
block.cmj: block.cmi
$(addsuffix .cmj, $(OTHERS)): caml_builtin_exceptions.cmj  block.cmj js.cmj js_unsafe.cmj
## since we use ppx
$(addsuffix .cmi, $(OTHERS)): js.cmi js_unsafe.cmj js_unsafe.cmi

RUNTIME := $(addsuffix .cmj, $(SOURCE_LIST))

BS_COMMON_FLAGS= -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform

ifdef BS_RELEASE_BUILD
BS_FLAGS=  $(BS_COMMON_FLAGS) -bs-package-output commonjs:lib/js -bs-package-output goog:lib/goog -bs-package-output amdjs:lib/amdjs
else
BS_FLAGS=  $(BS_COMMON_FLAGS) -bs-package-output commonjs:lib/js
endif

COMPFLAGS += $(BS_FLAGS) -I ../stdlib -nostdlib -nopervasives -open Pervasives -unsafe -w -40-49 -bin-annot


$(RUNTIME): $(COMPILER)

all: $(RUNTIME)

clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f *.rawlambda *.lam *.lambda *.map


.mli.cmi:
	$(COMPILER) $(INCLUDES) $(COMPFLAGS)  -c $<
.ml.cmj:
	$(COMPILER) $(INCLUDES) $(COMPFLAGS)  -c $<

-include .runtimedepend

ML_SOURCES=$(addsuffix .ml, $(OTHERS))
MLI_SOURCES=$(addsuffix .mli, $(OTHERS))

depend:
	$(CAMLDEP) $(INCLUDES) $(ML_SOURCES) $(MLI_SOURCES) | sed -e 's/\.cmo/.cmj/g' > .runtimedepend
	$(CAMLDEP) $(INCLUDES) $(ML_SOURCES) | sed -e 's/\.cmx/.cmj/g' >>.runtimedepend
