include ../Makefile.shared

COMPILER=../../lib/bsc.exe

BS_COMMON_FLAGS= -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform

BS_FLAGS=  $(BS_COMMON_FLAGS) $(BS_PKG_FLAGS)

COMPFLAGS += $(BS_FLAGS)  -nostdlib -nopervasives  -unsafe -warn-error A -w -40-49-103 -bin-annot 

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
	bs_string  js_nativeint js_int64 bs_obj caml_builtin_exceptions  block caml_primitive\
	caml_char caml_nativeint\

SOURCE_LIST= $(OTHERS) js 

RUNTIME := $(addsuffix .cmj, $(SOURCE_LIST)) $(addsuffix .cmi, $(SOURCE_LIST))

$(RUNTIME): $(COMPILER)

all: $(RUNTIME)
clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f *.rawlambda *.lam *.lambda *.map
.mli.cmi:
	$(COMPILER) -open Bs_stdlib_mini $(INCLUDES) $(COMPFLAGS)  -c $<
.ml.cmj:
	$(COMPILER) -open Bs_stdlib_mini $(INCLUDES) $(COMPFLAGS)  -c $<

bs_stdlib_mini.cmi:	bs_stdlib_mini.mli
	$(COMPILER) $(COMPFLAGS) -c $<


-include .depend


depend:
	node ../../scripts/runtimeDeps.js
	


