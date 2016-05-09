include ../Makefile.shared

COMPILER=../bin/bsc

OTHERS= caml_array caml_string \
	caml_obj caml_int64 \
	caml_exceptions	caml_utils caml_sys caml_io\
	caml_float caml_lexer caml_parser caml_primitive\
	caml_format caml_md5 caml_queue caml_hash caml_weak\
	caml_backtrace caml_int32 caml_gc typed_array \
	js_primitive caml_basic caml_oo curry

SOURCE_LIST= $(OTHERS) caml_builtin_exceptions  block  js fn 

caml_oo.cmj : caml_array.cmj
caml_format.cmj caml_io.cmj: curry.cmj


$(addsuffix .cmj, $(OTHERS)): caml_builtin_exceptions.cmj  block.cmj js.cmj fn.cmj

RUNTIME := $(addsuffix .cmj, $(SOURCE_LIST))


COMPFLAGS += $(MODULE_FLAGS) -w -40 -js-npm-output-path lib/js



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

depend:
	$(CAMLDEP) $(INCLUDES) *.mli *.ml | sed -e 's/\.cmo/.cmj/g' > .runtimedepend
	$(CAMLDEP) $(INCLUDES) *.ml | sed -e 's/\.cmx/.cmj/g' >>.runtimedepend

