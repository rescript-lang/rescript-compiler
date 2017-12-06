include ../Makefile.shared

COMPILER=../../lib/bsc.exe

MAP_FILES= node bs

SOURCE_LIST= node_path node_fs node_process dict node_module js_array js_string \
	js_re js_null_undefined node_buffer js_types js_json js_obj \
	js_vector js_list js_option js_result\
	js_mapperRt\
	bs_internalAVLset\
	bs_internalAVLtree\
	bs_Cmp\
	bs_Map\
	bs_Set\
	bs_MapString bs_MapInt\
	bs_SetInt bs_SetString\
	node_child_process js_boolean js_math\
	js_dict js_date js_global js_cast js_promise\
	dom dom_storage\

# bs_dyn bs_dyn_lib\

$(addsuffix .cmj, $(SOURCE_LIST)): $(addsuffix .cmj, $(MAP_FILES))

RUNTIME := $(addsuffix .cmj, $(SOURCE_LIST)) $(addsuffix .cmi, $(SOURCE_LIST))

BS_COMMON_FLAGS= -no-alias-deps -bs-no-version-header -absname -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-noassertfalse -bs-package-name bs-platform


BS_FLAGS=  $(BS_COMMON_FLAGS) $(BS_PKG_FLAGS)

COMPFLAGS += $(BS_FLAGS)  -I ../runtime -I ../stdlib -w +3-40-49 -warn-error A -bin-annot


node.cmi : $(COMPILER)
node.cmj : $(COMPILER)
bs.cmi : $(COMPILER)
bs.cmj : $(COMPILER)
$(RUNTIME): $(COMPILER)

all: $(RUNTIME)

clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f *.rawlambda *.lam *.lambda *.map

ifndef BS_RELEASE_BUILD
bs_MapString.ml: map.cppo.ml
	cppo -D TYPE_STRING $^ > $@
bs_MapInt.ml: map.cppo.ml
	cppo -D TYPE_INT $^ > $@
bs_MapString.mli: map.cppo.mli
	cppo -D TYPE_STRING $^ > $@
bs_MapInt.mli: map.cppo.mli
	cppo -D TYPE_INT $^ > $@
bs_SetInt.ml: set.cppo.ml
	cppo -D TYPE_INT $^ > $@
bs_SetInt.mli: set.cppo.mli
	cppo -D TYPE_INT $^ > $@
bs_SetString.ml: set.cppo.ml
	cppo -D TYPE_STRING $^ > $@
bs_SetString.mli: set.cppo.mli
	cppo -D TYPE_STRING $^ > $@
endif	


.mli.cmi:
	$(COMPILER) $(INCLUDES) $(COMPFLAGS)  -c $<
.ml.cmj:
	$(COMPILER) $(INCLUDES) $(COMPFLAGS)  -c $<


-include .depend

ML_SOURCES=$(addsuffix .ml, $(SOURCE_LIST))
MLI_SOURCES=$(addsuffix .mli, $(SOURCE_LIST))

depend:
	$(CAMLDEP) -native $(INCLUDES) $(ML_SOURCES) $(MLI_SOURCES) | sed -e 's/\.cmx/.cmj/g' >.depend
