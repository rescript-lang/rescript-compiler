include ../Makefile.shared

COMPILER=../../lib/bsc.exe

MAP_FILES= node bs

SOURCE_LIST= node_path node_fs node_process dict node_module js_array js_string \
	js_re js_null_undefined node_buffer js_types js_json js_obj \
	js_vector js_list js_option \
	js_console\
	js_result\
	js_mapperRt\
	bs_Array\
	bs_internalAVLset\
	bs_internalAVLtree\
	bs_SetIntM\
	bs_internalMutableAVL\
	bs_Queue\
	bs_List\
	bs_SortArray\
	bs_SortArrayInt\
	bs_SortArrayString\
	bs_Range\
	bs_internalBucketsType\
	bs_internalSetBuckets\
	bs_internalBuckets\
	bs_HashMap\
	bs_HashSet\
	bs_HashSetString\
	bs_HashSetInt\
	bs_Dict\
	bs_MapDict\
	bs_SetDict\
	bs_Map\
	bs_internalMapInt\
	bs_internalMapString\
	bs_MapString \
	bs_MapInt\
	bs_MapStringM \
	bs_MapIntM\
	bs_internalSet\
	bs_Set\
	bs_SetM\
	bs_MapM\
	bs_internalSetInt\
	bs_internalSetString\
	bs_SetInt\
	bs_SetIntM\
	bs_SetString\
	bs_SetStringM\
	bs_Stack\
	node_child_process \
	js_boolean js_math\
	js_dict js_date js_global js_cast js_promise\
	dom dom_storage\
	bs_HashMapInt\
	bs_HashMapString\

# bs_Bag\
# bs_BagM\
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
	rm -rf bs_HashSetString.ml bs_HashSetString.mli bs_HashSetInt.ml  bs_HashSetInt.mli \
	bs_HashMapInt.ml bs_HashMapInt.mli  bs_HashMapString.ml bs_HashMapString.mli \
	bs_MapInt.ml bs_MapInt.mli  bs_MapString.ml bs_MapString.mli \
	bs_internalSetInt.ml bs_internalSetString.ml \
	bs_SetInt.ml bs_SetInt.mli bs_SetString.ml  bs_SetString.mli \
	bs_SetIntM.ml bs_SetIntM.mli bs_SetStringM.ml bs_SetStringM.mli\
	bs_SortArrayInt.ml bs_SortArrayInt.mli bs_SortArrayString.ml bs_SortArrayString.mli\
	bs_internalMapInt.ml bs_internalMapInt.mli \
	bs_internalMapIntM.ml bs_internalMapIntM.mli \
	bs_internalMapString.ml bs_internalMapString.mli\
	bs_internalMapStringM.ml bs_internalMapStringM.mli\
	bs_MapStringM.mli bs_MapStringM.ml\
	bs_MapIntM.mli bs_MapIntM.ml

ifndef BS_RELEASE_BUILD
bs_HashSetString.ml: hashset.cppo.ml
	cppo -D TYPE_STRING $^ > $@
bs_HashSetInt.ml: hashset.cppo.ml
	cppo -D TYPE_INT $^ > $@
bs_HashSetString.mli: hashset.cppo.mli
	cppo -D TYPE_STRING $^ > $@
bs_HashSetInt.mli: hashset.cppo.mli
	cppo -D TYPE_INT $^ > $@
bs_HashMapString.ml: hashmap.cppo.ml
	cppo -D TYPE_STRING $^ > $@
bs_HashMapInt.ml: hashmap.cppo.ml
	cppo -D TYPE_INT $^ > $@
bs_HashMapString.mli: hashmap.cppo.mli
	cppo -D TYPE_STRING $^ > $@
bs_HashMapInt.mli: hashmap.cppo.mli
	cppo -D TYPE_INT $^ > $@
bs_MapString.ml: map.cppo.ml
	cppo -D TYPE_STRING $^ > $@
bs_MapInt.ml: map.cppo.ml
	cppo -D TYPE_INT $^ > $@
bs_MapString.mli: map.cppo.mli
	cppo -D TYPE_STRING $^ > $@
bs_MapInt.mli: map.cppo.mli
	cppo -D TYPE_INT $^ > $@
bs_MapStringM.mli: mapm.cppo.mli
	cppo -D TYPE_STRING $^ > $@
bs_MapIntM.mli: mapm.cppo.mli
	cppo -D TYPE_INT $^ > $@
bs_MapStringM.ml: mapm.cppo.ml
	cppo -D TYPE_STRING $^ > $@
bs_MapIntM.ml: mapm.cppo.ml
	cppo -D TYPE_INT $^ > $@
bs_internalMapInt.ml : internal_map.cppo.ml
	cppo -D TYPE_INT $^ > $@
bs_internalMapString.ml : internal_map.cppo.ml
	cppo -D TYPE_STRING $^ > $@
bs_internalSetInt.ml: internal_set.cppo.ml
	cppo -D TYPE_INT $^ > $@
bs_internalSetString.ml: internal_set.cppo.ml
	cppo -D TYPE_STRING $^ > $@

bs_SetInt.ml: set.cppo.ml
	cppo -D TYPE_INT $^ > $@
bs_SetString.ml: set.cppo.ml
	cppo -D TYPE_STRING $^ > $@

bs_SetInt.mli: set.cppo.mli
	cppo -D TYPE_INT $^ > $@
bs_SetString.mli: set.cppo.mli
	cppo -D TYPE_STRING $^ > $@

bs_SetIntM.ml: setm.cppo.ml
	cppo -D TYPE_INT $^ > $@
bs_SetStringM.ml: setm.cppo.ml
	cppo -D TYPE_STRING $^ > $@
bs_SetIntM.mli: setm.cppo.mli
	cppo -D TYPE_INT $^ > $@
bs_SetStringM.mli: setm.cppo.mli
	cppo -D TYPE_STRING $^ > $@

bs_SortArrayInt.ml : sort.cppo.ml
	cppo -D TYPE_INT $^ >$@
bs_SortArrayString.ml : sort.cppo.ml
	cppo -D TYPE_STRING $^ >$@
bs_SortArrayInt.mli : sort.cppo.mli
	cppo -D TYPE_INT $^ > $@
bs_SortArrayString.mli : sort.cppo.mli
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
