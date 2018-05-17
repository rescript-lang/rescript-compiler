include ../Makefile.shared

COMPILER=../../lib/bsc.exe

MAP_FILES= node belt

SOURCE_LIST= node_path node_fs node_process dict node_module js_array js_string \
	js_re js_null_undefined node_buffer js_types js_json js_obj \
	js_vector js_list js_option \
	js_console\
	js_result\
	js_mapperRt\
	belt_Array\
	belt_internalAVLset\
	belt_internalAVLtree\
	belt_internalMutableAVL\
	belt_List\
	belt_SortArray\
	belt_SortArrayInt\
	belt_SortArrayString\
	belt_Range\
	belt_internalBucketsType\
	belt_internalSetBuckets\
	belt_internalBuckets\
	belt_HashMap\
	belt_HashSet\
	belt_HashSetString\
	belt_HashSetInt\
	belt_Id\
	belt_MapDict\
	belt_SetDict\
	belt_Map\
	belt_internalMapInt\
	belt_internalMapString\
	belt_MapString \
	belt_MapInt\
	belt_Option\
	belt_Result\
	belt_internalSet\
	belt_Set\
	belt_MutableSet\
	belt_MutableSetInt\
	belt_MutableSetString\
	belt_MutableMap\
	belt_MutableMapInt\
	belt_MutableMapString\
	belt_internalSetInt\
	belt_internalSetString\
	belt_SetInt\
	belt_SetString\
	belt_MutableStack\
	belt_MutableQueue\
	node_child_process \
	js_boolean js_math\
	js_dict js_date js_global js_cast js_promise\
	dom dom_storage\
	belt_HashMapInt\
	belt_HashMapString\
	belt_Debug\

# belt_Bag\
# belt_BagM\
# belt_dyn belt_dyn_lib\

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
	rm -rf belt_HashSetString.ml belt_HashSetString.mli belt_HashSetInt.ml  belt_HashSetInt.mli \
	belt_HashMapInt.ml belt_HashMapInt.mli  belt_HashMapString.ml belt_HashMapString.mli \
	belt_MapInt.ml belt_MapInt.mli  belt_MapString.ml belt_MapString.mli \
	belt_internalSetInt.ml belt_internalSetString.ml \
	belt_SetInt.ml belt_SetInt.mli belt_SetString.ml  belt_SetString.mli \
	belt_MutableSetInt.ml belt_MutableSetInt.mli belt_MutableSetString.ml belt_MutableSetString.mli\
	belt_SortArrayInt.ml belt_SortArrayInt.mli belt_SortArrayString.ml belt_SortArrayString.mli\
	belt_internalMapInt.ml belt_internalMapInt.mli \
	belt_internalMapIntM.ml belt_internalMapIntM.mli \
	belt_internalMapString.ml belt_internalMapString.mli\
	belt_internalMapStringM.ml belt_internalMapStringM.mli\
	belt_MutableMapString.mli belt_MutableMapString.ml\
	belt_MutableMapInt.mli belt_MutableMapInt.ml

ifndef BS_RELEASE_BUILD
belt_HashSetString.ml: hashset.cppo.ml
	cppo -D TYPE_STRING $^ > $@
belt_HashSetInt.ml: hashset.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_HashSetString.mli: hashset.cppo.mli
	cppo -D TYPE_STRING $^ > $@
belt_HashSetInt.mli: hashset.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_HashMapString.ml: hashmap.cppo.ml
	cppo -D TYPE_STRING $^ > $@
belt_HashMapInt.ml: hashmap.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_HashMapString.mli: hashmap.cppo.mli
	cppo -D TYPE_STRING $^ > $@
belt_HashMapInt.mli: hashmap.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_MapString.ml: map.cppo.ml
	cppo -D TYPE_STRING $^ > $@
belt_MapInt.ml: map.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_MapString.mli: map.cppo.mli
	cppo -D TYPE_STRING $^ > $@
belt_MapInt.mli: map.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_MutableMapString.mli: mapm.cppo.mli
	cppo -D TYPE_STRING $^ > $@
belt_MutableMapInt.mli: mapm.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_MutableMapString.ml: mapm.cppo.ml
	cppo -D TYPE_STRING $^ > $@
belt_MutableMapInt.ml: mapm.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_internalMapInt.ml : internal_map.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_internalMapString.ml : internal_map.cppo.ml
	cppo -D TYPE_STRING $^ > $@
belt_internalSetInt.ml: internal_set.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_internalSetString.ml: internal_set.cppo.ml
	cppo -D TYPE_STRING $^ > $@

belt_SetInt.ml: set.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_SetString.ml: set.cppo.ml
	cppo -D TYPE_STRING $^ > $@

belt_SetInt.mli: set.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_SetString.mli: set.cppo.mli
	cppo -D TYPE_STRING $^ > $@

belt_MutableSetInt.ml: setm.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_MutableSetString.ml: setm.cppo.ml
	cppo -D TYPE_STRING $^ > $@
belt_MutableSetInt.mli: setm.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_MutableSetString.mli: setm.cppo.mli
	cppo -D TYPE_STRING $^ > $@

belt_SortArrayInt.ml : sort.cppo.ml
	cppo -D TYPE_INT $^ >$@
belt_SortArrayString.ml : sort.cppo.ml
	cppo -D TYPE_STRING $^ >$@
belt_SortArrayInt.mli : sort.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_gSortArrayString.mli : sort.cppo.mli
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
