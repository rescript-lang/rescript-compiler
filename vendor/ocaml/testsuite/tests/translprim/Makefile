newdefault: array_spec.ml.reference module_coercion.ml.reference
	$(MAKE) default

BASEDIR=../..
TOPFLAGS+=-dlambda
include $(BASEDIR)/makefiles/Makefile.dlambda
include $(BASEDIR)/makefiles/Makefile.common

GENERATED_SOURCES = array_spec.ml.reference module_coercion.ml.reference \
                    *.flat-float

ifeq "$(FLAT_FLOAT_ARRAY)" "true"
suffix = -flat
else
suffix = -noflat
endif

array_spec.ml.reference: array_spec.ml.reference$(suffix) \
                         $(FLAT_FLOAT_ARRAY).flat-float
	cp $< $@

module_coercion.ml.reference: module_coercion.ml.reference$(suffix) \
                              $(FLAT_FLOAT_ARRAY).flat-float
	cp $< $@

%.flat-float:
	@rm -f $(GENERATED_SOURCES)
	@touch $@
