newdefault: test.ml.reference
	@$(MAKE) default

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.toplevel
include $(BASEDIR)/makefiles/Makefile.common

GENERATED_SOURCES = test.ml.reference *.flat-float

ifeq "$(FLAT_FLOAT_ARRAY)" "true"
suffix = -flat
else
suffix = -noflat
endif

test.ml.reference: test.ml.reference$(suffix) $(FLAT_FLOAT_ARRAY).flat-float
	@cp $< $@

%.flat-float:
	@rm -f $(GENERATED_SOURCES)
	@touch $@
