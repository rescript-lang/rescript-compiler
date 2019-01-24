BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.several
include $(BASEDIR)/makefiles/Makefile.common

# The trigraph.ml test always fails under OpenBSD 6 / i386
# because of an unrelated warning emitted by the linker called by ocamlopt
# (see commit log for details).
# As a temporary workaround, we skip this test.
SKIP=test $$file = trigraph.ml \
     && test `uname -m` = i386 && test `uname -s` = OpenBSD
