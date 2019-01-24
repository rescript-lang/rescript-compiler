# Tests from manual, section intf-c
# main.ml: error message when equality is missing
# main_ok.ml: allow path expansion even when the target is missing (GPR#816)

SOURCES = curses.ml prog.ml
CSOURCES = curses_stubs.c
CLIBS = -cclib "$(BYTECCLIBS)"
LIBUNIX = -I $(BASEDIR)/../otherlibs/unix unix.cma

# Disable this test until we figure out how to test for the availability
# of curses.
.PHONY: disable
disable:
	@printf " ... testing prog => skipped\n"
	@printf " ... testing prog2 => skipped\n"

.PHONY: default
default: clean $(SOURCES) $(CSOURCES)
	@printf " ... testing prog"
	@$(MAKE) prog > /dev/null && echo " => passed" || echo " => failed"
	@printf " ... testing prog2"
	@$(MAKE) prog2 REDIRECT=">prog2.result 2>&1" \
	  >/dev/null 2>/dev/null || :
	@$(DIFF) prog2.reference prog2.result >/dev/null \
	&& echo " => passed" || echo " => failed"

# Should succeed
prog:
	$(OCAMLC) -custom -o prog $(LIBUNIX) $(SOURCES) $(CSOURCES) $(CLIBS)

# Should fail
prog2:  curses.cmo
	$(OCAMLC) -custom -o prog2 $(LIBUNIX) prog.ml $(CSOURCES) $(CLIBS) $(REDIRECT)

.PHONY: clean
clean:
	@rm -f *.cm* *.o *~ prog prog2

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common
