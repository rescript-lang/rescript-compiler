#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                 Xavier Clerc, SED, INRIA Rocquencourt                  *
#*                                                                        *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BASEDIR := $(shell pwd)
NO_PRINT=`$(MAKE) empty --no-print-directory >/dev/null 2>&1 \
	  && echo --no-print-directory`

FIND=find
include ../config/Makefile

ifeq "$(UNIX_OR_WIN32)" "unix"
  ifeq "$(SYSTEM)" "cygwin"
    find := /usr/bin/find
  else # Non-cygwin Unix
    find := find
  endif
else # Windows
  find := /usr/bin/find
  FLEXDLL_SUBMODULE_PRESENT := $(wildcard ../flexdll/Makefile)
  ifeq "$(FLEXDLL_SUBMODULE_PRESENT)" ""
    FLEXLINK_PREFIX=
  else
    ROOT:=$(shell cd .. && pwd| cygpath -m -f -)
    EMPTY=
    FLEXLINK_PREFIX:=OCAML_FLEXLINK="$(ROOT)/boot/ocamlrun \
                                     $(ROOT)/flexdll/flexlink.exe" $(EMPTY)
  endif
endif

failstamp := failure.stamp

ocamltest_directory := ../ocamltest

ocamltest_program := $(or \
  $(wildcard $(ocamltest_directory)/ocamltest.opt$(EXE)),\
  $(wildcard $(ocamltest_directory)/ocamltest$(EXE)))

ocamltest := $(FLEXLINK_PREFIX) $(ocamltest_program)

.PHONY: default
default:
	@echo "Available targets:"
	@echo "  all             launch all tests"
	@echo "  legacy          launch legacy tests"
	@echo "  new             launch new (ocamltest based) tests"
	@echo "  all-foo         launch all tests beginning with foo"
	@echo "  parallel        launch all tests using GNU parallel"
	@echo "  parallel-foo    launch all tests beginning with foo using \
	GNU parallel"
	@echo "  list FILE=f     launch the tests listed in f (one per line)"
	@echo "  one DIR=p       launch the tests located in path p"
	@echo "  promote DIR=p   promote the reference files for the tests in p"
	@echo "  lib             build library modules"
	@echo "  tools           build test tools"
	@echo "  clean           delete generated files"
	@echo "  report          print the report for the last execution"
	@echo
	@echo "all*, parallel* and list can automatically re-run failed test"
	@echo "directories if MAX_TESTSUITE_DIR_RETRIES permits"
	@echo "(default value = $(MAX_TESTSUITE_DIR_RETRIES))"

.PHONY: all
all:
	@rm -f _log
	@$(MAKE) $(NO_PRINT) legacy-without-report
	@$(MAKE) $(NO_PRINT) new-without-report
	@$(MAKE) $(NO_PRINT) report

.PHONY: legacy
legacy:
	@rm -f _log
	@$(MAKE) $(NO_PRINT) legacy-without-report
	@$(MAKE) $(NO_PRINT) report

.PHONY: legacy-without-report
legacy-without-report: lib tools
	@for dir in tests/*; do \
	  $(MAKE) $(NO_PRINT) exec-one DIR=$$dir; \
	done 2>&1 | tee -a _log
	@$(MAKE) $(NO_PRINT) retries

.PHONY: new
new:
	@rm -f _log
	@$(MAKE) $(NO_PRINT) new-without-report
	@$(MAKE) $(NO_PRINT) report

.PHONY: new-without-report
new-without-report: lib tools
	@rm -f $(failstamp)
	@(for file in `$(find) tests -name ocamltests`; do \
	  dir=`dirname $$file`; \
	  echo Running tests from \'$$dir\' ... ; \
	  (IFS=$$(printf "\r\n"); while read testfile; do \
	    TERM=dumb OCAMLRUNPARAM= \
	      $(ocamltest) $$dir/$$testfile || \
	      touch $(failstamp); \
	  done < $$file) || touch $(failstamp); \
	done || touch $(failstamp)) 2>&1 | tee -a _log
	@if [ -f $(failstamp) ]; then rm $(failstamp); exit 1; fi

.PHONY: all-%
all-%: lib tools
	@for dir in tests/$**; do \
	  $(MAKE) $(NO_PRINT) exec-one DIR=$$dir; \
	done 2>&1 | tee _log
	@$(MAKE) $(NO_PRINT) retries
	@$(MAKE) report

# The targets below use GNU parallel to paralellize tests
# 'make all' and 'make parallel' should be equivalent
#
# parallel uses specific logic to make sure the output of the commands
# run in parallel are not mangled. By default, it will reproduce
# the output of each completed command atomically, in order of completion.
#
# With the --keep-order option, we ask it to save the completed output
# and replay them in invocation order instead. In theory this costs
# a tiny bit of performance, but I could not measure any difference.
# In theory again, the reporting logic works fine with test outputs
# coming in in arbitrary order (so we should not need --keep-order),
# but keeping the output deterministic is guaranteed to make
# someone's life easier at least once in the future.
#
# Finally, note that the command we run has a 2>&1 redirection, as
# in the other make targets. If we removed the quoting around
# "$(MAKE) ... 2>&1", the rediction would apply to the complete output
# of parallel, and have a slightly different behavior: by default parallel
# cleanly separates the stdout and stderr output of each completed command,
# printing stderr first then stdout second (for each command).
# I chose to keep the previous behavior exactly unchanged,
# but the demangling separation is arguably nicer behavior that we might
# want to implement at the exec-one level to also have it in the 'all' target.
.PHONY: parallel-%
parallel-%: lib tools
	@echo | parallel >/dev/null 2>/dev/null \
	 || (echo "Unable to run the GNU parallel tool;";\
	     echo "You should install it before using the parallel* targets.";\
	     exit 1)
	@echo | parallel --gnu --no-notice >/dev/null 2>/dev/null \
	 || (echo "Your 'parallel' tool seems incompatible with GNU parallel.";\
	     echo "This target requires GNU parallel.";\
	     exit 1)
	@for dir in tests/$**; do echo $$dir; done \
	 | parallel --gnu --no-notice --keep-order \
	     "$(MAKE) $(NO_PRINT) exec-one DIR={} 2>&1" \
	 | tee _log
	@$(MAKE) $(NO_PRINT) retries
	@$(MAKE) report

.PHONY: parallel
parallel: parallel-*

.PHONY: list
list: lib tools
	@if [ -z "$(FILE)" ]; \
	  then echo "No value set for variable 'FILE'."; \
	  exit 1; \
	fi
	@while read LINE; do \
	  $(MAKE) $(NO_PRINT) exec-one DIR=$$LINE; \
	done <$(FILE) 2>&1 | tee _log
	@$(MAKE) $(NO_PRINT) retries
	@$(MAKE) report

.PHONY: one
one: lib tools
	@if [ -z "$(DIR)" ]; then \
	  echo "No value set for variable 'DIR'."; \
	  exit 1; \
	fi
	@if [ ! -d $(DIR) ]; then \
	  echo "Directory '$(DIR)' does not exist."; \
	  exit 1; \
	fi
	@$(MAKE) $(NO_PRINT) exec-one DIR=$(DIR)

.PHONY: exec-one
exec-one:
	@if [ ! -f $(DIR)/Makefile -a ! -f $(DIR)/ocamltests ]; then \
	  for dir in $(DIR)/*; do \
	    if [ -d $$dir ]; then \
	      $(MAKE) exec-one DIR=$$dir; \
	    fi; \
	  done; \
	elif [ -f $(DIR)/Makefile ]; then \
	  echo "Running tests from '$$DIR' ..."; \
	  cd $(DIR) && \
	  $(MAKE) TERM=dumb BASEDIR=$(BASEDIR) || echo '=> unexpected error'; \
	fi

.PHONY: clean-one
clean-one:
	@if [ ! -f $(DIR)/Makefile ]; then \
	  for dir in $(DIR)/*; do \
	    if [ -d $$dir ]; then \
	      $(MAKE) clean-one DIR=$$dir; \
	    fi; \
	  done; \
	else \
	  cd $(DIR) && $(MAKE) TERM=dumb BASEDIR=$(BASEDIR) clean; \
	fi

.PHONY: promote
promote:
	@if [ -z "$(DIR)" ]; then \
	  echo "No value set for variable 'DIR'."; \
	  exit 1; \
	fi
	@if [ ! -d $(DIR) ]; then \
	  echo "Directory '$(DIR)' does not exist."; \
	  exit 1; \
	fi
	@cd $(DIR) && $(MAKE) TERM=dumb BASEDIR=$(BASEDIR) promote

.PHONY: lib
lib:
	@cd lib && $(MAKE) -s BASEDIR=$(BASEDIR)

.PHONY: tools
tools:
	@cd tools && $(MAKE) -s BASEDIR=$(BASEDIR)

.PHONY: clean
clean:
	@cd lib && $(MAKE) BASEDIR=$(BASEDIR) clean
	@cd tools && $(MAKE) BASEDIR=$(BASEDIR) clean
	@for file in `$(FIND) interactive tests -name Makefile`; do \
	  (cd `dirname $$file` && $(MAKE) BASEDIR=$(BASEDIR) clean); \
	done
	$(FIND) . -name '*_ocamltest*' | xargs rm -rf
	rm -f $(failstamp)

.PHONY: report
report:
	@if [ ! -f _log ]; then echo "No '_log' file."; exit 1; fi
	@awk -f makefiles/summarize.awk <_log

.PHONY: retry-list
retry-list:
	@while read LINE; do \
	  if [ -n "$$LINE" ] ; then \
	    echo re-ran $$LINE>>_log; \
	    $(MAKE) $(NO_PRINT) clean-one DIR=$$LINE; \
	    $(MAKE) $(NO_PRINT) exec-one DIR=$$LINE 2>&1 | tee -a _log ; \
	  fi \
	done <_retries;
	@$(MAKE) $(NO_PRINT) retries

.PHONY: retries
retries:
	@awk -v retries=1 -v max_retries=$(MAX_TESTSUITE_DIR_RETRIES) \
	     -f makefiles/summarize.awk <_log >_retries
	@test `cat _retries | wc -l` -eq 0 || $(MAKE) $(NO_PRINT) retry-list
	@rm -f _retries

.PHONY: empty
empty:
