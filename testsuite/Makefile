#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#                 Xavier Clerc, SED, INRIA Rocquencourt                 #
#                                                                       #
#   Copyright 2010 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

BASEDIR := $(shell pwd)
NO_PRINT=`$(MAKE) empty --no-print-directory >/dev/null 2>&1 \
	  && echo --no-print-directory`

FIND=find
include ../config/Makefile

.PHONY: default
default:
	@echo "Available targets:"
	@echo "  all             launch all tests"
	@echo "  list FILE=f     launch the tests listed in f (one per line)"
	@echo "  one DIR=p       launch the tests located in path p"
	@echo "  promote DIR=p   promote the reference files for the tests in p"
	@echo "  lib             build library modules"
	@echo "  clean           delete generated files"
	@echo "  report          print the report for the last execution"

.PHONY: all
all: lib
	@for dir in tests/*; do \
	  $(MAKE) $(NO_PRINT) exec-one DIR=$$dir; \
	done 2>&1 | tee _log
	@$(MAKE) report

.PHONY: list
list: lib
	@if [ -z "$(FILE)" ]; \
	  then echo "No value set for variable 'FILE'."; \
	  exit 1; \
	fi
	@while read LINE; do \
	  $(MAKE) $(NO_PRINT) exec-one DIR=$$LINE; \
	done <$(FILE) 2>&1 | tee _log
	@$(MAKE) report

.PHONY: one
one: lib
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
	@if [ ! -f $(DIR)/Makefile ]; then \
	  for dir in $(DIR)/*; do \
	    if [ -d $$dir ]; then \
	      $(MAKE) exec-one DIR=$$dir; \
	    fi; \
	  done; \
	else \
	  echo "Running tests from '$$DIR' ..."; \
	  cd $(DIR) && \
	  $(MAKE) TERM=dumb BASEDIR=$(BASEDIR) || echo '=> unexpected error'; \
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

.PHONY: clean
clean:
	@cd lib && $(MAKE) BASEDIR=$(BASEDIR) clean
	@for file in `$(FIND) interactive tests -name Makefile`; do \
	  (cd `dirname $$file` && $(MAKE) BASEDIR=$(BASEDIR) clean); \
	done

.PHONY: report
report:
	@if [ ! -f _log ]; then echo "No '_log' file."; exit 1; fi
	@awk -f makefiles/summarize.awk <_log

.PHONY: empty
empty:
