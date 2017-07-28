#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1997 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU General Public License.                  #
#                                                                       #
#########################################################################

include ../config/Makefile

# Files to install
FILES=	caml-font.el caml-hilit.el caml.el camldebug.el \
	inf-caml.el caml-compat.el caml-help.el caml-types.el \
	caml-xemacs.el caml-emacs.el

# Where to install. If empty, automatically determined.
#EMACSDIR=

# Name of Emacs executable
EMACS=emacs

# Where to install ocamltags script
SCRIPTDIR = $(BINDIR)

# Command for byte-compiling the files
COMPILECMD=(progn \
	      (setq load-path (cons "." load-path)) \
	      (byte-compile-file "caml-xemacs.el") \
	      (byte-compile-file "caml-emacs.el") \
	      (byte-compile-file "caml.el") \
	      (byte-compile-file "inf-caml.el") \
	      (byte-compile-file "caml-help.el") \
	      (byte-compile-file "caml-types.el") \
	      (byte-compile-file "caml-font.el") \
	      (byte-compile-file "camldebug.el"))

install:
	@if test "$(EMACSDIR)" = ""; then \
	  $(EMACS) --batch --eval 't; see PR#5403'; \
	  set xxx `($(EMACS) --batch --eval "(mapcar 'print load-path)") \
				2>/dev/null | \
	           sed -n -e 's/^"\(.*\/site-lisp\).*/\1/gp' | \
		   sort -u`; \
	  if test "$$2" = "" -o "$$3" != ""; then \
	    echo "Cannot determine Emacs site-lisp directory:"; \
            shift; while test "$$1" != ""; do echo "\t$$1"; shift; done; \
	  else \
	  $(MAKE) EMACSDIR="$$2" simple-install; \
	  fi; \
	else \
	  $(MAKE) simple-install; \
	fi

# install the .el files, but do not compile them.
install-el:
	$(MAKE) NOCOMPILE=true install

simple-install:
	@echo "Installing in $(EMACSDIR)..."
	if test -d $(EMACSDIR); then : ; else mkdir -p $(EMACSDIR); fi
	cp $(FILES) $(EMACSDIR)
	if [ -z "$(NOCOMPILE)" ]; then \
	  cd $(EMACSDIR); $(EMACS) --batch --eval '$(COMPILECMD)'; \
	fi

ocamltags:	ocamltags.in
	sed -e 's:@EMACS@:$(EMACS):' ocamltags.in >ocamltags
	chmod a+x ocamltags

install-ocamltags: ocamltags
	cp ocamltags $(SCRIPTDIR)/ocamltags

# This is for testing purposes
compile-only:
	$(EMACS) --batch --eval '$(COMPILECMD)'

clean:
	rm -f ocamltags *~ \#*# *.elc
