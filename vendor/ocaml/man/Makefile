#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

include ../config/Makefile

INSTALL_DIR=$(DESTDIR)$(MANDIR)/man$(MANEXT)

install:
	for i in *.m; do cp $$i $(INSTALL_DIR)/`basename $$i .m`.$(MANEXT); done
	echo '.so man$(MANEXT)/ocamlc.$(MANEXT)' \
	     > $(INSTALL_DIR)/ocamlc.opt.$(MANEXT)
	echo '.so man$(MANEXT)/ocamlopt.$(MANEXT)' \
	     > $(INSTALL_DIR)/ocamlopt.opt.$(MANEXT)
	echo '.so man$(MANEXT)/ocamlcp.$(MANEXT)' \
	     > $(INSTALL_DIR)/ocamloptp.$(MANEXT)
