#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                         Alain Frisch, LexiFi                           *
#*                                                                        *
#*   Copyright 2012 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BASEDIR=../..
MODULES=testing misc identifiable numbers strongly_connected_components
INCLUDES= -I $(OTOPDIR)/utils
ADD_COMPFLAGS=$(INCLUDES)
CMO_FILES+="misc.cmo"

include $(BASEDIR)/makefiles/Makefile.several
include $(BASEDIR)/makefiles/Makefile.common

BYTECODE_ONLY=true
