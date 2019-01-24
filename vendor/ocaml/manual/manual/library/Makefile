CORE_INTF=Pervasives.tex

STDLIB_INTF=Arg.tex Array.tex ArrayLabels.tex Char.tex Complex.tex \
  Digest.tex Filename.tex Format.tex \
  Gc.tex Genlex.tex Hashtbl.tex Int32.tex Int64.tex \
  Lazy.tex Lexing.tex List.tex ListLabels.tex Map.tex Marshal.tex \
  MoreLabels.tex Nativeint.tex Obj.tex Oo.tex \
  Parsing.tex Printexc.tex Printf.tex Queue.tex Random.tex Scanf.tex \
  Set.tex Sort.tex Stack.tex Stream.tex String.tex StringLabels.tex Sys.tex \
  Weak.tex Callback.tex Buffer.tex StdLabels.tex \
  Bytes.tex BytesLabels.tex Spacetime.tex

COMPILER_LIBS_PLUGIN_HOOKS=Pparse.tex Typemod.tex

COMPILER_LIBS_INTF=Asthelper.tex Astmapper.tex Asttypes.tex \
	Lexer.tex Location.tex Longident.tex Parse.tex Pprintast.tex Printast.tex \
	$(COMPILER_LIBS_PLUGIN_HOOKS)

OTHERLIB_INTF=Unix.tex UnixLabels.tex Str.tex \
  Graphics.tex GraphicsX11.tex \
  Thread.tex Mutex.tex Condition.tex Event.tex ThreadUnix.tex \
  Dynlink.tex Bigarray.tex


INTF=$(CORE_INTF) $(STDLIB_INTF) $(COMPILER_LIBS_INTF) $(OTHERLIB_INTF)

MLIS=$(CSLDIR)/stdlib/*.mli \
	$(CSLDIR)/utils/*.mli \
	$(CSLDIR)/parsing/*.mli \
	$(CSLDIR)/driver/pparse.mli \
	$(CSLDIR)/typing/typemod.mli \
	$(CSLDIR)/bytecomp/simplif.mli \
	$(CSLDIR)/otherlibs/bigarray/bigarray.mli \
	$(CSLDIR)/otherlibs/dynlink/dynlink.mli \
	$(CSLDIR)/otherlibs/graph/graphics.mli \
	$(CSLDIR)/otherlibs/graph/graphicsX11.mli \
	$(CSLDIR)/otherlibs/str/*.mli \
	$(CSLDIR)/otherlibs/systhreads/*.mli \
	$(CSLDIR)/otherlibs/unix/*.mli

BLURB=core.tex builtin.tex stdlib.tex compilerlibs.tex \
  libunix.tex libstr.tex libnum.tex libgraph.tex \
  libthreads.tex libdynlink.tex libbigarray.tex

FILES=$(BLURB) $(INTF)

FORMAT=../../tools/format-intf
TEXQUOTE=../../tools/texquote2

CSLDIR=$(RELEASEDIR)

VPATH=.:$(CSLDIR)/stdlib:$(CSLDIR)/parsing:$(CSLDIR)/otherlibs/unix:$(CSLDIR)/otherlibs/str:$(CSLDIR)/otherlibs/graph:$(CSLDIR)/otherlibs/threads:$(CSLDIR)/otherlibs/dynlink:$(CSLDIR)/otherlibs/bigarray

etex-files: $(BLURB)
all: libs
	sh ./check-stdlib-modules $(CSLDIR)

libs: $(FILES)

OCAMLDOC=$(if $(wildcard $(CSLDIR)/ocamldoc/ocamldoc.opt),\
  $(CSLDIR)/ocamldoc/ocamldoc.opt,\
  $(CSLDIR)/byterun/ocamlrun $(CSLDIR)/ocamldoc/ocamldoc)

$(INTF): interfaces
interfaces: $(MLIS)
	$(OCAMLDOC) -latex \
	-I $(CSLDIR)/utils \
	-I $(CSLDIR)/stdlib \
	-I $(CSLDIR)/parsing \
	-I $(CSLDIR)/typing \
	-I $(CSLDIR)/driver \
	-I $(CSLDIR)/bytecomp \
	-I $(CSLDIR)/otherlibs/bigarray \
	-I $(CSLDIR)/otherlibs/dynlink \
	-I $(CSLDIR)/otherlibs/graph \
	-I $(CSLDIR)/otherlibs/str \
	-I $(CSLDIR)/otherlibs/systhreads \
	-I $(CSLDIR)/otherlibs/unix \
	$(MLIS) \
	-sepfiles \
	-latextitle "1,subsection*" \
	-latextitle "2,subsubsection*" \
	-latex-type-prefix "TYP" \
	-latex-module-prefix "" \
	-latex-module-type-prefix "" \
	-latex-value-prefix ""
	mv -f Ast_helper.tex Asthelper.tex
	mv -f Ast_mapper.tex Astmapper.tex

clean:
	rm -f $(FILES)


.SUFFIXES:
.SUFFIXES: .tex .etex .mli

.etex.tex: $(TEXQUOTE)
	@$(TEXQUOTE) < $*.etex > $*.texquote_error.tex\
	&& cp $*.texquote_error.tex $*.tex\
	|| printf "Failure when generating %s\n" $*.tex


.mli.tex: $(FORMAT)
	$(FORMAT) $< > $*.tex < $<
