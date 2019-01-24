BASEDIR=../..

# The -keep_dwarf_unwind option of ld was introduced in ld version 224.1.
# (The last released version where it is not supported is version 136.)
default:
	@printf " ... testing 'unwind_test':"
	@if [ ! $(SYSTEM) = macosx ]; then \
	  echo " => skipped (not on Mac OSX)"; \
	elif $(BYTECODE_ONLY); then \
	  echo " => skipped (bytecode only)"; \
	else \
	  LDFULL="`ld -v 2>&1`"; \
	  LD="`echo $$LDFULL | grep -o \"ld64-[0-9]*\"`"; \
	  LDVER="`echo $$LD | sed \"s/ld64-//\"`"; \
	  if [[ -z "$$LD" ]]; then \
	    echo " => skipped (unknown linker: pattern ld64-[0-9]* not found" \
	    echo "    in 'ld -v' output)"; \
	  elif [[ $$LDVER -lt 224 ]]; then \
	   echo " => skipped (ld version is $$LDVER, only 224 or above " \
	   echo "    are supported)"; \
	  else \
	    $(MAKE) native_macosx_tests; \
	  fi; \
	fi

native_macosx_tests:
	@$(MAKE) clean ; $(MAKE) unwind_test && \
	./unwind_test >/dev/null 2>&1 && echo " => passed" || echo " => failed"

unwind_test:
	@$(OCAMLOPT) -c -opaque mylib.mli
	@$(OCAMLOPT) -c driver.ml
	@$(OCAMLOPT) -c mylib.ml
	@$(OCAMLOPT) -ccopt -I -ccopt $(CTOPDIR)/byterun -c stack_walker.c
	@$(OCAMLOPT) -cclib -Wl,-keep_dwarf_unwind -o unwind_test mylib.cmx \
	             driver.cmx stack_walker.o

clean:
	@rm -f *.cm* *.o unwind_test

include $(BASEDIR)/makefiles/Makefile.common
