
INCLUDES = macros.prehtml version.prehtml

OCAMLGRAPHHOME = /users/www-perso/projets/ocamlgraph

ALL = index.fr.html  index.en.html

install: all
	scp $(ALL) filliatr@acces:$(OCAMLGRAPHHOME)

all: $(ALL)

clean::
	rm -f *~ *.fr.html *.en.html

YAMLPP=yamlpp macros.prehtml version.prehtml

version.prehtml:
	make -C .. www/version.prehtml

# RULES

.SUFFIXES: .html .prehtml .fr.html .en.html

%.fr.html: %.prehtml $(INCLUDES)
	$(YAMLPP) -l fr $< -o $@

%.html: %.prehtml $(INCLUDES)
	$(YAMLPP) $< -o $@

%.en.html: %.prehtml $(INCLUDES)
	$(YAMLPP) -l en $< -o $@

