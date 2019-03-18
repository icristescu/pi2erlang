#
# Pure OCaml, no packages, no _tags
#

# bin-annot is required for Merlin and other IDE-like tools

.PHONY: 	all clean byte native profile debug test

OCB_FLAGS = -tag bin_annot -use-ocamlfind
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native byte # profile debug

clean:
			$(OCB) -clean

native:
			$(OCB) trad.native

byte:
			$(OCB) trad.byte

profile:
			$(OCB) -tag profile trad.native

debug:
			$(OCB) -tag debug trad.byte

test: 		native
			./trad.native "OCaml" "OCamlBuild" "users"
