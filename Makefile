RESULT = computorv2
SOURCES = src/types.ml src/utils.ml src/lexeme.ml src/lexer.ml src/main.ml
LIBS=Str
OCAMLMAKEFILE = OCamlMakefile

-include $(OCAMLMAKEFILE)

re: clean bc
