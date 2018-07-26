RESULT = computorv2
SOURCES = src/types.ml src/utils.ml \
			src/lexeme.ml src/lexer.ml \
			src/complex.ml src/matrix.ml src/nbr.ml src/operator.ml src/entity.ml src/parser.ml \
			src/complex_calculator.ml src/conversion.ml src/operations.ml src/resolve.ml \
			src/main.ml
LIBS=Str
OCAMLMAKEFILE = OCamlMakefile

-include $(OCAMLMAKEFILE)

fclean: clean

re: clean bc
