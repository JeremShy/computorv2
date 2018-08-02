RESULT = computorv2
SOURCES = src/types.ml src/utils.ml \
			src/lexeme.ml src/lexer.ml \
			src/ft_complex.ml src/matrix.ml src/nbr.ml src/operator.ml src/entity.ml src/parser.ml \
			src/complex_calculator.ml src/conversion.ml src/operations.ml src/resolve.ml \
			src/ast.ml src/draw.ml src/simplify.ml \
			src/equation.ml \
			src/main.ml
LIBS=Str Graphics
OCAMLMAKEFILE = OCamlMakefile

-include $(OCAMLMAKEFILE)

auto-ocp-indent: $(SOURCES)
	for mlfile in $^; do ocp-indent -i $$mlfile; done

fclean: clean

re: clean bc
