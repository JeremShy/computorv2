let () =
	Utils.print_lex_lst (Lexer.lexer (Array.get Sys.argv 1))
