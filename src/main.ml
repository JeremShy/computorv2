let rec computorv2 () =
	print_string "> " ;
	let line = read_line () in
	let lexed_line = Lexer.lexer line in
	Utils.print_lex_lst lexed_line ;
	let parsed_line = Parser.parser lexed_line in
	computorv2 ()

let () =
	try computorv2 () with
		| End_of_file -> print_endline "Goodbye."
