let rec computorv2 (state:(string, Entity.definable) Hashtbl.t) =
	print_string "> " ;
	let line = read_line () in
	let lexed_line = Lexer.lexer line in
	Utils.print_lex_lst lexed_line ;
	let parsed_line = Parser.parser lexed_line in
	begin
		match parsed_line with
		| Entity.FunctionDefinition (name, parameter, expr) -> print_endline ("Defining function " ^ name ^ " with variable " ^ parameter ^ " and expression : ") ; Utils.print_entity_lst expr
		| Entity.VariableDefinition (name, expr) -> print_endline ("Defining variable " ^ name ^ " = "); Utils.print_entity_lst expr
		| _ -> print_endline "Not yet handled"
	end ;
	computorv2 state

let () =
	let state = Hashtbl.create 32 in
	try computorv2 state with
		| End_of_file -> print_endline "Goodbye."
