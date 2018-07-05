let lex_type_to_string = function
	| Types.Operator -> "Operator"
	| Types.Symbole -> "Symbole"
	| Types.String -> "String"
	| Types.RealInteger -> "RealInteger"
	| Types.RealFloat -> "RealFloat"
	| Types.IMultipleInteger -> "IMultipleInteger"
	| Types.IMultipleFloat -> "IMultipleFloat"
	| Types.FunctionBeginning -> "FunctionBeginning"

let print_lex_lst lst =
	print_newline () ;
	List.iter (fun x -> print_endline x#to_string) lst ; print_newline ()
