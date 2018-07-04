let lex_type_to_string = function
	| Types.Operator -> "Operator"
	| Types.Symbole -> "Symbole"
	| Types.String -> "String"
	| Types.RealInteger -> "RealInteger"
	| Types.RealFloat -> "RealFloat"
	| Types.IMultipleInteger -> "IMultipleInteger"
	| Types.IMultipleFloat -> "IMultipleFloat"

let print_lex_lst lst =
	List.iter (fun x -> Printf.printf "%s - " x#to_string) lst ; print_char '\n'
