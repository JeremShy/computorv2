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

let print_nbr (nbr:Nbr.nbr) =
	match nbr with
	| RealInteger x  -> print_int x
	| RealFloat x -> print_float x
	| IMultipleFloat x -> print_float x ; print_char 'i'
	| IMultipleInteger x -> print_int x ; print_char 'i'
	| Matrix m -> raise (Invalid_argument "Not yet handled")
	| ComplexNbr c -> print_string (c#describe)
	

let rec print_entity_lst (lst:Entity.entity list) =
	match lst with
	| Entity.Nbr(elem)::tl -> begin
								match elem with
									| RealInteger x  -> print_int x
									| RealFloat x -> print_float x
									| IMultipleFloat x -> print_float x ; print_char 'i'
									| IMultipleInteger x -> print_int x ; print_char 'i'
									| Matrix m -> raise (Invalid_argument "Not yet handled")
									| ComplexNbr c -> print_string (c#describe)
								end ; print_entity_lst tl
	| Operator(elem)::tl -> begin
								match elem with
								| Addition -> print_string "+"
								| Multiplication -> print_string "*"
								| Substraction -> print_string "-"
								| Modulo -> print_string "%"
								| Division -> print_string "/"
								| Power -> print_string "^"
								| MatrixMultiplication -> print_string "**"
								| FunctionApplication -> print_string "$"
                          end ; print_entity_lst tl
 	| Variable(name)::tl -> print_string name ; print_entity_lst tl
 	| Func(f)::tl -> print_string f ; print_entity_lst tl
	| [] -> print_newline ()
