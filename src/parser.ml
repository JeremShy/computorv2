(*
	Returns a pair of two list of elements ordered in RPN, the lvalue and the rvalue
	There can't be any symbol in this list, but there can be operators.
*)

let get_operator_priority lexeme =
	if (lexeme#get_type <> Types.Operator) then
		raise (Invalid_argument "get_operator_priority: This lexeme isn't an operator")
	else if lexeme#get_content = "*" || lexeme#get_content = "**" || lexeme#get_content = "/" || lexeme#get_content = "%" then
		1
	else if lexeme#get_content = "^" then
		2
	else
		0


let count_equals_symbols lst =
	let rec recu lst n =
		match lst with
		| hd::tl when (hd#get_type = Types.Symbole && hd#get_content = "=") -> recu tl (n + 1)
		| hd::tl -> recu tl n
		| [] -> n
	in
	recu lst 0

let get_lvalue_and_rvalue lst =
	let rec recu lst lvalue rvalue inlvalue =
		match lst with
		| hd::tl when (hd#get_type = Types.Symbole && hd#get_content = "=") -> recu tl lvalue [] false
		| hd::tl -> if inlvalue = true then
						recu tl (lvalue @ [hd])	[] 				inlvalue else
						recu tl lvalue 			(rvalue @ [hd])	inlvalue
			| [] -> (lvalue, rvalue)
	in
	recu lst [] [] true

let create_elem_from_lex_nbr = function
	| hd when hd#get_type = Types.IMultipleFloat	->	Entity.Nbr(Nbr.IMultipleFloat(float_of_string hd#get_content))
	| hd when hd#get_type = Types.IMultipleInteger	->	Entity.Nbr(Nbr.IMultipleInteger(int_of_string hd#get_content))
	| hd when hd#get_type = Types.RealFloat			->	Entity.Nbr(Nbr.RealFloat(float_of_string hd#get_content))
	| hd when hd#get_type = Types.RealInteger		->	Entity.Nbr(Nbr.RealInteger(int_of_string hd#get_content))
	| _ -> raise (Invalid_argument "create_elem_from_lex_nbr: argument is not a numeric lexeme")

let get_max_priority operator_lst =
	let rec recu operator_lst maxi =
		match operator_lst with
		| hd::tail -> if get_operator_priority hd > maxi then recu tail (get_operator_priority hd) else recu tail maxi
		| [] -> maxi
	in
	recu operator_lst 0

(* Returns a pair containing the next group, and the rest of the list *)
let get_next_group lst : (Lexeme.lexeme list * Lexeme.lexeme list) =
	let rec recu lst buffer lvl =
		match lst with
		| hd::tail when ((hd#get_type = Types.Symbole && hd#get_content = "(") || (hd#get_type = Types.FunctionBeginning)) ->
			if (lvl = 0) then
				recu tail [] 1
			else
				recu tail (buffer @ [hd]) (lvl + 1)
		| hd::tail when  (hd#get_type = Types.Symbole && hd#get_content = ")") ->
			if (lvl = 1) then
				(buffer, tail)
			else if (lvl = 0) then
				raise (Invalid_argument "get_next_group: wtf")
			else
				recu tail (buffer @ [hd]) (lvl - 1)
		| hd::tail -> if lvl = 0 then ([hd], tail) else recu tail (buffer @ [hd]) lvl
		| [] -> raise (Invalid_argument "Unexpected end of string")
	in
	recu lst [] 0

let rec convert_op_buffer = function
	| hd::tail when hd#get_type <> Types.Operator -> raise (Invalid_argument "convert_op_buffer")
	| hd::tail when hd#get_content = "+" -> Entity.Operator(Operator.Addition)::(convert_op_buffer tail)
	| hd::tail when hd#get_content = "-" -> Entity.Operator(Operator.Substraction)::(convert_op_buffer tail)
	| hd::tail when hd#get_content = "/" -> Entity.Operator(Operator.Division)::(convert_op_buffer tail)
	| hd::tail when hd#get_content = "*" -> Entity.Operator(Operator.Multiplication)::(convert_op_buffer tail)
	| hd::tail when hd#get_content = "^" -> Entity.Operator(Operator.Power)::(convert_op_buffer tail)
	| hd::tail when hd#get_content = "**" -> Entity.Operator(Operator.MatrixMultiplication)::(convert_op_buffer tail)
	| hd::tail -> raise (Invalid_argument ("convert_op_buffer 2 -- " ^ hd#get_content))
	| [] -> []

let rec polonaise_me lexemes =
	let rec recu lexemes op_buffer ret_buffer expecting_operator =
		match lexemes with
		| hd::tl when hd#get_type = Types.IMultipleInteger || hd#get_type = Types.IMultipleFloat
						||	hd#get_type = Types.RealInteger || hd#get_type = Types.RealFloat -> recu tl op_buffer (ret_buffer @ [create_elem_from_lex_nbr hd]) true
		(* | hd::tl when hd#get_type = Types.String *)
		| hd::tl when hd#get_type = Types.Operator -> if (get_operator_priority hd > get_max_priority op_buffer) then
														let (next_group, fin) = get_next_group tl in
														ret_buffer @ (polonaise_me next_group) @ (convert_op_buffer (hd::op_buffer)) @ polonaise_me fin
													else
														recu tl (op_buffer @ [hd]) ret_buffer false
		| [] -> ret_buffer @ convert_op_buffer op_buffer
		| _ -> raise (Invalid_argument "Not yet handled")
	in
	recu lexemes [] [] false


let parser lexemes =
	if count_equals_symbols lexemes <> 1 then
		raise (Types.Parser_error "Too many or too few '=' symbols were given")
	else
		let (lvalue, rvalue) = get_lvalue_and_rvalue lexemes in
	print_endline "lvalue : " ; Utils.print_lex_lst lvalue ;
	print_endline "rvalue : " ; Utils.print_lex_lst rvalue ;
	let (r, l) = (polonaise_me lvalue,  rvalue) in
	Utils.print_entity_lst r
