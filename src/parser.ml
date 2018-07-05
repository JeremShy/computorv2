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

let polonaise_me lexemes =
	let rec recu lexemes op_buffer =
		()
	in
	recu lexemes []


let parser lexemes =
	if count_equals_symbols lexemes <> 1 then
		raise (Types.Parser_error "Too many or too few '=' symbols were given")
	else
		let (lvalue, rvalue) = get_lvalue_and_rvalue lexemes in
	print_endline "lvalue : " ; Utils.print_lex_lst lvalue ;
	print_endline "rvalue : " ; Utils.print_lex_lst rvalue ;
	(polonaise_me lvalue, polonaise_me rvalue)
