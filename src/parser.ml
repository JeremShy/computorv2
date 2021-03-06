  (*
  Returns the type of the expression (VariableDefinition, FunctionDefinition or ExpressionSolving) and the necessary parameters.
  TODO : Add EquationSolving
  *)

let get_operator_priority lexeme =
  if (lexeme#get_type <> Types.Operator) then
    raise (Invalid_argument "get_operator_priority: This lexeme isn't an operator")
  else if lexeme#get_content = "*" || lexeme#get_content = "**" || lexeme#get_content = "/" || lexeme#get_content = "%" then
    1
  else if lexeme#get_content = "^" then
    2
  else if lexeme#get_content = "$" then
    3
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

let create_elem_from_lex_nbr (nbr:Lexeme.lexeme) : Entity.entity = match nbr with
  | hd when hd#get_type = Types.IMultipleFloat	->	Entity.Nbr(Nbr.ComplexNbr(new Ft_complex.complex 0. (float_of_string hd#get_content)))
  | hd when hd#get_type = Types.IMultipleInteger	->	Entity.Nbr(Nbr.ComplexNbr(new Ft_complex.complex 0. (float_of_string hd#get_content)))
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
let get_next_group (lst:Lexeme.lexeme list) : (Lexeme.lexeme list * Lexeme.lexeme list) =
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

let rec convert_op_buffer (param:Lexeme.lexeme list) : Entity.entity list = match param with
  | hd::tail when hd#get_type <> Types.Operator -> raise (Invalid_argument "convert_op_buffer")
  | hd::tail when hd#get_content = "+" -> Entity.Operator(Operator.Addition)::(convert_op_buffer tail)
  | hd::tail when hd#get_content = "-" -> Entity.Operator(Operator.Substraction)::(convert_op_buffer tail)
  | hd::tail when hd#get_content = "/" -> Entity.Operator(Operator.Division)::(convert_op_buffer tail)
  | hd::tail when hd#get_content = "*" -> Entity.Operator(Operator.Multiplication)::(convert_op_buffer tail)
  | hd::tail when hd#get_content = "^" -> Entity.Operator(Operator.Power)::(convert_op_buffer tail)
  | hd::tail when hd#get_content = "**" -> Entity.Operator(Operator.MatrixMultiplication)::(convert_op_buffer tail)
  | hd::tail when hd#get_content = "$" -> Entity.Operator(Operator.FunctionApplication)::(convert_op_buffer tail)
  | hd::tail when hd#get_content = "%" -> Entity.Operator(Operator.Modulo)::(convert_op_buffer tail)
  | hd::tail -> raise (Invalid_argument ("convert_op_buffer 2 -- " ^ hd#get_content))
  | [] -> []

  (*
  **	Polonaise_me : Transforms a list of lexeme into an expression (a list of entities. using recu, which has the following parameters) :
  **		Lexemes : The lexemes not yet processed.
  **		op_buffer : The operand buffer for the algorithm
  **		ret_buffer : The return buffer, which will serv as return value
  **		expecting_operator : boolean, true if an operator is expected (it's in the name, you gotta admit it).
  Alternatively true and false.
  *)

let rec polonaise_me (lexemes:Lexeme.lexeme list) : Entity.expression =
  let rec recu lexemes op_buffer (ret_buffer:Entity.expression) expecting_operator =
    if (expecting_operator = false) then
      begin
        match lexemes with
        | hd::tl when hd#get_type = Types.IMultipleInteger || hd#get_type = Types.IMultipleFloat ||	hd#get_type = Types.RealInteger || hd#get_type = Types.RealFloat
          -> recu tl op_buffer (ret_buffer @ [create_elem_from_lex_nbr hd]) true

        | hd::tl when hd#get_type = Types.String -> recu tl op_buffer (ret_buffer @ [Variable(hd#get_content)]) true
        | hd::tl when hd#get_type = Types.FunctionBeginning ->
          let (next_group, fin) = get_next_group (hd::tl) in
          recu fin op_buffer (ret_buffer@((Entity.Func(hd#get_content))::((polonaise_me next_group) @ [Entity.Operator(Operator.FunctionApplication)]))) true

        | hd::tl when hd#get_type = Types.Symbole && hd#get_content = "(" ->
          let (next_group, fin) = get_next_group (hd::tl) in
          recu fin op_buffer (ret_buffer@(polonaise_me next_group)) true
        | []  -> raise (Types.Parser_error "Expected symbol 1")
        | hd::tl   -> raise (Types.Parser_error ("Expected symbol 2 " ^ (hd#to_string)))
      end
    else
      begin
        match lexemes with
        | hd::tl when hd#get_type = Types.Operator ->
          (*TODO : get_max_priority isn't useful : Your normally can just check the head of op_buffer*)
          if (get_operator_priority hd < get_max_priority op_buffer) then
            recu tl [hd] (ret_buffer  @ (convert_op_buffer op_buffer)) false
          else
            recu tl (hd::op_buffer) ret_buffer false

        (* 5x = 5 * x*)
        | hd::tl when hd#get_type = Types.String || (hd#get_type = Types.Symbole && hd#get_content = "(") -> recu ((new Lexeme.lexeme "*" Types.Operator)::lexemes) op_buffer ret_buffer true

        | hd::tl when hd#get_type = Types.IMultipleInteger || hd#get_type = Types.IMultipleFloat ||	hd#get_type = Types.RealInteger || hd#get_type = Types.RealFloat -> let n = create_elem_from_lex_nbr hd in
          begin match n with
            | Entity.Nbr(n) -> if Nbr.is_negative n then recu ((new Lexeme.lexeme "+" Types.Operator)::lexemes) op_buffer ret_buffer true else raise (Types.Execution_error "Expected operator")
            | _ -> failwith "wtf 129"
          end

        | [] -> ret_buffer @ convert_op_buffer op_buffer
        | _ -> raise (Types.Execution_error "Expected operator")
      end
  in
  recu lexemes [] [] false

let is_function_definition lvalue rvalue = match lvalue with
  | fbeg::variable_name::closing_par::[] when fbeg#get_type = Types.FunctionBeginning
                                           && variable_name#get_type = Types.String
                                           && closing_par#get_type = Types.Symbole && closing_par#get_content = ")" ->
    begin
      match rvalue with
      | inter::[] when inter#get_type = Types.Symbole && inter#get_content = "?" -> None
      | _ -> Some(fbeg#get_content, variable_name#get_content)
    end
  | _ -> None

let is_variable_definition lvalue rvalue = match lvalue with
  | variable_name::[] when variable_name#get_type = Types.String -> Some(variable_name#get_content)
  | _ -> None

let is_expression_solving lvalue rvalue = match rvalue with
  | inter::[] when inter#get_type = Types.Symbole && inter#get_content = "?" -> Some()
  | _ -> None

let is_equation_solving lvalue rvalue : (string * Entity.variable * Entity.entity) option = match lvalue with
  | fbeg::variable_name::closing_par::[] when fbeg#get_type = Types.FunctionBeginning
                                           && variable_name#get_type = Types.String
                                           && closing_par#get_type = Types.Symbole && closing_par#get_content = ")" ->
    (
      match rvalue with
      | nbr::inter::[] when (nbr#get_type = Types.String || nbr#get_type = Types.IMultipleInteger || nbr#get_type = Types.IMultipleFloat || nbr#get_type = Types.RealFloat || nbr#get_type = Types.RealInteger)
                         && (inter#get_type = Types.Symbole && inter#get_content = "?") -> Some(fbeg#get_content, variable_name#get_content,
                                                                                                if nbr#get_type = String then Entity.Variable(nbr#get_content) else (create_elem_from_lex_nbr nbr))
      | _ -> None
    )
  | _ -> None

let parser lexemes =
  if count_equals_symbols lexemes <> 1 then
    raise (Types.Parser_error "Too many or too few '=' symbols were given")
  else
    let (lvalue, rvalue) = get_lvalue_and_rvalue lexemes in
    (* print_endline "lvalue : " ; Utils.print_lex_lst lvalue ;
       print_endline "rvalue : " ; Utils.print_lex_lst rvalue ; *)
    match is_expression_solving lvalue rvalue with
    | Some () -> Entity.ExpressionSolving(polonaise_me lvalue)
    | None ->
      match is_equation_solving lvalue rvalue with
      | Some (fname, unknown_name, value) -> Entity.EquationSolving(fname, unknown_name, value)
      | None ->
        match is_function_definition lvalue rvalue with
        | Some (function_name, variable_name) -> Entity.FunctionDefinition (function_name, variable_name, polonaise_me rvalue)
        | None ->
          match is_variable_definition lvalue rvalue with
          | Some (variable_name) -> Entity.VariableDefinition(variable_name, polonaise_me rvalue)
          | None ->  raise (Types.Parser_error "Not yet handled")
(* let (r, l) = (polonaise_me lvalue,  rvalue) in
   Utils.print_entity_lst r *)
