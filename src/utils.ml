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
  (
    match nbr with
    | RealInteger x  -> print_int x
    | RealFloat x -> print_float x
    | Matrix m -> raise (Invalid_argument "Not yet handled")
    | ComplexNbr c -> print_string (c#describe)
  ) ; print_newline ()

let operator_to_string = function
  | Operator.Addition -> "+"
  | Operator.Multiplication -> "*"
  | Operator.Substraction -> "-"
  | Operator.Modulo -> "%"
  | Operator.Division -> "/"
  | Operator.Power -> "^"
  | Operator.MatrixMultiplication -> "**"
  | Operator.FunctionApplication -> "$"

let rec print_entity_lst (lst:Entity.entity list) =
  match lst with
  | Entity.Nbr(elem)::tl -> begin
      match elem with
      | RealInteger x  -> print_int x
      | RealFloat x -> print_float x
      | Matrix m -> raise (Invalid_argument "Not yet handled")
      | ComplexNbr c -> print_string (c#describe)
    end ; print_entity_lst tl
  | Operator(elem)::tl -> print_string (operator_to_string elem) ; print_entity_lst tl
  | Variable(name)::tl -> print_string name ; print_entity_lst tl
  | Func(f)::tl -> print_string f ; print_entity_lst tl
  | [] -> print_newline ()
