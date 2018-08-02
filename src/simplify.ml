let get_priority op = match op with
  | (Operator.Addition | Operator.Substraction) -> 1
  | (Operator.Multiplication | Operator.Division| Operator.Modulo | Operator.MatrixMultiplication) -> 2
  | (Operator.Power) -> 3
  | Operator.FunctionApplication -> 4


(*
  Simplify must follow a chain of same priority operators and concatenate the numbers.

  We need a function that must take a number, an operator, and a tree. This function will return the tree with the necessary concatenation.
  So if we have something like:

    +
  1     +
    ..    2

it will output


      +
  ..    3


  Sth like:
  create_tree nbr op tree =
  if tree is a leaf ->
    if tree is a number -> (do op nbr number op)
      else



*)

(* let simplify (node:Ast.node) : Ast.node =
  let rec do_simplify node priority =
  do_simplify node (get_priority Operator.Addition) ;
  raise (Types.Execution_error "Not yet handled") *)
