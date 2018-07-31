type leaf = Nbr of Nbr.nbr | Func of Entity.func | Variable of Entity.variable
type node = Leaf of leaf | Node of Operator.operator * node * node
type stack_elem = Node_elem of node | Entity_elem of leaf

(*
  Par example,  12+ => Node(+, 1, 2)
                123*+ => Node(+, 1, Node( *, 2, 3 ))
*)



let ast_from_expr (expr:Entity.expression) : node =
  let rec recu (expr:Entity.expression) (stack:stack_elem list)  : stack_elem list =
    match expr with
    | [] -> stack
    | Entity.Nbr(n)::tl -> recu tl (Entity_elem(Nbr(n)) :: stack)
    | Entity.Variable(n)::tl -> recu tl (Entity_elem(Variable(n)) :: stack)
    | Entity.Operator(op)::tl ->
    (
      match stack with
      | Entity_elem(n2)::Entity_elem(n1)::stack_tl ->
        recu tl (Node_elem(Node(op, Leaf(n1), Leaf(n2)))::stack_tl)
      | Node_elem(n2)::Entity_elem(n1)::stack_tl ->
        recu tl (Node_elem(Node(op, Leaf(n1), n2))::stack_tl)
      | Entity_elem(n2)::Node_elem(n1)::stack_tl ->
        recu tl (Node_elem(Node(op, n1, Leaf(n2)))::stack_tl)
      | Node_elem(n2)::Node_elem(n1)::stack_tl ->
        recu tl (Node_elem(Node(op, n1, n2))::stack_tl)
      | _ -> raise (Types.Execution_error "Unexpected operator")
    )
    | _ -> []
  in
  let rez = recu expr [] in
  match rez with
  | Node_elem(n)::[] -> n
  | _ -> raise (Types.Execution_error "Unexpected eof")
