type leaf = Nbr of Nbr.nbr | Func of Entity.func | Variable of Entity.variable
type node = Leaf of leaf | Node of Operator.operator * node * node

(*
  Par example,  12+ => Node(+, 1, 2)
                123*+ => Node(+, 1, Node( *, 2, 3 ))
*)

let ast_from_expr (expr:Entity.expression) : node =
  raise (Types.Execution_error "Not yet handled")
