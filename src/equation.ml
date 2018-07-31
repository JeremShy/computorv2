type monome = (Nbr.nbr * int) (* coefficient and power *)
type polynome = monome list (* Polynome = monome + monome + ... *)

(*
  12x*+3x4^*+

  1 + 2x
  +
  3 * x^4

*)

let equation_to_polynome (equation:Entity.expression) : polynome =
  []

let equation (params:Entity.operation_type) (state:(string, Entity.definable) Hashtbl.t) =
  match params with
  | EquationSolving(func_name, var_name, rez) ->
  begin
    let rezval =
      match rez with
      | Variable(v) ->
        begin
          try match Hashtbl.find state v with
          | Func(f) -> raise (Types.Execution_error "Can't solve with a function as rvalue.")
          | Variable(vnbr) -> vnbr
          with  | Not_found -> raise (Types.Execution_error ("Unknown definable : " ^ v))
        end
      | Entity.Nbr(n) -> n
      | _ -> raise (Types.Execution_error "EquationSolving error 15")
    in
    print_string "Rezval : " ; Utils.print_nbr rezval ;
    let func =
      try match Hashtbl.find state func_name with
          | Func(f) -> f
          | Variable(vnbr) -> raise (Types.Execution_error "Not a function")
          with  | Not_found -> raise (Types.Execution_error ("Unknown function : " ^ func_name))
      in
      print_string "Function : " ; Utils.print_entity_lst (func#get_expr) ;
      if (func#get_param_name <> var_name) then
        raise (Types.Execution_error "Wrong variable name for this function")
      ;
      let to_solve : Entity.expression = (func#get_expr) @ (Entity.Operator(Operator.Substraction) :: Entity.Nbr(rezval) :: []) in
      print_string "to_solve : " ; Utils.print_entity_lst (to_solve)
  end
  | _ -> raise (Types.Execution_error "Equation Solving error")
