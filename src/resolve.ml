(* 11+ *)

let int_power a b =
  int_of_float ((float_of_int a) ** (float_of_int b))

(*
	* buffer	= the working stack from which n1 and n2 must be extracted
*)
let do_op buffer op =
	match buffer with
	| Entity.Nbr(n2) :: Entity.Nbr(n1) :: tl -> (* The numbers are inverted in the polish stack *)
	begin
		let converted_1, converted_2 = Conversion.convert n1 n2 in
		match op with
		| Operator.Addition			-> Entity.Nbr(Operations.add converted_1 converted_2)::tl
		| Operator.Substraction		-> Entity.Nbr(Operations.sub converted_1 converted_2)::tl
		| Operator.Division			-> Entity.Nbr(Operations.div converted_1 converted_2)::tl
		| Operator.Multiplication	-> Entity.Nbr(Operations.mul converted_1 converted_2)::tl
		| Operator.Power			-> Entity.Nbr(Operations.pow converted_1 converted_2)::tl
		| Operator.Modulo			-> Entity.Nbr(Operations.modulus converted_1 converted_2)::tl
		| _ -> failwith "Not yet handled"
	end
	| _ -> raise (Types.Execution_error "Error 12")

let rec resolve (expr:Entity.expression) (state:(string, Entity.definable) Hashtbl.t) =
	(*
		* expr		= Reverse polish expression to solve
	 	* buffer	= The working stack of the evaluation algorithm
	*)
	let rec recu (expr:Entity.expression) (buffer: Entity.bufferable list) =
		match expr with
		| Entity.Nbr(a)::tl -> recu tl (Entity.Nbr(a)::buffer)
		| Entity.Func(a)::tl -> recu tl (Entity.Func(a)::buffer)
		| Operator(o)::tl ->
			begin
			match o with
			| FunctionApplication -> recu tl (func_operator buffer state)
			| _ -> recu tl (do_op buffer o)
			end
		| Entity.Variable(v)::tl -> begin
										try match (Hashtbl.find state v)
										with
										| Variable(nbr) -> recu tl (Nbr(nbr)::buffer)
										| Func(f) -> raise (Types.Execution_error "Error : This is a function and not a variale")
										with | Not_found -> raise (Types.Execution_error "Variable not defined")
									end
		| [] ->
				if (List.length buffer <> 1) then raise (Types.Execution_error "Error while resolving the expression")
				else
				begin
					match (List.hd buffer) with
					| Entity.Func(f) -> raise (Types.Execution_error "A function was not applied")
					| Entity.Nbr(n) -> n
				end
		in
	recu expr []

and apply_function (f:Entity.func_obj) (value:Nbr.nbr) (old_state:(string, Entity.definable) Hashtbl.t) : Nbr.nbr =
    let state = Hashtbl.copy old_state in
	Hashtbl.replace state f#get_param_name (Entity.Variable(value)) ;
	resolve f#get_expr state
and func_operator (buffer:Entity.bufferable list) (state:(string, Entity.definable) Hashtbl.t) =
	match buffer with
	| Entity.Nbr(n)::Entity.Func(f)::tl ->
	(
		try match Hashtbl.find state f with
		| Entity.Func(f_obj) -> Entity.Nbr(apply_function f_obj n state)::tl
		| Entity.Variable(v) -> raise (Types.Execution_error "Error 70")
		with
		| Not_found -> raise (Types.Execution_error ("Error : Unknown function : " ^ f))
	)
	| _ -> raise (Types.Execution_error "Error 118")
