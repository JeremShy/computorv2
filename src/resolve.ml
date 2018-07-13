(* 11+ *)

let int_power a b =
  int_of_float ((float_of_int a) ** (float_of_int b))

let is_special_op (op:Operator.operator) = function
  | Multiplication | Division | Power | Modulo -> true
  |  ->

let do_simple_op buffer state matrix_compatible int_op float_op matrix_matrix_op matrix_scalar_op operation =
	match buffer with
	| Entity.Nbr(n2)::Entity.Nbr(n1)::tl ->
	begin
		match n1 with
		| Nbr.RealInteger(x) ->
		begin
			match n2 with
			| Nbr.RealInteger(x2) -> (Entity.Nbr(Nbr.RealInteger(int_op x x2)))::tl
			| Nbr.RealFloat(f) -> (Entity.Nbr(Nbr.RealFloat(float_op (float_of_int x) f)))::tl
			| Nbr.IMultipleInteger(xi) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex (float_of_int x) (float_of_int xi))))::tl
			| Nbr.IMultipleFloat(fi) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex (float_of_int x) fi)))::tl
			| Nbr.Matrix(m) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| ComplexNbr(z) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex (float_op (z#get_real_part) (float_of_int x)) z#get_imaginary_part)))::tl
		end
		| Nbr.RealFloat(f) ->
		begin
			match n2 with
			| Nbr.RealInteger(x) -> (Entity.Nbr(Nbr.RealFloat(float_op f (float_of_int x))))::tl
			| Nbr.RealFloat(f2) -> (Entity.Nbr(Nbr.RealFloat(float_op f f2)))::tl
			| Nbr.IMultipleInteger(xi) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex f (float_of_int xi))))::tl
			| Nbr.IMultipleFloat(fi) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex f fi)))::tl
			| Nbr.Matrix(m) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.ComplexNbr(z) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex (float_op z#get_real_part f) z#get_imaginary_part)))::tl
		end
		| Nbr.IMultipleInteger(xi) ->
		begin
			match n2 with
			| Nbr.RealInteger(x) -> (Entity.Nbr(Nbr.IMultipleInteger(int_op xi xi)))::tl
			| Nbr.RealFloat(f) -> (Entity.Nbr(Nbr.IMultipleFloat(float_op (float_of_int xi) f)))::tl
			| Nbr.IMultipleInteger(xi2) -> (Entity.Nbr(Nbr.IMultipleInteger(int_op xi xi2)))::tl
			| Nbr.IMultipleFloat(fi) -> (Entity.Nbr(Nbr.IMultipleFloat(float_op (float_of_int xi) fi)))::tl
			| Nbr.Matrix(m) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.ComplexNbr(z) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex z#get_real_part (float_op (float_of_int xi) z#get_imaginary_part))))::tl
		end
		| Nbr.IMultipleFloat(fi) ->
		begin
			match n2 with
			| Nbr.RealInteger(x) -> (Entity.Nbr(Nbr.IMultipleFloat(float_op fi (float_of_int x))))::tl
			| Nbr.RealFloat(f) -> (Entity.Nbr(Nbr.IMultipleFloat(float_op fi f)))::tl
			| Nbr.IMultipleInteger(xi) -> (Entity.Nbr(Nbr.IMultipleFloat(float_op fi (float_of_int xi))))::tl
			| Nbr.IMultipleFloat(fi2) -> (Entity.Nbr(Nbr.IMultipleFloat(float_op fi fi2)))::tl
			| Nbr.Matrix(m) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.ComplexNbr(z) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex z#get_real_part (float_op fi z#get_imaginary_part))))::tl
		end
		| Nbr.Matrix(m) ->
		begin
			match n2 with
			| Nbr.RealInteger(x) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.RealFloat(f) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.IMultipleInteger(xi) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.IMultipleFloat(fi) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.Matrix(m) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.ComplexNbr(z) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
		end
		| Nbr.ComplexNbr(z) ->
		begin
			match n2 with
			| Nbr.RealInteger(x) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex (float_op z#get_real_part (float_of_int x)) z#get_imaginary_part)))::tl
			| Nbr.RealFloat(f) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex (float_op z#get_real_part f) z#get_imaginary_part)))::tl
			| Nbr.IMultipleInteger(xi) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex z#get_real_part (float_op z#get_imaginary_part (float_of_int xi)))))::tl
			| Nbr.IMultipleFloat(fi) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex z#get_real_part (float_op z#get_imaginary_part fi))))::tl
			| Nbr.Matrix(m) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.ComplexNbr(z2) -> Entity.Nbr(Nbr.ComplexNbr(new Complex.complex (float_op z#get_real_part z2#get_real_part) (float_op z#get_imaginary_part z2#get_imaginary_part)))::tl
		end
	end
	| _ -> raise (Types.Execution_error "Error : Can only add two numbers for the moment")

let rec resolve (expr:Entity.expression) (state:(string, Entity.definable) Hashtbl.t) =
	let rec recu (expr:Entity.expression) (buffer: Entity.bufferable list) =
		match expr with
		| Entity.Nbr(a)::tl -> recu tl (Entity.Nbr(a)::buffer)
		| Entity.Func(a)::tl -> recu tl (Entity.Func(a)::buffer)
		| Operator(o)::tl ->
			begin
			match o with
			| Addition ->
				recu tl (do_simple_op buffer state false ( + ) ( +. ) false false Operator.Addition)
			| Multiplication ->
				recu tl (do_simple_op buffer state false ( * ) ( *. ) false false Operator.Multiplication) (* TODO: Ajouter l'operateur pour l'operation multiplication de matrix et int *)
			| Substraction ->
				recu tl (do_simple_op buffer state false ( - ) ( -. ) false false Operator.Substraction)
			| Division ->
				recu tl (do_simple_op buffer state false ( / ) ( /. ) false false Operator.Division)
			| Modulo ->
				recu tl (do_simple_op buffer state false ( mod ) mod_float false false Operator.Modulo)
			| Power ->
				recu tl (do_simple_op buffer state false int_power ( ** ) false false Operator.Power)
			| MatrixMultiplication ->
				recu tl (do_simple_op buffer state false int_power ( ** ) false false Operator.MatrixMultiplication) (* TODO: Ajouter des fonctions cools pour l'operateur de multiplication de matrix *)
			| FunctionApplication ->
				recu tl (func_operator buffer state)
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
