(* 11+ *)

let int_power a b =
	int_of_float ((float_of_int a) ** (float_of_int b))

let rec do_simple_op buffer state matrix_compatible int_op float_op matrix_matrix_op matrix_scalar_op =
	match buffer with
	| Entity.Nbr(n1)::Entity.Nbr(n2)::tl ->
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
			| Nbr.RealFloat(f2) -> (Entity.Nbr(Nbr.RealFloat(float_op f f2)))::tl
			| Nbr.IMultipleInteger(xi) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex f (float_of_int xi))))::tl
			| Nbr.IMultipleFloat(fi) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex f fi)))::tl
			| Nbr.Matrix(m) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.ComplexNbr(z) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex (float_op z#get_real_part f) z#get_imaginary_part)))::tl
			| _ -> do_simple_op (Entity.Nbr(n2)::Entity.Nbr(n1)::tl) state matrix_compatible int_op float_op matrix_matrix_op matrix_scalar_op
		end
		| Nbr.IMultipleInteger(xi) ->
		begin
			match n2 with
			| Nbr.IMultipleInteger(xi2) -> (Entity.Nbr(Nbr.IMultipleInteger(int_op xi xi2)))::tl
			| Nbr.IMultipleFloat(fi) -> (Entity.Nbr(Nbr.IMultipleFloat(float_op (float_of_int xi) fi)))::tl
			| Nbr.Matrix(m) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.ComplexNbr(z) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex z#get_real_part (float_op (float_of_int xi) z#get_imaginary_part))))::tl
			| _ -> do_simple_op (Entity.Nbr(n2)::Entity.Nbr(n1)::tl) state matrix_compatible int_op float_op matrix_matrix_op matrix_scalar_op
		end
		| Nbr.IMultipleFloat(fi) ->
		begin
			match n2 with
			| Nbr.IMultipleFloat(fi2) -> (Entity.Nbr(Nbr.IMultipleFloat(float_op fi fi2)))::tl
			| Nbr.Matrix(m) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.ComplexNbr(z) -> (Entity.Nbr(Nbr.ComplexNbr(new Complex.complex z#get_real_part (float_op fi z#get_imaginary_part))))::tl
			| _ -> do_simple_op (Entity.Nbr(n2)::Entity.Nbr(n1)::tl) state matrix_compatible int_op float_op matrix_matrix_op matrix_scalar_op
		end
		| Nbr.Matrix(m) ->
		begin
			match n2 with
			| Nbr.Matrix(m) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| Nbr.ComplexNbr(z) -> raise (Types.Execution_error "Can't use this operator on a matrix.")
			| _ -> raise (Types.Execution_error "Can't use this operator on a matrix.")
		end
		| Nbr.ComplexNbr(z) ->
		begin
			match n2 with
			| Nbr.ComplexNbr(z2) -> Entity.Nbr(Nbr.ComplexNbr(new Complex.complex (float_op z#get_real_part z2#get_real_part) (float_op z#get_imaginary_part z2#get_imaginary_part)))::tl
			| _ -> do_simple_op (Entity.Nbr(n2)::Entity.Nbr(n1)::tl) state matrix_compatible int_op float_op matrix_matrix_op matrix_scalar_op
		end
	end
	| _ -> raise (Types.Execution_error "Error : Can only add two numbers for the moment")


let resolve (expr:Entity.expression) state =
	let rec recu (expr:Entity.expression) (buffer: Entity.bufferable list) =
		match expr with
		| Entity.Nbr(a)::tl -> recu tl (Entity.Nbr(a)::buffer)
		| Entity.Func(a)::tl -> recu tl (Entity.Func(a)::buffer)
		| Operator(o)::tl ->
			begin
			match o with
			| Addition ->
				recu tl (do_simple_op buffer state false ( + ) ( +. ) false false)
			| Multiplication ->
				recu tl (do_simple_op buffer state false ( * ) ( *. ) false false) (* TODO: Ajouter l'operateur pour l'operation multiplication de matrix et int *)
			| Substraction ->
				recu tl (do_simple_op buffer state false ( - ) ( -. ) false false)
			| Division ->
				recu tl (do_simple_op buffer state false ( / ) ( /. ) false false)
			| Modulo ->
				recu tl (do_simple_op buffer state false ( mod ) mod_float false false)
			| Power ->
				recu tl (do_simple_op buffer state false int_power ( ** ) false false)
			| MatrixMultiplication ->
				recu tl (do_simple_op buffer state false int_power ( ** ) false false) (* TODO: Ajouter des fonctions cools pour l'operateur de multiplication de matrix *)
			| _ -> raise (Types.Execution_error "Not yet handled !")
			end
		| Entity.Variable(v)::tl -> raise (Types.Execution_error "Not yet handled !")
		| [] ->
				if (List.length buffer <> 1) then raise (Types.Execution_error "Error while resolving the expression")
				else
				begin
					match (List.hd buffer) with
					| Entity.Func(f) -> raise (Types.Execution_error "A function was not applied")
					| sth -> sth
				end
		in
	recu expr []
