(* 11+ *)

let rec do_addition buffer state =
	match buffer with
	| Entity.Nbr(n1)::Entity.Nbr(n2)::tl ->
	begin
		match n1 with
		| Nbr.RealInteger(x) ->
		begin
			match n2 with
			| Nbr.RealInteger(x2) -> (Entity.Nbr(Nbr.RealInteger(x + x2)))::tl
			| Nbr.RealFloat(f) -> (Entity.Nbr(Nbr.RealFloat((float_of_int x) + x2)))::tl
		end
		| _ -> raise (Types.Execution_error "")
	end
	| _ -> raise (Types.Execution_error "Error : Can only add two numbers for the moment")

	(* | Entity.Nbr(Nbr.RealInteger(x))::Entity.Nbr(Nbr.RealFloat(f))::fin -> (recu tl (Entity.Nbr((Nbr.RealFloat(f +. (float_of_int x))))::fin)) (* x + f *)
	| Entity.Nbr(Nbr.RealInteger(x1))::Entity.Nbr(Nbr.RealInteger(x2))::fin -> (recu tl (Entity.Nbr(Nbr.RealInteger(x1 + x2))::fin)) (* x + x *)
	| Entity.Nbr(Nbr.RealFloat(f))::Entity.Nbr(Nbr.RealInteger(x))::fin -> (recu tl (Entity.Nbr((Nbr.RealFloat(f +. (float_of_int x))))::fin))
	| Entity.Nbr(Nbr.RealFloat(f1))::Entity.Nbr(Nbr.RealFloat(f2))::fin -> (recu tl (Entity.Nbr(Nbr.RealFloat(f1 +. f2))::fin)) *)
	(* | _ -> () *)


let resolve (expr:Entity.expression) state =
	let rec recu expr (buffer: Entity.entity list) =
		match expr with
		| Entity.Nbr(a)::tl -> recu tl (Entity.Nbr(a)::buffer)
		| Entity.Func(a)::tl -> recu tl (Entity.Func(a)::buffer)
		| Operator(o)::tl ->
			begin
			match o with
			| Addition ->
						begin
						end
			| _ -> ()
			end
		| _ -> ()
		in
	recu expr []
