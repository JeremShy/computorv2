(* 11+ *)

let resolve (expr:Entity.expression) state =
	let rec recu expr (buffer: Entity.entity list) =
		match expr with
		| Entity.Nbr(a)::tl -> recu tl (Entity.Nbr(a)::buffer)
		| Entity.Func(a)::tl -> recu tl (Entity.Func(a)::buffer)
		(* | Operator(o)::tl ->
			match o with
			| Addition -> *)
		| _ -> ()
		in
	recu expr []
