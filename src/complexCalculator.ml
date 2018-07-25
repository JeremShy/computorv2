let add n1 n2 = match n2 with
	| Nbr.RealInteger(x) -> new Complex.complex ((n1#get_real_part) +. (float_of_int x)) n1#get_imaginary_part
	| Nbr.RealFloat(f) -> new Complex.complex ((n1#get_real_part) +. f) n1#get_imaginary_part
	| Nbr.IMultipleInteger(xi) -> new Complex.complex n1#get_real_part (n1#get_imaginary_part +. (float_of_int xi))
	| Nbr.IMultipleFloat(fi) -> new Complex.complex n1#get_real_part (n1#get_imaginary_part +. fi)
	| Nbr.ComplexNbr(c) -> new Complex.complex (n1#get_real_part +. c#get_real_part) (n1#get_imaginary_part +. c#get_imaginary_part)
	| _ -> failwith "a"

let div_cs c s = match s with
	| Nbr.RealInteger(x) -> new Complex.complex ((c#get_real_part) /. (float_of_int x)) c#get_imaginary_part
	| Nbr.RealFloat(f) -> new Complex.complex ((c#get_real_part) /. f) c#get_imaginary_part
	| Nbr.IMultipleInteger(xi) -> new Complex.complex c#get_real_part (c#get_imaginary_part +. (float_of_int xi))
	| Nbr.IMultipleFloat(fi) -> new Complex.complex c#get_real_part (c#get_imaginary_part +. fi)
	| Nbr.ComplexNbr(c) -> new Complex.complex (c#get_real_part +. c#get_real_part) (c#get_imaginary_part +. c#get_imaginary_part)
	| _ -> failwith "a"

let do_op n1 n2 op =
	match n1 with
	| ComplexNbr(c) ->	(*complex op simple*)
		begin
			match op with
			| Operator.Addition -> add n1 n2
			| _ -> failwith "error"
		end
	| _ ->				(* simple op complex*)
		begin
			match op with
			| Operator.Addition -> add n1 n2
			| _ -> failwith "error"
		end
