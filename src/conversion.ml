let get_conv_value = function
	| Nbr.RealInteger(_)	-> 0
	| Nbr.RealFloat(_)		-> 1
	| Nbr.ComplexNbr(_)		-> 2
	| Nbr.Matrix(_)			-> 3

let to_real_float = function
	| Nbr.RealInteger(x) -> Nbr.RealFloat(float_of_int x)
	| Nbr.RealFloat(f) -> Nbr.RealFloat(f)
	| _ -> raise (Types.Execution_error "Can't convert this value to float")

let to_complex_nbr = function
	| Nbr.RealInteger(x) -> Nbr.ComplexNbr(new Complex.complex (float_of_int x) 0.)
	| Nbr.RealFloat(f) -> Nbr.ComplexNbr(new Complex.complex f 0.)
	| Nbr.ComplexNbr(c) -> Nbr.ComplexNbr(c)
	| _ -> raise (Types.Execution_error "Can't convert this value to float")

let convert in1 in2 =
	let v1, v2 = (get_conv_value in1, get_conv_value in2) in
	if (v1 = 3 || v2 = 3) then
		(in1, in2) (* Special case of scalar-matrix or matrix-matrix operation. *)
	else
	(
		let maxi = max v1 v2 in
		if maxi = 0 then (in1, in2)
		else if maxi = 1 then (to_real_float in1, to_real_float in2)
		else if maxi = 2 then (to_complex_nbr in1, to_complex_nbr in2)
		else failwith "This case is not supposed to happen !"
	)
