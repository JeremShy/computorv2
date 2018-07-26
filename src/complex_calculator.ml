let is_int f =
	classify_float (fst (modf f)) = FP_zero

let expand_two_complex c1 c2 =
	let (a, b), (c, d) = (c1#expand, c2#expand) in
	(a, b, c, d)

let to_nbr c =
	if (c#get_imaginary_part = 0.) then Nbr.RealFloat(c#get_real_part)
	else ComplexNbr(c)

let from_nbr = function
	| Nbr.ComplexNbr(c) -> c
	| Nbr.RealFloat(f) -> new Complex.complex f 0.
	| _ -> failwith "Error from_nbr"

let add c1 c2 =
	to_nbr (new Complex.complex (c1#get_real_part +. c2#get_real_part) (c1#get_imaginary_part +. c2#get_imaginary_part))

let sub c1 c2 =
	to_nbr (new Complex.complex (c1#get_real_part -. c2#get_real_part) (c1#get_imaginary_part -. c2#get_imaginary_part))

let div c1 c2 =
	let (a, b, c, d) = expand_two_complex c1 c2 in
	if (c ** 2. +. d ** 2. = 0.) then raise (Types.Execution_error "Division by 0") else
	to_nbr (
				new Complex.complex
				((a *. c +. b *. d) /. (c ** 2. +. d ** 2.))
				((b *. c -. a *. d) /. (c ** 2. +. d ** 2.))
			)

let mul (c1:Complex.complex) (c2:Complex.complex) =
	let (a, b, c, d) = expand_two_complex c1 c2 in
	to_nbr (
				new Complex.complex
				(a *. c -. b *. d)
				(a *. d +. b *. c)
			)

let pow (c1:Complex.complex) (c2:Complex.complex) =
	if c2#get_imaginary_part <> 0. then raise (Types.Execution_error "Can not pow to imaginary number") else
	if not (is_int c2#get_real_part) then raise (Types.Execution_error "Can pow only to natural numbers") else
	let n = (int_of_float c2#get_real_part) in
	let rec recu (c:Complex.complex) (n:int) (acc:Complex.complex) : Complex.complex =
		if n = 0 then acc
		else (recu c (n - 1) (from_nbr (mul c acc)))
	in
	to_nbr (recu c1 n (new Complex.complex 1. 0.))
