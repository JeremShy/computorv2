let expand_two_complex c1 c2 =
	let (a, b), (c, d) = (c1#expand, c2#expand) in
	(a, b, c, d)

let to_nbr c =
	if (c#get_imaginary_part = 0.) then Nbr.RealFloat(c#get_real_part)
	else ComplexNbr(c)



let add c1 c2 =
	to_nbr (new Complex.complex (c1#get_real_part +. c2#get_real_part) (c1#get_imaginary_part +. c2#get_imaginary_part))

let sub c1 c2 =
	to_nbr (new Complex.complex (c1#get_real_part -. c2#get_real_part) (c1#get_imaginary_part -. c2#get_imaginary_part))

let div c1 c2 =
	let (a, b, c, d) = expand_two_complex c1 c2 in
	to_nbr (
				new Complex.complex
				((a *. c +. b *. d) /. (c ** 2. +. d ** 2.))
				((b *. c -. a *. d) /. (c ** 2. +. d ** 2.))
			)

let mul c1 c2 =
	let (a, b, c, d) = expand_two_complex c1 c2 in
	to_nbr (
				new Complex.complex
				(a *. c -. b *. d)
				(a *. d +. b *. c)
			)
