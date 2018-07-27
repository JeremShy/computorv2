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
  | Nbr.RealFloat(f) -> new Ft_complex.complex f 0.
  | _ -> failwith "Error from_nbr"

let add c1 c2 =
  to_nbr (new Ft_complex.complex (c1#get_real_part +. c2#get_real_part) (c1#get_imaginary_part +. c2#get_imaginary_part))

let sub c1 c2 =
  to_nbr (new Ft_complex.complex (c1#get_real_part -. c2#get_real_part) (c1#get_imaginary_part -. c2#get_imaginary_part))

let div c1 c2 =
  let (a, b, c, d) = expand_two_complex c1 c2 in
  if (c ** 2. +. d ** 2. = 0.) then raise (Types.Execution_error "Division by 0") else
    to_nbr (
      new Ft_complex.complex
        ((a *. c +. b *. d) /. (c ** 2. +. d ** 2.))
        ((b *. c -. a *. d) /. (c ** 2. +. d ** 2.))
    )

let mul (c1:Ft_complex.complex) (c2:Ft_complex.complex) =
  let (a, b, c, d) = expand_two_complex c1 c2 in
  to_nbr (
    new Ft_complex.complex
      (a *. c -. b *. d)
      (a *. d +. b *. c)
  )

let pow (c1:Ft_complex.complex) (c2:Ft_complex.complex) =
  if c2#get_imaginary_part <> 0. then raise (Types.Execution_error "Can not pow to imaginary number") else
  if not (is_int c2#get_real_part) then raise (Types.Execution_error "Can pow only to natural numbers") else
    let n = (int_of_float c2#get_real_part) in
    let rec recu (c:Ft_complex.complex) (n:int) (acc:Ft_complex.complex) : Ft_complex.complex =
      if n = 0 then acc
      else (recu c (n - 1) (from_nbr (mul c acc)))
    in
    to_nbr (recu c1 n (new Ft_complex.complex 1. 0.))
