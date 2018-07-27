type nbr = RealInteger of int | RealFloat of float | ComplexNbr of Ft_complex.complex | Matrix of Matrix.matrix

let is_negative (n:nbr) : bool = match n with
  | RealInteger(i)  -> i < 0
  | RealFloat(f)    -> f < 0.
  | ComplexNbr(c)   -> c#is_negative
  | Matrix(m) -> false
