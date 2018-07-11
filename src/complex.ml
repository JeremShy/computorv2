class complex (real:float) (imaginary:float) =
object
	val mutable _real_part = real
	val mutable _imaginary_part = imaginary

	method get_real_part = _real_part
	method get_imaginary_part = _imaginary_part
	method describe = "(" ^ (string_of_float _real_part) ^ " + " ^ (string_of_float _imaginary_part) ^ "i)"
end
