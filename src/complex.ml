class complex real imaginary =
object
	val mutable _real_part = real
	val mutable _imaginary_part = imaginary

	method get_real_part : float = _real_part
	method get_imaginary_part : float = _imaginary_part
end
