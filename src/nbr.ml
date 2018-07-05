type simple_nbr = RealInteger of int | RealFloat of float | IMultipleInteger of int | IMultipleFloat of float


(* Stub object *)
class matrix =
object
end

type complicated_nbr = SimpleNbr of simple_nbr | Matrix of matrix
type nbr = RealInteger of int | RealFloat of float | IMultipleInteger of int | IMultipleFloat of float | Matrix of matrix

let complicated_nbr_to_nbr (cnbr:complicated_nbr) : nbr =
 	match cnbr with
	| SimpleNbr snbr -> begin
							match snbr with
							| RealInteger n -> RealInteger(n)
							| RealFloat n -> RealFloat(n)
							| IMultipleInteger n -> IMultipleInteger(n)
							| IMultipleFloat n -> IMultipleFloat(n)
						end
	| Matrix m -> Matrix m
