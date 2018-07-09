type variable = string
type entity = Nbr of Nbr.nbr | Func of Func.func | Operator of Operator.operator | Variable of variable | OpeningParenthese | ClosingParenthese

type expression = entity list (* Polonaised list of entities *)

type operation_type = Function_definition of (string * variable * expression)	(* Function name, function parameter, function definition *)
						| Variable_definition of (variable * (entity list))		(* Variable and expression *)
						| Expression_solving of expression						(* a + 2 = ? ou f(2) = ? *)
						| Equation_solving of (string * Nbr.nbr)				(* Function name,  Value to be equal to *)
