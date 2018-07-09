type variable = string
type func = string
type entity = Nbr of Nbr.nbr | Func of func | Operator of Operator.operator | Variable of variable | OpeningParenthese | ClosingParenthese

type expression = entity list (* Polonaised list of entities *)

type operation_type = FunctionDefinition of (string * variable * expression)	(* Function name, function parameter, function definition *)
						| VariableDefinition of (variable * expression)		(* Variable and expression *)
						| ExpressionSolving of expression						(* a + 2 = ? ou f(2) = ? *)
						| EquationSolving of (string * Nbr.nbr)				(* Function name,  Value to be equal to *)
