type variable = (string * Nbr.nbr)
type entity = Nbr of Nbr.nbr | Func of Func.func | Operator of Operator.operator | Variable of variable | OpeningParenthese | ClosingParenthese4

(* type variable = string * value *)
