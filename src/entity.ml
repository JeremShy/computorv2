type variable = string
type func = string
type entity = Nbr of Nbr.nbr | Func of func | Operator of Operator.operator | Variable of variable

type expression = entity list (* Polonaised list of entities *)

class func_obj (param_name:string) (expr:expression) =
  object
    val _expr  = expr
    val _param_name  = param_name

    method get_expr = _expr
    method get_param_name = _param_name
  end

type definable = Func of func_obj | Variable of Nbr.nbr


type operation_type = FunctionDefinition of (string * variable * expression)	(* Function name, function parameter, function definition *)
                    | VariableDefinition of (variable * expression)		(* Variable and expression *)
                    | ExpressionSolving of expression						(* a + 2 = ? ou f(2) = ? *)
                    | EquationSolving of (string * Nbr.nbr)				(* Function name,  Value to be equal to *)

type bufferable = Func of func | Nbr of Nbr.nbr
