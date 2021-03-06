let handle_line (line:string) (state:(string, Entity.definable) Hashtbl.t) =
  if (String.trim line = "") then () else
    begin
      try begin
        let lexed_line = Lexer.lexer line in
        (* Utils.print_lex_lst lexed_line ; *)
        let parsed_line = Parser.parser lexed_line in
        match parsed_line with
        | Entity.FunctionDefinition (name, parameter, expr) ->
          print_endline ("Defining function " ^ name ^ " with variable " ^ parameter ^ " and expression : ") ;
          Utils.print_entity_lst expr ;
          let tree = Ast.ast_from_expr expr in
          Draw.draw_tree tree ;
          let tree = Simplify.simplify tree in
          Draw.draw_tree tree ;
          Hashtbl.replace state name (Entity.Func(new Entity.func_obj parameter expr))
        | Entity.VariableDefinition (name, expr) ->
          begin
            print_endline ("Defining variable " ^ name ^ " = "); Utils.print_entity_lst expr ;
            let nbr = Resolve.resolve expr state in
            Utils.print_nbr nbr ;
            Hashtbl.replace state name (Entity.Variable(nbr))
          end
        | Entity.EquationSolving(name, unknown, value) -> Equation.equation parsed_line state
        | Entity.ExpressionSolving expr -> Utils.print_nbr (Resolve.resolve expr state)
      end with
      | Types.Execution_error(s) -> Printf.printf "Execution error : %s\n" s
      | Types.Parser_error(s) -> Printf.printf "Syntax Error : %s\n" s
      | Types.Lexer_error -> Printf.printf "An error occured while trying to lex the command.\n"
      | Failure(s) -> Printf.printf "An error occured : %s\n" s
    end

let rec computorv2 (state:(string, Entity.definable) Hashtbl.t) =
  print_string "> " ;
  (
    try
      let line = read_line () in
      handle_line line state
    with
    | Sys.Break -> print_newline ()
  ) ;
  computorv2 state


let () =
  Sys.catch_break true ;
  let state = Hashtbl.create 32 in
  try computorv2 state with
  | End_of_file -> print_endline "Goodbye."
