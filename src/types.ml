type lex_type = Operator | Symbole | FunctionBeginning | String | IMultipleInteger | IMultipleFloat | RealInteger | RealFloat
exception Parser_error of string
exception Lexer_error
