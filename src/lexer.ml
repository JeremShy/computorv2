let rec lexer str =
	let operatorReg = Str.regexp "\\(\\*\\*\\|\\*\\|\\+\\|-\\|/\\|%\\|\\^\\)\\(.*\\)" in
	let symboleReg = Str.regexp "\\(=\\|\\?\\|(\\|)\\|\\[\\|\\]\\|,\\|;\\)\\(.*\\)" in
	let stringReg = Str.regexp "\\([a-zA-Z]+\\)\\(.*\\)" in
	let iMultipleFloatReg = Str.regexp "\\([0-9]*\\.[0-9]+i\\)\\(.*\\)" in
	let iMultipleIntegerReg = Str.regexp "\\([0-9]*i\\)\\(.*\\)" in
	let realFloatReg = Str.regexp "\\([0-9]*\\.[0-9]+\\)\\(.*\\)" in
	let realIntegerReg = Str.regexp "\\([0-9]+\\)\\(.*\\)" in
	Printf.printf "str : [%s]\n" str ;
	if str = "" then []
	else if (Str.string_match operatorReg str 0) then
		new Lexeme.lexeme (Str.matched_group 1 str) Types.Operator :: (try lexer (Str.matched_group 2 str) with | _ -> [])
	else if (Str.string_match symboleReg str 0) then
		new Lexeme.lexeme (Str.matched_group 1 str) Types.Symbole :: (try lexer (Str.matched_group 2 str) with | _ -> [])
	else if (Str.string_match stringReg str 0) then
		new Lexeme.lexeme (Str.matched_group 1 str) Types.String :: (try lexer (Str.matched_group 2 str) with | _ -> [])
	else if (Str.string_match iMultipleFloatReg str 0) then
		new Lexeme.lexeme (Str.matched_group 1 str) Types.IMultipleFloat :: (try lexer (Str.matched_group 2 str) with | _ -> [])
	else if (Str.string_match iMultipleIntegerReg str 0) then
		new Lexeme.lexeme (Str.matched_group 1 str) Types.IMultipleInteger :: (try lexer (Str.matched_group 2 str) with | _ -> [])
	else if (Str.string_match realFloatReg str 0) then
		new Lexeme.lexeme (Str.matched_group 1 str) Types.RealFloat :: (try lexer (Str.matched_group 2 str) with | _ -> [])
	else if (Str.string_match realIntegerReg str 0) then
		new Lexeme.lexeme (Str.matched_group 1 str) Types.RealInteger :: (try lexer (Str.matched_group 2 str) with | _ -> [])
	else
		raise Types.Parser_error
