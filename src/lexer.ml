let rec lexer str =
  let isalpha = function
    | 'a' .. 'z' | 'A' .. 'Z' ->  true
    | _ -> false
  in
  let strWithoutFirstN str n =
    String.sub str n ((String.length str) - n)
  in
  let strWithoutLastN str n =
    String.sub str 0 ((String.length str) - n)
  in
  let getStrChars str =
    let rec recursive str n =
      if (n < String.length str) && ((str.[n] >= 'a' && str.[n] <= 'z') || (str.[n] >= 'A' && str.[n] <= 'Z')) then
        recursive str (n + 1)
      else
        n
    in
    recursive str 0
  in
  let startsWithOperator (str:string) =
    if (str.[0] = '*' && str.[1] = '*') then
      Some("**",  strWithoutFirstN str 2)
    else if (str.[0] = '*' || str.[0] = '/' || str.[0] = '+' || str.[0] = '-' || str.[0] = '%' || str.[0] = '*' || str.[0] = '^') then
      Some(String.make 1 str.[0], strWithoutFirstN str 1)
    else
      None
  in
  let startsWithSymbole (str:string) =
    if (str.[0] = '(' || str.[0] = ')' || str.[0] = '=' || str.[0] = '?' || str.[0] = ';' || str.[0] = ',' || str.[0] = '=' || str.[0] = '?') then
      Some(String.make 1 str.[0], strWithoutFirstN str 1)
    else
      None
  in
  let startsWithString (str:string) =
    if (str.[0] >= 'a' && str.[0] <= 'z') || (str.[0] >= 'A' && str.[0] <= 'Z') then
      if (getStrChars str = 1 && str.[0] = 'i') then None else
        Some((String.sub str 0 (getStrChars str)), (strWithoutFirstN str (getStrChars str)))
    else
      None
  in
  let startsWithIMultipleInteger (str:string) =
    let iMultipleIntegerReg = Str.regexp "\\(-?[0-9]*i\\)\\(.*\\)" in
    if (Str.string_match iMultipleIntegerReg str 0) then
      begin
        if Str.matched_group 2 str <> "" && (isalpha (Str.matched_group 2 str).[0] = true) then
          None
        else let s = strWithoutLastN (Str.matched_group 1 str) 1 in
          (
            if s = "" then
              Some ("1", Str.matched_group 2 str)
            else
              Some (s, Str.matched_group 2 str)
          )
      end
    else
      None
  in
  let startsWithIMultipleFloat (str:string) =
    let iMultipleFloatReg1 = Str.regexp "\\(-?[0-9]*\\.[0-9]+i\\)\\(.*\\)" in
    let iMultipleFloatReg2 = Str.regexp "\\(-?[0-9]+\\.[0-9]*i\\)\\(.*\\)" in
    if (Str.string_match iMultipleFloatReg1 str 0) && (Str.matched_group 2 str = "" || isalpha (Str.matched_group 2 str).[0] = false) then
      Some (strWithoutLastN (Str.matched_group 1 str) 1, Str.matched_group 2 str)
    else if (Str.string_match iMultipleFloatReg2 str 0) && (Str.matched_group 2 str = "" || isalpha (Str.matched_group 2 str).[0] = false) then
      Some (strWithoutLastN (Str.matched_group 1 str) 1, Str.matched_group 2 str)
    else
      None
  in
  let startsWithRealInteger (str:string) =
    let realIntegerReg = Str.regexp "\\(-?[0-9]+\\)\\(.*\\)" in
    if (Str.string_match realIntegerReg str 0) then
      Some ((Str.matched_group 1 str), Str.matched_group 2 str)
    else
      None
  in
  let startsWithRealFloat(str:string) =
    let realFloatReg1 = Str.regexp "\\(-?[0-9]*\\.[0-9]+\\)\\(.*\\)" in
    let realFloatReg2 = Str.regexp "\\(-?[0-9]+\\.[0-9]*\\)\\(.*\\)" in
    if (Str.string_match realFloatReg1 str 0) then
      Some ((Str.matched_group 1 str),  Str.matched_group 2 str)
    else if (Str.string_match realFloatReg2 str 0) then
      Some ((Str.matched_group 1 str),  Str.matched_group 2 str)
    else
      None
  in
  let startsWithFunctionBeginning (str:string) =
    match startsWithString str with
    | Some(matched, en) -> if en = "" then None
      else if (en.[0] = '(') then
        Some (matched ^ "(", String.sub en 1 ((String.length en) - 1))
      else
        None
    | None -> None
  in
  if str = "" then [] else
  if (str.[0] = ' ' )|| (str.[0] = '\t') then lexer (strWithoutFirstN str 1) else
    match startsWithSymbole str with
    | Some(matched, en) -> (new Lexeme.lexeme matched Types.Symbole) :: (lexer en)
    | None ->
      match startsWithFunctionBeginning str with
      | Some(matched, en) -> (new Lexeme.lexeme matched Types.FunctionBeginning) :: (lexer en)
      | None ->
        match startsWithString str with
        | Some(matched, en) -> (new Lexeme.lexeme matched Types.String) :: (lexer en)
        | None ->
          match startsWithIMultipleFloat str with
          | Some(matched, en) -> (new Lexeme.lexeme matched Types.IMultipleFloat) :: (lexer en)
          | None ->
            match startsWithIMultipleInteger str with
            | Some(matched, en) -> (new Lexeme.lexeme matched Types.IMultipleInteger) :: (lexer en)
            | None ->
              match startsWithRealFloat str with
              | Some(matched, en) -> (new Lexeme.lexeme matched Types.RealFloat) :: (lexer en)
              | None ->
                match startsWithRealInteger str with
                | Some(matched, en) -> (new Lexeme.lexeme matched Types.RealInteger) :: (lexer en)
                | None ->
                  match startsWithOperator str with
                  | Some(matched, en) -> (new Lexeme.lexeme matched Types.Operator) :: (lexer en)
                  | None -> raise Types.Lexer_error
