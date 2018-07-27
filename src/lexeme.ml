class lexeme (content:string) (p_type:Types.lex_type) =
  object (s)
    val _content = content
    val _type = p_type

    method get_content = _content
    method get_type = _type
    method to_string = "[lexeme: (_content=" ^ _content ^ "),(_type=" ^ (Utils.lex_type_to_string _type) ^ ")]"
  end
