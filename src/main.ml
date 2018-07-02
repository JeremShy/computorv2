let () =
  print_endline "coucou" ;
  let rec f lexbuf =
    ignore (Computorv2.token lexbuf) ;
    f lexbuf
  in
  try
    f (Lexing.from_string "abc def") with
  | End_of_file -> print_endline "end"
