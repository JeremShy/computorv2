{
}

rule token = parse
	| ['a' - 'z']+ as word
		{print_endline word ; token lexbuf}
	| ['0' - '9']+'i'
		{token lexbuf}
	| _ as c {token lexbuf}
	| eof {raise End_of_file}
