fun read_file (infile:string) =
   let 
    	val instream = TextIO.openIn infile
	fun loop instream =
		case TextIO.inputLine instream of
	             SOME line => line :: loop instream
    	    	   | NONE      => []
    in
	 loop instream before TextIO.closeIn instream
    end

(*
Now to tokenize a line you can type: 
val lexer = SimpLex.makeLexer (fn _ => first_line);
lexer();
*)