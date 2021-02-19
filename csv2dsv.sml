val bkslsh = #"\\";
val lf = #"\n";

(* implements file check *)

exception UnescapedBackslash
fun convertDelimiters(infilename, delim1, outfilename, delim2) =
	let
		val infile = TextIO.openIn(infilename)
		val outfile = TextIO.openOut(outfilename)

		(* implements extracting line and returns as string *)
		fun nextLine(ret, instream, enc) =
			case TextIO.input1(instream) of
				SOME c =>
					if enc = false andalso c = lf then ret
					else nextLine(ret ^ str(c), instream, c = bkslsh)
			|	NONE => if enc then raise UnescapedBackslash else ret

		(* implements writing to output *)
		fun flush(s) = TextIO.output(outfile, s)
		
		(* implements conversion of line
		   enc 		= if odd number of \ were encountered
		   buffer 	= the current buffer value to be written *)
		fun process(enc, nil) 		= if enc then raise UnescapedBackslash else flush(str(lf))
		|	process(enc, c :: line)	=
			if enc = false then
				if c = bkslsh then process(true, line)
				else (
					if c = delim1 		then flush(str(delim2))
					else if c = delim2	then flush("\\" ^ str(delim2))
					else 					 flush(str(c))
				) before process(false, line)
			else
				if c = bkslsh 		then flush(str(c) ^ str(c)) before process(false, line)
				else if c = delim1	then flush(str(c)) before process(false, line)
				else if c = lf		then flush(str(bkslsh) ^ str(c)) before process(false, line)
				else raise UnescapedBackslash
		
		fun throwException() =
			let
				val outfile = TextIO.openOut(outfilename)
			in
				TextIO.output(outfile, ""); raise UnescapedBackslash
		end

		(* implement line iteration *)
		fun iterate(instream) =
			let
				val line = nextLine("", instream, false)
			in
				process(false, explode(line)) before (if TextIO.lookahead(instream) = NONE then flush("") else iterate(instream))
		end
		handle UnescapedBackslash => TextIO.closeOut(outfile) before TextIO.closeIn(infile) before throwException()
	in
		(* actual execution of the function which calls the iteration and closes the files *)
		iterate(infile) before TextIO.closeOut(outfile) before TextIO.closeIn(infile)
end

