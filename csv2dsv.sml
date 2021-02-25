fun convertDelimiters(infilename, delim1, outfilename, delim2) =
	let
		(* exceptions to be called *)
		exception emptyInputFile
		exception ImproperDoubleQuotes
		exception NoNewlineAtEOF
		exception UnevenFields of string

		(* useful variables *)
		val quote = #"\""
		val lf = #"\n"

		(* opening the files *)
		val infile = TextIO.openIn(infilename)
		handle e => raise emptyInputFile
		val outfile = TextIO.openOut(outfilename)

		(* process the next line using a DFA and return the number of delimiters *)
		fun process(state, count) =
			case TextIO.input1(infile) of
				SOME c	=>
					if c = lf then
						if state = 1 then	(TextIO.output(outfile, str(c)); process(1, count))
						else				(TextIO.output(outfile, (if state = 3 then str(quote) else "") ^ str(c)); count)
					else (
						if state = 0 orelse state = 2 then
							if c = quote then
								(TextIO.output(outfile, str(c)); process(1, count))
							else if c = delim1 then
								(TextIO.output(outfile, str(delim2)); process(0, count + 1))
							else if state = 0 then (TextIO.output(outfile, str(quote) ^ str(c)); process(3, count))
							else raise ImproperDoubleQuotes
						else if state = 1 then
							(TextIO.output(outfile, str(c)); process(if c = quote then 2 else 1, count))
						else
							if c = quote then raise ImproperDoubleQuotes
							else if c = delim1 then
								(TextIO.output(outfile, str(quote) ^ str(delim2)); process(0, count + 1))
							else (TextIO.output(outfile, str(c)); process(3, count)))
			|	NONE	=> if state = 1 then raise ImproperDoubleQuotes else raise NoNewlineAtEOF

		(* process first line and store number of fields *)
		val count = process(0, 1)

		(* implement line iteration *)
		fun iterate(lineNum) =
			if TextIO.lookahead(infile) = NONE then TextIO.closeOut(outfile) before TextIO.closeIn(infile)
			else (
				let
					val curr = process(0, 1)
				in
					if curr = count then iterate(lineNum + 1)
					else raise UnevenFields("Expected: " ^ Int.toString(count) ^ " fields, Present: " ^ Int.toString(curr) ^ " fields on Line " ^ Int.toString(lineNum) ^ str(lf))
			end)
		handle UnevenFields(s) => (TextIO.closeOut(outfile) before TextIO.closeIn(infile);
			let
				val outfile = TextIO.openOut(outfilename)
			in
				(TextIO.output(outfile, ""); TextIO.closeOut(outfile); print(s))
		end)

	in
		(* actual execution of the function which calls the iteration and closes the files *)
		iterate(2)
end

(* conversion functions *)
fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename, #",", outfilename, #"\t")
fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename, #"\t", outfilename, #",")