val quote = #"\"";
val lf = #"\n";

exception ImproperDoubleQuotes
exception NoNewlineAtEOF
exception UnevenFields
exception emptyInputFile
fun convertDelimiters(infilename, delim1, outfilename, delim2) =
	let
		val infile = TextIO.openIn(infilename)
		handle e => raise emptyInputFile
		val outfile = TextIO.openOut(outfilename)

		(* implements writing to output *)
		fun flush(s) = TextIO.output(outfile, s)

		(* process the first line and return the number of delimiters *)
		fun process(state, count: int, ret) =
			case TextIO.input1(infile) of
				SOME c	=>
					if c = lf then
						if state = 1 then	process(1, count, ret ^ str(c))
						else				(flush(ret ^ (if state = 3 then str(quote) else "") ^ str(c)); count)
					else (
					if state = 0 orelse state = 2 then
						if c = quote then
							process(1, count, ret ^ str(c))
						else if c = delim1 then
							process(0, count + 1, ret ^ str(delim2))
						else
							if state = 2 then raise ImproperDoubleQuotes else process(3, count, ret ^ str(quote) ^ str(c))
					else if state = 1 then
						process(if c = quote then 2 else 1, count, ret ^ str(c))
					else
						if c = quote then raise ImproperDoubleQuotes
						else if c = delim1 then
							process(0, count + 1, ret ^ str(quote) ^ str(delim2))
						else process(3, count, ret ^ str(c)))
			|	NONE	=> raise NoNewlineAtEOF
		
		fun throwException(e) =
			let
				val outfile = TextIO.openOut(outfilename)
			in
				TextIO.output(outfile, ""); raise e
		end

		val count = process(0, 0, "")

		(* implement line iteration *)
		fun iterate(lineNum) =
			if TextIO.lookahead(infile) = NONE then TextIO.closeOut(outfile) before TextIO.closeIn(infile) else (
				let
					val curr = process(0, 0, "")
				in
					if curr = count then iterate(lineNum + 1)
					else (print("Expected: " ^ Int.toString(count) ^ " fields, Present: " ^ Int.toString(curr) ^ " fields on Line " ^ Int.toString(lineNum)); raise UnevenFields)
				end)
		handle e => TextIO.closeOut(outfile) before TextIO.closeIn(infile) before throwException(e)
	in
		(* actual execution of the function which calls the iteration and closes the files *)
		iterate(2)
end

fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename, #",", outfilename, #"\t")
fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename, #"\t", outfilename, #",")