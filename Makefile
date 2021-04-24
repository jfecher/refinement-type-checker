.PHONY: clean
.PHONY: main

main:
	dune exec ./main.exe

clean:
	@rm *.cmo *.cmi lexer.ml lexer.mli parser.ml parser.mli refined
