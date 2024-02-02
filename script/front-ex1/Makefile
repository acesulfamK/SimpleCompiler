FILE = parser.mly lexer.mll ast.ml  

print_ast: $(FILE) print_ast.ml
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c print_ast.ml
	ocamlc -o print_ast unix.cma ast.cmo lexer.cmo parser.cmo print_ast.cmo

print_lex: lexer.cmo parser.cmo print_lex.ml
	ocamlc -o print_lex lexer.cmo parser.cmo print_lex.ml
<<<<<<< HEAD
=======

clean:
	rm  -f *.cmi *.cmo parser.ml lexer.ml parser.mli print_ast
>>>>>>> 8d2787e7d6438cf8feaeeb3ef6b849e3fb65f8fd

clean:
	rm  -f *.cmi *.cmo parser.ml lexer.ml parser.mli print_ast