all: render.e
clean:
	rm -f *.cm* lexer.ml parser.ml render.e

model.cmi: model.mli
	ocamlc -c $<

parser.cmi: parser.mli model.cmi
	ocamlc -c $<

parser.ml: parser.mly
	ocamlyacc $<

parser.cmo: parser.ml parser.cmi model.cmi 
	ocamlc -c $<

lexer.ml: lexer.mll
	ocamllex $<

lexer.cmo: lexer.ml parser.cmo  
	ocamlc -c $<

math.cmo:
	ocamlc -c math.ml

transformations.cmo: math.cmo
	ocamlc -c $< transformations.ml

engine.cmo: math.cmo transformations.cmo
	ocamlc -c $< engine.ml

render.e: parser.cmo lexer.cmo math.cmo transformations.cmo engine.cmo 
	ocamlc graphics.cma unix.cma $+ main.ml -o $@