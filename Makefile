.phony: all intro doc
all: doc

%.html:%.ml
	./extract_doc.sh $<

doc : intro intro.html

intro:
	ocamlbuild -cflags -dtypes intro.native

clean::
	rm -f *~
	ocamlbuild -clean
clean::
	rm intro.html
# arg_unparse: arg_unparse.ml
# 	ocamlopt -dtypes $< -o $@

# base_unparse: base_unparse.ml
# 	ocamlopt -dtypes $< -o $@
