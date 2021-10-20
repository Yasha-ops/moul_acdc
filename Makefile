.PHONY:run build perm all start
CC=ocamlfind ocamlc
LDFLAGS=-syntax camlp4o -package camlp4 -ppopt pa_macro.cmo -linkpkg -package graphics -package unix

SRC=patterns.ml test.ml
EXEC=output

all: $(EXEC) correct

$(EXEC):
	$(CC) $(LDILBS) $(LDFLAGS) -o $(EXEC) $(SRC)

correct:
	$(CC) $(LDILBS) $(LDFLAGS) -o correct correction.ml compare.ml

check: $(EXEC) correct
	./verif.sh

clean:
	rm *.cmi *.cmo $(EXEC) correct -rf

mrproper:
	rm *.cmi *.cmo -rf
