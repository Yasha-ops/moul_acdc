.PHONY:run build perm all start

CC=ocamlfind ocamlc
LDFLAGS=-syntax camlp4o -package camlp4 -ppopt pa_macro.cmo -linkpkg -package graphics -package unix

SRC=sandbox.ml

EXEC=output

all: $(EXEC) correct

$(EXEC):
	$(CC) $(LDILBS) $(LDFLAGS) -o $(EXEC) $(SRC) main.ml

correct:
	$(CC) $(LDILBS) $(LDFLAGS) -o correct $(SRC) correct.ml

check: clean $(EXEC) correct
	./verif.sh

clean:
	rm *.cmi *.cmo $(EXEC) correction/*.cmo correction/*.cmi -rf correct student_file/*.cmi student_file/*.cmo

mrproper:
	rm *.cmi *.cmo -rf

