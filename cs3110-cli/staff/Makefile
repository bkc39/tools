LINK=$(dir $(shell which ocamlc))
STD_LIB=`ocamlc -where`

all: compile

assertions:
	ocamlfind ocamlc -package qcheck -a -o assertions.cma serializer.mli assertions.mli serializer.ml assertions.ml
	mv assertions.cmi assertions.cmo assertions.cma ${STD_LIB}
	rm serializer.cmi serializer.cmo

compile:
	corebuild -Is include,assertions,util main.native
	mv main.native cs3110-staff

clean:
	corebuild -clean
	rm -rf _cms

install: assertions compile clean

uninstall:
	rm cs3110-staff
	rm ${STD_LIB}/assertions.cmi
	rm ${STD_LIB}/assertions.cmo
	rm ${STD_LIB}/assertions.cma

link:
	ln -s `pwd`/cs3110-staff $(LINK)

unlink:
	rm $(LINK)
