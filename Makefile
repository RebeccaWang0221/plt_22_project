CC = gcc
CFLAGS = -g -Wall
LDFLAGS = -g

default: rattle.native libintlist.a

rattle.native:
	ocamlbuild -pkgs llvm rattle.native

libintlist.a: intlist.o floatlist.o strops.o
	ar -crs libintlist.a intlist.o floatlist.o strops.o
	ranlib libintlist.a

intlist.o: intlist.h intlist.c

floatlist.o: floatlist.h floatlist.c

strops.o: strops.h strops.c

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *.native
	rm -f *.o *.a a.out *.s
