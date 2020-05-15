CC = gcc
CFLAGS = -g -Wall
LDFLAGS = -g

default: rattle.native libintlist.a

rattle.native:
	ocamlbuild -pkgs llvm rattle.native

libintlist.a: intlist.o floatlist.o strlist.o intarray.o floatarray.o strarray.o strops.o  exp.o
	ar -crs libintlist.a intlist.o floatlist.o strlist.o intarray.o floatarray.o strarray.o strops.o exp.o 
	ranlib libintlist.a

intlist.o: intlist.h intlist.c

floatlist.o: floatlist.h floatlist.c

strlist.o: strlist.h strlist.c

intarray.o: intarray.h intarray.c

floatarray.o: floatarray.h floatarray.c

strarray.o: strarray.h strarray.c

strops.o: strops.h strops.c

exp.o: exp.h exp.c

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *.native
	rm -f *.o *.a a.out llvm.out *.s
