CXX = g++
CXXFLAGS = -Wall -g -std=c++11

default: rattle.native # archive file

rattle.native: scanner.mll parser.mly semant.ml ast.mli sast.mli rattle.ml pretty.ml irgen.ml
# rattle.native: rattle.ml
	ocamlbuild -use-ocamlfind -r rattle.native -pkgs llvm

main: test.o ratlist.h
	$(CXX) $(CXXFLAGS) -o main test.o

test.o: test.cpp ratlist.h
	$(CXX) $(CXXFLAGS) -c test.cpp

rattlib.a: ratlist.o

	ar -crs rattlib.a ratlist.o
	ranlib rattlib.a

ratlist.o: ratlist.h

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *.native
	rm -f main *.o
	rm -f -r */_build
	rm -f *.out

