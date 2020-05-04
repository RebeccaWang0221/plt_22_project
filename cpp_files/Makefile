CXX = g++
CXXFLAGS = -Wall -g -std=c++11

main: test.o ratlist.h
	$(CXX) $(CXXFLAGS) -o main test.o

test.o: test.cpp ratlist.h
	$(CXX) $(CXXFLAGS) -c test.cpp

.PHONY: clean

clean:
	rm main *.o

