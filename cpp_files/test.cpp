#include <iostream>
#include "ratlist.h"

using namespace std;

int main() {
	RatList<int> lst;
	for (int i = 0; i < 10; i++) {
		lst.append(i);
	}
	cout << lst << endl;
}
