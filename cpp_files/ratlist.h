#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

template<typename T>
class RatList;
template<typename T>
ostream& operator<<(ostream &os, const RatList<T> &rhs);

template<typename T>
class RatList {

	public:

		RatList();

		RatList(const RatList &other);

		void append(T elem);

		void push(T elem);

		void insert(int idx, T elem);

		void remove(int idx);

		int index(T elem) const;

		T pop(int idx);

		void reverse();

		T& operator[](int i);

		int size() const { return data.size(); }

		bool is_empty() const { return data.empty(); }

		friend ostream& operator<< <T>(ostream& os, const RatList<T> &rhs);

	private:

		vector<T> data;

};

template<typename T>
RatList<T>::RatList() {

}

template<typename T>
RatList<T>::RatList(const RatList &other) {
	for (int i = 0; i < other.size(); i++) {
		data.push_back(other.data[i]);
	}
}

template<typename T>
void RatList<T>::append(T elem) {
	data.push_back(elem);
}

template<typename T>
void RatList<T>::push(T elem) {
	data.insert(data.begin(), elem);
}

template<typename T>
void RatList<T>::insert(int idx, T elem) {
	if (idx > this->size()) {
		return;
	}
	data.insert(data.begin() + idx, elem);
}

template<typename T>
void RatList<T>::remove(int idx) {
	data.erase(data.begin() + idx);
}

template<typename T>
int RatList<T>::index(T elem) const {
	int idx = 0;
	for (auto i = data.begin(); i != data.end() && *i != elem; i++) {
		idx++;
	}
	return idx;
}

template<typename T>
T RatList<T>::pop(int idx) {
	T val = data[idx];
	data.erase(data.begin() + idx);
	return val;
}

template<typename T>
void RatList<T>::reverse() {
	std::reverse(data.begin(), data.end());
}

template<typename T>
T& RatList<T>::operator[](int i) {
	return data[i];
}

template<typename T>
ostream& operator<<(ostream& os, const RatList<T> &rhs) {
	os << "[ ";
	for (auto i = rhs.data.begin(); i != rhs.data.end(); i++) {
		os << *i << " ";
	}
	os << "]";
	return os;
}
