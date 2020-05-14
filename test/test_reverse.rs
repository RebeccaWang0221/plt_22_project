
def void reverse_lst(list<int> lst) {
    int mid = len(lst) / 2;
    int start = 0;
    int end = len(lst) - 1;
    for int i in range(mid) {
        int tmp = lst[start];
        lst[start] = lst[end];
        lst[end] = tmp;
        start++;
        end--;
    }
}

list<int> x = [1,2,3,4,5,6];
reverse_lst(x);
print(x);
