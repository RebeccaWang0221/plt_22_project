struct IntArray {
    int *data;
    int size;
};

void init_int_arr(struct IntArray *arr, int size);

void assign_int_arr(struct IntArray *arr, int idx, int val);

int get_int_arr(struct IntArray *arr, int idx);

int int_arr_size(struct IntArray *arr);

char contains_int_arr(struct IntArray *arr, int val);

void print_int_arr(struct IntArray *arr);
