struct FloatArray {
    double *data;
    int size;
};

void init_float_arr(struct FloatArray *arr, int size);

void assign_float_arr(struct FloatArray *arr, int idx, double val);

double get_float_arr(struct FloatArray *arr, int idx);

int float_arr_size(struct FloatArray *arr);

char contains_float_arr(struct FloatArray *arr, double val);

void print_float_arr(struct FloatArray *arr);
