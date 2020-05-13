struct StrArray {
    char **data;
    int size;
};

void init_str_arr(struct StrArray *arr, int size);

void assign_str_arr(struct StrArray *arr, int idx, char *val);

char *get_str_arr(struct StrArray *arr, int idx);

int str_arr_size(struct StrArray *arr);

char contains_str_arr(struct StrArray *arr, char *val);

void copy_str_arr(struct StrArray *src, struct StrArray *tgt);

void print_str_arr(struct StrArray *arr);
