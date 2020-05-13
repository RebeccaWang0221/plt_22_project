#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "strarray.h"

void init_str_arr(struct StrArray *arr, int size) {
    arr->size = size;
    arr->data = (char **)malloc(sizeof(char *) * size);
    for (int i = 0; i < size; i++) {
        arr->data[i] = "";
    }
}

void assign_str_arr(struct StrArray *arr, int idx, char *val) {
    if (idx < arr->size) {
        arr->data[idx] = val;
    }
}

char *get_str_arr(struct StrArray *arr, int idx) {
    if (idx < arr->size) {
        return arr->data[idx];
    }
    return 0;
}

int str_arr_size(struct StrArray *arr) {
    return arr->size;
}

char contains_str_arr(struct StrArray *arr, char *val) {
    for (int i = 0; i < arr->size; i++) {
        if (strcmp(get_str_arr(arr, i), val) == 0) {
            return 1;
        }
    }
    return 0;
}

void copy_str_arr(struct StrArray *src, struct StrArray *tgt) {
    for (int i = 0; i < src->size; i++) {
        char *tmp = (char *)malloc(sizeof(src->data[i]));
        strcpy(tmp, src->data[i]);
        tgt->data[i] = tmp;
    }
}

void print_str_arr(struct StrArray *arr) {
    printf("%s ", "{");
    for (int i = 0; i < arr->size; i++) {
        printf("%s ", get_str_arr(arr, i));
    }
    printf("}\n");
}
