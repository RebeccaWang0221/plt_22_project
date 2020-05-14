#include <stdlib.h>
#include <stdio.h>
#include "intarray.h"

void init_int_arr(struct IntArray *arr, int size) {
    arr->size = size;
    arr->data = (int *)malloc(sizeof(int) * size);
    for (int i = 0; i < size; i++) {
        arr->data[i] = 0;
    }
}

void assign_int_arr(struct IntArray *arr, int idx, int val) {
    if (idx < arr->size) {
        arr->data[idx] = val;
    }
}

int get_int_arr(struct IntArray *arr, int idx) {
    if (idx < arr->size) {
        return arr->data[idx];
    }
    return 0;
}

int int_arr_size(struct IntArray *arr) {
    return arr->size;
}

char contains_int_arr(struct IntArray *arr, int val) {
    for (int i = 0; i < arr->size; i++) {
        if (get_int_arr(arr, i) == val) {
            return 1;
        }
    }
    return 0;
}

void copy_int_arr(struct IntArray *src, struct IntArray *tgt) {
    for (int i = 0; i < src->size; i++) {
        tgt->data[i] = src->data[i];
    }
}

void print_int_arr(struct IntArray *arr) {
    printf("%s ", "{");
    for (int i = 0; i < arr->size; i++) {
        printf("%d ", get_int_arr(arr, i));
    }
    printf("}\n");
}
