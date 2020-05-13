#include <stdlib.h>
#include <stdio.h>
#include "floatarray.h"

void init_float_arr(struct FloatArray *arr, int size) {
    arr->size = size;
    arr->data = (double *)malloc(sizeof(double) * size);
    for (int i = 0; i < size; i++) {
        arr->data[i] = 0.0;
    }
}

void assign_float_arr(struct FloatArray *arr, int idx, double val) {
    if (idx < arr->size) {
        arr->data[idx] = val;
    }
}

double get_float_arr(struct FloatArray *arr, int idx) {
    if (idx < arr->size) {
        return arr->data[idx];
    }
    return 0;
}

int float_arr_size(struct FloatArray *arr) {
    return arr->size;
}

char contains_float_arr(struct FloatArray *arr, double val) {
    for (int i = 0; i < arr->size; i++) {
        if (get_float_arr(arr, i) == val) {
            return 1;
        }
    }
    return 0;
}

void print_float_arr(struct FloatArray *arr) {
    printf("%s ", "{");
    for (int i = 0; i < arr->size; i++) {
        printf("%f ", get_float_arr(arr, i));
    }
    printf("}\n");
}
