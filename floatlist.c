#include <stdio.h>
#include <stdlib.h>
#include "floatlist.h"

struct FloatNode* create_float_node(double val, struct FloatNode *next) {
    struct FloatNode *node = (struct FloatNode*)malloc(sizeof(struct FloatNode));
    node->val = val;
    node->next = next;
    return node;
}

void init_float_list(struct FloatList *lst) {
    lst->head = NULL;
    lst->size = 0;
}

void free_float_list(struct FloatList *lst) {
    struct FloatNode *curr = lst->head;
    struct FloatNode *next = NULL;
    while (curr != NULL) {
        next = curr->next;
        free(curr);
        curr = next;
    }
}

void append_float(struct FloatList *lst, double val) {
    if (lst->size == 0) {
        lst->head = create_float_node(val, NULL);
    } else {
        struct FloatNode *curr = lst->head;
        while (curr->next != NULL) {
            curr = curr->next;
        }
        curr->next = create_float_node(val, NULL);
    }
    lst->size++;
}

void remove_float(struct FloatList *lst, int idx) { // TODO: handle index out of bounds
    if (idx == 0) {
        struct FloatNode *tmp = lst->head;
        lst->head = tmp->next;
        free(tmp);
        lst->size--;
    } else {
        struct FloatNode *tmp = lst->head;
        struct FloatNode *prev;
        int i = 0;
        while (i < idx && i < lst->size - 1) {
            prev = tmp;
            tmp = tmp->next;
            i++;
        }
        prev->next = tmp->next;
        free(tmp);
        lst->size--;
    }
}

double get_float(struct FloatList *lst, int idx) { // TODO: handle index out of bounds
    if (idx == 0) {
        return lst->head->val;
    }
    struct FloatNode *curr = lst->head;
    int i = 0;
    while(i < idx && i < lst->size - 1) {
        curr = curr->next;
        i++;
    }
    return curr->val;
}

void insert_float(struct FloatList *lst, int idx, double val) {
    if (idx == 0) {
        lst->head = create_float_node(val, lst->head);
    } else {
        struct FloatNode *curr = lst->head;
        struct FloatNode *prev;
        int i = 0;
        while (i < idx && i < lst->size) {
            prev = curr;
            curr = curr->next;
            i++;
        }
        prev->next = create_float_node(val, curr);
    }
    lst->size++;
}

int index_float(struct FloatList *lst, double val) {
    struct FloatNode *curr = lst->head;
    int i = 0;
    while (curr != NULL) {
        if (curr->val == val) {
            return i;
        }
        curr = curr->next;
        i++;
    }
    return -1;
}

double pop_float(struct FloatList *lst, int idx) {
    if (idx == 0) {
        struct FloatNode *tmp = lst->head;
        lst->head = tmp->next;
        double val = tmp->val;
        free(tmp);
        lst->size--;
        return val;
    } else {
        struct FloatNode *tmp = lst->head;
        struct FloatNode *prev;
        int i = 0;
        while (i < idx && i < lst->size - 1) {
            prev = tmp;
            tmp = tmp->next;
            i++;
        }
        prev->next = tmp->next;
        double val = tmp->val;
        free(tmp);
        lst->size--;
        return val;
    }
}

int float_list_size(struct FloatList *lst) {
    return lst->size;
}

void print_float_list(struct FloatList *lst) {
    struct FloatNode *curr = lst->head;
    while (curr != NULL) {
        printf("%f ", curr->val);
        curr = curr->next;
    }
    printf("\n");
}
