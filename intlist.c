#include <stdio.h>
#include <stdlib.h>
#include "intlist.h"

struct IntNode* create_int_node(int val, struct IntNode *next) {
    struct IntNode *node = (struct IntNode*)malloc(sizeof(struct IntNode));
    node->val = val;
    node->next = next;
    return node;
}

void init_int_list(struct IntList *lst) {
    lst->head = NULL;
    lst->size = 0;
}

void free_int_list(struct IntList *lst) {
    struct IntNode *curr = lst->head;
    struct IntNode *next = NULL;
    while (curr != NULL) {
        next = curr->next;
        free(curr);
        curr = next;
    }
}

void append_int(struct IntList *lst, int val) {
    if (lst->size == 0) {
        lst->head = create_int_node(val, NULL);
    } else {
        struct IntNode *curr = lst->head;
        while (curr->next != NULL) {
            curr = curr->next;
        }
        curr->next = create_int_node(val, NULL);
    }
    lst->size++;
}

void remove_int(struct IntList *lst, int idx) { // TODO: handle index out of bounds
    if (idx == 0) {
        struct IntNode *tmp = lst->head;
        lst->head = tmp->next;
        free(tmp);
        lst->size--;
    } else {
        struct IntNode *tmp = lst->head;
        struct IntNode *prev;
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

int get_int(struct IntList *lst, int idx) { // TODO: handle index out of bounds
    if (idx == 0) {
        return lst->head->val;
    }
    struct IntNode *curr = lst->head;
    int i = 0;
    while(i < idx && i < lst->size - 1) {
        curr = curr->next;
        i++;
    }
    return curr->val;
}

void insert_int(struct IntList *lst, int idx, int val) {
    if (idx == 0) {
        lst->head = create_int_node(val, lst->head);
    } else {
        struct IntNode *curr = lst->head;
        struct IntNode *prev;
        int i = 0;
        while (i < idx && i < lst->size) {
            prev = curr;
            curr = curr->next;
            i++;
        }
        prev->next = create_int_node(val, curr);
    }
    lst->size++;
}

int index_int(struct IntList *lst, int val) {
    struct IntNode *curr = lst->head;
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

int pop_int(struct IntList *lst, int idx) {
    if (idx == 0) {
        struct IntNode *tmp = lst->head;
        lst->head = tmp->next;
        int val = tmp->val;
        free(tmp);
        lst->size--;
        return val;
    } else {
        struct IntNode *tmp = lst->head;
        struct IntNode *prev;
        int i = 0;
        while (i < idx && i < lst->size - 1) {
            prev = tmp;
            tmp = tmp->next;
            i++;
        }
        prev->next = tmp->next;
        int val = tmp->val;
        free(tmp);
        lst->size--;
        return val;
    }
}

void assign_int(struct IntList *lst, int idx, int val) {
    if (idx == 0) {
        lst->head->val = val;
    } else {
        struct IntNode *node = lst->head;
        int i = 0;
        while (i < idx && i < lst->size - 1) {
            node = node-> next;
            i++;
        }
        node->val = val;
    }
}

int int_list_size(struct IntList *lst) {
    return lst->size;
}

char contains_int(struct IntList *lst, int val) {
    if (index_int(lst, val) == -1) {
        return 0;
    }
    return 1;
}

void copy_int_list(struct IntList *src, struct IntList *tgt) {
    struct IntNode *node = src->head;
    while (node != NULL) {
        append_int(tgt, node->val);
        node = node->next;
    }
}

void print_int_list(struct IntList *lst) {
    struct IntNode *curr = lst->head;
    printf("%s ", "[");
    while (curr != NULL) {
        printf("%d ", curr->val);
        curr = curr->next;
    }
    printf("]\n");
}
