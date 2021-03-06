struct IntNode {
    int val;
    struct IntNode *next;
};

struct IntList {
    struct IntNode *head;
    int size;
};

struct IntNode* create_int_node(int val, struct IntNode *next);

void init_int_list(struct IntList *lst);

void append_int(struct IntList *lst, int val);

int get_int(struct IntList *lst, int idx);

void remove_int(struct IntList *lst, int idx);

void print_int_list(struct IntList *lst);

void insert_int(struct IntList *lst, int idx, int val);

int index_int(struct IntList *lst, int val);

int pop_int(struct IntList *lst, int idx);

void free_int_list(struct IntList *lst);

char contains_int(struct IntList *lst, int val);

int int_list_size(struct IntList *lst);

void copy_int_list(struct IntList *src, struct IntList *tgt);

void assign_int(struct IntList *lst, int idx, int val);
