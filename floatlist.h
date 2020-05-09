struct FloatNode {
    double val;
    struct FloatNode *next;
};

struct FloatList {
    struct FloatNode *head;
    int size;
};

struct FloatNode* create_float_node(double val, struct FloatNode *next);

void init_float_list(struct FloatList *lst);

void append_float(struct FloatList *lst, double val);

double get_float(struct FloatList *lst, int idx);

void remove_float(struct FloatList *lst, int idx);

void print_float_list(struct FloatList *lst);

void insert_float(struct FloatList *lst, int idx, double val);

int index_float(struct FloatList *lst, double val);

double pop_float(struct FloatList *lst, int idx);

int float_list_size(struct FloatList *lst);

void free_float_list(struct FloatList *lst);
