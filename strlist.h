struct StrNode {
    char *val;
    struct StrNode *next;
};

struct StrList {
    struct StrNode *head;
    int size;
};

struct StrNode* create_str_node(char *val, struct StrNode *next);

void init_str_list(struct StrList *lst);

void append_str(struct StrList *lst, char *val);

char *get_str(struct StrList *lst, int idx);

void remove_str(struct StrList *lst, int idx);

void print_str_list(struct StrList *lst);

void insert_str(struct StrList *lst, int idx, char *val);

int index_str(struct StrList *lst, char *val);

char *pop_str(struct StrList *lst, int idx);

int str_list_size(struct StrList *lst);

char contains_str(struct StrList *lst, char *val);

void free_str_list(struct StrList *lst);

void assign_str(struct StrList *lst, int idx, char *val);
