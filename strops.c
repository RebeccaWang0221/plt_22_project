#include <stdio.h>
#include <string.h>

int str_comp(char *s1, char *s2) {
    int res = strcmp(s1, s2);
    if (res == 0) {
        return 1;
    } else {
        return 0;
    }
}

int str_diff(char *s1, char *s2) {
    int res = strcmp(s1, s2);
    if (res == 0) {
        return 0;
    } else {
        return 1;
    }
}
