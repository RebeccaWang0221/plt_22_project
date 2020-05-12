#include <stdio.h>
#include <stdlib.h>
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

char *str_concat(char *s1, char *s2) {
    int len = strlen(s1) + strlen(s2);
    char *new_str = (char *)malloc(sizeof(char) * len);
    strcpy(new_str, s1);
    strcat(new_str, s2);
    return new_str;
}

int str_size(char *s) {
    int len = 0;
    for (int i = 0; s[i] != '\0'; i++) {
        len++;
    }
    return len;
}

char contains_strstr(char *s, char *c) {
    if (strstr(s, c) == NULL) {
        return 0;
    } else {
        return 1;
    }
}

char *access_str(char *s, int idx) {
    if (idx < str_size(s)) {
      char ch = s[idx];
      char *ch2 = (char *)malloc(sizeof(char));
      *ch2 = ch;
      return ch2;
    }
    return "";
}
