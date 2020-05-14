#! Basic test file !#
int a = 10;
int b = 20;

int buf = a; 
a = b;
b = buf;

print(a);
print(b);