print("-------------------------------------------");
print("*** Normal range loop ***");

for int i in range(5) {
    print(i);
}

print("-------------------------------------------");
print("*** Range loop w/ start, end, and step ***");

for int j in range(0, 10, 2) {
    print(j);
}

print("-------------------------------------------");
print("*** for in loop *** ");

list<float> lst = [1.1, 2.3, 3.4];
for float v in lst {
    print(v);
}

print("-------------------------------------------");
print("*** Enhanced irange loop ***");

int arr[] = {1,2,3,4,5};
for int z in irange(arr) {
    print(z);
}

print("-------------------------------------------");
