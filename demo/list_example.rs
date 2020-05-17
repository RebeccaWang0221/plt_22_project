
print("- After Initialization:");

list<int> lst = [1,2,4];
print(lst);

print("- After append 5:");
lst.append(5);
print(lst);

print("- After insert 3 into index 2:");
lst.insert(2, 3);
print(lst);

print("- After remove last index:");
lst.remove(len(lst) - 1);
print(lst);

print("- After pop index 0:");
int y = lst.pop(0);
print(y);
print(lst);

print("- Index of 3:");
print(lst.index(3));

print("- Length of list:");
print(len(lst));
