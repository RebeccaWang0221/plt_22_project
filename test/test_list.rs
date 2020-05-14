list<string> x = ["apple", "banana", "cherry"];
print(x[1]);
x.append("dragonfruit");
x.insert(0, "test");
print(x);
print(len(x));

list<int> y = [1, 2, 3, 4];
print(y[1]);				
y.append(5);			
y.remove(1);	
y.insert(1,3); #! insert 3 at y[1] !#
print(y);

list<int> z;
for int i in range(10){
    z.append(i);
} 
print(z);




