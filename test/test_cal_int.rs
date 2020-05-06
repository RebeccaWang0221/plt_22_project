def int add(int x, int y){
  return x + y;
}

def int sub(int x, int y){
  return x - y;
}

def int mul(int x, int y){
  return x * y;
}

def int div(int x, int y){
  return x / y;
}

def int mod(int x, int y){
  return x % y;
}

def int inc(int x){
  return x++;
}

def int dec(int x){
  return x--;
}

def int add_equal(int x){
  x+=5;
  return x;
}

def int sub_equal(int x){
  x-=5;
  return x;
}

def int mul_equal(int x){
  x*=5;
  return x;
}

def int div_equal(int x){
  x/=5;
  return x;
}

print(add(10, 5));
print(sub(10, 5));
print(mul(10, 5));
print(inc(10));
print(dec(10));
print(add_equal(10));
print(sub_equal(10));
print(mul_equal(10));
print(div_equal(10));