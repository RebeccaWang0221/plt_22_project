def bool and_op(bool x, bool y){
  bool ans;
  ans = x and y;
  return ans;
}

def bool or_op(bool x, bool y){
  bool ans;
  ans = x or y;
  return ans;
}

def bool not_op(bool x){
  bool ans;
  ans = !x;
  return ans;
}

print(and_op(true, true));
print(and_op(false, false));
print(and_op(true, false));
print(or_op(true, false));
print(or_op(false, false));
print(not_op(false));
