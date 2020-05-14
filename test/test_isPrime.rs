def int isPrime(int a){
  if a<2 {
    return 0;
  }
  else {
    for int i in range(2, a){
      if a%i == 0 {
        return 0;
      }
    }
    return 1;
  }
  return 0;
}

print(isPrime(1));
print(isPrime(5)); 
print(isPrime(8)); 
