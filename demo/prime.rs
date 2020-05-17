def string isPrime(int a) {
    if a == 1 {
        return "is not prime";
    } elif a == 2 {
        return "is prime";
    } else {
        for int i in range(2, a) {
            if a % i == 0 {
                return "is not prime";
            }
        }
        return "is prime";
    }
}

print(isPrime(1));
print(isPrime(29));
print(isPrime(190));
