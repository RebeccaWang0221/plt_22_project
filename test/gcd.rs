def int computeGCD(int x, int y) {
    int small;
    int gcd;
    if x > y {
        small = y;
    } else {
        small = x;
    }
    for int i in range(1, small + 1) {
        int mod_x = x % i;
        int mod_y = y % i;
        if mod_x == 0 and mod_y == 0 {
            gcd = i;
        }
    }
    return gcd;
}

print(computeGCD(10, 5));
