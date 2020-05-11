#include <math.h>
#include "exp.h"

int pow_int(int n1, int n2){
    int ans = pow((double) n1, (double) n2);
    return ans;
}

double pow_float(double n1, double n2) {
    double ans = pow(n1, n2);
    return ans;
}
