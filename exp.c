#include <stdio.h> 
#include <math.h>
int pow_int(int *n1, int *n2){
    int ans;
    
    ans = pow((double) *n1, (double) *n2);
    return (int)ans;
}

double pow_float(double *n1, double *n2)
{
    double ans;

    ans = pow(*n1, *n2);
    return ans;
}