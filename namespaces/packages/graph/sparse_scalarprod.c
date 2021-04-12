#include <stdio.h>
void sparse_scalarprod(int * Y, int *X, int * X2, int n) {
    static int T1[1];
    int t2  = 0;
    for(int i1 = 0; i1 < n; i1++) {
        if(X[i1] != 0 && X2[i1] != 0) {
            t2 = t2 + X[i1] * X2[i1];
        }
    }
    T1[0] = t2;
    Y[0] = T1[0];
}

int main() {
    int a[5] = {0, 2, 3, 6, 0};
    int b[5] = {3, 0, 4, 8, 1};
    int * a2 = a;
    int * b2 = b;
    int out = 0;
    sparse_scalarprod(&out, a2, b2, 5);
    printf("%d\n", out);
    return 0;
}