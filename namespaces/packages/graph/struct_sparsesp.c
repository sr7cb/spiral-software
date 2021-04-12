#include<stdio.h>
#include<stdlib.h>
struct sparse_vec {
    int * index;
    int * value;
    int * length;
};

void sparse_scalarprod(int * Y, struct sparse_vec *X, struct sparse_vec *X2){
        static int T1[1];
        int t2 = 0;
        int i1 = 0;
        int i2 = 0; 
        while(i1 < X[0].length[0] && i2 < X2[0].length[0]) {
            if(X2[0].index[i2] < X[0].index[i1]) {
                i2 = i2 +1;
            }
            else if(X[0].index[i1] < X2[0].index[i2]) {
                 i1 = i1 +1;
            }
            else {
                t2 = t2 + X[0].value[i1] * X2[0].value[i2];
                i1 = i1 + 1;
                i2 = i2 + 1;
             }
         }
         T1[0] = t2;
         Y[0] = T1[0];
}

int main(){
    struct sparse_vec * a = malloc(sizeof(struct sparse_vec));
    a[0].index = malloc(3 * sizeof(int));
    a[0].index[0] = 1;
    a[0].index[1] = 2;
    a[0].index[2] = 3;
    a[0].value = malloc(3 * sizeof(int));
    a[0].value[0] = 2;
    a[0].value[1] = 3;
    a[0].value[2] = 6;
    a[0].length = malloc(1 * sizeof(int));
    a[0].length[0] = 3;
    struct sparse_vec * b = malloc(sizeof(struct sparse_vec));
    b[0].index = malloc(4 * sizeof(int));
    b[0].index[0] = 0;
    b[0].index[1] = 2;
    b[0].index[2] = 3;
    b[0].index[3] = 4;
    b[0].value = malloc(4 * sizeof(int));
    b[0].value[0] = 3;
    b[0].value[1] = 4;
    b[0].value[2] = 8;
    b[0].value[3] = 1;
    b[0].length = malloc(1 * sizeof(int));
    b[0].length[0] = 4;
    int out = 0;

    sparse_scalarprod(&out, a, b);
    printf("%d\n", out);
    return 0;
}