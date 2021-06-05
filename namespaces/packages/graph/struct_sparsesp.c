#include<stdio.h>
#include<stdlib.h>
struct sparse_vec {
    int * index;
    int * value;
    int length;
};

void sparse_scalarprod(int * Y, struct sparse_vec *X, struct sparse_vec *X2){
        static int T1[1];
        int t2 = 0;
        int i1 = 0;
        int i2 = 0; 
        while(i1 < (*X).length && i2 < (*X).length) {
            if((*X2).index[i2] < (*X).index[i1]) {
                i2 = i2 +1;
            }
            else if((*X).index[i1] < (*X2).index[i2]) {
                 i1 = i1 +1;
            }
            else {
                t2 = t2 + (*X).value[i1] * (*X2).value[i2];
                i1 = i1 + 1;
                i2 = i2 + 1;
             }
         }
         T1[0] = t2;
         Y[0] = T1[0];
}

int main(){
    struct sparse_vec * a = malloc(sizeof(struct sparse_vec));
    (*a).index = malloc(3 * sizeof(int));
    (*a).index[0] = 1;
    (*a).index[1] = 2;
    (*a).index[2] = 3;
    (*a).value = malloc(3 * sizeof(int));
    (*a).value[0] = 2;
    (*a).value[1] = 3;
    (*a).value[2] = 6;
    (*a).length = 3;
    struct sparse_vec * b = malloc(sizeof(struct sparse_vec));
    (*b).index = malloc(3 * sizeof(int));
    (*b).index[0] = 0;
    (*b).index[1] = 2;
    (*b).index[2] = 3;
    (*b).value = malloc(3 * sizeof(int));
    (*b).value[0] = 3;
    (*b).value[1] = 4;
    (*b).value[2] = 8;
    (*b).length = 3;
    int out = 0;

    sparse_scalarprod(&out, a, b);
    printf("%d\n", out);
    return 0;
}