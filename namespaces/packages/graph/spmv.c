#include<stdio.h>
#include<stdlib.h>
struct sparse_vec {
    int * index;
    int * value;
    int length;
};

//void spmv(int *y, int * x, struct sparse_vec *v) {
//    int itr = 0;
//    for(int i = 0; i < 3; i++) {
//        if((*v).index[itr] == i) {
//            printf("entered %d", (*v).index[itr]);
//            for(int j = x[i]; j < x[i+1]; j++) {
//                y[x[3+1+j]] = y[x[3+1+j]] + x[3+x[3]+j+1] * (*v).value[itr];
//                printf("\n");            
//            }
//            itr++;
//        }
//    }
//}

void spmv(int  *Y, int  *X, struct sparse_vec *spr_arr1, int n1) {
    int i5, i6, itr1;
    itr1 = 0;
    for (int i5 = 0; i5 < n1; i5++) {
        if ((((*spr_arr1).index[itr1] == i5))) {
            for (int i6 = X[i5]; i6 < X[(i5 + 1)]; i6++) {
                Y[X[(n1 + (1 + i6))]] = (Y[X[(n1 + (1 + i6))]] + (X[(n1 + (X[n1] + (i6 + 1)))]*(*spr_arr1).value[itr1]));
            }
            itr1 = (itr1 + 1);
        }
    }
}

int main() {
    int * output = malloc(3 * sizeof(int));
    int * input = malloc(12 * sizeof(int));
    /* 6    0   2
       0    0   0  
       1    9   0
    */
    input[0] = 0;
    input[1] = 2;
    input[2] = 3;
    input[3] = 4;
    input[4] = 0;
    input[5] = 2;
    input[6] = 2;
    input[7] = 0;
    input[8] = 6;
    input[9] = 1;
    input[10] = 9;
    input[11] = 2;

    struct sparse_vec * a = malloc(sizeof(struct sparse_vec));
    (*a).index = malloc(2 * sizeof(int));
    (*a).index[0] = 0;
    (*a).index[1] = 2;
    (*a).value = malloc(2 * sizeof(int));
    (*a).value[0] = 2;
    (*a).value[1] = 2;
    (*a).length = 3;


    for(int i = 0; i < 3; i++){
        output[i] = 0;
    }

    //mxm2(output, input, input2);
    spmv(output, input, a, 3);
    printf("\n");
    for(int i = 0; i < 3; i++) {
            printf("%d\t", output[i]);
    }
    printf("\n");
   /* 16, 0, 2*/
    return 0;
}