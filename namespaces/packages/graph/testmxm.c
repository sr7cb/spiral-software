#include <stdio.h>
#include <stdlib.h>

/*(void mxm(int  *Y, int  *X) {
    for(int i1 = 0; i1 <= 2; i1++) {
        for(int i2 = 0; i2 <= 2; i2++) {
            printf("iteration i:%d j:%d ", i1, i2);
            static int T1[1];
            static int T2[6];
            for(int i3 = 0; i3 <= 5; i3++) {
                T2[i3] = X[((((0 <= i3) && (i3 <= 2))) ? (((3*i1) + i3)) : ((3*(i3 - 3)) + i2))];
            }
            for(int i = 0; i < 6; i ++) {
                printf("%d\t", T2[i]);
            }
            printf("\n");
            int t1 = 0;
            for(int i11 = 0; i11 <= 5; i11++) {
                t1 = (t1 + (X[(i11 + 3)]*T2[i11]));
            }
            T1[0] = t1;
            printf("result is %d\n", T1[0]);
            Y[((3*i1) + i2)] = T1[0];
        }
    }
}*/

int main() {
    int * output = malloc(9 * sizeof(int));
    int * input = malloc(9 * sizeof(int)); 
    input[0] = 6;
    input[1] = 2;
    input[2] = 2;
    input[3] = 7;
    input[4] = 5;
    input[5] = 4;
    input[6] = 1;
    input[7] = 9;
    input[8] = 3;

    for(int i = 0; i < 9; i++){
        output[i] = 0;
    }

    mxm(output, input);

    for(int i = 0; i < 3; i++) {
        for(int j = 0; j < 3; j++) {
            printf("%d\t", output[i*3 + j]);
        }
        printf("\n");
    }
    return 0;
}