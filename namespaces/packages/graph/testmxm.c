#include <stdio.h>
#include <stdlib.h>

/*void mxm(int  *Y, int  *X) {
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
            for(int i11 = 0; i11 <= 2; i11++) {
                printf("t1 before: %d\t", t1);
                t1 = (t1 + (T2[(i11 + 3)]*T2[i11]));
                printf("t1 after: %d\n", t1);
            }
            T1[0] = t1;
            printf("result is %d\n", T1[0]);
            Y[((3*i1) + i2)] = T1[0];
        }
    }
}*/



/*void mxm(int  *Y, int  *X) {
    for(int i5 = 0; i5 <= 2; i5++) {
        for(int i6 = 0; i6 <= 2; i6++) {
            printf("iteration i:%d j:%d ", i5, i6);
            static int T3[1];
            static int T4[3];
            for(int i21 = 0; i21 <= 2; i21++) {
                T4[i21] = X[((3*i21) + i6)];
            }
             for(int i = 0; i < 3; i ++) {
                printf("%d\t", T4[i]);
            }
            printf("\n");
            int *t2;
            t2 = X;
            int t4 = 0;
            for(int i25 = 0; i25 <= 2; i25++) {
                printf("values before: %d, %d\t",t2[(3*i5) + i25], T4[i25]);
                t4 = (t4 + (t2[(3*i5) + i25]*T4[i25]));
            }
            T3[0] = t4;
            printf("result is %d\n", T3[0]);
            Y[((3*i5) + i6)] = T3[0];
        }
    }
}
*/

/*void mxm(int  *Y, int  *X) {
    for(int i5 = 0; i5 <= 2; i5++) {
        for(int i6 = 0; i6 <= 2; i6++) {
            printf("iteration i:%d j:%d ", i5, i6);
            static int T1[1];
            static int T2[3];
            for(int i7 = 0; i7 <= 2; i7++) {
                T2[i7] = X[((3*i5) + i7)];
            }
             for(int i = 0; i < 3; i ++) {
                printf("%d\t", T2[i]);
            }
            printf("\n");
            int  *t2;
            t2 = X;
            int t3 = 0;
            for(int i11 = 0; i11 <= 2; i11++) {
                printf("values before: %d, %d\t",t2[(3*i11) + i6], T2[i11]);
                t3 = (t3 + (t2[(3*i11) + i6]*T2[i11]));
            }
            T1[0] = t3;
             printf("result is %d\n", T1[0]);
            Y[((3*i5) + i6)] = T1[0];
        }
    }
}
*/

/*void mxm(double  *Y, double  *X) {
    for(int i5 = 0; i5 <= 2; i5++) {
        for(int i6 = 0; i6 <= 2; i6++) {
            static double T1[1];
            static double T2[3];
            for(int i7 = 0; i7 <= 2; i7++) {
                T2[i7] = X[((3*i5) + i7)];
            }
            int t3 = 0;
            for(int i11 = 0; i11 <= 2; i11++) {
                t3 = (t3 + (X[((3*i11) + i6)]*T2[i11]));
            }
            T1[0] = t3;
            Y[((3*i5) + i6)] = T1[0];
        }
    }
}*/

void mxm(double  *Y, double  *X) {
    for(int i5 = 0; i5 <= 2; i5++) {
        for(int i6 = 0; i6 <= 2; i6++) {
            double  *t2;
            t2 = X;
            static double T1[1];
            T1[0] = 0;
            for (int i7 = 0; i7 < 3; i7++) {
                double T2[1] = {0};
                T2[0] = (T2[0] + X[((3*i5) + i7)]);
                printf("values before: %f, %f\t",t2[(3*i7) + i6], T2[0]);
                T1[0] = (T1[0] + (t2[((3*i7) + i6)]*T2[0]));
                printf("\n");
            }
            Y[((3*i5) + i6)] = T1[0];
        }
    }
}

void mxm2(double  *Y, double  *X, double  *B1) {
    for(int i5 = 0; i5 <= 2; i5++) {
        for(int i6 = 0; i6 <= 2; i6++) {
            double  *t2;
            t2 = X;
            static double T1[1];
            T1[0] = 0;
            for (int i7 = 0; i7 < 3; i7++) {
                double T2[1] = {0};
                T2[0] = (T2[0] + B1[((3*i5) + i7)]);
                 printf("values before: %f, %f\t",t2[(3*i7) + i6], T2[0]);
                T1[0] = (T1[0] + (t2[((3*i7) + i6)]*T2[0]));
                 printf("\n");
            }
            Y[((3*i5) + i6)] = T1[0];
        }
    }
}

/*
void mxmmergedtrace(double  *Y, double  *X) {
    Y[0] = 0;
    for(int i5 = 0; i5 <= 2; i5++) {
        double  *t1;
        t1 = X;
        for (int i7 = 0; i7 < 3; i7++) {
            double T1[1] = {0};
            T1[0] = (T1[0] + X[((3*i5) + i7)]);
            Y[0] = (Y[0] + (t1[((3*i7) + i5)]*T1[0]));
        }
    }
}*/

void mxmmergedtrace(double  *Y, double  *X) {
    Y[0] = 0;
    for (int i5 = 0; i5 < 3; i5++) {
        double T1[1] = {0};
        double  *t2;
        t2 = X;
        T1[0] = T1[0];
        for (int i11 = 0; i11 < 3; i11++) {
            double T2[1] = {0};
            T2[0] = (T2[0] + X[((3*i5) + i11)]);
            T1[0] = (T1[0] + (t2[((3*i11) + i5)]*T2[0]));
        }
        Y[0] = Y[0];
        for (int i8 = 0; i8 < 1; i8++) {
            Y[0] = (Y[0] + T1[0]);
        }
    }
}

void maskedmxm(double  *Y, double  *X, double  *M1) {
    for(int i5 = 0; i5 <= 2; i5++) {
        for(int i6 = 0; i6 <= 2; i6++) {
            if (((M1[((3*i5) + i6)] == 1))) {
                double  *t1;
                t1 = X;
                double T1[1] = {0};
                T1[0] = 0;
                for (int i7 = 0; i7 < 3; i7++) {
                    double T2[1] = {0};
                    T2[0] = (T2[0] + X[((3*i5) + i7)]);
                    T1[0] = (T1[0] + (t1[((3*i7) + i6)]*T2[0]));
                }
                Y[((3*i5) + i6)] = T1[0];
            } else {
                double T3[1];
                T3[0] = 0;
                Y[((3*i5) + i6)] = T3[0];
            }
        }
    }
}

int main() {
    double * output = malloc(9 * sizeof(double));
    double * input = malloc(9 * sizeof(double));
    double * input2 = malloc(9 * sizeof(double));  
    /* 6    2   2
       7    5   4  
       1    9   3
    */
    input[0] = 6;
    input[1] = 2;
    input[2] = 2;
    input[3] = 7;
    input[4] = 5;
    input[5] = 4;
    input[6] = 1;
    input[7] = 9;
    input[8] = 3;

    input2[0] = 1;
    input2[1] = 1;
    input2[2] = 1;
    input2[3] = 0;
    input2[4] = 1;
    input2[5] = 0;
    input2[6] = 0;
    input2[7] = 1;
    input2[8] = 1;

    for(int i = 0; i < 9; i++){
        output[i] = 0;
    }

    //mxm2(output, input, input2);
    maskedmxm(output, input, input2);

    for(int i = 0; i < 3; i++) {
        for(int j = 0; j < 3; j++) {
            printf("%f\t", output[i*3 + j]);
        }
        printf("\n");
    }
      /* 52   40  26
         81   75  46  
         72   74  47
    */
    return 0;
}