#include <iostream>

using namespace std;
int main() {
    int n = 3;
    int * a = new int[16];
    int * b = new int[16];
    a[0] = 0;
    a[1] = 1;
    a[2] = 3;
    a[3] = 6;

    a[4] = 1;
    a[5] = 0;
    a[6] = 2;
    a[7] = 0;
    a[8] = 1;
    a[9] = 2;

    a[10] = 2;
    a[11] = 3;
    a[12] = 4;
    a[13] = 1;
    a[14] = 1;
    a[15] = 1;
    
    b[0] = 0;
    b[1] = 2;
    b[2] = 4;
    b[3] = 6;

    b[4] = 0;
    b[5] = 2;
    b[6] = 0;
    b[7] = 2;
    b[8] = 0;
    b[9] = 2;

    b[10] = 8;
    b[11] = 2;
    b[12] = 5;
    b[13] = 4;
    b[14] = 9;
    b[15] = 6;

    int * c = new int[9];
    for(auto i = 0; i < 9; i++) {
        c[i] = 0;
    }
    for(auto row = 0; row < n; row++){
        for(auto val = a[row]; val < a[row+1]; val++){
            for (auto bval = b[*(a+n+1+val)]; bval < b[*(a+n+1+val)+1]; bval++){
                cout << "index 1: " << *(a+n+a[n] + 1 + val)<<
                " index 2: " << *(b+n+b[n] + 1 + bval) << endl;
                int result = *(a+n+a[n] + 1 + val) * *(b+n+b[n] + 1 + bval);   
                c[row*3+b[n+1+bval]] += result;
                cout << endl;
            }
        }
    }
    for(int i = 0; i < 3; i++){
        for(int j = 0; j < 3; j++){
            cout << c[i*3+j] << "\t";
        }
        cout << endl;
    }
    return 0;
}
