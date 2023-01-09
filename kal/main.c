#include <stdio.h>

extern double find();

// double bar(double x) {
//     return x;
// }

int main() {
    printf("result = %lf\n", find());
    return 0;
}