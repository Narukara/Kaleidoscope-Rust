#include <stdio.h>

extern double _find();

int main() {
    printf("result = %lf\n", _find());
    return 0;
}