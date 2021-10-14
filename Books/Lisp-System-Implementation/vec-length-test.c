#include <stdio.h>

#define cell int

#define vecsize(k) (2 + ((k) + sizeof(cell)-1) / sizeof(cell))

int main() {

    size_t ans = sizeof(int);
    printf("size of int: %li\n", ans);
    printf("size of short: %li\n", sizeof(short));

    printf("%li\n", vecsize(3));
    printf("%li\n", vecsize(4));
    printf("%li\n", vecsize(5));
    printf("%li\n", vecsize(9));
    return 0;
}
