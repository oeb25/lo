#include <stdio.h>

typedef struct Dog {
    char* name;
} Dog;

int factorial(int x) {
    int if_block_name_0;
    if (x > 1) {
        if_block_name_0 = (x * factorial(x - 1));
    } else {
        if_block_name_0 = x;
    };
    return if_block_name_0;
}

int main() {
    Dog fiddo;
    char* block_name1;
    {
        printf("%d we are printing in here!\n", 123);
        block_name1 = "Fiddo";
    };
    fiddo.name = block_name1;
    for (int i = 0; i < 36; ++i) {
        printf("factoriall(%d) = %d\n", i, factorial(i));
    };
    char* block_name2;
    {
        printf("%d\n", 1337);
        block_name2 = fiddo.name;
    };
    printf("%d %s\n", factorial(19), block_name2);
    return 0;
}
