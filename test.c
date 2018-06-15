#include <stdio.h>

typedef struct APerson {
    char* name;
    float weight;
} APerson;

typedef struct Dog {
    char* name;
    int age;
    APerson owner;
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
        char* name = "Fiddo";
        printf("We'll call him %s!\n", name);
        block_name1 = name;
    };
    fiddo.name = block_name1;
    fiddo.age = 5;
    fiddo.owner.name = "John";
    printf("%s's age is %d in human years and %d in dog years!\n", fiddo.name, fiddo.age, factorial(fiddo.age));
    for (int i = 0; i < fiddo.age + 1; ++i) {
        printf("factorial(%d) = %d\n", i, factorial(i));
    };
    printf("Who knew dog years where calculated using factorial!\n");
    int block_name2;
    {
        printf("Happy birthday %s!\n", fiddo.name);
        fiddo.age += 1;
        block_name2 = fiddo.age;
    };
    printf("%s is now %d years old!\n", fiddo.name, block_name2);
    return 0;
}
