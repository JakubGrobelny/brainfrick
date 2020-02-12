#include <stdio.h>

const unsigned long long tape_size = 65536;
char tape[65536];

int main() {
    char* ptr = tape;
    *(ptr + 0) = getchar();
    *(ptr + 1) += *ptr * 1;
    *(ptr + 0) = 0;
    putchar(*(ptr + 0));
    ptr +=1;
    putchar(*(ptr + 0));
    return 0;
}
