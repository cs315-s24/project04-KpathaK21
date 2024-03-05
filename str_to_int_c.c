#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t char_to_digit(char ch, int base) {
    switch (base) {
        case 2:
            if (ch == '0' || ch == '1') {
                return ch - '0';
            }
            break;
        case 10:
            if (ch >= '0' && ch <= '9') {
                return ch - '0';
            }
            break;
        case 16:
            if (ch >= '0' && ch <= '9') {
                return ch - '0';
            }
            if (ch >= 'a' && ch <= 'f') {
                return ch - 'a' + 0xa;
            }
            if (ch >= 'A' && ch <= 'F') {
                return ch - 'A' + 0xa;
            }
            break;
    }
}

int str_to_int_c(char *str, int base) {
    int retval = 0;
    int place_val = 1;
    int digit;

    for (int i = strlen(str) - 1; i >= 0; i--) {
        digit = char_to_digit(str[i], base);
        retval += digit * place_val;
        place_val *= base;
    }
    return retval;
}
