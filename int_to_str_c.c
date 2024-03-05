#include <stdint.h>

#define CONV_MAX_CHARS 34

void int_to_str_c(uint32_t value, char *result_str, int obase) {
    uint32_t div, rem;
    int i, j = 0, len;
    char tmp[CONV_MAX_CHARS];

    len = 0;
    if (value == 0) {
        tmp[len++] = '0';
    } else {
        while (value != 0) {
            div = value / obase;
            rem = value % obase;
            if (rem <= 9)
                tmp[len] = '0' + rem;
            else if (rem >= 10 && rem <= 15)
                tmp[len] = 'a' - 0xa + rem;
            len += 1;
            value = div;
        }
    }

    /* Put base prefix into result string */
    if (obase == 2) {
        result_str[j] = '0';
        result_str[j + 1] = 'b';
        j += 2;
    } else if (obase == 16) {
        result_str[j] = '0';
        result_str[j + 1] = 'x';
        j += 2;
    }

    /* Reverse tmp string and put into result string */
    for (i = len; i > 0; i--, j++) {
        result_str[j] = tmp[i-1];
    }
    result_str[j] = '\0';
}
