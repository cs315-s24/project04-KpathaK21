#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "project04.h"
#include "rv_emu.h"

extern bool g_verbose;

void copy_array(int *dst, int *src, int len) {
    for (int i = 0; i < len; i++) {
        dst[i] = src[i];
    }
}

void print_array(char *prefix, int *arr, int len) {
    printf("%s", prefix);
    for (int i = 0; i < len; i++) {
        printf(" %d", arr[i]);
    }
    printf("\n");
}

void fib_rec_test(int start_arg, char **argv, rv_state *state) {
    int r;
    int n = atoi(argv[start_arg]);

    r = fib_rec_c(n);
    printf("C: %d\n", r);

    r = fib_rec_s(n);
    printf("Asm: %d\n", r);

    rv_init(state, (uint32_t*) fib_rec_s, n, 0, 0, 0);
    r = rv_emulate(state);
    printf("Emu: %d\n", r);
}

void get_bitseq_test(int start_arg, char **argv, rv_state *state) {
    int r;
    int n = atoi(argv[start_arg]);
    int start = atoi(argv[start_arg + 1]);
    int end = atoi(argv[start_arg + 2]);

    r = get_bitseq_c(n, start, end);
    printf("C: %d\n", r);

    r = get_bitseq_s(n, start, end);
    printf("Asm: %d\n", r);

    rv_init(state, (uint32_t *) get_bitseq_s, n, start, end, 0);
    r = rv_emulate(state);
    printf("Emu: %d\n", r);
}

void pal_test(int start_arg, char **argv, rv_state *state) {
    bool r;
    char *str = argv[start_arg];
    int end = strlen(str) - 1;

    r = pal_rec_c(str, 0, end);
    printf("C: %s\n", r ? "is a palindrome" : "is not a palindrome");

    r = pal_rec_s(str, 0, end);
    printf("Asm: %s\n", r ? "is a palindrome" : "is not a palindrome");

    rv_init(state, (uint32_t*) pal_rec_s, (uint64_t) str, 0, end, 0);
    r = rv_emulate(state);
    printf("Emu: %s\n", r ? "is a palindrome" : "is not a palindrome");
}

void int_to_str_test(int start_arg, char **argv, rv_state *state) {
    char output_str[MAX_ARRAY];

    int value = atoi(argv[start_arg]);
    int base = 10;
    if (!strncmp(argv[start_arg + 1], "-o", 2))
        base = atoi(argv[start_arg + 2]);

    memset(output_str, 0, sizeof(output_str));
    int_to_str_c(value, output_str, base);
    printf("C: %s\n", output_str);

    memset(output_str, 0, sizeof(output_str));
    int_to_str_s(value, output_str, base);
    printf("Asm: %s\n", output_str);

    memset(output_str, 0, sizeof(output_str));
    rv_init(state, (uint32_t*) int_to_str_s, value, (uint64_t) output_str, base, 0);
    rv_emulate(state);
    printf("Emu: %s\n", output_str);
}

void max3_test(int start_arg, char **argv, rv_state *state) {
    int r;
    int a = atoi(argv[start_arg]);
    int b = atoi(argv[start_arg + 1]);
    int c = atoi(argv[start_arg + 2]);

    r = max3_c(a, b, c);
    printf("C: %d\n", r);

    r = max3_s(a, b, c);
    printf("Asm: %d\n", r);

    rv_init(state, (uint32_t *) max3_s, a, b, c, 0);
    r = rv_emulate(state);
    printf("Emu: %d\n", r);
}

void midpoint_test(int start_arg, char **argv, rv_state *state) {
    int r;
    int start = atoi(argv[start_arg]);
    int end = atoi(argv[start_arg + 1]);

    r = midpoint_c(start, end);
    printf("C: %d\n", r);

    r = midpoint_s(start, end);
    printf("Asm: %d\n", r);

    rv_init(state, (uint32_t *) midpoint_s, start, end, 0, 0);
    r = rv_emulate(state);
    printf("Emu: %d\n", r);
}

void quadratic_test(int start_arg, char **argv, rv_state *state) {
    int r;
    int x = atoi(argv[start_arg]);
    int a = atoi(argv[start_arg + 1]);
    int b = atoi(argv[start_arg + 2]);
    int c = atoi(argv[start_arg + 3]);

    r = quadratic_c(x, a, b, c);
    printf("C: %d\n", r);

    r = quadratic_s(x, a, b, c);
    printf("Asm: %d\n", r);

    rv_init(state, (uint32_t *) quadratic_s, x, a, b, c);
    r = rv_emulate(state);
    printf("Emu: %d\n", r);
}

void sort_test(int start_arg, int argc, char **argv, rv_state *state) {
    int tmp[MAX_ARRAY];
    int a[MAX_ARRAY] = {0,};
    int len = 0;

    for (int i = 0; i < argc - start_arg; i++) {
        a[i] = atoi(argv[i + start_arg]);
        len++;
    }
    
    copy_array(tmp, a, len);
    sort_c(tmp, len);
    print_array("C:", tmp, len);

    copy_array(tmp, a, len);
    sort_s(tmp, len);
    print_array("Asm:", tmp, len);

    copy_array(tmp, a, len);
    rv_init(state, (uint32_t*) sort_s, (uint64_t) tmp, len, 0, 0);
    rv_emulate(state);
    print_array("Emu:", tmp, len);
 
}

void str_to_int_test(int start_arg, int argc, char **argv, rv_state *state) {
    char *str = argv[start_arg];
    int base = 10;
    if (!strncmp(str, "0b", 2)) {
        base = 2;
        str += 2;
    } else if (!strncmp(str, "0x", 2)) {
        base = 16;
        str += 2;
    }

    int r = str_to_int_c(str, base);
    printf("C: %d\n", r);

    r = str_to_int_s(str, base);
    printf("Asm: %d\n", r);

    rv_init(state, (uint32_t*) str_to_int_s, (uint64_t) str, base, 0, 0);
    r = rv_emulate(state);
    printf("Emu: %d\n", r);
}

void to_upper_test(int start_arg, char **argv, rv_state *state) {
    // Make a copy of the string since we modify it in place
    char tmp[MAX_ARRAY];
    char *str = argv[start_arg];

    strncpy(tmp, str, MAX_ARRAY);
    to_upper_c(str, tmp);
    printf("C: %s\n", tmp);

    strncpy(tmp, str, MAX_ARRAY);
    to_upper_s(str, tmp);
    printf("Asm: %s\n", tmp);

    strncpy(tmp, str, MAX_ARRAY);
    rv_init(state, (uint32_t*) to_upper_s, (uint64_t) str, (uint64_t) tmp, 0, 0);
    rv_emulate(state);
    printf("Emu: %s\n", tmp);
}

int parse_params(int argc, char **argv, rv_state *state) {
    // Parse optional command line parameters which must precede
    // the name of the emulated program and its parameters
    state->analyze = false;
    state->cache_sim = false;
    state->i_cache.type = CACHE_NONE;

    int i;
    for (i = 1; i < argc; i++) {
        if (argv[i][0] != '-') {
            // Return the index of the first non-optional parameter,
            // which is the name of the emulated program
            break;
        } else if (!strcmp(argv[i], "-a")) {
            state->analyze = true;
        } else if (!strcmp(argv[i], "-dm")) {
            state->cache_sim = true;
            state->i_cache.type = CACHE_DM;
            state->i_cache.size = atoi(argv[i + 1]);
            state->i_cache.block_size = atoi(argv[i + 2]);
            state->i_cache.ways = 1;
            i += 2;
        } else if (!strcmp(argv[i], "-sa")) {
            state->cache_sim = true;
            state->i_cache.type = CACHE_SA;
            state->i_cache.size = atoi(argv[i + 1]);
            state->i_cache.block_size = atoi(argv[i + 2]);
            state->i_cache.ways = atoi(argv[i + 3]);
            i += 3;
        } else if (!strcmp(argv[i], "-v")) {
            state->verbose = true;
            g_verbose = true;
        }
    }
    return i;
}

void usage() {
    printf("usage: project04 <prog> [<arg1> ...]\n");
    exit(-1);
}

int main(int argc, char **argv) {
    if (argc == 1) {
        usage();
    }

    rv_state state;
    int i = parse_params(argc, argv, &state);
    
    if (!strcmp(argv[i], "fib_rec")) {
        fib_rec_test(i + 1, argv, &state);
    } else if (!strcmp(argv[i], "get_bitseq")) {
        get_bitseq_test(i + 1, argv, &state);
    } else if (!strcmp(argv[i], "int_to_str")) {
        int_to_str_test(i + 1, argv, &state);
    } else if (!strcmp(argv[i], "is_pal")) {
        pal_test(i + 1, argv, &state);
    } else if (!strcmp(argv[i], "max3")) {
        max3_test(i + 1, argv, &state);
    } else if (!strcmp(argv[i], "midpoint")) {
        midpoint_test(i + 1, argv, &state);
    } else if (!strcmp(argv[i], "quadratic")) {
        quadratic_test(i + 1, argv, &state);
    } else if (!strcmp(argv[i], "sort")) {
        sort_test(i + 1, argc, argv, &state);
    } else if (!strcmp(argv[i], "str_to_int")) {
        str_to_int_test(i + 1, argc, argv, &state);
    } else if (!strcmp(argv[i], "to_upper")) {
        to_upper_test(i + 1, argv, &state);
    } else {
        usage();
    }

    if (state.analyze) {
        rv_print(&state.analysis);
    }

    if (state.cache_sim) {
        cache_print(&state.i_cache, "(I)");
    }

    return 0;
}
