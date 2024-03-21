.global str_to_int_s
.align 4
# a0 = str at least sig place, a1 = base, a2 = len
# returns: integer representation of the string as a signed word
calculate:
    mv t0, zero                 # t0 is the int ret val
    li t1, 1                    # t1 is place value
calc_loop:
    beq a2, zero, calc_done     # calculated at all places in str
    lb t2, (a0)                 # t2 = *str
    li t3, 'a'
    blt t2, t3, calc_upper_or_numeral
    addi t2, t2, -87            # -'a' + 0xa
    j calc_math
calc_upper_or_numeral:
    li t3, 'A'
    blt t2, t3, calc_numeral
    addi t2, t2, -55            # -'A' + 0xa
    j calc_math
calc_numeral:
    addi t2, t2, -48            # -'0'
calc_math:
    mul t3, t2, t1              # t3 = digit * place value
    add t0, t0, t3              # ret val += t3
    mul t1, t1, a1              # place value *= base
    addi a2, a2, -1             # len--
    addi a0, a0, -1             # str--
    j calc_loop
calc_done:
    mv a0, t0                   # set up ret val
    ret

# a0: string to convert, expressed as a decimal, hex (0x) or binary (0b) string
# a1: input base
# returns: integer representation of the string as a signed word
str_to_int_s:
    addi sp, sp, -8             # prologue
    sd ra, (sp)

# find the length, leaving a0 pointing to the char in the least significant place
si_get_len:
    mv a2, zero                 # a2 is str len and func arg
si_len_loop:
    lb t1, (a0)                 # t1 = *str
    beq t1, zero, si_len_done   # found '\0'?
    addi a2, a2, 1              # len++
    addi a0, a0, 1              # str++
    j si_len_loop
si_len_done:
    addi a0, a0, -1             # back up a char
    call calculate              # a0 = str at least sig place, a1 = base, a2 = len

    ld ra, (sp)                 # epilogue
    addi sp, sp, 8
    ret
