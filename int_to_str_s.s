.global int_to_str_s
.align 4

# a0: 32 bit signed integer
# a1: output string, assumed to be 32 chars
# a2: output base
int_to_str_s:
    li t4, 2
    bne a2, t4, prefix_not_bin
    li t4, '0'              # base 2, prefix "0b"
    sb t4, (a1)
    li t4, 'b'
    sb t4, 1(a1)
    addi a1, a1, 2
prefix_not_bin:
    li t4, 16
    bne a2, t4, prefix_none # base 10, no prefix
    li t4, '0'              # must be base 16, prefix "0x"
    sb t4, (a1)
    li t4, 'x'
    sb t4, 1(a1)
    addi a1, a1, 2
prefix_none:
    mv a3, a1               # save for later reversing

mod_loop:
    beq a0, zero, reverse
    rem t0, a0, a2          # t0 = num % base
    div a0, a0, a2          # a0 = num / base
    li t1, 9
    bgt t0, t1, mod_letter
    addi t0, t0, '0'        # t0 is <= 9, turn it into a numeric char
    j mod_store
mod_letter:
    addi t0, t0, 55         # t0 is > 9, turn it into a letter ('A' - 0xa = 55)
mod_store:
    sb t0, (a1)
    addi a1, a1, 1          # string++
    j mod_loop
reverse:
    addi a1, a1, -1         # back up a1 from the '\0' to the last char
rev_loop:
    blt a1, a3, rev_done    # stop when pointers cross
    lb t1, (a1)             # reverse output string
    lb t3, (a3)
    sb t1, (a3)
    sb t3, (a1)
    addi a3, a3, 1          # walk pointers toward each other
    addi a1, a1, -1
    j rev_loop
rev_done:
    sb zero, 1(a3)          # null terminate the output str
    ret
