.global midpoint_s
.align 4
midpoint_s:
    add a0, a0, a1
    srli a0, a0, 1
    ret
