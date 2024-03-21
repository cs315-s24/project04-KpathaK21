#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "rv_emu.h"
#include "bits.h"

#define DEBUG 0




static void unsupported(char *s, uint32_t n) {
    printf("unsupported %s 0x%x\n", s, n);
    exit(-1);
}

static uint32_t get_rd(uint32_t iw) {
    return get_bits(iw, 7, 5);
}

static uint32_t get_funct3(uint32_t iw) {
    return get_bits(iw, 12, 3);
}

static uint32_t get_rs1(uint32_t iw) {
    return get_bits(iw, 15, 5);
}

static uint32_t get_rs2(uint32_t iw) {
    return get_bits(iw, 20, 5);
}

static int64_t sb_offset(uint32_t iw) {
    uint32_t bit11 = get_bit(iw, 7);
    uint32_t bit12 = get_bit(iw, 31);
    uint32_t bits10_5 = get_bits(iw, 25, 6);
    uint32_t bits4_1 = get_bits(iw, 8, 4);
    uint32_t offset = 0;
    
    offset |= (bit12 << 12) | (bit11 << 11) | (bits10_5 << 5) | (bits4_1 << 1);
    int64_t signed_offset = sign_extend(offset, 13);
    return signed_offset;
}


static void run_r_format(rv_state *s, uint32_t iw) {
    uint32_t rd = get_rd(iw);
    uint32_t func3 = get_funct3(iw);
    uint64_t rs1 = get_rs1(iw);
    uint64_t rs2 = get_rs2(iw);
    bool bit_30 = get_bit(iw, 30);
    bool bit_25 = get_bit(iw, 25);

    switch (func3) {
        case 0b000:
            if (bit_25) // mul
                s->regs[rd] = s->regs[rs1] * s->regs[rs2];
            else if (bit_30)  // sub
                s->regs[rd] = s->regs[rs1] - s->regs[rs2];
            else  // add
                s->regs[rd] = s->regs[rs1] + s->regs[rs2];
            break;
        case 0b001:  // sll
            s->regs[rd] = s->regs[rs1] << s->regs[rs2];
            break;
        case 0b101:  // sra if bit_30, else srl
            if (bit_30)
                s->regs[rd] = ((int32_t) s->regs[rs1]) >> s->regs[rs2];
            else
                s->regs[rd] = s->regs[rs1] >> s->regs[rs2];
            break;
        case 0b111:  // and
            s->regs[rd] = s->regs[rs1] & s->regs[rs2];
            break;
        default:
            unsupported("r format func3", func3);
    }
    s->pc += 4;
}




static void run_i_format(rv_state *s, uint32_t iw, rv_format fmt) {
    uint32_t rd = get_rd(iw);
    uint32_t func3 = get_funct3(iw);
    uint64_t v1 = s->regs[get_rs1(iw)];
    uint32_t imm_unsigned = get_bits(iw, 20, 12);
    int64_t imm = sign_extend(imm_unsigned, 12);
    uint32_t shamt = get_bits(iw, 20, 5);
    int64_t sum = v1 + imm;
        
    switch (func3) {
        case 0b0:  // addi
            if (fmt == FMT_I_JALR)
                s->pc = (uint64_t) sum;  // for ret/jalr, pc = rs1 + offset
            else if (fmt == FMT_I_ARITH)
                s->regs[rd] = sum;  // arithmetic sum
            break;

       case 0b001:  // slli
            s->regs[rd] = v1 << shamt;
            break;
        case 0b101:  // srXi
            bool arith = get_bits(iw, 30, 1);
            if (arith)
                s->regs[rd] = ((int64_t) v1) >> shamt;
            else
                s->regs[rd] = v1 >> shamt;
            break;
            break;
        case 0b100: // lb
            s->regs[rd] = (int64_t)(*(int8_t*)(s->regs[get_rs1(iw)] + imm));
            break;
        case 0b011: // ld
            s->regs[rd] = *((int64_t*)(s->regs[get_rs1(iw)] + imm));
            break;
        case 0b010: // lw
            s->regs[rd] = *((int32_t*)(s->regs[get_rs1(iw)] + imm));
            break;
        case 0b110: // li
            s->regs[rd] = imm;
            break;

        default:
            unsupported("i format func3", func3);
    }

    if (fmt != FMT_I_JALR)
        s->pc += 4;

}



static void run_sb_format(rv_state *s, uint32_t iw) {
    uint32_t func3 = get_funct3(iw);
    uint64_t v1 = s->regs[get_rs1(iw)];
    uint64_t v2 = s->regs[get_rs2(iw)];
    bool taken = false;

    switch(func3) {
        case 0b000:  // beq
            taken = v1 == v2;
            break;
        case 0b001:  // bne
            taken = v1 != v2;
            break;
        case 0b100:  // blt
            taken = ((int64_t) v1) < ((int64_t) v2);
            break;
        case 0b101:  // bge
            taken = ((int64_t) v1) >= ((int64_t) v2);
            break;
        case 0b110:	//bltu
        	taken = ((int64_t) v1) < ((int64_t) v2);
        	break;
        case 0b111: //bgeu
        	taken = ((int64_t) v1) >= ((int64_t) v2);
        	break;
        default:
            unsupported("sb func3", func3);
    }

    if (taken)
        s->pc += sb_offset(iw);
    else
        s->pc += 4;
}

static void run_uj_format(rv_state *s, uint32_t iw) {
    /* if rd != 0 it's a jal, so link ra
     * if rd == 0 it's a j, so don't change ra
     * either way, calc the offset and apply it to pc
     */
    uint32_t rd = get_rd(iw);
    if (rd != 0)
        s->regs[rd] = ((uint64_t) s->pc) + 4;

    uint32_t offset = 0;
    uint32_t bit20 = get_bit(iw, 31);
    uint32_t bit11 = get_bit(iw, 20);
    uint32_t bits10_1 = get_bits(iw, 21, 10);
    uint32_t bits19_12 = get_bits(iw, 12, 8);

    offset |= (bit20 << 19) | (bits19_12 << 11) | (bit11 << 10) | bits10_1;
    int64_t signed_offset = sign_extend(offset, 20) * 2;
    s->pc += signed_offset;
}



static void rv_one(rv_state *state) {
  uint32_t iw = *((uint32_t*) state->pc);
  uint32_t opcode = get_bits(iw, 0, 7);

#if DEBUG
  printf("iw: %x\n", iw);
#endif

   // Update instruction count
  state->analysis.i_count++;

  switch (opcode) {
    case FMT_R:
      state->analysis.ir_count++;
      run_r_format(state, iw);
      break;
    case FMT_I_LOAD:
      state->analysis.ir_count++;
      state->analysis.ld_count++;
      if (state->cache_sim) {
        uint32_t data = cache_lookup(&state->i_cache, state->pc);
        // Use the data obtained from the cache
      } else {
        uint32_t data = *((uint32_t*) state->pc);
      }
      run_i_format(state, iw, opcode);
      break;
   case FMT_I_JALR:
      state->analysis.ir_count++;
      state->analysis.j_count++;
      if (state->cache_sim) {
        uint32_t data = cache_lookup(&state->i_cache, state->pc);
        // Use the data obtained from the cache
      } else {
        uint32_t data = *((uint32_t*) state->pc);
      }
      run_i_format(state, iw, opcode);
      break;
    case FMT_I_ARITH:
      state->analysis.ir_count++;
      run_i_format(state, iw, opcode);
      break;
   case FMT_SB:
      run_sb_format(state, iw);
      // Update conditional branch counters
      if (get_bit(iw, 14)) { // Check if it's a conditional branch
        if (state->pc != (uint64_t) state->pc + sb_offset(iw)) {
          state->analysis.b_taken++;
        } else {
          state->analysis.b_not_taken++;
        }
      }
      break;
    case FMT_UJ:
      state->analysis.j_count++;
      run_uj_format(state, iw);
      break;
	case FMT_S:
      state->analysis.ir_count++;
      run_s_format(state, iw);
      break;
    default:
      unsupported("format", opcode);
    
  }
}



void rv_init(rv_state *state, uint32_t *target, 
             uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3) {
    state->pc = (uint64_t) target;
    state->regs[RV_A0] = a0;
    state->regs[RV_A1] = a1;
    state->regs[RV_A2] = a2;
    state->regs[RV_A3] = a3;

    state->regs[RV_ZERO] = 0;  // zero is always 0  (:
    state->regs[RV_RA] = RV_STOP;
    state->regs[RV_SP] = (uint64_t) &state->stack[STACK_SIZE];

    memset(&state->analysis, 0, sizeof(rv_analysis));
    cache_init(&state->i_cache);
}

uint64_t rv_emulate(rv_state *state) {
    while (state->pc != RV_STOP) {
        rv_one(state);
    }
    return state->regs[RV_A0];
}

static void print_pct(char *fmt, int numer, int denom) {
    double pct = 0.0;

    if (denom)
        pct = (double) numer / (double) denom * 100.0;
    printf(fmt, numer, pct);
}

void rv_print(rv_analysis *a) {
    int b_total = a->b_taken + a->b_not_taken;

    printf("=== Analysis\n");
    print_pct("Instructions Executed  = %d\n", a->i_count, a->i_count);
    print_pct("R-type + I-type        = %d (%.2f%%)\n", a->ir_count, a->i_count);
    print_pct("Loads                  = %d (%.2f%%)\n", a->ld_count, a->i_count);
    print_pct("Stores                 = %d (%.2f%%)\n", a->st_count, a->i_count);    
    print_pct("Jumps/JAL/JALR         = %d (%.2f%%)\n", a->j_count, a->i_count);
    print_pct("Conditional branches   = %d (%.2f%%)\n", b_total, a->i_count);
    print_pct("  Branches taken       = %d (%.2f%%)\n", a->b_taken, b_total);
    print_pct("  Branches not taken   = %d (%.2f%%)\n", a->b_not_taken, b_total);
}


