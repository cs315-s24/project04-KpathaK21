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


// Function to build a 4-bit immediate value from smaller components

uint32_t build_imm4(uint32_t imm4, uint32_t imm3, uint32_t imm2, uint32_t imm1,
                    int len3, int len2, int len1)
{
 
    uint32_t imm = imm4;
    imm = imm << len3;
    imm = imm | imm3;
    imm = imm << len2;
    imm = imm | imm2;
    imm = imm << len1;
    imm = imm | imm1;
    imm = imm << 1;
    return imm;
}

// Function to emulate RISC-V B-type instructions
static void emu_b_type(rv_state *state, uint32_t iw) {
    uint32_t funct3 = get_bits(iw, 12, 3);
    uint32_t rs1 = get_bits(iw, 15, 5);
    uint32_t rs2 = get_bits(iw, 20, 5);
    uint32_t immu1_4 = get_bits(iw, 8, 4);
    uint32_t immu5_10 = get_bits(iw, 25, 6);
    uint32_t immu11 = get_bits(iw, 7, 1);
    uint32_t immu12 = get_bits(iw, 31, 1);
    // Calculate the branch target address
    uint32_t immu = build_imm4(immu12, immu11, immu5_10, immu1_4, 1, 6, 4);
    int64_t imm = sign_extend(immu, 13);
    
 	bool branch_taken = false;

// Check branch condition based on funct3
if (funct3 == 0b001)
    { 
        if (state->regs[rs1] != state->regs[rs2])
        {
            branch_taken = true;
        }
    }
    else if (funct3 == 0b000)
    { 
        if (state->regs[rs1] == state->regs[rs2])
        {
            branch_taken = true;
        }
    }
    else if (funct3 == 0b101)
    { 
        if (((int64_t)state->regs[rs1]) >= ((int64_t)state->regs[rs2]))
        {
            branch_taken = true;
        }
    }
    else if (funct3 == 0b100)
    { 
        if (((int64_t)state->regs[rs1]) < ((int64_t)state->regs[rs2]))
        {
            branch_taken = true;
        }
    }
    else
    {
        unsupported("b-type funct3", funct3);
    }

    
  // Update program counter based on branch result
 
    if (branch_taken)
    {
        state->pc += imm;
        state->analysis.b_taken++;
    }
    else
    {
        state->pc += 4;
        state->analysis.b_not_taken++;
    }
}

// Function to emulate RISC-V I-type instructions
void emu_i_type(rv_state *state, uint32_t iw)
{
    
    uint32_t opcode = get_bits(iw, 0, 7);
    uint32_t rd = get_bits(iw, 7, 5);
    uint32_t funct3 = get_bits(iw, 12, 3);
    uint32_t rs1 = get_bits(iw, 15, 5);
    uint64_t immu = get_bits(iw, 20, 12);
    int64_t imm = sign_extend(immu, 12);
    uint64_t dest = state->regs[rs1] + imm;

// Handle different I-type instructions based on opcode and funct3
   if (opcode == FMT_I_ARITH)
    { 
        if (funct3 == 0b000)
        { // addi
            state->regs[rd] = state->regs[rs1] + imm;
        }
        else if (funct3 == 0b101 && (imm & 0xFC0) == 0b000000)
        {                         
            imm = imm & 0b111111; 
            state->regs[rd] = state->regs[rs1] >> imm;
        }
        else if (funct3 == 0b001 && opcode == FMT_I_ARITH)
        {                                 
            uint32_t shamt = immu & 0x1F; 
            state->regs[rd] = state->regs[rs1] << shamt;
        }
        else
        {
            unsupported("I-type funct3", funct3);
        }
        state->analysis.ir_count += 1;
    }
// Handle load instructions
   else
    { 
        if (funct3 == 0b000)
        {
            state->regs[rd] = *(uint8_t *)dest;
        }
        else if (funct3 == 0b010)
        { 
            state->regs[rd] = *(uint32_t *)dest;
        }
        else if (funct3 == 0b011)
        { 
            state->regs[rd] = *(uint64_t *)dest;
        }
        else
        {
            unsupported("I-type funct3", funct3);
        }
        state->analysis.ld_count += 1;
    }

    state->pc += 4; 
}

// Function to emulate RISC-V JAL instruction

static void emu_jal(rv_state *state, uint32_t iw)
{
    uint32_t rd = get_bits(iw, 7, 5); 

    
    int32_t imm20 = get_bits(iw, 31, 1) << 20;
    int32_t imm10_1 = get_bits(iw, 21, 10) << 1;
    int32_t imm11 = get_bits(iw, 20, 1) << 11;
    int32_t imm19_12 = get_bits(iw, 12, 8) << 12;
    int32_t imm = imm20 | imm19_12 | imm11 | imm10_1;
    imm = sign_extend(imm, 21); 


// Store return address if rd is not zero
     if (rd != 0)
    { 
        state->regs[rd] = state->pc + 4;
    }

    
    state->pc += imm;
}

void emu_jalr(rv_state *state, uint32_t iw)
{
    uint32_t rs1 = (iw >> 15) & 0b1111; 
    uint64_t val = state->regs[rs1];   

// Update program counter with jump offset
    state->pc = val; 
}

// Function to emulate RISC-V JALR instruction

static void emu_r_type(rv_state *state, uint32_t iw) {
   uint32_t rd = get_bits(iw, 7, 5);
    uint32_t rs1 = get_bits(iw, 15, 5);
    uint32_t rs2 = get_bits(iw, 20, 5);
    uint32_t funct3 = get_bits(iw, 12, 3);
    uint32_t funct7 = get_bits(iw, 25, 7);
    uint32_t opcode = get_bits(iw, 0, 7);

// Perform different operations based on funct3 and funct7
    if (funct3 == 0b000 && funct7 == 0b0000000)
    { 
        state->regs[rd] = state->regs[rs1] + state->regs[rs2];
    }
    else if (funct3 == 0b000 && funct7 == 0b0100000)
    { 
        state->regs[rd] = state->regs[rs1] - state->regs[rs2];
    }
    else if (funct3 == 0b000 && funct7 == 0b0000001)
    { 
        state->regs[rd] = state->regs[rs1] * state->regs[rs2];
    }
    else if (funct3 == 0b100 && funct7 == 0b0000001)
    { 
        state->regs[rd] = state->regs[rs1] / state->regs[rs2];
    }
    else if (funct3 == 0b111 && funct7 == 0b0000000)
    { 
        state->regs[rd] = state->regs[rs1] & state->regs[rs2];
    }
        else if (funct3 == 0b101 && funct7 == 0b0000000 && opcode == FMT_R)
    { 
        state->regs[rd] = state->regs[rs1] >> state->regs[rs2];
    }
    else if (funct3 == 0b001 && funct7 == 0b0000000 && opcode == FMT_R)
    { 
        state->regs[rd] = state->regs[rs1] << state->regs[rs2];
    }
    else if (funct3 == 0b001 && funct7 == 0b0000000 && opcode == FMT_R_WORD)
    { 
        state->regs[rd] = (uint32_t)(((uint32_t)state->regs[rs1]) << state->regs[rs2]);
    }
    else if (funct3 == 0b101 && funct7 == 0b0100000 && opcode == FMT_R_WORD)
    { 
        state->regs[rd] = ((int32_t)state->regs[rs1]) >> state->regs[rs2];
    }
    else if (funct3 == 0b110 && funct7 == 0b0000001)
    { 
        state->regs[rd] = state->regs[rs1] % state->regs[rs2];
    }
    else
    {
        unsupported("R-type funct3", funct3);
    }
        state->pc += 4;
}


static void emu_li(rv_state *state, uint32_t rd, int32_t imm)
{
   
    state->regs[rd] = imm;
}
static void emu_mv(rv_state *state, uint32_t rd, uint32_t rs1)
{
    
    state->regs[rd] = state->regs[rs1];
}

// Function to emulate RISC-V store instructions
static void emu_store(rv_state *state, uint32_t iw)
{
    uint32_t rs1 = get_bits(iw, 15, 5);                                             
    uint32_t rs2 = get_bits(iw, 20, 5);                                             
    uint32_t funct3 = get_bits(iw, 12, 3);                                          
    int32_t imm = sign_extend(get_bits(iw, 7, 5) | (get_bits(iw, 25, 7) << 5), 12); 

    uint64_t addr = state->regs[rs1] + imm; // Calculate address

  // Store data to memory based on function code
    switch (funct3)
    {
    case LDST_BYTE: 
        *((uint8_t *)addr) = (uint8_t)state->regs[rs2];
        break;
    case LDST_WORD: 
        *((uint32_t *)addr) = (uint32_t)state->regs[rs2];
        break;
    case LDST_HALF: 
        *((uint16_t *)addr) = (uint16_t)state->regs[rs2];
        break;
    case LDST_DOUBLE: 
        *((uint64_t *)addr) = state->regs[rs2];
        break;
    default:
        unsupported("Store funct3", funct3);
        return;
    }

    state->pc += 4; 
}

// Function to emulate RISC-V load instructions
static void emu_load(rv_state *state, uint32_t iw)
{
    uint32_t rd = get_bits(iw, 7, 5);                    
    uint32_t rs1 = get_bits(iw, 15, 5);                  
    uint32_t funct3 = get_bits(iw, 12, 3);               
    int32_t imm = sign_extend(get_bits(iw, 20, 12), 12); 

    uint64_t addr = state->regs[rs1] + imm;     uint32_t data;
   switch (funct3)
    {
    case LDST_BYTE:
        data = *((uint8_t *)addr); 
        break;
    case LDST_WORD:
        data = *((uint32_t *)addr); 
        break;
    case LDST_HALF:
        data = *((uint16_t *)addr); 
        break;
    case LDST_DOUBLE: 
        state->regs[rd] = *((uint64_t *)(state->regs[rs1] + imm));
        break;
    default:
        unsupported("L-type funct3", funct3);
        return;
    }

    state->regs[rd] = data; 

    state->pc += 4; 
}


static void rv_one(rv_state *state) {
  uint32_t iw = *((uint32_t*) state->pc);
   iw = cache_lookup(&state->i_cache, (uint64_t)state->pc);
  uint32_t opcode = get_bits(iw, 0, 7);
	
#if DEBUG
  printf("iw: %x\n", iw);
#endif

  
  state->analysis.i_count++;

  switch (opcode) {
  
    case FMT_R_WORD:
    case FMT_R:
        emu_r_type(state, iw);
        state->analysis.ir_count++;
        break;
    
    case FMT_I_JALR:
        emu_jalr(state, iw);
        state->analysis.j_count++;
        break;
    case FMT_I_LOAD: 
    case FMT_I_ARITH:
        emu_i_type(state, iw);
        break;
        
	
	
    case FMT_SB:
        emu_b_type(state, iw);
        break;

    case FMT_S:
        emu_store(state, iw);
        state->analysis.st_count++;
        break;

    case FMT_UJ:
        emu_jal(state, iw);
        state->analysis.j_count++;
        break;
        
    default:
      unsupported("unknown opcode - ", opcode);
    
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
