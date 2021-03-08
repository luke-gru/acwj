#include "defs.h"
#include "data.h"
#include "decl.h"
#include "gen_ir.h"
#include "vec.h"
#include "ir_lower.h"

static FILE *OutfileIR;
#define OutfileIR Outfile
static struct ASTnode *cur_function = NULL;
static int just_compared = 0;

static int cgvalue_reg(IRValue *val) {
    int reg = -1;
    switch (val->t) {
        case ir_temp_t:
        case ir_sym_t:
            reg = val->reg;
            ASSERT(reg > -1);
            break;
        case ir_imm_t:
            ASSERT(0);
        case ir_ptr_t:
            ASSERT(0);
        case ir_ary_t:
            ASSERT(0);
        default:
            ASSERT(0);
    }
    return reg;
}

static void lowerValue(IRValue *val, int reg) {
    switch (val->t) {
        case ir_imm_t:
            cgloadint(val->as.imm, P_INT, reg);
            break;
        case ir_sym_t:
           if (isglobalsym(val->as.sym)) {
               cgloadglob(val->as.sym, reg);
           } else {
               cgloadlocal(val->as.sym, reg);
           }
           break;
        case ir_temp_t: // move
           cgmove(val->reg, reg);
           break;
        // TODO
        default:
            fprintf(stderr, "Tried to lower val t %d\n", val->t);
            ASSERT(0);
    }
    just_compared = 0;
}

static void lowerNode(IRNode *curnode) {
    fprintf(stderr, "lowering %d\n", curnode->op);
    switch (curnode->op) {
        /*case IR_LOAD_IMM: {*/
            /*r = cgloadint(curnode->imm, curnode->type);*/
            /*curnode->r3->r = r;*/
            /*break;*/
        /*}*/
        /*case IR_WIDEN: {*/
            /*r = cgwiden(lastnode->r3->r, lastnode->type, curnode->type);*/
            /*curnode->r3->r = r;*/
            /*break;*/
        /*}*/
        /*case IR_STORE_LOCAL:*/
            /*r = cgstorlocal(lastnode->r3->r, curnode->sym);*/
            /*curnode->r3->r = r;*/
            /*break;*/
        /*case IR_LOAD_LOCAL:*/
            /*r = cgloadlocal(curnode->sym, 0);*/
            /*curnode->r3->r = r;*/
            /*break;*/
        /*case IR_EQ:*/
        /*case IR_NE:*/
        /*case IR_LT:*/
        /*case IR_GT:*/
        /*case IR_LE:*/
        /*case IR_GE:*/
            /*if (curnode->next && curnode->next->op == IR_IF) {*/
                /*r = cgcompare_and_jump(curnode->ast->op, curnode->r1->r, curnode->r2->r, curnode->next->bbout2->ilabel, curnode->type);*/
                /*curnode->r3->r = r; // NOREG*/
            /*} else {*/
                /*r = cgcompare_and_set(curnode->ast->op, curnode->r1->r, curnode->r2->r, curnode->r1->type);*/
                /*curnode->r3->r = r;*/
            /*}*/
            /*break;*/
        /*case IR_IF: // already generated*/
            /*break;*/
        /*case IR_JUMP:*/
            /*cgjumptolabel(curnode->label);*/
            /*break;*/
        /*case IR_IMM: {*/
            /*int reg = lowerValue(curnode->result);*/
            /*cgloadint(curnode->ast->type, curnode->result->as.imm, reg);*/
            /*break;*/
        /*}*/
        case IR_ASSIGN: {
            // TODO: move immediate directly if curnode->arg1 is an immediate
            // Right now, arg1 is always a register
            int reg = cgvalue_reg(curnode->arg1);
            cgstorlocal(reg, curnode->result->as.sym);
            just_compared = 0;
            break;
        }
        case IR_TMP_ASSIGN: {
            int reg = cgvalue_reg(curnode->result);
            lowerValue(curnode->arg1, reg);
            break;
        }
        case IR_RETURN: {
            int reg = cgvalue_reg(curnode->result);
            cgreturn(reg, CurFunctionSym);
            just_compared = 0;
            break;
        }
        case IR_EQ: {
            // arg1 and arg2 are temp registers
            int regout = cgvalue_reg(curnode->result);
            int reg1 = cgvalue_reg(curnode->arg1);
            int reg2 = cgvalue_reg(curnode->arg2);
            cgcompare_and_set(curnode->ast->op, reg1, reg2, regout, curnode->ast->left->type);
            just_compared = 1;
            break;
        }
        case IR_IF: {
            if (just_compared) {
                cgjumpnz(curnode->bbout2->ilabel);
            } else {
                cgjumpunless(cgvalue_reg(curnode->arg1), curnode->bbout2->ilabel);
            }
            cgjump(curnode->bbout1->ilabel);
            just_compared = 0;
            break;
        }
        case IR_JUMP: {
            cgjump(curnode->bbout1->ilabel);
            just_compared = 0;
            break;
        }
        case IR_ARGUMENT: {
            cgcopyarg(curnode->ast->sym, cgvalue_reg(curnode->result), curnode->argnum);
            break;
        }
        case IR_CALL: {
            cgcall(curnode->ast->sym, curnode->argnum);
            break;
        }
        case IR_SUBTRACT: {
            break;
        }
        case IR_END_OF_FUNC: {
            cgfuncpostamble(curnode->ast->sym);
            break;
        }
        default:
            fprintf(stderr, "unhandled IR op: %s (%d)\n", ir_opname(curnode->op), curnode->op);
            ASSERT(0);
            break;
    }
}

#define NUMFREEREGS 4
#define FIRSTPARAMREG 9
#define MAXREGISTERPARAMS 6
static int spillreg=0;
static int lastspill=-1;
#define SPILLING(reg) lastspill = reg

// List of available registers
// and their names
static int freereg[NUMFREEREGS];
static char *reglist[] = {
  "%r10", "%r11", "%r12",
  "%r13", "%r9", "%r8",
  "%rcx", "%rdx", "%rsi", "%rdi"
};
static char *breglist[] = {
  "%r10b", "%r11b", "%r12b",
  "%r13b", "%r9b", "%r8b",
  "%cl", "%dl", "%sil", "%dil"
};
static char *dreglist[] = {
  "%r10d", "%r11d", "%r12d",
  "%r13d", "%r9d", "%r8d",
  "%ecx", "%edx", "%esi", "%edi"
};

// Set all registers as available.
// But if keepreg is positive (including 0), don't free that one.
static void regalloc_freeall_registers(int keepreg) {
  int i;
  for (i = 0; i < NUMFREEREGS; i++) {
    if (i != keepreg) freereg[i] = 1;
  }
}

/**
 * FREE:
 * IN USE: 0 1 2 3
 * SPILL:
 * In this case, spills register 0, returns it:
 * FREE:
 * IN USE: 0 1 2 3
 * SPILL:  0
 */
// Allocate a free register. Return the number of
// the register. Die if no available registers.
static int regalloc_alloc_register(IRNode *node) {
  int reg;
  int i;
  for (i=0; i<NUMFREEREGS; i++) {
    if (freereg[i]) { // if free
      freereg[i]= 0; // mark in use
      return(i);
    }
  }
  if (O_nospill) {
    fatalv("Ran out of registers and nospill option is on");
  }
  // no more free registers, we must spill the oldest allocd register
  reg = (spillreg % NUMFREEREGS);
  spillreg++;
  SPILLING(reg);
  node->spillreg = reg;
  /*fprintf(Outfile, "# spilling reg %d\n", reg);*/
  /*pushreg(reg);*/
  return (reg);
}

static void regAllocNode(IRNode *node) {
    if (!node->result) { return; }
    IRValue *val = IR_NodeValue(node);
    if (val->reg > -1) { return; } // already allocated
    switch (val->t) {
        case ir_sym_t:
        case ir_temp_t:
            val->reg = regalloc_alloc_register(node);
            break;
        default:
            // do nothing
            return;
    }
}

static void regAllocBlock(BasicBlock *bb) {
    if (bb->regalloced) return;
    IRNode *curnode = bb->nodes;
    while (curnode) {
        regAllocNode(curnode);
        curnode = curnode->next;
    }
    bb->regalloced = 1;
}

static void lowerBlock(BasicBlock *bb) {
    if (bb->lowered) return;
    if (bb->ilabel) {
        fprintf(OutfileIR, "L%d:\n", bb->ilabel);
    }
    if (bb->func) {
        cur_function = bb->func;
        if (bb->slabel) {
            cgfuncpreamble(bb->func->sym);
        }
    }
    IRNode *curnode = bb->nodes;
    int r = -1;
    while (curnode) {
        lowerNode(curnode);
        curnode = curnode->next;
    }
    bb->lowered = 1;
}

void IRRegAlloc(struct IRModule *mod) {
    BasicBlock *bb = mod->blocks;

    vec_void_t vbbs;
    vec_init(&vbbs);

    vec_push(&vbbs, bb);
    while (vbbs.length > 0) {
        bb = vec_pop(&vbbs);
        ASSERT(bb);
        regAllocBlock(bb);
        if (bb->succs[0] && !bb->succs[0]->regalloced) {
            vec_push(&vbbs, bb->succs[0]);
        }
        if (bb->succs[1] && !bb->succs[1]->regalloced) {
            vec_push(&vbbs, bb->succs[1]);
        }
    }

    vec_deinit(&vbbs);
}

void IRLower(struct IRModule *mod) {
    if (O_parseOnly) return;
    BasicBlock *bb = mod->blocks;

    vec_void_t vbbs;
    vec_init(&vbbs);

    vec_push(&vbbs, bb);
    while (vbbs.length > 0) {
        bb = vec_pop(&vbbs);
        ASSERT(bb);
        lowerBlock(bb);
        if (bb->succs[0] && !bb->succs[0]->lowered) {
            vec_push(&vbbs, bb->succs[0]);
        }
        if (bb->succs[1] && !bb->succs[1]->lowered) {
            vec_push(&vbbs, bb->succs[1]);
        }
    }

    vec_deinit(&vbbs);
}
