#include "defs.h"
#include "data.h"
#include "decl.h"
#include "gen_ir.h"
#include "vec.h"

static FILE *OutfileIR;
#define OutfileIR Outfile
static struct ASTnode *cur_function = NULL;

static int cgvalue_reg(IRValue *val) {
    int reg = -1;
    switch (val->t) {
        case ir_temp_t:
        case ir_sym_t:
            reg = val->reg;
            ASSERT(reg > -1);
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


static void lowerNode(IRNode *curnode) {
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
        case IR_RETURN: {
            int reg = cgvalue_reg(curnode->result);
            cgreturn(reg, CurFunctionSym);
            break;
        }
        case IR_END_OF_FUNC:
            cgfuncpostamble(curnode->ast->sym);
            break;
        default:
            fprintf(stderr, "unhandled IR op: %s (%d)", ir_opname(curnode->op), curnode->op);
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
    if (val->reg > -1) { return; }
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
        cgfuncpreamble(bb->func->sym);
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
