#include "decl.h"

#ifndef GEN_IR_H
#define GEN_IR_H

typedef struct Reg {
    int v; // virtual
    int r; // real
} Reg;

typedef enum IROp {
    IR_LOAD_GLOBAL = 1,
    IR_LOAD_PARAM,
    IR_LOAD_LOCAL,
    IR_LOAD_IMM,
    IR_STORE_GLOBAL,
    IR_STORE_LOCAL,
    IR_STORE_PARAM,
    IR_ADD,
    IR_SUBTRACT,
    IR_MULTIPLY,
    IR_DIVIDE,
    IR_MODULO,
    IR_EQ,
    IR_NE,
    IR_LT,
    IR_GT,
    IR_LE,
    IR_GE,
    IR_LOGOR,
    IR_LOGAND,
    IR_RETURN,
    IR_WIDEN,
    IR_IF,
    IR_JUMP,
    IR_IDENT_PHI,
    IR_REG_PHI,
    IR_TOBOOL,
    IR_LAST // sentinel
} IROp;

typedef int IRLabel;
struct BasicBlock;

typedef struct IRNode {
    IROp op;
    Reg *r1; // (ex: IR_ADD r1, r2 -> r3)
    Reg *r2;
    Reg *r3; // result register
    int imm; // immediate value, for certain nodes (IR_IMM)
    int type; // type of node
    struct symtable *ctype; // composite type of symbol, if needed
    struct symtable *sym; // Pointer to symbol
    int label;   // for jump nodes
    int ssa_num; // for identifiers (@a1, @a2)
    struct IRNode *phi0; // predecessor 0 for PHI nodes
    struct IRNode *phi1; // predecessor 1 for PHI nodes
    struct BasicBlock *bbout1; // bbout1 for IR_IF nodes, for example
    struct BasicBlock *bbout2; // bbout2 for IR_IF nodes, for example
    struct IRNode *next; // next in basic block list
    struct IRNode *prev;
} IRNode;

typedef struct BasicBlock {
    struct ASTnode *func;
    IRLabel ilabel; // beginning label
    char *slabel;
    IRNode *nodes; // first node
    IRNode *last;
    int done;
    int dumped;
    struct BasicBlock *preds[2];
    struct BasicBlock *succs[2];
} BasicBlock;

typedef struct IRModule {
    char *filename;
    BasicBlock *blocks;
} IRModule;

extern IRModule *cur_module;

IRModule *new_module(char *filename);
BasicBlock *new_bb(char *label_name);

#endif
