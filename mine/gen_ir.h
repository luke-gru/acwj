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
    IR_RETURN,
    IR_WIDEN,
    IR_LAST // sentinel
} IROp;

typedef int IRLabel;

typedef struct IRNode {
    IROp op;
    Reg *r1; // (ex: IR_ADD r1, r2, r3)
    Reg *r2;
    Reg *r3;
    int imm;
    int type;
    struct symtable *ctype;
    struct symtable *sym;
    struct IRNode *next;
    struct IRNode *prev;
} IRNode;

typedef struct BasicBlock {
    struct ASTnode *func;
    IRLabel labeli;
    char *labels;
    IRNode *nodes;
    IRNode *last;
    int done;
    struct BasicBlock *pred;
    struct BasicBlock *succ;
} BasicBlock;

typedef struct IRModule {
    char *filename;
    BasicBlock *blocks;
} IRModule;

#endif
