#include "decl.h"

#ifndef GEN_IR_H
#define GEN_IR_H

typedef enum {
    ir_empty_t = 0, // empty values
    ir_temp_t,
    ir_sym_t,
    ir_imm_t,
    ir_ptr_t,
    ir_ary_t,
} ir_val_t;

typedef struct IRValue {
    ir_val_t t;
    union {
        int temp;
        struct symtable *sym;
        int imm;
        struct IRValue *val;
    } as;
    int reg; // non-virtual register for ir_sym_t and ir_temp_t
} IRValue;

typedef enum IROp {
    IR_ADD = 0,
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
    IR_IMM,
    IR_PTR,
    IR_ASSIGN,
    IR_VAR,
    IR_RETURN,
    IR_JUMP, // 15
    IR_IF,
    IR_TMP_ASSIGN, // for code-generated assigns
    IR_ARGUMENT,
    IR_CALL,
    IR_END_OF_FUNC,
} IROp;

struct BasicBlock;

typedef struct IRNode {
    IROp op;
    IRValue *arg1;
    IRValue *arg2;
    IRValue *result;
    struct ASTnode *ast;
    int type; // type of node (ast->type)
    int argnum; // for IR_ARGUMENT
    struct symtable *ctype; // composite type of symbol, if needed (ast->ctype)
    struct symtable *sym; // Pointer to symbol
    struct BasicBlock *bbout1; // for IR_IF, IR_JUMP
    struct BasicBlock *bbout2; // for IR_IF
    int spillreg; // do we spill a register before generating asm for this node? If > -1, yes
    struct IRNode *next; // next in basic block list
    struct IRNode *prev;
} IRNode;

//struct CFGNode;
typedef struct BasicBlock {
    struct ASTnode *func; // for A_FUNCTION
    int ilabel; // beginning label
    char *slabel;
    IRNode *nodes; // first node
    IRNode *last;
    int done;
    int dumped; // for string dumping
    int patched; // breaks have been patched
    int lowered; // has asm code been generated for this block
    int regalloced; // has this block been regalloced
    int num_preds;
    int num_succs;
    struct BasicBlock *preds[3];
    struct BasicBlock *succs[2];
    //struct CFGNode *cfg_node;
} BasicBlock;

typedef struct IRData {
    IRValue *val;
    struct IRData *next;
} IRData;

typedef struct IRModule {
    char *filename;
    IRData *data; // first static data item
    BasicBlock *blocks;
} IRModule;

extern IRModule *cur_module;

//typedef struct CFGNode {
    //struct CFGNode *node_out1;
    //struct CFGNode *node_out2;
    //struct CFGNode *node_in1;
    //struct CFGNode *node_in2;
    //struct BasicBlock *bb;
//} CFGNode;

//typedef struct CFG {
    //CFGNode *start;
    //CFGNode *exit;
//} CFG;

IRValue *IR_NodeValue(IRNode *node);
IRValue *IR_NewTempValue(void);
IRValue *IR_NewSymbolValue(struct symtable *sym);
IRValue *IR_NewImmValue(int imm);
IRValue *IR_NewPtrValue(IRValue *val);
IRValue *IR_NewEmptyValue(void);

IRModule *new_module(char *filename);
//BasicBlock *new_bb(char *label_name);
const char *ir_opname(int op);

void genIRFinish(void);

#endif
