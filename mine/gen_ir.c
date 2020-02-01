#include "defs.h"
#include "gen_ir.h"

typedef struct ASTnode ASTnode;
typedef struct symtable Sym;

static int vnum = 1;
static int labelid = 1;
static BasicBlock *cur_bb = NULL;
static IRModule *cur_module = NULL;
static ASTnode *cur_func = NULL;
static IRNode *cur_node = NULL;

static int vreg(Reg *r) {
  return (r->v);
}

Reg *new_vreg(void) {
  Reg *r = (Reg*)calloc(1, sizeof(Reg));
  r->r = -1;
  r->v = vnum++;
  return (r);
}

IRLabel new_label(void) {
  return (labelid++);
}

BasicBlock *new_bb(char *label_name) {
  BasicBlock *bb = (BasicBlock*)calloc(1, sizeof(BasicBlock));
  if (label_name) {
    bb->labels = label_name;
  } else {
    bb->labeli = new_label();
  }
  return (bb);
}

IRModule *new_module(char *filename) {
  IRModule *m = (IRModule*)calloc(1, sizeof(IRModule));
  m->filename = filename ? strdup(filename) : NULL;
  m->blocks = NULL;
  return (m);
}

IRNode *new_node(IROp op) {
  IRNode *n = (IRNode*)calloc(1, sizeof(IRNode));
  n->op = op;
  return n;
}

BasicBlock *genIRFunction(struct ASTnode *n) {
  struct ASTnode *func = n;
  cur_func = func;
  BasicBlock *bb = new_bb(func->sym->name);
  cur_bb = bb;
  genIR(func->left);
  cur_func = NULL;
  return (bb);
}

void emitIR(IRNode *ir) {
  if (cur_node) {
    ir->prev = cur_node;
  }
  cur_node = ir;
  if (cur_bb && cur_bb->done)
    cur_bb = NULL;
  if (cur_bb) {
    if (cur_bb->last) {
      cur_bb->last->next = ir;
      cur_bb->last = ir;
    } else {
      cur_bb->last = ir;
      cur_bb->nodes = ir;
    }
  } else {
    cur_bb = new_bb(NULL);
    cur_bb->nodes = ir;
    cur_bb->last = ir;
  }
}

void emitBlock(BasicBlock *bb) {
  bb->done = 1;
}

IRNode *genIRExpr(struct ASTnode *n) {
  genIR(n);
  ASSERT(cur_node);
  return (cur_node);
}

IRNode *genIRReturn(struct ASTnode *n) {
  IRNode *ir = new_node(IR_RETURN);
  IRNode *expr = NULL;
  if (n->left) {
    expr = genIRExpr(n->left);
    ir->r1 = expr->r3;
    ir->type = expr->type;
    ir->ctype = expr->ctype;
  }
  emitIR(ir);
  return (ir);
}

IRNode *genIRImm(struct ASTnode *n) {
  IRNode *ir = new_node(IR_LOAD_IMM);
  ir->imm = n->intvalue;
  ir->type = n->type; ir->ctype = n->ctype;
  ir->r3 = new_vreg();
  emitIR(ir);
  return (ir);
}

IRNode *genIRWiden(ASTnode *n) {
  IRNode *ir = new_node(IR_WIDEN);
  ir->type = n->type;
  ir->ctype = n->ctype;
  IRNode *expr = genIRExpr(n->left);
  ir->r1 = expr->r3;
  ir->r3 = new_vreg();
  emitIR(ir);
  return (ir);
}

IRNode *genIRBinop(struct ASTnode *n) {
  IROp op;
  switch (n->op) {
    case A_ADD:
      op = IR_ADD;
      break;
    case A_SUBTRACT:
      op = IR_SUBTRACT;
      break;
    default:
      fatalv("unknown binop: %d", n->op);
  }
  IRNode *ir = new_node(op);
  ir->type = n->type; ir->ctype = n->ctype;
  IRNode *left = genIRExpr(n->left);
  IRNode *right = genIRExpr(n->right);
  ir->r1 = left->r3;
  ir->r2 = right->r3;
  ir->r3 = new_vreg();
  emitIR(ir);
  return (ir);
}

IRNode *genIRStore(ASTnode *n) {
  IROp op = IR_STORE_GLOBAL;
  IRNode *ir_n;
  ASSERT(n->right->op == A_IDENT); // FIXME: make work for DEREF too
  Sym *sym = n->right->sym;
  switch (sym->class) {
    case C_PARAM:
      op = IR_STORE_PARAM;
      break;
    case C_LOCAL:
      op = IR_STORE_LOCAL;
      break;
  }
  ir_n = new_node(op);
  ir_n->type = n->right->type; ir_n->ctype = n->right->ctype;
  ir_n->sym = sym;
  ir_n->r1 = genIRExpr(n->left)->r3;
  ir_n->r3 = new_vreg();
  emitIR(ir_n);
  return (ir_n);
}

IRNode *genIRLoad(ASTnode *n) {
  IROp op = IR_LOAD_GLOBAL;
  IRNode *ir_n;
  Sym *sym = n->sym;
  switch (sym->class) {
    case C_PARAM:
      op = IR_LOAD_PARAM;
      break;
    case C_LOCAL:
      op = IR_LOAD_LOCAL;
      break;
  }
  ir_n = new_node(op);
  ir_n->type = n->type; ir_n->ctype = n->ctype;
  ir_n->sym = sym;
  ir_n->r3 = new_vreg();
  emitIR(ir_n);
  return (ir_n);
}

BasicBlock *genIR(struct ASTnode *n) {
  IRNode *ir_n;
  switch (n->op) {
    case A_FUNCTION:
      return (genIRFunction(n));
      break;
    case A_ADD:
      genIRBinop(n);
      break;
    case A_RETURN:
      genIRReturn(n);
      break;
    case A_INTLIT:
      genIRImm(n);
      break;
    case A_GLUE:
      if (n->left) genIR(n->left);
      if (n->right) genIR(n->right);
      break;
    case A_WIDEN:
      genIRWiden(n);
      break;
    case A_ASSIGN:
      genIRStore(n);
      break;
    case A_IDENT:
      genIRLoad(n);
      break;
    default:
      fatalv("Unknown AST op: %s (%d)", opname(n->op), n->op);
  }
  return (cur_bb);
}

void dumpIRNode(IRNode *n, FILE *f) {
  switch (n->op) {
    case IR_ADD:
      fprintf(f, "add r%d, r%d -> r%d\n", vreg(n->r1), vreg(n->r2), vreg(n->r3));
      break;
    case IR_SUBTRACT:
      fprintf(f, "sub r%d, r%d -> r%d\n", vreg(n->r1), vreg(n->r2), vreg(n->r3));
      break;
    case IR_LOAD_IMM:
      fprintf(f, "loadimm %d (%s) -> r%d\n", n->imm, typename(n->type, NULL), vreg(n->r3));
      break;
    case IR_RETURN:
      if (n->r1)
        fprintf(f, "ret r%d\n", vreg(n->r1));
      else
        fprintf(f, "ret\n");
      break;
    case IR_WIDEN:
      fprintf(f, "widen r%d -> r%d (%s)\n", vreg(n->r1), vreg(n->r3), typename(n->type, n->ctype));
      break;
    case IR_STORE_LOCAL:
      fprintf(f, "storeloc @%s r%d -> r%d (%s)\n", n->sym->name, vreg(n->r1), vreg(n->r3), typename(n->type, n->ctype));
      break;
    case IR_STORE_PARAM:
      fprintf(f, "storeparam @%s r%d -> r%d (%s)\n", n->sym->name, vreg(n->r1), vreg(n->r3), typename(n->type, n->ctype));
      break;
    case IR_STORE_GLOBAL:
      fprintf(f, "storeglob @%s r%d -> r%d (%s)\n", n->sym->name, vreg(n->r1), vreg(n->r3), typename(n->type, n->ctype));
      break;
    case IR_LOAD_LOCAL:
      fprintf(f, "loadloc @%s -> r%d (%s)\n", n->sym->name, vreg(n->r3), typename(n->type, n->ctype));
      break;
    default:
      fatalv("Unknown IR node (%d)", n->op);
  }
}

void dumpBBLabel(BasicBlock *bb, FILE *f) {
  if (bb->labels) {
    fprintf(f, "%s:\n", bb->labels);
  } else {
    fprintf(f, "L%d:\n", bb->labeli);
  }
}

void dumpIR(BasicBlock *bb, FILE *f) {
  BasicBlock *b = bb;
  IRNode *n;
  while (b) {
    dumpBBLabel(b, f);
    n = b->nodes;
    while (n) {
      dumpIRNode(n, f);
      n = n->next;
    }
    b = b->succ;
  }
}
