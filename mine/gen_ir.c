#include "defs.h"
#include "gen_ir.h"

typedef struct ASTnode ASTnode;
typedef struct symtable Sym;

static int vnum = 1;
static int labelid = 1;
static BasicBlock *cur_bb = NULL;
static BasicBlock *start_bb = NULL;
IRModule *cur_module = NULL;
static ASTnode *cur_func = NULL;
static IRNode *cur_node = NULL;

IRNode *genIRExpr(struct ASTnode *n);

#define CASE_OP(op) case op: return (#op)
#define CASE_DEFAULT(ret) default: return (ret)
const char *ir_opname(int op) {
  switch (op) {
    CASE_OP(IR_LOAD_GLOBAL);
    CASE_OP(IR_LOAD_PARAM);
    CASE_OP(IR_LOAD_LOCAL);
    CASE_OP(IR_LOAD_IMM);
    CASE_OP(IR_STORE_GLOBAL);
    CASE_OP(IR_STORE_LOCAL);
    CASE_OP(IR_STORE_PARAM);
    CASE_OP(IR_ADD);
    CASE_OP(IR_SUBTRACT);
    CASE_OP(IR_EQ);
    CASE_OP(IR_RETURN);
    CASE_OP(IR_WIDEN);
    CASE_OP(IR_IF);
    CASE_OP(IR_JUMP);
    CASE_OP(IR_IDENT_PHI);
    CASE_OP(IR_REG_PHI);
    CASE_DEFAULT(NULL);
  }
}

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
  if (!start_bb) start_bb = bb;
  if (label_name) {
    bb->slabel = label_name;
  } else {
    bb->ilabel = new_label();
  }
  bb->func = cur_func;
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

IRNode *dupIRNode(IRNode *node) {
  ASSERT(node);
  IRNode *dup = new_node(node->op);
  memcpy(dup, node, sizeof(IRNode));
  return (dup);
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

// Add the IRNode to the current basic block, or a new basic
// block if the current one is `done`.
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

IRNode *genIRReturn(struct ASTnode *n) {
  IRNode *ir = new_node(IR_RETURN);
  IRNode *expr = NULL;
  if (n->left) {
    expr = genIRExpr(n->left);
    ASSERT(expr->r3);
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
  ASSERT(expr->r3);
  ir->r1 = expr->r3;
  ir->r3 = new_vreg();
  emitIR(ir);
  return (ir);
}

IRNode *genIRTobool(ASTnode *n) {
  IRNode *ir = new_node(IR_TOBOOL);
  ir->type = n->type;
  ir->ctype = n->ctype;
  IRNode *expr = genIRExpr(n->left);
  ASSERT(expr->r3);
  ir->r1 = expr->r3;
  ir->r3 = new_vreg();
  emitIR(ir);
  return (ir);
}

IRNode *genIRBinop(struct ASTnode *n) {
  IROp op;
  switch (n->op) {
    case A_ADD:
      op = IR_ADD; break;
    case A_SUBTRACT:
      op = IR_SUBTRACT; break;
    case A_MULTIPLY:
      op = IR_MULTIPLY; break;
    case A_DIVIDE:
      op = IR_DIVIDE; break;
    case A_MODULO:
      op = IR_MODULO; break;
    case A_EQ:
      op = IR_EQ; break;
    case A_NE:
      op = IR_NE; break;
    case A_LT:
      op = IR_LT; break;
    case A_GT:
      op = IR_GT; break;
    case A_LE:
      op = IR_LE; break;
    case A_GE:
      op = IR_GE; break;
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
  ir_n->ssa_num = sym->ssa_num+1;
  sym->ssa_num++;
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
  ir_n->ssa_num = sym->ssa_num;
  ir_n->r3 = new_vreg();
  emitIR(ir_n);
  return (ir_n);
}

IRNode *genJump(int label) {
  IRNode *ir_n = new_node(IR_JUMP);
  ir_n->label = label;
  emitIR(ir_n);
  return (ir_n);
}

typedef struct PhiNodeData {
  IRNode *pred0;
  IRNode *pred1;
  struct PhiNodeData *next;
} PhiNodeData;

PhiNodeData *get_phi_node_data(PhiNodeData *first, IRNode *ir_n0, IRNode *ir_n1) {
  PhiNodeData *cur = first;
  while (cur) {
    if (ir_n0) {
      if (cur->pred0 && ir_n0->op == cur->pred0->op && !strcmp(cur->pred0->sym->name, ir_n0->sym->name)) {
        return (cur);
      }
    } else {
      if (cur->pred1 && ir_n1->op == cur->pred1->op && !strcmp(cur->pred1->sym->name, ir_n1->sym->name)) {
        return (cur);
      }
    }
    cur = cur->next;
  }
  return (NULL);
}

PhiNodeData *new_phi_node_data(IRNode *pred0, IRNode *pred1) {
  PhiNodeData *ret = (PhiNodeData*)calloc(1, sizeof(PhiNodeData));
  ASSERT(pred0);
  if (!pred0) {
    ASSERT(pred1);
    pred0 = dupIRNode(pred1);
    pred0->ssa_num = pred1->ssa_num - 1;
  }
  ret->pred0 = pred0;
  if (!pred1) {
    ASSERT(pred0);
    pred1 = dupIRNode(ret->pred0);
    pred1->ssa_num = pred0->ssa_num - 1;
  }
  ret->pred1 = pred1;
  return (ret);
}

PhiNodeData *collect_phi_nodes(BasicBlock *bb0, BasicBlock *bb1) {
  PhiNodeData *last = NULL;
  PhiNodeData *first = NULL;
  PhiNodeData *existing;
  IRNode *ir_node = bb0->nodes;
  while (ir_node) {
    switch (ir_node->op) {
      case IR_STORE_LOCAL:
      case IR_STORE_PARAM:
      case IR_STORE_GLOBAL:
        if (first) {
          if ((existing = get_phi_node_data(first, ir_node, NULL))) {
            existing->pred0 = ir_node; // has most recent ssa_num
          } else {
            last->next = new_phi_node_data(ir_node, NULL);
            last = last->next;
          }
        } else {
          first = last = new_phi_node_data(ir_node, NULL);
        }
    }
    ir_node = ir_node->next;
  }
  ir_node = bb1->nodes;
  while (ir_node) {
    switch (ir_node->op) {
      case IR_STORE_LOCAL:
      case IR_STORE_PARAM:
      case IR_STORE_GLOBAL:
        if (first) {
          if ((existing = get_phi_node_data(first, ir_node, ir_node))) {
            existing->pred1 = ir_node;
          } else {
            last->next = new_phi_node_data(NULL, ir_node);
            last = last->next;
          }
        } else {
          first = last = new_phi_node_data(NULL, ir_node);
        }
    }
    ir_node = ir_node->next;
  }
  return (first);
}

void bb_insert_node_before(BasicBlock *bb, IRNode *new, IRNode **before) {
  if (*before) {
    (*before)->prev = new;
    new->next = *before;
    bb->nodes = new;
  } else {
    bb->nodes = bb->last = new;
  }
}

int phi_type(IRNode *pred0, IRNode *pred1) {
  if (pred0 && pred1) {
    if (pred0->type != pred1->type) {
      fatal("Error: mismatched types");
    }
    return (pred0->type);
  } else if (pred0) {
    return (pred0->type);
  } else  if (pred1) {
    return (pred1->type);
  } else {
    ASSERT(0);
  }
  return (P_NONE);
}

Sym *phi_ctype(IRNode *pred0, IRNode *pred1) {
  if (pred0 && pred1) {
    if (pred0->ctype != pred1->ctype) {
      fatal("Error: mismatched types");
    }
    return (pred0->ctype);
  } else if (pred0) {
    return (pred0->ctype);
  } else  if (pred1) {
    return (pred1->ctype);
  } else {
    ASSERT(0);
  }
  return (NULL);
}

Sym *phi_sym(IRNode *pred0, IRNode *pred1) {
  if (pred0 && pred1) {
    if (pred0->sym != pred1->sym) {
      fatal("Error: Symbol mismatch");
    }
    return (pred0->sym);
  } else if (pred0) {
    return (pred0->sym);
  } else if (pred1) {
    return (pred1->sym);
  } else {
    ASSERT(0);
  }
  return (NULL);
}

void add_phi_ident_node(BasicBlock *bb, IRNode *pred0, IRNode *pred1) {
  IRNode *phi = new_node(IR_IDENT_PHI);
  ASSERT(pred0 && pred1);
  phi->phi0 = pred0;
  phi->phi1 = pred1;
  phi->sym = phi_sym(pred0, pred1);
  phi->ssa_num = ++phi->sym->ssa_num;
  phi->type = phi_type(pred0, pred1);
  phi->ctype = phi_ctype(pred0, pred1);
  bb_insert_node_before(bb, phi, &bb->nodes);
}

void add_phi_ident_nodes(BasicBlock *bb) {
  PhiNodeData *philist = collect_phi_nodes(bb->preds[0], bb->preds[1]);
  PhiNodeData *cur = philist;
  while (cur) {
    add_phi_ident_node(bb, cur->pred0, cur->pred1);
    cur = cur->next;
  }
}

// add phi register node (phi of 2 registers)
int add_phi_regnode(BasicBlock *bb, IRNode *n1, IRNode *n2) {
  ASSERT(n1);
  ASSERT(n2);
  IRNode *phi = new_node(IR_REG_PHI);
  phi->r1 = new_vreg();
  phi->type = phi_type(n1, n2);
  phi->ctype  = phi_ctype(n1, n2);
  phi->phi0 = n1;
  phi->phi1 = n2;
  phi->r3 = phi->r1;
  bb_insert_node_before(bb, phi, &bb->nodes);
  return (1);
}

IRNode *genIRIf(ASTnode *n) {
  IRNode *cond = genIRExpr(n->left);
  IRNode *ir_n = new_node(IR_IF);
  ir_n->r1 = cond->r3;
  ir_n->bbout1 = new_bb(NULL);
  ir_n->bbout2 = new_bb(NULL);
  BasicBlock *outbb = new_bb(NULL);
  cur_bb->succs[0] = ir_n->bbout1;
  cur_bb->succs[1] = ir_n->bbout2;
  ir_n->bbout1->preds[0] = cur_bb;
  ir_n->bbout2->preds[0] = cur_bb;

  emitIR(ir_n);
  cur_bb->done = 1;
  cur_bb = ir_n->bbout1;
  genIR(n->mid); // true compound statement
  genJump(outbb->ilabel);
  cur_bb->done = 1;

  cur_bb = ir_n->bbout2;
  if (n->right) { // false compound statement
    genIR(n->right);
  }
  genJump(outbb->ilabel);
  cur_bb->done = 1;

  ir_n->bbout1->succs[0] = outbb;
  ir_n->bbout2->succs[0] = outbb;
  outbb->preds[0] = ir_n->bbout1;
  outbb->preds[1] = ir_n->bbout2;
  cur_bb = outbb;
  add_phi_ident_nodes(outbb);
  return (ir_n);
}

IRNode *genIRTernary(ASTnode *n) {
  IRNode *if_expr, *else_expr;
  IRNode *cond = genIRExpr(n->left);
  IRNode *ir_n = new_node(IR_IF);
  ir_n->r1 = cond->r3;
  ir_n->bbout1 = new_bb(NULL);
  ir_n->bbout2 = new_bb(NULL);
  BasicBlock *outbb = new_bb(NULL);
  cur_bb->succs[0] = ir_n->bbout1;
  cur_bb->succs[1] = ir_n->bbout2;
  ir_n->bbout1->preds[0] = cur_bb;
  ir_n->bbout2->preds[0] = cur_bb;

  emitIR(ir_n);

  cur_bb->done = 1;
  cur_bb = ir_n->bbout1;
  if_expr = genIRExpr(n->mid); // true expr
  ir_n->type = if_expr->type;
  ir_n->ctype = if_expr->ctype;
  genJump(outbb->ilabel);
  cur_bb->done = 1;

  cur_bb = ir_n->bbout2;
  else_expr = genIRExpr(n->right); // else expr
  genJump(outbb->ilabel);
  cur_bb->done = 1;

  ir_n->bbout1->succs[0] = outbb;
  ir_n->bbout2->succs[0] = outbb;
  outbb->preds[0] = ir_n->bbout1;
  outbb->preds[1] = ir_n->bbout2;
  cur_bb = outbb;
  add_phi_ident_nodes(outbb);
  if (add_phi_regnode(outbb, if_expr, else_expr)) {
      ir_n->r3 = outbb->nodes->r3;
  } else {
      ir_n->r3 = new_vreg();
  }
  return (ir_n);
}

IRNode *genIRExpr(struct ASTnode *n) {
  switch (n->op) {
    case A_ADD:
    case A_SUBTRACT:
    case A_EQ:
      return (genIRBinop(n));
    case A_INTLIT:
      return (genIRImm(n));
    case A_WIDEN:
      return (genIRWiden(n));
    case A_ASSIGN:
      return (genIRStore(n));
    case A_IDENT:
      return (genIRLoad(n));
    case A_TERNARY:
      return (genIRTernary(n));
    case A_TOBOOL:
      return (genIRTobool(n));
    default:
      fatalv("Unknown AST op: %s (%d)", opname(n->op), n->op);
  }
}

BasicBlock *genIR(struct ASTnode *n) {
  start_bb = NULL;
  IRNode *ir_n;
  switch (n->op) {
    case A_FUNCTION:
      return (genIRFunction(n));
    case A_RETURN:
      genIRReturn(n);
      break;
    case A_GLUE:
      if (n->left) genIR(n->left);
      if (n->right) genIR(n->right);
      break;
    case A_IF:
      genIRIf(n);
      break;
    default:
      genIRExpr(n);
      break;
  }
  return (start_bb);
}

void dumpIRNode(IRNode *n, FILE *f) {
  ASSERT(n);
  switch (n->op) {
    case IR_ADD:
      fprintf(f, "add r%d, r%d -> r%d\n", vreg(n->r1), vreg(n->r2), vreg(n->r3));
      break;
    case IR_SUBTRACT:
      fprintf(f, "sub r%d, r%d -> r%d\n", vreg(n->r1), vreg(n->r2), vreg(n->r3));
      break;
    case IR_EQ:
      fprintf(f, "eq r%d, r%d -> r%d\n", vreg(n->r1), vreg(n->r2), vreg(n->r3));
      break;
    case IR_NE:
      fprintf(f, "neq r%d, r%d -> r%d\n", vreg(n->r1), vreg(n->r2), vreg(n->r3));
      break;
    case IR_MULTIPLY:
      fprintf(f, "mult r%d, r%d -> r%d\n", vreg(n->r1), vreg(n->r2), vreg(n->r3));
      break;
    case IR_DIVIDE:
      fprintf(f, "div r%d, r%d -> r%d\n", vreg(n->r1), vreg(n->r2), vreg(n->r3));
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
      ASSERT(n->r1);
      ASSERT(n->r3);
      fprintf(f, "widen r%d -> r%d (%s)\n", vreg(n->r1), vreg(n->r3), typename(n->type, n->ctype));
      break;
    case IR_TOBOOL:
      ASSERT(n->r1);
      ASSERT(n->r3);
      fprintf(f, "tobool r%d (%s) -> r%d\n", vreg(n->r1), typename(n->type, n->ctype), vreg(n->r3));
      break;
    case IR_STORE_LOCAL:
      fprintf(f, "storeloc @%s%d r%d -> r%d (%s)\n", n->sym->name, n->ssa_num, vreg(n->r1), vreg(n->r3), typename(n->type, n->ctype));
      break;
    case IR_STORE_PARAM:
      fprintf(f, "storeparam @%s r%d -> r%d (%s)\n", n->sym->name, vreg(n->r1), vreg(n->r3), typename(n->type, n->ctype));
      break;
    case IR_STORE_GLOBAL:
      fprintf(f, "storeglob @%s r%d -> r%d (%s)\n", n->sym->name, vreg(n->r1), vreg(n->r3), typename(n->type, n->ctype));
      break;
    case IR_LOAD_LOCAL:
      fprintf(f, "loadloc @%s%d -> r%d (%s)\n", n->sym->name, n->ssa_num, vreg(n->r3), typename(n->type, n->ctype));
      break;
    case IR_LOAD_PARAM:
      fprintf(f, "loadparam @%s%d -> r%d (%s)\n", n->sym->name, n->ssa_num, vreg(n->r3), typename(n->type, n->ctype));
      break;
    case IR_LOAD_GLOBAL:
      fprintf(f, "loadglob @%s%d -> r%d (%s)\n", n->sym->name, n->ssa_num, vreg(n->r3), typename(n->type, n->ctype));
      break;
    case IR_IF:
      fprintf(f, "if_br r%d L%d, L%d\n", vreg(n->r1), n->bbout1->ilabel, n->bbout2->ilabel);
      break;
    case IR_JUMP:
      fprintf(f, "jump L%d\n", n->label);
      break;
    case IR_IDENT_PHI:
      ASSERT(n->phi0);
      ASSERT(n->phi1);
      fprintf(f, "phi @%s%d (@%s%d,@%s%d)\n", n->sym->name, n->ssa_num, n->sym->name,
          n->phi0->ssa_num, n->sym->name, n->phi1->ssa_num);
      break;
    case IR_REG_PHI:
      ASSERT(n->phi0->r3);
      ASSERT(n->phi1->r3);
      ASSERT(n->r1);
      fprintf(f, "phi r%d (r%d,r%d)\n", vreg(n->r1), vreg(n->phi0->r3), vreg(n->phi1->r3));
      break;
    default:
      fatalv("Unknown IR node (%d)", n->op);
  }
}

void dumpBBLabel(BasicBlock *bb, FILE *f) {
  if (bb->slabel) {
    fprintf(f, "%s:\n", bb->slabel);
  } else {
    fprintf(f, "L%d:\n", bb->ilabel);
  }
}

void dumpIR0(BasicBlock *bb, FILE *f, int recurse) {
  BasicBlock *b = bb;
  IRNode *n;
  if (b && !b->dumped) {
    dumpBBLabel(b, f);
    n = b->nodes;
    while (n) {
      dumpIRNode(n, f);
      n = n->next;
    }
    b->dumped = 1;
    if (bb->succs[0] && recurse) dumpIR0(bb->succs[0], f, 1);
    if (bb->succs[1] && recurse) dumpIR0(bb->succs[1], f, 1);
  }
}

void dumpIR(BasicBlock *bb, FILE *f) {
  dumpIR0(bb, f, 1);
}
