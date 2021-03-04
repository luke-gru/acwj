#include "defs.h"
#include "gen_ir.h"

typedef struct ASTnode ASTnode;
typedef struct symtable Sym;

static int tempnum = 1;
static int labelid = 1;
static BasicBlock *cur_bb = NULL;
static BasicBlock *start_bb = NULL; // start basic block for genIR()
/*static CFG *cur_cfg = NULL;*/
IRModule *cur_module = NULL;
static ASTnode *cur_func = NULL;
static IRNode *cur_node = NULL;

IRNode *genIRExpr(struct ASTnode *n);

#define CASE_OP(op) case op: return (#op)
#define CASE_DEFAULT(ret) default: return (ret)
const char *ir_opname(int op) {
  switch (op) {
    CASE_OP(IR_ADD);
    CASE_OP(IR_SUBTRACT);
    CASE_OP(IR_MULTIPLY);
    CASE_OP(IR_DIVIDE);
    CASE_OP(IR_MODULO);
    CASE_OP(IR_EQ);
    CASE_OP(IR_NE);
    CASE_OP(IR_LT);
    CASE_OP(IR_GT);
    CASE_OP(IR_LE);
    CASE_OP(IR_GE);
    CASE_OP(IR_IMM);
    CASE_OP(IR_ASSIGN);
    CASE_OP(IR_VAR);
    CASE_OP(IR_RETURN);
    CASE_DEFAULT(NULL);
  }
}

int new_label(void) {
  return (labelid++);
}

/*CFG *new_cfg(CFGNode *start) {*/
    /*CFG *cfg = (CFG*)calloc(1, sizeof(CFG));*/
    /*memset(cfg, 0, sizeof(CFG));*/
    /*cfg->start = start;*/
    /*cur_cfg = cfg;*/
    /*return cfg;*/
/*}*/

/*CFGNode *new_cfgnode(BasicBlock *bb) {*/
    /*CFGNode *cfg_n = (CFGNode*)calloc(1, sizeof(CFGNode));*/
    /*memset(cfg_n, 0, sizeof(CFGNode));*/
    /*cfg_n->bb = bb;*/
    /*return cfg_n;*/
/*}*/

/*CFGNode *cur_cfg_node(void) {*/
    /*ASSERT(cur_bb);*/
    /*return cur_bb->cfg_node;*/
/*}*/

BasicBlock *new_bb(char *label_name) {
  BasicBlock *bb = (BasicBlock*)calloc(1, sizeof(BasicBlock));
  memset(bb, 0, sizeof(BasicBlock));
  if (label_name) {
    bb->slabel = label_name;
  } else {
    bb->ilabel = new_label();
  }
  /*bb->cfg_node = new_cfgnode(bb);*/
  bb->func = cur_func;
  if (!start_bb) {
      start_bb = bb;
      /*if (!cur_cfg) {*/
          /*cur_cfg = new_cfg(bb->cfg_node);*/
      /*}*/
  }
  return (bb);
}

static void bb_succ(BasicBlock *bb, BasicBlock *succ) {
    bb->succs[bb->num_succs] = succ;
    succ->preds[succ->num_preds] = bb;
    bb->num_succs++;
    succ->num_preds++;
}

IRModule *new_module(char *filename) {
  IRModule *m = (IRModule*)calloc(1, sizeof(IRModule));
  memset(m, 0, sizeof(IRModule));
  m->filename = filename ? strdup(filename) : NULL;
  m->blocks = NULL;
  return (m);
}

IRNode *new_node(IROp op) {
  IRNode *n = (IRNode*)calloc(1, sizeof(IRNode));
  memset(n, 0, sizeof(IRNode));
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

  /*cgfuncpreamble(n->sym);*/
  /*n->sym->endlabel = new_label();*/
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

static int isCompoundIRExpr(IRNode *ir_n) {
    switch (ir_n->op) {
        case IR_IMM:
        case IR_VAR:
            return (0);
        default:
            return (1);
    }
}

IRNode *genIRReturn(struct ASTnode *n) {
  IRNode *ir = new_node(IR_RETURN);
  ir->ast = n;
  IRNode *expr = NULL;
  if (n->left) {
    expr = genIRExpr(n->left);
    if (isCompoundIRExpr(expr)) {
        emitIR(expr);
    }
    ir->type = expr->type;
    ir->ctype = expr->ctype;
    ir->result = IR_NodeValue(expr);
  }
  emitIR(ir);
  return (ir);
}

IRNode *genIRImm(struct ASTnode *n) {
  IRNode *ir = new_node(IR_IMM);
  ir->ast = n;
  ir->type = n->type; ir->ctype = n->ctype;
  ir->result = IR_NewImmValue(n->intvalue);
  return (ir);
}

IRNode *genIRWiden(ASTnode *n) {
  /*IRNode *ir = new_node(IR_WIDEN);*/
  /*ir->ast = n;*/
  /*ir->type = n->type;*/
  /*ir->ctype = n->ctype;*/
  return (genIRExpr(n->left));
  /*ASSERT(expr->r3);*/
  /*ir->r1 = expr->r3;*/
  /*ir->r3 = new_vreg(ir->type);*/
  /*return (ir);*/
}

IRNode *genIRTobool(ASTnode *n) {
  return (genIRExpr(n->left));
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
  ir->ast = n;
  ir->type = n->type; ir->ctype = n->ctype;
  IRNode *left = genIRExpr(n->left);
  IRNode *right = genIRExpr(n->right);
  ir->arg1 = IR_NodeValue(left);
  ir->arg2 = IR_NodeValue(right);
  ir->result = IR_NewTempValue();
  return (ir);
}

IRValue IR_NodeValue(IRNode *n) {
    switch (n->op) {
        case IR_ADD:
        case IR_SUBTRACT:
        case IR_MULTIPLY:
        case IR_DIVIDE:
        case IR_MODULO:
        case IR_EQ:
        case IR_NE:
        case IR_LT:
        case IR_GT:
        case IR_LE:
        case IR_GE:
        case IR_VAR:
        case IR_IMM:
        case IR_RETURN:
        case IR_TMP_ASSIGN:
            return n->result;
        default:
            fprintf(stderr, "Invalid value for node\n");
            exit(1);
    }
}

IRValue IR_NewTempValue(void) {
    ir_val_t t = ir_temp_t;
    IRValue val = {
        .t = t,
        .as = {
            .temp = tempnum
        }
    };
    tempnum++;
    return val;
}

IRValue IR_NewSymbolValue(struct symtable *sym) {
    ir_val_t t = ir_sym_t;
    IRValue val = {
        .t = t,
        .as = {
            .sym = sym
        }
    };
    return val;
}

IRValue IR_NewImmValue(int imm) {
    ir_val_t t = ir_imm_t;
    IRValue val = {
        .t = t,
        .as = {
            .imm = imm
        }
    };
    return val;
}

IRValue IR_NewEmptyValue(void) {
    ir_val_t t = ir_empty_t;
    IRValue val = {
        .t = t,
        .as = {
            .imm = 0
        }
    };
    return val;
}

IRNode *genIRIdent(ASTnode *n) {
  IROp op = IR_VAR;
  IRNode *ir_n = new_node(op);
  ir_n->ast = n;
  ir_n->type = n->type; ir_n->ctype = n->ctype;
  ir_n->result = IR_NewSymbolValue(n->sym);
  return (ir_n);
}


IRNode *genJump(BasicBlock *bb) {
  IRNode *ir_n = new_node(IR_JUMP);
  ir_n->bbout1 = bb;
  emitIR(ir_n);
  return (ir_n);
}

/*typedef struct PhiNodeData {*/
  /*IRNode *pred0;*/
  /*IRNode *pred1;*/
  /*struct PhiNodeData *next;*/
/*} PhiNodeData;*/

/*PhiNodeData *get_phi_node_data(PhiNodeData *first, IRNode *ir_n0, IRNode *ir_n1) {*/
  /*PhiNodeData *cur = first;*/
  /*while (cur) {*/
    /*if (ir_n0) {*/
      /*if (cur->pred0 && ir_n0->op == cur->pred0->op && !strcmp(cur->pred0->sym->name, ir_n0->sym->name)) {*/
        /*return (cur);*/
      /*}*/
    /*} else {*/
      /*if (cur->pred1 && ir_n1->op == cur->pred1->op && !strcmp(cur->pred1->sym->name, ir_n1->sym->name)) {*/
        /*return (cur);*/
      /*}*/
    /*}*/
    /*cur = cur->next;*/
  /*}*/
  /*return (NULL);*/
/*}*/

/*PhiNodeData *new_phi_node_data(IRNode *pred0, IRNode *pred1) {*/
  /*PhiNodeData *ret = (PhiNodeData*)calloc(1, sizeof(PhiNodeData));*/
  /*ASSERT(pred0);*/
  /*if (!pred0) {*/
    /*ASSERT(pred1);*/
    /*pred0 = dupIRNode(pred1);*/
    /*pred0->ssa_num = pred1->ssa_num - 1;*/
  /*}*/
  /*ret->pred0 = pred0;*/
  /*if (!pred1) {*/
    /*ASSERT(pred0);*/
    /*pred1 = dupIRNode(ret->pred0);*/
    /*pred1->ssa_num = pred0->ssa_num - 1;*/
  /*}*/
  /*ret->pred1 = pred1;*/
  /*return (ret);*/
/*}*/

/*PhiNodeData *collect_phi_nodes(BasicBlock *bb0, BasicBlock *bb1) {*/
  /*PhiNodeData *last = NULL;*/
  /*PhiNodeData *first = NULL;*/
  /*PhiNodeData *existing;*/
  /*IRNode *ir_node = bb0->nodes;*/
  /*while (ir_node) {*/
    /*switch (ir_node->op) {*/
      /*case IR_STORE_LOCAL:*/
      /*case IR_STORE_PARAM:*/
      /*case IR_STORE_GLOBAL:*/
        /*if (first) {*/
          /*if ((existing = get_phi_node_data(first, ir_node, NULL))) {*/
            /*existing->pred0 = ir_node; // has most recent ssa_num*/
          /*} else {*/
            /*last->next = new_phi_node_data(ir_node, NULL);*/
            /*last = last->next;*/
          /*}*/
        /*} else {*/
          /*first = last = new_phi_node_data(ir_node, NULL);*/
        /*}*/
    /*}*/
    /*ir_node = ir_node->next;*/
  /*}*/
  /*ir_node = bb1->nodes;*/
  /*while (ir_node) {*/
    /*switch (ir_node->op) {*/
      /*case IR_STORE_LOCAL:*/
      /*case IR_STORE_PARAM:*/
      /*case IR_STORE_GLOBAL:*/
        /*if (first) {*/
          /*if ((existing = get_phi_node_data(first, ir_node, ir_node))) {*/
            /*existing->pred1 = ir_node;*/
          /*} else {*/
            /*last->next = new_phi_node_data(NULL, ir_node);*/
            /*last = last->next;*/
          /*}*/
        /*} else {*/
          /*first = last = new_phi_node_data(NULL, ir_node);*/
        /*}*/
    /*}*/
    /*ir_node = ir_node->next;*/
  /*}*/
  /*return (first);*/
/*}*/

/*void bb_insert_node_before(BasicBlock *bb, IRNode *new, IRNode **before) {*/
  /*if (*before) {*/
    /*(*before)->prev = new;*/
    /*new->next = *before;*/
    /*bb->nodes = new;*/
  /*} else {*/
    /*bb->nodes = bb->last = new;*/
  /*}*/
/*}*/

/*int phi_type(IRNode *pred0, IRNode *pred1) {*/
  /*if (pred0 && pred1) {*/
    /*if (pred0->type != pred1->type) {*/
      /*fatal("Error: mismatched types");*/
    /*}*/
    /*return (pred0->type);*/
  /*} else if (pred0) {*/
    /*return (pred0->type);*/
  /*} else  if (pred1) {*/
    /*return (pred1->type);*/
  /*} else {*/
    /*ASSERT(0);*/
  /*}*/
  /*return (P_NONE);*/
/*}*/

/*Sym *phi_ctype(IRNode *pred0, IRNode *pred1) {*/
  /*if (pred0 && pred1) {*/
    /*if (pred0->ctype != pred1->ctype) {*/
      /*fatal("Error: mismatched types");*/
    /*}*/
    /*return (pred0->ctype);*/
  /*} else if (pred0) {*/
    /*return (pred0->ctype);*/
  /*} else  if (pred1) {*/
    /*return (pred1->ctype);*/
  /*} else {*/
    /*ASSERT(0);*/
  /*}*/
  /*return (NULL);*/
/*}*/

/*Sym *phi_sym(IRNode *pred0, IRNode *pred1) {*/
  /*if (pred0 && pred1) {*/
    /*if (pred0->sym != pred1->sym) {*/
      /*fatal("Error: Symbol mismatch");*/
    /*}*/
    /*return (pred0->sym);*/
  /*} else if (pred0) {*/
    /*return (pred0->sym);*/
  /*} else if (pred1) {*/
    /*return (pred1->sym);*/
  /*} else {*/
    /*ASSERT(0);*/
  /*}*/
  /*return (NULL);*/
/*}*/

/*void add_phi_ident_node(BasicBlock *bb, IRNode *pred0, IRNode *pred1) {*/
  /*IRNode *phi = new_node(IR_IDENT_PHI);*/
  /*ASSERT(pred0 && pred1);*/
  /*phi->phi0 = pred0;*/
  /*phi->phi1 = pred1;*/
  /*phi->sym = phi_sym(pred0, pred1);*/
  /*phi->ssa_num = ++phi->sym->ssa_num;*/
  /*phi->type = phi_type(pred0, pred1);*/
  /*phi->ctype = phi_ctype(pred0, pred1);*/
  /*bb_insert_node_before(bb, phi, &bb->nodes);*/
/*}*/

/*void add_phi_ident_nodes(BasicBlock *bb) {*/
  /*PhiNodeData *philist = collect_phi_nodes(bb->preds[0], bb->preds[1]);*/
  /*PhiNodeData *cur = philist;*/
  /*while (cur) {*/
    /*add_phi_ident_node(bb, cur->pred0, cur->pred1);*/
    /*cur = cur->next;*/
  /*}*/
/*}*/

/*// add phi register node (phi of 2 registers)*/
/*int add_phi_regnode(BasicBlock *bb, IRNode *n1, IRNode *n2) {*/
  /*ASSERT(n1);*/
  /*ASSERT(n2);*/
  /*IRNode *phi = new_node(IR_REG_PHI);*/
  /*phi->type = phi_type(n1, n2);*/
  /*phi->r1 = new_vreg(phi->type);*/
  /*phi->ctype  = phi_ctype(n1, n2);*/
  /*phi->phi0 = n1;*/
  /*phi->phi1 = n2;*/
  /*phi->r3 = phi->r1;*/
  /*bb_insert_node_before(bb, phi, &bb->nodes);*/
  /*return (1);*/
/*}*/

IRNode *genIRIf(ASTnode *n) {
  IRNode *cond = genIRExpr(n->left);
  emitIR(cond);
  IRNode *ir_n = new_node(IR_IF);
  ir_n->arg1 = IR_NodeValue(cond);
  ir_n->ast = n;
  ir_n->bbout1 = new_bb(NULL);
  if (n->right) {
    ir_n->bbout2 = new_bb(NULL);
  }

  // connect out
  /*cur_cfg_node()->node_out1 = ir_n->bbout1->cfg_node;*/
  /*cur_cfg_node()->node_out2 = ir_n->bbout2->cfg_node;*/

  BasicBlock *outbb = new_bb(NULL);

  if (!n->right) {
      ir_n->bbout2 = outbb;
  }
  bb_succ(cur_bb, ir_n->bbout1);
  if (n->right) {
      bb_succ(cur_bb, ir_n->bbout2);
  } else {
      bb_succ(cur_bb, outbb);
  }

  // connect in
  /*ir_n->bbout1->cfg_node->node_in1 = cur_cfg_node();*/
  /*ir_n->bbout2->cfg_node->node_in1 = cur_cfg_node();*/

  // connect outbb
  /*outbb->cfg_node->node_in1 = ir_n->bbout1->cfg_node;*/
  /*outbb->cfg_node->node_in2 = ir_n->bbout2->cfg_node;*/

  emitIR(ir_n);
  cur_bb->done = 1;
  cur_bb = ir_n->bbout1;
  genIR(n->mid); // true compound statement
  genJump(outbb);
  cur_bb->done = 1;

  if (n->right) { // false compound statement
    cur_bb = ir_n->bbout2;
    genIR(n->right);
    genJump(outbb); // redundant jump
    cur_bb->done = 1;
  }

  bb_succ(ir_n->bbout1, outbb);
  if (n->right) {
      bb_succ(ir_n->bbout2, outbb);
  }
  cur_bb = outbb;
  /*add_phi_ident_nodes(outbb);*/
  return (ir_n);
}

IRNode *genIRWhile(ASTnode *n) {
  BasicBlock *b_block = new_bb(NULL);
  BasicBlock *c_block = new_bb(NULL);

  bb_succ(cur_bb, b_block);
  bb_succ(b_block, b_block);
  bb_succ(b_block, c_block);

  IRNode *ir_if = new_node(IR_IF);
  ir_if->arg1 = IR_NodeValue(genIRExpr(n->left));
  ir_if->ast = n;
  ir_if->bbout1 = b_block;
  ir_if->bbout2 = c_block;
  emitIR(ir_if);
  cur_bb->done = 1;

  cur_bb = b_block;

  genIR(n->right);
  IRNode *ir_if1 = new_node(IR_IF);
  ir_if1->arg1 = IR_NodeValue(genIRExpr(n->left));
  ir_if1->ast = n;
  ir_if1->bbout1 = b_block;
  ir_if1->bbout2 = c_block;
  emitIR(ir_if1);
  cur_bb->done = 1;

  cur_bb = c_block;
  return ir_if1;
}

static IRNode *new_tmp_assign_node(IRNode *val) {
    IRNode *node = new_node(IR_TMP_ASSIGN);
    node->result = IR_NewTempValue();
    node->arg1 = IR_NodeValue(val);
    if (isCompoundIRExpr(val)) {
        emitIR(val);
    }
    return node;
}

static IRNode *new_tmp_assign_node_with(IRNode *val, IRValue tmp_val) {
    IRNode *node = new_node(IR_TMP_ASSIGN);
    node->result = tmp_val;
    node->arg1 = IR_NodeValue(val);
    if (isCompoundIRExpr(val)) {
        emitIR(val);
    }
    return node;
}


// logical or (a || b), it short circuits
// it translates to:
// res = false
// if a goto La else goto Lb
// La:
// res = true
// goto Ld
// Lb:
// if b goto Lc else goto Ld
// Lc:
// res = true
// goto Ld
// Ld:
// ...
IRNode *genIRLogor(ASTnode *n) {
    // res = false
    ASTnode *false_node = mkastleaf(A_INTLIT, P_INT, NULL, NULL, 0);
    IRNode *ir_res_false = genIRExpr(false_node);
    IRNode *ir_res_tmp = new_tmp_assign_node(ir_res_false);
    emitIR(ir_res_tmp);
    IRValue res_val = IR_NodeValue(ir_res_tmp);

    IRNode *ir_if = new_node(IR_IF);
    IRNode *cond = genIRExpr(n->left); // a
    if (isCompoundIRExpr(cond)) {
        emitIR(cond);
    }
    ir_if->arg1 = IR_NodeValue(cond);
    ir_if->ast = n;

    BasicBlock *a_block = new_bb(NULL);
    ir_if->bbout1 = a_block;
    BasicBlock *b_block = new_bb(NULL);
    ir_if->bbout2 = b_block;

    emitIR(ir_if);

    // graph setup
    bb_succ(cur_bb, a_block);
    bb_succ(cur_bb, b_block);

    BasicBlock *c_block = new_bb(NULL);
    BasicBlock *d_block = new_bb(NULL);
    bb_succ(b_block, c_block);
    bb_succ(b_block, d_block);
    bb_succ(c_block, d_block);

    cur_bb->done = 1;
    cur_bb = a_block;

    // La:
    ASTnode *true_node = mkastleaf(A_INTLIT, P_INT, NULL, NULL, 1);
    IRNode *ir_res_true = genIRExpr(true_node);
    IRNode *ir_res_tmp1 = new_tmp_assign_node_with(ir_res_true, res_val);
    emitIR(ir_res_tmp1);

    IRNode *goto_out = new_node(IR_JUMP);
    goto_out->bbout1 = d_block;
    emitIR(goto_out);
    cur_bb->done = 1;

    cur_bb = b_block;

    // Lb:
    IRNode *ir_b_if = new_node(IR_IF);
    IRNode *b_cond = genIRExpr(n->right); // b
    if (isCompoundIRExpr(b_cond)) {
        emitIR(b_cond);
    }
    ir_b_if->arg1 = IR_NodeValue(b_cond);
    ir_b_if->ast = n;
    ir_b_if->bbout1 = c_block;
    ir_b_if->bbout2 = d_block;
    emitIR(ir_b_if);
    cur_bb->done = 1;

    cur_bb = c_block;

    ASTnode *true_node1 = mkastleaf(A_INTLIT, P_INT, NULL, NULL, 1);
    IRNode *ir_res_true1 = genIRExpr(true_node1);
    IRNode *ir_res_tmp2 = new_tmp_assign_node_with(ir_res_true1, res_val);
    emitIR(ir_res_tmp2);

    IRNode *goto_out1 = new_node(IR_JUMP);
    goto_out1->bbout1 = d_block;
    emitIR(goto_out1);
    cur_bb->done = 1;

    cur_bb = d_block;

    return ir_res_false;
}

// logical and (a && b), it short circuits
// translates to:
// res = true
// if a goto Lb else goto Lc
// Lb:
// if b goto Ld else goto Lc
// Lc:
// res = false
// goto Ld
// Ld:
// ...
IRNode *genIRLogand(ASTnode *n) {
    // res = true
    ASTnode *true_node = mkastleaf(A_INTLIT, P_INT, NULL, NULL, 1);
    IRNode *ir_res_true = genIRExpr(true_node);
    IRNode *ir_res_tmp = new_tmp_assign_node(ir_res_true);
    emitIR(ir_res_tmp);
    IRValue res_val = IR_NodeValue(ir_res_tmp);

    IRNode *ir_if = new_node(IR_IF);
    IRNode *cond = genIRExpr(n->left); // a
    if (isCompoundIRExpr(cond)) {
        emitIR(cond);
    }
    ir_if->arg1 = IR_NodeValue(cond);
    ir_if->ast = n;

    BasicBlock *b_block = new_bb(NULL);
    ir_if->bbout1 = b_block;
    BasicBlock *c_block = new_bb(NULL);
    ir_if->bbout2 = c_block;

    emitIR(ir_if);

    // graph setup
    bb_succ(cur_bb, b_block);
    bb_succ(cur_bb, c_block);

    BasicBlock *d_block = new_bb(NULL);
    bb_succ(b_block, c_block);
    bb_succ(b_block, d_block);
    bb_succ(c_block, d_block);

    cur_bb->done = 1;
    cur_bb = b_block;

    // Lb:
    IRNode *ir_b_if = new_node(IR_IF);
    IRNode *b_cond = genIRExpr(n->right); // b
    if (isCompoundIRExpr(b_cond)) {
        emitIR(b_cond);
    }
    ir_b_if->arg1 = IR_NodeValue(b_cond);
    ir_b_if->ast = n;
    ir_b_if->bbout1 = d_block;
    ir_b_if->bbout2 = c_block;
    emitIR(ir_b_if);
    cur_bb->done = 1;

    cur_bb = c_block;

    ASTnode *false_node = mkastleaf(A_INTLIT, P_INT, NULL, NULL, 0);
    IRNode *ir_res_false = genIRExpr(false_node);
    IRNode *ir_res_tmp2 = new_tmp_assign_node_with(ir_res_false, res_val);
    emitIR(ir_res_tmp2);

    IRNode *goto_out = new_node(IR_JUMP);
    goto_out->bbout1 = d_block;
    emitIR(goto_out);
    cur_bb->done = 1;

    cur_bb = d_block;

    return ir_res_true;
}

/*IRNode *genIRTernary(ASTnode *n) {*/
  /*IRNode *if_expr, *else_expr;*/
  /*IRNode *cond = genIRExpr(n->left);*/
  /*IRNode *ir_n = new_node(IR_IF);*/
  /*ir_n->ast = n;*/
  /*ir_n->r1 = cond->r3;*/
  /*ir_n->bbout1 = new_bb(NULL);*/
  /*ir_n->bbout2 = new_bb(NULL);*/
  /*BasicBlock *outbb = new_bb(NULL);*/
  /*cur_bb->succs[0] = ir_n->bbout1;*/
  /*cur_bb->succs[1] = ir_n->bbout2;*/
  /*ir_n->bbout1->preds[0] = cur_bb;*/
  /*ir_n->bbout2->preds[0] = cur_bb;*/

  /*emitIR(ir_n);*/

  /*cur_bb->done = 1;*/
  /*cur_bb = ir_n->bbout1;*/
  /*if_expr = genIRExpr(n->mid); // true expr*/
  /*ir_n->type = if_expr->type;*/
  /*ir_n->ctype = if_expr->ctype;*/
  /*genJump(outbb->ilabel);*/
  /*cur_bb->done = 1;*/

  /*cur_bb = ir_n->bbout2;*/
  /*else_expr = genIRExpr(n->right); // else expr*/
  /*genJump(outbb->ilabel);*/
  /*cur_bb->done = 1;*/

  /*ir_n->bbout1->succs[0] = outbb;*/
  /*ir_n->bbout2->succs[0] = outbb;*/
  /*outbb->preds[0] = ir_n->bbout1;*/
  /*outbb->preds[1] = ir_n->bbout2;*/
  /*cur_bb = outbb;*/
  /*add_phi_ident_nodes(outbb);*/
  /*if (add_phi_regnode(outbb, if_expr, else_expr)) {*/
      /*ir_n->r3 = outbb->nodes->r3;*/
  /*} else {*/
      /*ir_n->r3 = new_vreg(ir_n->type);*/
  /*}*/
  /*return (ir_n);*/
/*}*/

IRNode *genIRExpr(struct ASTnode *n) {
  switch (n->op) {
    case A_ADD:
    case A_SUBTRACT:
    case A_MULTIPLY:
    case A_DIVIDE:
    case A_MODULO:
    case A_EQ:
    case A_NE:
    case A_LT:
    case A_GT:
    case A_LE:
    case A_GE:
      return (genIRBinop(n));
    case A_INTLIT:
      return (genIRImm(n));
    case A_WIDEN:
      return (genIRWiden(n));
    case A_IDENT:
      return (genIRIdent(n));
    case A_LOGOR:
      return (genIRLogor(n));
    case A_LOGAND:
      return (genIRLogand(n));
    /*case A_TERNARY:*/
      /*return (genIRTernary(n));*/
    case A_TOBOOL:
      return (genIRTobool(n));
    default:
      fatalv("Unknown AST op: %s (%d)", opname(n->op), n->op);
  }
}

static IRNode *genIRAssign(struct ASTnode *n) {
    IRNode *ir_n = new_node(IR_ASSIGN);
    ir_n->type = n->type; ir_n->ctype = n->ctype;
    IRNode *n_right = genIRExpr(n->right);
    ir_n->result = IR_NodeValue(n_right);
    IRNode *n_left = genIRExpr(n->left);
    if (isCompoundIRExpr(n_left)) {
        emitIR(n_left);
    }
    ir_n->arg1 = IR_NodeValue(n_left);
    emitIR(ir_n);
    return (ir_n);
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
    case A_ASSIGN:
      genIRAssign(n);
      break;
    case A_IF:
      genIRIf(n);
      break;
    case A_WHILE:
      genIRWhile(n);
      break;
    /*case A_LOGOR:*/
      /*genIRLogor(n);*/
      /*break;*/
    /*case A_LOGAND:*/
      /*genIRLogand(n);*/
      /*break;*/
    default:
      genIRExpr(n);
      break;
  }
  return (start_bb);
}

void genIRFinish(void) {
    /*ASSERT(cur_cfg);*/
    ASSERT(cur_bb);
    /*cur_cfg->exit = cur_bb->cfg_node;*/
}

static char *IR_ValueStr(IRValue val) {
    char buf[100] = {0};
    switch (val.t) {
        case ir_empty_t:
            return strdup("empty?");
        case ir_temp_t:
            sprintf(buf, "t%d", val.as.temp);
            return strdup(buf);
        case ir_sym_t:
            sprintf(buf, "%s", val.as.sym->name);
            return strdup(buf);
        case ir_imm_t:
            sprintf(buf, "%d", val.as.imm);
            return strdup(buf);
        default:
            fatalv("Unknown val type (%d)", val.t);
    }
}

void dumpIRNode(IRNode *n, FILE *f) {
  ASSERT(n);
  switch (n->op) {
    case IR_ADD:
      fprintf(f, "%s = %s + %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1), IR_ValueStr(n->arg2));
      break;
    case IR_SUBTRACT:
      fprintf(f, "%s = %s - %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1), IR_ValueStr(n->arg2));
      break;
    case IR_MULTIPLY:
      fprintf(f, "%s = %s * %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1), IR_ValueStr(n->arg2));
      break;
    case IR_DIVIDE:
      fprintf(f, "%s = %s / %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1), IR_ValueStr(n->arg2));
      break;
    case IR_MODULO:
      fprintf(f, "%s = %s %% %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1), IR_ValueStr(n->arg2));
      break;
    case IR_EQ:
      fprintf(f, "%s = %s == %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1), IR_ValueStr(n->arg2));
      break;
    case IR_NE:
      fprintf(f, "%s = %s != %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1), IR_ValueStr(n->arg2));
      break;
    case IR_LT:
      fprintf(f, "%s = %s < %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1), IR_ValueStr(n->arg2));
      break;
    case IR_GT:
      fprintf(f, "%s = %s > %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1), IR_ValueStr(n->arg2));
      break;
    case IR_LE:
      fprintf(f, "%s = %s <= %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1), IR_ValueStr(n->arg2));
      break;
    case IR_GE:
      fprintf(f, "%s = %s >= %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1), IR_ValueStr(n->arg2));
      break;
    case IR_RETURN:
      fprintf(f, "ret %s\n", IR_ValueStr(n->result));
      break;
    case IR_IMM:
      fprintf(f, "%s = %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1));
      break;
    case IR_ASSIGN:
      fprintf(f, "%s = %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1));
      break;
    case IR_TMP_ASSIGN:
      fprintf(f, "%s = %s\n", IR_ValueStr(n->result), IR_ValueStr(n->arg1));
      break;
    case IR_IF:
      if (n->bbout2) {
          fprintf(f, "if %s goto L%d else goto L%d\n", IR_ValueStr(n->arg1), n->bbout1->ilabel, n->bbout2->ilabel);
      } else {
          fprintf(f, "if %s continue else goto L%d\n", IR_ValueStr(n->arg1), n->bbout1->ilabel);
      }
      break;
    case IR_JUMP:
      fprintf(f, "goto L%d\n", n->bbout1->ilabel);
      break;
    case IR_VAR:
      fatalv("Unknown IR node for printing (%d)", n->op);
    default:
      fatalv("Unknown IR node for printing (%d)", n->op);
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
