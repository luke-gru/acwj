#include "defs.h"
#include "data.h"
#include "decl.h"

#define NOLABEL 0

// AST tree functions
// Copyright (c) 2019 Warren Toomey, GPL3

// Build and return a generic AST node
struct ASTnode *mkastnode(int op, int type,
    struct symtable *ctype,
    struct ASTnode *left,
    struct ASTnode *mid,
    struct ASTnode *right,
    struct symtable *sym, int intvalue) {
  struct ASTnode *n;

  // Malloc a new ASTnode
  n = (struct ASTnode *) malloc(sizeof(struct ASTnode));
  if (n == NULL) {
    fprintf(stderr, "Unable to malloc in mkastnode()\n");
    exit(1);
  }
  // Copy in the field values and return it
  ASSERT(op >= A_FIRST && op <= A_LAST);
  n->op = op;
  n->type = type;
  n->ctype = ctype;
  n->left = left;
  n->mid = mid;
  n->right = right;
  n->sym = sym;
  n->intvalue = intvalue;
  n->line = Line;
  n->col = column();
  n->rvalue = 0;
  return (n);
}

struct ASTnode *dupastnode(struct ASTnode *n, int recurse) {
  ASSERT(n);
  struct ASTnode *newn;
  newn = mkastnode(n->op, n->type, n->ctype, n->left, n->mid, n->right,
      n->sym, n->intvalue);
  newn->rvalue = n->rvalue;
  if (!recurse) {
    return (newn);
  }
  if (newn->left)
    newn->left = dupastnode(newn->left, recurse);
  if (newn->mid)
    newn->mid = dupastnode(newn->mid, recurse);
  if (newn->right)
    newn->right = dupastnode(newn->right, recurse);
  return (newn);
}

// Make an AST leaf node
struct ASTnode *mkastleaf(int op, int type, struct symtable *ctype, struct symtable *sym, int intvalue) {
  return (mkastnode(op, type, ctype, NULL, NULL, NULL, sym, intvalue));
}

// Make a unary AST node: only one child
struct ASTnode *mkastunary(int op, int type, struct symtable *ctype, struct ASTnode *left, struct symtable *sym, int intvalue) {
  return (mkastnode(op, type, ctype, left, NULL, NULL, sym, intvalue));
}

static int label_id = 1;

// Generate and return a new label number
// just for AST dumping purposes
static int gendumplabel(void) {
  return (label_id++);
}

// Given an AST tree, print it out and follow the
// traversal of the tree that genAST() follows
void dumpAST(struct ASTnode *n, int label, int level) {
  int Lfalse, Lstart, Lend;
  struct ASTnode *casen;
  int i;

  if (!n) return;

  // early return cases
  switch (n->op) {
    case A_IF:
      Lfalse = gendumplabel();
      for (i=0; i < level; i++) fprintf(stdout, " ");
      fprintf(stdout, "A_IF");
      if (n->right) { Lend = gendumplabel();
        fprintf(stdout, ", end L%d", Lend);
      }
      fprintf(stdout, "\n");
      dumpAST(n->left, Lfalse, level+2);
      dumpAST(n->mid, NOLABEL, level+2);
      if (n->right) dumpAST(n->right, NOLABEL, level+2);
      return;
    case A_TERNARY:
      for (i=0; i < level; i++) fprintf(stdout, " ");
      fprintf(stdout, "A_TERNARY\n");
      dumpAST(n->left, NOLABEL, level+2);
      dumpAST(n->mid, NOLABEL, level+2);
      dumpAST(n->right, NOLABEL, level+2);
      return;
    case A_WHILE:
      Lstart = gendumplabel();
      for (i=0; i < level; i++) fprintf(stdout, " ");
      fprintf(stdout, "A_WHILE, start L%d\n", Lstart);
      Lend = gendumplabel();
      dumpAST(n->left, Lend, level+2);
      dumpAST(n->right, NOLABEL, level+2);
      return;
    case A_SWITCH:
      for (i=0; i < level; i++) fprintf(stdout, " ");
      fprintf(stdout, "A_SWITCH %d\n", n->intvalue);
      for (casen = n->right; casen != NULL; casen=casen->right) {
        dumpAST(casen, NOLABEL, level); // case node itself, with integer value
        dumpAST(casen->left, NOLABEL, level+2); // compound statements
      }
      return;
    case A_CASE:
      for (i=0; i < level; i++) fprintf(stdout, " ");
      fprintf(stdout, "A_CASE %d\n", n->intvalue); return;
    case A_DEFAULT:
      for (i=0; i < level; i++) fprintf(stdout, " ");
      fprintf(stdout, "A_DEFAULT\n"); return;
    case A_GLUE:
      for (i=0; i < level; i++) fprintf(stdout, " ");
      fprintf(stdout, "GLUE\n");
      // General AST node handling
      if (n->left) dumpAST(n->left, NOLABEL, level+2);
      if (n->right) dumpAST(n->right, NOLABEL, level+2);
      return;
    default:
      break;
  }

  for (i=0; i < level; i++) fprintf(stdout, " ");

  switch (n->op) {
    case A_FUNCTION:
      fprintf(stdout, "A_FUNCTION %s\n", n->sym->name); break;
    case A_ADD:
      fprintf(stdout, "A_ADD\n"); break;
    case A_SUBTRACT:
      fprintf(stdout, "A_SUBTRACT\n"); break;
    case A_MULTIPLY:
      fprintf(stdout, "A_MULTIPLY\n"); break;
    case A_DIVIDE:
      fprintf(stdout, "A_DIVIDE\n"); break;
    case A_MODULO:
      fprintf(stdout, "A_MODULO\n"); break;
    case A_EQ:
      fprintf(stdout, "A_EQ\n"); break;
    case A_NE:
      fprintf(stdout, "A_NE\n"); break;
    case A_LT:
      fprintf(stdout, "A_LE\n"); break;
    case A_GT:
      fprintf(stdout, "A_GT\n"); break;
    case A_LE:
      fprintf(stdout, "A_LE\n"); break;
    case A_GE:
      fprintf(stdout, "A_GE\n"); break;
    case A_LSHIFT:
      fprintf(stdout, "A_LSHIFT\n"); break;
    case A_RSHIFT:
      fprintf(stdout, "A_RSHIFT\n"); break;
    case A_INTLIT:
      fprintf(stdout, "A_INTLIT %d\n", n->intvalue); break;
    case A_STRLIT:
      fprintf(stdout, "A_STRLIT (asm label %d)\n", n->intvalue); break;
    case A_IDENT:
      if (n->rvalue)
        fprintf(stdout, "A_IDENT rval %s (%s)\n", n->sym->name, typename(n->sym->type, n->sym->ctype));
      else
        fprintf(stdout, "A_IDENT %s (%s)\n", n->sym->name, typename(n->sym->type, n->sym->ctype));
      return;
    case A_ASSIGN:
      fprintf(stdout, "A_ASSIGN\n"); break;
    case A_AS_ADD:
      fprintf(stdout, "A_AS_ADD\n"); break;
    case A_AS_SUBTRACT:
      fprintf(stdout, "A_AS_SUBTRACT\n"); break;
    case A_AS_MULTIPLY:
      fprintf(stdout, "A_AS_MULTIPLY\n"); break;
    case A_AS_DIVIDE:
      fprintf(stdout, "A_AS_DIVIDE\n"); break;
    case A_LOGOR:
      fprintf(stdout, "A_LOGOR\n"); break;
    case A_LOGAND:
      fprintf(stdout, "A_LOGOR\n"); break;
    case A_BITOR:
      fprintf(stdout, "A_BITOR\n"); break;
    case A_BITXOR:
      fprintf(stdout, "A_BITXOR\n"); break;
    case A_BITAND:
      fprintf(stdout, "A_BITAND\n"); break;
    case A_WIDEN:
      fprintf(stdout, "A_WIDEN\n"); break;
    case A_RETURN:
      fprintf(stdout, "A_RETURN\n"); break;
    case A_FUNCALL:
      if (n->sym) {
        fprintf(stdout, "A_FUNCALL %s\n", n->sym->name); break;
      } else {
        fprintf(stdout, "A_FUNCALL\n"); break;
      }
    case A_ADDR:
      if (n->sym) {
        fprintf(stdout, "A_ADDR %s\n", n->sym->name); break;
      } else {
        fprintf(stdout, "A_ADDR\n"); break;
      }
    case A_DEREF:
      if (n->rvalue)
        fprintf(stdout, "A_DEREF rval (%s)\n", typename(n->type, NULL));
      else
        fprintf(stdout, "A_DEREF (%s)\n", typename(n->type, NULL));
      break;
    case A_SCALE:
      fprintf(stdout, "A_SCALE %d\n", n->size); break;
    case A_PREINC:
      fprintf(stdout, "A_PREINC\n"); break;
    case A_PREDEC:
      fprintf(stdout, "A_PREDEC\n"); break;
    case A_POSTINC:
      fprintf(stdout, "A_POSTINC\n"); break;
    case A_POSTDEC:
      fprintf(stdout, "A_POSTDEC\n"); break;
    case A_NEGATE:
      fprintf(stdout, "A_NEGATE\n"); break;
    case A_INVERT:
      fprintf(stdout, "A_INVERT\n"); break;
    case A_LOGNOT:
      fprintf(stdout, "A_LOGNOT\n"); break;
    case A_TOBOOL:
      fprintf(stdout, "A_TOBOOL\n"); break;
    case A_CAST:
      fprintf(stdout, "A_CAST %s\n", typename(n->type, NULL)); break;
    case A_BREAK:
      fprintf(stdout, "A_BREAK\n"); break;
    case A_CONTINUE:
      fprintf(stdout, "A_CONTINUE\n"); break;
    case A_LABEL:
      fprintf(stdout, "A_LABEL %s\n", n->sym->name); break;
    case A_GOTO:
      fprintf(stdout, "A_GOTO %s\n", n->sym->name); break;
    case A_EMPTY:
      fprintf(stdout, "A_EMPTY\n"); break;
    case A_SEQUENCE:
      fprintf(stdout, "A_SEQUENCE\n"); break;
    default:
      ASSERT(n->op < A_LAST);
      fatald("Unknown dumpAST AST node operator", n->op);
  }

  if (n->left) dumpAST(n->left, NOLABEL, level+2);
  if (n->right) dumpAST(n->right, NOLABEL, level+2);
}

/**
 * Rewrite v.val++ to be equivalent to (v.val = v.val + 1, v.val - 1)
 *
 before:
       A_POSTINC
          A_DEREF (char*)
            A_ADD
              A_ADDR v
              A_INTLIT 0

 after:
    A_SEQUENCE
       A_ASSIGN
          A_ADD
            A_DEREF rval (char*)
              A_ADD
                A_ADDR v
                A_INTLIT 0
            A_INTLIT 1
          A_DEREF (char*)
            A_ADD
              A_ADDR v
              A_INTLIT 0
      A_SUBTRACT
        A_DEREF rval (char*)
          A_ADD
            A_ADDR v
            A_INTLIT 0
        A_INTLIT 1
 *
 */
struct ASTnode *rewrite_postinc(struct ASTnode *tree) {
  struct ASTnode *left = tree->left;
  struct ASTnode *addnode;
  struct ASTnode *subnode;
  struct ASTnode *sequencenode;
  struct ASTnode *right;
  struct ASTnode *assignnode;
  struct ASTnode *intlit;
  int intval_incr = 1;

  right = dupastnode(left, 1);
  right->rvalue = 1;

  if (ptrtype(right->type)) {
    intval_incr = typesize(value_at(right->type), right->ctype);
  }

  intlit = mkastleaf(A_INTLIT, P_INT, NULL, NULL, intval_incr);
  addnode = mkastnode(A_ADD, right->type, right->ctype, right, NULL, intlit, NULL, 0);
  left->rvalue = 0;
  assignnode = mkastnode(A_ASSIGN, right->type, right->ctype, addnode, NULL, left, NULL, 0);

  subnode = dupastnode(addnode, 1);
  subnode->op = A_SUBTRACT;
  sequencenode = mkastnode(A_SEQUENCE, subnode->type, subnode->ctype, assignnode, NULL, subnode, NULL, 0);

  return (sequencenode);
}

/**
 * Rewrite v.val-- to be equivalent to (v.val = v.val - 1, v.val + 1)
 *
 before:
       A_POSTDEC
          A_DEREF (char*)
            A_ADD
              A_ADDR v
              A_INTLIT 0

 after:
    A_SEQUENCE
       A_ASSIGN
          A_SUBTRACT
            A_DEREF rval (char*)
              A_ADD
                A_ADDR v
                A_INTLIT 0
            A_INTLIT 1
          A_DEREF (char*)
            A_ADD
              A_ADDR v
              A_INTLIT 0
      A_ADD
        A_DEREF rval (char*)
          A_ADD
            A_ADDR v
            A_INTLIT 0
        A_INTLIT 1
 *
 */
struct ASTnode *rewrite_postdec(struct ASTnode *tree) {
  struct ASTnode *left = tree->left;
  struct ASTnode *addnode;
  struct ASTnode *subnode;
  struct ASTnode *sequencenode;
  struct ASTnode *right;
  struct ASTnode *assignnode;
  struct ASTnode *intlit;
  int intval_decr = 1;

  right = dupastnode(left, 1);
  right->rvalue = 1;

  if (ptrtype(right->type)) {
    intval_decr = typesize(value_at(right->type), right->ctype);
  }

  intlit = mkastleaf(A_INTLIT, P_INT, NULL, NULL, intval_decr);
  subnode = mkastnode(A_SUBTRACT, right->type, right->ctype, right, NULL, intlit, NULL, 0);
  left->rvalue = 0;
  assignnode = mkastnode(A_ASSIGN, right->type, right->ctype, subnode, NULL, left, NULL, 0);

  addnode = dupastnode(subnode, 1);
  addnode->op = A_ADD;
  sequencenode = mkastnode(A_SEQUENCE, addnode->type, addnode->ctype, assignnode, NULL, addnode, NULL, 0);

  return (sequencenode);
}

/**
 * Rewrite ++v.val to be equivalent to (v.val = v.val + 1)
 *
 before:
       A_PREINC
          A_DEREF (char*)
            A_ADD
              A_ADDR v
              A_INTLIT 0

 after:
       A_ASSIGN
          A_ADD
            A_DEREF rval (char*)
              A_ADD
                A_ADDR v
                A_INTLIT 0
            A_INTLIT 1
          A_DEREF (char*)
            A_ADD
              A_ADDR v
              A_INTLIT 0
 *
 */
struct ASTnode *rewrite_preinc(struct ASTnode *tree) {
  struct ASTnode *left = tree->left;
  struct ASTnode *addnode;
  struct ASTnode *right;
  struct ASTnode *assignnode;
  struct ASTnode *intlit;
  int intval_incr = 1;

  right = dupastnode(left, 1);
  right->rvalue = 1;

  if (ptrtype(right->type)) {
    intval_incr = typesize(value_at(right->type), right->ctype);
  }

  intlit = mkastleaf(A_INTLIT, P_INT, NULL, NULL, intval_incr);
  addnode = mkastnode(A_ADD, right->type, right->ctype, right, NULL, intlit, NULL, 0);
  left->rvalue = 0;
  assignnode = mkastnode(A_ASSIGN, right->type, right->ctype, addnode, NULL, left, NULL, 0);

  return (assignnode);
}

/**
 * Rewrite --v.val to be equivalent to (v.val = v.val - 1)
 *
 before:
       A_PREDEC
          A_DEREF (char*)
            A_ADD
              A_ADDR v
              A_INTLIT 0

 after:
       A_ASSIGN
          A_SUBTRACT
            A_DEREF rval (char*)
              A_ADD
                A_ADDR v
                A_INTLIT 0
            A_INTLIT 1
          A_DEREF (char*)
            A_ADD
              A_ADDR v
              A_INTLIT 0
 *
 */
struct ASTnode *rewrite_predec(struct ASTnode *tree) {
  struct ASTnode *left = tree->left;
  struct ASTnode *subnode;
  struct ASTnode *right;
  struct ASTnode *assignnode;
  struct ASTnode *intlit;
  int intval_decr = 1;

  right = dupastnode(left, 1);
  right->rvalue = 1;

  if (ptrtype(right->type)) {
    intval_decr = typesize(value_at(right->type), right->ctype);
  }

  intlit = mkastleaf(A_INTLIT, P_INT, NULL, NULL, intval_decr);
  subnode = mkastnode(A_SUBTRACT, right->type, right->ctype, right, NULL, intlit, NULL, 0);
  left->rvalue = 0;
  assignnode = mkastnode(A_ASSIGN, right->type, right->ctype, subnode, NULL, left, NULL, 0);

  return (assignnode);
}

struct ASTnode *rewrite_tree(struct ASTnode *tree) {
  switch (tree->op) {
    case A_POSTINC:
      return (rewrite_postinc(tree));
    case A_POSTDEC:
      return (rewrite_postdec(tree));
    case A_PREINC:
      return (rewrite_preinc(tree));
    case A_PREDEC:
      return (rewrite_predec(tree));
  }
  return (NULL);
}

struct ASTnode *toboolnode(struct ASTnode *tree) {
  struct ASTnode *booln;
  switch (tree->op) {
    case A_EQ:
    case A_NE:
    case A_LT:
    case A_GT:
    case A_LE:
    case A_GE:
      return (tree);
  }
  tree->rvalue = 1;
  booln = mkastunary(A_TOBOOL, tree->type, tree->ctype, tree, NULL, 0);
  return (booln);
}
