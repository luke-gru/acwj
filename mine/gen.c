#include <assert.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Generic code generator
// Copyright (c) 2019 Warren Toomey, GPL3

// Generate and return a new label number
int genlabel(void) {
  static int id = 1;
  return (id++);
}

// Generate the code for an IF statement
// and an optional ELSE clause
static int genIF(struct ASTnode *n, int looptoplabel, int loopendlabel) {
  int Lfalse, Lend;

  // Generate two labels: one for the
  // false compound statement, and one
  // for the end of the overall IF statement.
  // When there is no ELSE clause, Lfalse _is_
  // the ending label!
  Lfalse = genlabel();
  if (n->right)
    Lend = genlabel();

  // Generate the condition code followed
  // by a zero jump to the false label.
  // We cheat by sending the Lfalse label as a register.
  genAST(n->left, Lfalse, NOLABEL, NOLABEL, n->op);
  genfreeregs();

  // Generate the true compound statement
  genAST(n->mid, NOREG, looptoplabel, loopendlabel, n->op);
  genfreeregs();

  // If there is an optional ELSE clause,
  // generate the jump to skip to the end
  if (n->right)
    cgjump(Lend);

  // Now the false label
  cglabel(Lfalse);

  // Optional ELSE clause: generate the
  // false compound statement and the
  // end label
  if (n->right) {
    genAST(n->right, NOREG, looptoplabel, loopendlabel, n->op);
    genfreeregs();
    cglabel(Lend);
  }

  return (NOREG);
}

static int genWhile(struct ASTnode *n) {
  int Lstart, Lend;

  Lstart = genlabel();
  Lend = genlabel();
  cglabel(Lstart);

  // Generate the condition code followed
  // by a jump to the end label.
  // We cheat by sending the Lend label as a register.
  genAST(n->left, Lend, Lstart, Lend, n->op);
  genfreeregs();

  // compound statement
  genAST(n->right, NOLABEL, Lstart, Lend, n->op);
  genfreeregs();

  cgjump(Lstart);
  cglabel(Lend);
  return (NOREG);
}

// Generate the code to copy the arguments of a
// function call to its parameters, then call the
// function itself. Return the register that holds
// the function's return value.
static int gen_funcall(struct ASTnode *n) {
  struct ASTnode *gluetree = n->left;
  int reg;
  int numargs=0;

  // If there is a list of arguments, walk this list
  // from the last argument (right-hand child) to the
  // first
  while (gluetree) {
    // Calculate the expression's value
    reg = genAST(gluetree->right, NOREG, NOLABEL, NOLABEL, gluetree->op);
    // Copy this into the n'th function parameter: size is 1, 2, 3, ...
    cgcopyarg(reg, gluetree->size);
    // Keep the first (highest) number of arguments
    if (numargs==0) numargs= gluetree->size;
    genfreeregs();
    gluetree = gluetree->left;
  }

  // Call the function, clean up the stack (based on numargs),
  // and return its result
  return (cgcall(n->sym, numargs));
}

int genSwitch(struct ASTnode *n) {
  int *casevals, *caselabels;
  int Linternal_switch_dispatch, Lend;
  int i, reg, defaultlabel = 0, casecount = 0;
  struct ASTnode *c;

  int numcases = n->intvalue;
  // Create jump table memory
  // Create arrays for the case values and associated labels.
  // Ensure that we have at least one position in each array.
  casevals = (int *) malloc((numcases + 1) * sizeof(int));
  caselabels = (int *) malloc((numcases + 1) * sizeof(int));
  // Generate labels for the internal switch dispatch, and the
  // end of the switch statement. Set a default label for
  // the end of the switch, in case we don't have a default.
  Linternal_switch_dispatch = genlabel();
  Lend = genlabel();
  defaultlabel = Lend;

  // Output the code to calculate the switch condition
  reg = genAST(n->left, NOREG, NOLABEL, NOLABEL, 0);
  cgjump(Linternal_switch_dispatch);
  genfreeregs();

  // Walk the right-child linked list to
  // generate the code for each case
  for (i = 0, c = n->right; c != NULL; i++, c = c->right) {

    // Get a label for this case. Store it
    // and the case value in the arrays.
    // Record if it is the default case.
    caselabels[i] = genlabel();
    casevals[i] = c->intvalue;
    cglabel(caselabels[i]);
    if (c->op == A_DEFAULT)
      defaultlabel = caselabels[i];
    else
      casecount++;

    // Generate the case code. Pass in the end label for the breaks
    genAST(c->left, NOREG, NOLABEL, Lend, 0);
    genfreeregs();
  }

  // Ensure the last case jumps past the switch table
  cgjump(Lend);

  // Now output the switch table and the end label.
  cgswitch(reg, casecount, Linternal_switch_dispatch, caselabels, casevals, defaultlabel);
  cglabel(Lend);

  free(caselabels);
  free(casevals);

  return (NOREG);
}

// Given an AST, generate assembly code recursively.
// Return the register id with the tree's final value
int genAST(struct ASTnode *n, int reg, int looptoplabel, int loopendlabel, int parentASTop) {
  int leftreg, rightreg;
  leftreg = -1;
  rightreg = -1;

  if (O_parseOnly)
    assert(0); // shouldn't even be called if O_parseOnly == 1

  // We now have specific AST node handling at the top
  switch (n->op) {
    case A_FUNCTION:
      cgfuncpreamble(n->sym);
      n->sym->endlabel = genlabel();
      genAST(n->left, NOREG, NOLABEL, NOLABEL, n->op);
      cgfuncpostamble(n->sym);
      return (NOREG);
    case A_IF:
      return (genIF(n, looptoplabel, loopendlabel));
    case A_WHILE:
      return (genWhile(n));
    case A_SWITCH:
      return (genSwitch(n));
    case A_BREAK:
      cgjump(loopendlabel);
      return (NOREG);
    case A_CONTINUE:
      cgjump(looptoplabel);
      return (NOREG);
    case A_GLUE:
      // Do each child statement, and free the
      // registers after each child
      leftreg = genAST(n->left, NOREG, looptoplabel, loopendlabel, n->op);
      genfreeregs();
      rightreg = genAST(n->right, NOREG, looptoplabel, loopendlabel, n->op);
      genfreeregs();
      return (NOREG);
    case A_FUNCALL:
      return (gen_funcall(n));
    default: // continue below
      break;
  }

  // Get the left and right sub-tree values
  if (n->left) {
    leftreg = genAST(n->left, NOREG, looptoplabel, looptoplabel, n->op);
  }
  if (n->right) {
    rightreg = genAST(n->right, leftreg, looptoplabel, loopendlabel, n->op);
  }

  switch (n->op) {
  case A_ADD:
    return (cgadd(leftreg, rightreg));
  case A_SUBTRACT:
    return (cgsub(leftreg, rightreg));
  case A_MULTIPLY:
    return (cgmul(leftreg, rightreg));
  case A_DIVIDE:
    return (cgdiv(leftreg, rightreg));
  case A_EQ:
  case A_NE:
  case A_LT:
  case A_GT:
  case A_LE:
  case A_GE:
    // If the parent AST node is an A_IF, generate a compare
    // followed by a jump. Otherwise, compare registers and
    // set one to 1 or 0 based on the comparison.
    if (parentASTop == A_IF || parentASTop == A_WHILE)
      return (cgcompare_and_jump(n->op, leftreg, rightreg, reg));
    else
      return (cgcompare_and_set(n->op, leftreg, rightreg));
  case A_INTLIT:
    return (cgloadint(n->intvalue));
  case A_IDENT:
    // Load our value if we are an rvalue
    // or we are being dereferenced
    if (n->rvalue || parentASTop == A_DEREF) {
      if (n->sym->class == C_GLOBAL || n->sym->class == C_EXTERN) {
        return (cgloadglob(n->sym, n->op));
      } else {
        return (cgloadlocal(n->sym, n->op));
      }
    } else {
      return (NOREG); // lvalue, let the ASSIGN node do the 'store' work
    }
  case A_ASSIGN:
    switch (n->right->op) {
      case A_IDENT: // ex: a = 12
        if (n->right->sym->class == C_GLOBAL || n->right->sym->class == C_EXTERN) {
          return (cgstorglob(leftreg, n->right->sym));
        } else {
          return (cgstorlocal(leftreg, n->right->sym));
        }
      case A_DEREF: // ex: *a = 12
        return (cgstorderef(leftreg, rightreg, n->right->type));
      default: fatald("Can't A_ASSIGN in genAST(), op", n->op);
    }
  case A_RETURN:
    cgreturn(leftreg, CurFunctionSym);
    return (NOREG);
  case A_WIDEN:
    return (cgwiden(leftreg, n->left->type, n->type));
  case A_SCALE:
    // Small optimisation: use shift if the
    // scale value is a known power of two
    switch (n->size) {
      case 2: return (cgshlconst(leftreg, 1));
      case 4: return (cgshlconst(leftreg, 2));
      case 8: return (cgshlconst(leftreg, 3));
      default:
        rightreg = cgloadint(n->size);
        return (cgmul(leftreg, rightreg));
    }
  case A_ADDR:
    return (cgaddress(n->sym));
  case A_DEREF:
    // If we are an rvalue, dereference to get the value we point at
    // otherwise leave it for A_ASSIGN to store through the pointer
    if (n->rvalue) {
      return (cgderef(leftreg, n->left->type));
    } else {
      return (leftreg);
    }
  case A_STRLIT:
    return (cgloadglobstr(n->intvalue));
  case A_BITAND:
    return (cgbitand(leftreg, rightreg));
  case A_BITOR:
    return (cgbitor(leftreg, rightreg));
  case A_BITXOR:
    return (cgbitxor(leftreg, rightreg));
  case A_LSHIFT:
    return (cgshl(leftreg, rightreg));
  case A_RSHIFT:
    return (cgshr(leftreg, rightreg));
  case A_POSTINC: // doesn't have children
    if (n->sym->class == C_GLOBAL || n->sym->class == C_EXTERN) {
      return (cgloadglob(n->sym, n->op));
    } else {
      return (cgloadlocal(n->sym, n->op));
    }
  case A_POSTDEC: // doesn't have children
    if (n->sym->class == C_GLOBAL || n->sym->class == C_EXTERN) {
      return (cgloadglob(n->sym, n->op));
    } else {
      return (cgloadlocal(n->sym, n->op));
    }
  case A_PREINC: // has 1 child
    if (n->left->sym->class == C_GLOBAL || n->left->sym->class == C_EXTERN) {
      return (cgloadglob(n->left->sym, n->op));
    } else {
      return (cgloadlocal(n->left->sym, n->op));
    }
  case A_PREDEC: // has 1 child
    if (n->left->sym->class == C_GLOBAL || n->left->sym->class == C_EXTERN) {
      return (cgloadglob(n->left->sym, n->op));
    } else {
      return (cgloadlocal(n->left->sym, n->op));
    }
  case A_NEGATE:
    return (cgnegate(leftreg));
  case A_INVERT:
    return (cginvert(leftreg));
  case A_LOGNOT:
    return (cglognot(leftreg));
  case A_TOBOOL: {
    int endlabel = reg;
    // If the parent AST node is an A_IF or A_WHILE, generate
    // a compare followed by a jump. Otherwise, set the register
    // to 0 or 1 based on it's zeroeness or non-zeroeness
    return (cgboolean(leftreg, parentASTop, endlabel));
  }
  default:
    assert(n->op < A_LAST);
    fatald("Unknown AST operator in genAST", n->op);
  }
  assert(0);
}

void genpreamble() {
  if (O_parseOnly) return;
  cgpreamble();
}
void genpostamble() {
  if (O_parseOnly) return;
  cgpostamble();
}
void genfreeregs() {
  if (O_parseOnly) return;
  freeall_registers();
}
void genprintint(int reg) {
  if (O_parseOnly) return;
  cgprintint(reg);
}

void genglobsym(struct symtable *sym) {
  if (O_parseOnly) return;
  cgglobsym(sym);
}

// NOTE: works for pointers to all primitive types as well
int genprimsize(int ptype) {
  cgprimsize(ptype); // fine if parseOnly, doesn't actually generate code
}

int genglobstr(char *strvalue) {
  if (O_parseOnly) return (NOREG);
  int l = genlabel();
  cgglobstr(l, strvalue);
  return (l);
}

int genalign(int type, int offset, int direction) {
  return (cgalign(type, offset, direction));
}
