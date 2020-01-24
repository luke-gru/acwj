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
int genIF(struct ASTnode *n, int looptoplabel, int loopendlabel) {
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
  genfreeregs(-1);

  // Generate the true compound statement
  genAST(n->mid, NOREG, looptoplabel, loopendlabel, n->op);
  genfreeregs(-1);

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
    genfreeregs(-1);
    cglabel(Lend);
  }

  return (NOREG);
}

// Generate code for a ternary expression
int genTernary(struct ASTnode *n) {
  int Lfalse, Lend;
  int reg, expreg;
  // Generate two labels: one for the
  // false expression, and one for the
  // end of the overall expression
  Lfalse = genlabel();
  Lend = genlabel();
  genAST(n->left, Lfalse, NOLABEL, NOLABEL, n->op);
  genfreeregs(-1);

  // Get a register to hold the result of the two expressions
  reg = alloc_register();
  // Generate the true expression and the false label.
  // Move the expression result into the known register.
  expreg = genAST(n->mid, NOLABEL, NOLABEL, NOLABEL, n->op);
  cgmove(expreg, reg);
  // Don't free the register holding the result, though!
  genfreeregs(reg);
  cgjump(Lend);
  cglabel(Lfalse);
  // Generate the false expression and the end label.
  // Move the expression result into the known register.
  expreg = genAST(n->right, NOLABEL, NOLABEL, NOLABEL, n->op);
  cgmove(expreg, reg);
  // Don't free the register holding the result, though!
  genfreeregs(reg);
  cglabel(Lend);
  return (reg);
}

int genWhile(struct ASTnode *n) {
  int Lstart, Lend;

  Lstart = genlabel();
  Lend = genlabel();
  cglabel(Lstart);

  // Generate the condition code followed
  // by a jump to the end label.
  // We cheat by sending the Lend label as a register.
  genAST(n->left, Lend, Lstart, Lend, n->op);
  genfreeregs(-1);

  // compound statement
  genAST(n->right, NOLABEL, Lstart, Lend, n->op);
  genfreeregs(-1);

  cgjump(Lstart);
  cglabel(Lend);
  return (NOREG);
}

int is_builtin_function(struct ASTnode *n) {
  if (!strcmp(n->sym->name, "__builtin_vararg_addr_setup"))
    return 1;
  return 0;
}

int gen_builtin_function(struct ASTnode *n) {
  if (!strcmp(n->sym->name, "__builtin_vararg_addr_setup")) {
    return cg_builtin_vararg_addr_setup();
  }
  return (NOREG);
}

// Generate the code to copy the arguments of a
// function call to its parameters, then call the
// function itself. Return the register that holds
// the function's return value.
int gen_funcall(struct ASTnode *n) {
  struct ASTnode *gluetree = n->left;
  int reg;
  int numargs=0;
  int numspilled=0;

  if (is_builtin_function(n)) {
    return gen_builtin_function(n);
  }

  numargs = gluetree ? gluetree->size : 0;
  numspilled = num_spilled_args(n->sym, numargs);

  // stack pointer must be 16-byte aligned before calls to functions
  if (numspilled > 0 && numspilled % 2 != 0) {
    cgpush0(); // padding
  }

  // If there is a list of arguments, walk this list
  // from the last argument (right-hand child) to the
  // first
  while (gluetree) {
    // Calculate the expression's value
    reg = genAST(gluetree->right, NOREG, NOLABEL, NOLABEL, gluetree->op);
    // Copy this into the n'th function parameter: size is 1, 2, 3, ...
    cgcopyarg(n->sym, reg, gluetree->size);
    genfreeregs(-1);
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
  genfreeregs(-1);

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
    if (c->left) { // otherwise, it's a fallthru case
      genAST(c->left, NOREG, NOLABEL, Lend, 0);
    }
    genfreeregs(-1);
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

// `||` operation
int gen_logor(struct ASTnode *n) {
  int Lend;
  int reg, res;
  int r = alloc_register(); // result register

  Lend = genlabel();

  res = genAST(n->left, NOLABEL, NOLABEL, NOLABEL, n->op);
  cgboolean(res, 0, NOLABEL);
  cgmove(res, r);
  genfreeregs(r);
  // if res is non-zero, jump to end (short-circuit)
  cgjumpif(r, Lend);

  res = genAST(n->right, NOLABEL, NOLABEL, NOLABEL, n->op);
  cgboolean(res, 0, NOLABEL);
  cgmove(res, r);
  genfreeregs(r);

  cglabel(Lend);
  return (r);
}

// `&&` operation
int gen_logand(struct ASTnode *n) {
  int Lend;
  int reg, res;
  int r;

  Lend = genlabel();

  res = genAST(n->left, NOLABEL, NOLABEL, NOLABEL, n->op);
  r = alloc_register();
  cgboolean(res, 0, NOLABEL);
  cgmove(res, r);
  genfreeregs(r);
  // if res is zero, jump to end (short-circuit)
  cgjumpunless(r, Lend);

  res = genAST(n->right, NOLABEL, NOLABEL, NOLABEL, n->op);
  cgboolean(res, 0, NOLABEL);
  cgmove(res, r);
  genfreeregs(r);

  cglabel(Lend);
  return (r);
}

// Given an AST, generate assembly code recursively.
// Return the register id with the tree's final value
int genAST(struct ASTnode *n, int reg, int looptoplabel, int loopendlabel, int parentASTop) {
  int leftreg, rightreg;
  leftreg = NOREG;
  rightreg = NOREG;

  if (O_parseOnly)
    return (NOREG);

  ASSERT(n);
  GenNode = n;

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
    case A_TERNARY:
      return (genTernary(n));
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
      if (n->left) {
        leftreg = genAST(n->left, NOREG, looptoplabel, loopendlabel, n->op);
      }
      genfreeregs(-1);
      if (n->right) {
        rightreg = genAST(n->right, NOREG, looptoplabel, loopendlabel, n->op);
        genfreeregs(-1);
      }
      return (NOREG);
    case A_FUNCALL:
      return (gen_funcall(n));
    case A_LOGOR:
      return gen_logor(n);
    case A_LOGAND:
      return gen_logand(n);
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
    if (parentASTop == A_IF || parentASTop == A_WHILE || parentASTop == A_TERNARY)
      return (cgcompare_and_jump(n->op, leftreg, rightreg, reg));
    else
      return (cgcompare_and_set(n->op, leftreg, rightreg));
  case A_INTLIT:
    return (cgloadint(n->intvalue));
  case A_IDENT:
    // Load our value if we are an rvalue
    // or we are being dereferenced
    if (n->rvalue || parentASTop == A_DEREF) {
      if (isglobalsym(n->sym)) {
        return (cgloadglob(n->sym, n->op));
      } else {
        return (cgloadlocal(n->sym, n->op));
      }
    } else {
      return (NOREG); // lvalue, let the ASSIGN node do the 'store' work
    }
  case A_ASSIGN:
  case A_AS_ADD:
  case A_AS_SUBTRACT:
  case A_AS_MULTIPLY:
  case A_AS_DIVIDE:
    switch (n->op) {
      case A_AS_ADD:
        leftreg = cgadd(leftreg, rightreg);
        n->right = n->left;
        break;
      case A_AS_SUBTRACT:
        leftreg = cgsub(leftreg, rightreg);
        n->right = n->left;
        break;
      case A_AS_MULTIPLY:
        leftreg = cgmul(leftreg, rightreg);
        n->right = n->left;
        break;
      case A_AS_DIVIDE:
        leftreg = cgdiv(leftreg, rightreg);
        n->right = n->left;
        break;
    }
    switch (n->right->op) {
      case A_IDENT: // ex: a = 12
        if (isglobalsym(n->right->sym)) {
          return (cgstorglob(leftreg, n->right->sym));
        } else {
          return (cgstorlocal(leftreg, n->right->sym));
        }
      case A_DEREF: // ex: *a = 12
        return (cgstorderef(leftreg, rightreg, n->right->type));
      default:
        fatald("Can't A_ASSIGN in genAST(), op", n->op);
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
    if (n->sym) {
      return (cgaddress(n->sym));
    // FIXME: make sure n->sym always exists, change expr.c in member_access
    } else {
      ASSERT(n->left);
      ASSERT(!n->right);
      ASSERT(leftreg != NOREG);
      return leftreg;
    }
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
    if (isglobalsym(n->sym)) {
      return (cgloadglob(n->sym, n->op));
    } else {
      return (cgloadlocal(n->sym, n->op));
    }
  case A_POSTDEC: // doesn't have children
    if (isglobalsym(n->sym)) {
      return (cgloadglob(n->sym, n->op));
    } else {
      return (cgloadlocal(n->sym, n->op));
    }
  case A_PREINC: // has 1 child
    if (isglobalsym(n->left->sym)) {
      return (cgloadglob(n->left->sym, n->op));
    } else {
      return (cgloadlocal(n->left->sym, n->op));
    }
  case A_PREDEC: // has 1 child
    if (isglobalsym(n->left->sym)) {
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
    // If the parent AST node is an A_IF or A_WHILE or A_TERNARY, generate
    // a compare followed by a jump. Otherwise, set the register
    // to 0 or 1 based on it's zeroeness or non-zeroeness
    return (cgboolean(leftreg, parentASTop, endlabel));
  }
  case A_CAST:
    return (leftreg); // assignment lvalue type takes care of proper assembly operation
  case A_GOTO:
    cggoto(n->sym);
    return (NOREG);
  case A_LABEL:
    cggotolabel(n->sym);
    return (NOREG);
  case A_EMPTY:
    return (NOREG);
  default:
    ASSERT(n->op < A_LAST);
    fatald("Unknown AST operator in genAST", n->op);
  }
  ASSERT(0);
}

void genpreamble() {
  if (O_parseOnly) return;
  cgpreamble();
}
void genpostamble() {
  if (O_parseOnly) return;
  cgpostamble();
}
void genfreeregs(int keepreg) {
  if (O_parseOnly) return;
  freeall_registers(keepreg);
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
  return cgprimsize(ptype); // fine if parseOnly, doesn't actually generate code
}

int genglobstr(char *strvalue) {
  int l = genlabel();
  if (O_parseOnly) return (l);
  cgglobstr(l, strvalue);
  return (l);
}

int genalign(int type, int offset, int direction) {
  return (cgalign(type, offset, direction));
}
