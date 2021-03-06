#include <stdarg.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Code generator for x86-64
// Copyright (c) 2019 Warren Toomey, GPL3

#define CHARSZ 1
#define INTSZ 4
#define LONGSZ 8
#define PTRSZ 8

#define ASSERT_REG(r) ASSERT(r >= 0 && r < (FIRSTPARAMREG+1))

#define NUMFREEREGS 4
#define FIRSTPARAMREG 9
#define MAXREGISTERPARAMS 6

#ifdef NDEBUG
#define
#define cgdebug(fmt, ...) (void)0
#else
#define cgdebug(fmt, ...) debugnoisy("cg", fmt, __VA_ARGS__)
#endif

static int AsmComments = 1;
static int spillreg=0;
static int lastspill=-1;

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

// Position of next local variable relative to stack base pointer.
// We store the offset as positive to make aligning the stack pointer easier
static int localOffset;
static int stackOffset;

// Flag to say which section were are outputting in to
enum { no_seg, text_seg, data_seg } currSeg = no_seg;

static int Needs_case_eval_code = 0;
static int Generated_case_eval_code = 0;
void cgcommentsource(char *func);

void cgtextseg() {
  if (currSeg != text_seg) {
    fputs("\t.text\n", Outfile);
    currSeg = text_seg;
  }
}

void cgdataseg() {
  if (currSeg != data_seg) {
    fputs("\t.data\n", Outfile);
    currSeg = data_seg;
  }
}

// Reset the position of new local variables when parsing a new function
void cgresetlocals(void) {
  localOffset = 0;
}

// Set all registers as available.
// But if reg is positive (including 0), don't free that one.
void freeall_registers(int keepreg) {
  int i;
  for (i = 0; i < NUMFREEREGS; i++) {
    if (i != keepreg) freereg[i] = 1;
  }
}

static void pushreg(int r) {
  fprintf(Outfile, "\tpushq\t%s\n", reglist[r]);
}

static void popreg(int r) {
  fprintf(Outfile, "\tpopq\t%s\n", reglist[r]);
}

// Spill all registers on the stack
void spill_all_regs(void) {
  int i;

  fprintf(Outfile, "# spilling regs\n");
  for (i = 0; i < NUMFREEREGS; i++) {
    pushreg(i);
  }
}

static void unspill_all_regs(void) {
  int i;

  fprintf(Outfile, "# unspilling regs\n");
  for (i = NUMFREEREGS - 1; i >= 0; i--) {
    popreg(i);
  }
}

void spill_all_paramregs() {
  int i;
  fprintf(Outfile, "# spilling param regs\n");
  for (i = 0; i < MAXREGISTERPARAMS; i++) {
    // push rdi, rsi, ... r9
    pushreg(FIRSTPARAMREG-i);
  }
}

void unspill_all_paramregs() {
  int i;
  int lastparamreg = NUMFREEREGS;
  fprintf(Outfile, "# unspilling param regs\n");
  for (i = lastparamreg; i <= FIRSTPARAMREG; i++) {
    // pop r9, r8... rsi, rdi
    popreg(i);
  }
}

#define SPILLING(reg) lastspill = reg
#if 0
#define UNSPILLING(reg) ASSERT(reg == lastspill); lastspill--
#endif

/*
 * Order of spills/unspills:
 * spill 0
 * spill 1
 * spill 2
 * unspill 2
 * unspill 1
 * unspill 0
 */
int UNSPILLING(int reg, int next) {
  ASSERT_REG(reg);
  if (reg != lastspill) {
    fprintf(stderr,
        "Warning: UNSPILL reg %d (%s) doesn't match SPILL reg %d (%s) for node line %d:%d\n",
        reg, reglist[reg], lastspill, reglist[lastspill], GenNode->line, GenNode->col);
    fprintf(Outfile,
        "# Warning: UNSPILL reg %d (%s) doesn't match SPILL reg %d (%s) for node line %d:%d\n",
        reg, reglist[reg], lastspill, reglist[lastspill], GenNode->line, GenNode->col);
  }
  lastspill = next;
  return (1);
}

// Used to reset codegen state between files
void cgreset(void) {
  lastspill = -1;
  spillreg = 0;
  localOffset = 0;
  stackOffset = 0;
  currSeg = no_seg;
  freeall_registers(-1);
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
int alloc_register(void) {
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
  fprintf(Outfile, "# spilling reg %d\n", reg);
  pushreg(reg);
  return (reg);
}


/**
 * FREE:
 * IN USE: 0 1 2 3
 * SPILL: 0
 * In this case, free_register(3) unspills it:
 * FREE:
 * IN USE: 0 1 2 3
 * SPILL: 0
 */
// Return a register to the list of available registers.
// Check to see if it's not already there. The newest allocated
// register MUST be freed first.
void free_register(int reg) {
  int unspill;
  int nextunspill;
  if (freereg[reg] != 0) {
    fatalv("Error trying to free register %d, it's not in use\n", reg);
  }
  if (spillreg > 0) { // unspill the latest spill
    spillreg--;
    unspill = (spillreg % NUMFREEREGS);
    nextunspill = ((spillreg - 1) % NUMFREEREGS);
    fprintf(Outfile, "# unspilling reg %d, next unspill expected: %d\n", unspill, nextunspill);
    UNSPILLING(unspill, nextunspill);
    popreg(unspill);
  } else {
    freereg[reg]= 1; // mark as free
  }
}


// Print out the assembly preamble
void cgpreamble(char *filename) {
  ASSERT(Outfile);
  freeall_registers(-1);
  fprintf(Outfile, ".file %c%s%c\n", '"', filename, '"');
}

// Print out the assembly postamble
void cgpostamble() {
  if (Needs_case_eval_code && !Generated_case_eval_code) {
    cgtextseg();
    // internal switch(expr) routine
    // params: %rdx = switch table, %rax = expr
    fputs(
    "\t.globl __internal_switch\n"
	  "__internal_switch:\n"
	  "        pushq   %rsi\n"
	  "        movq    %rdx, %rsi\n"
	  "        movq    %rax, %rbx\n"
	  "        cld\n"
	  "        lodsq\n"
	  "        movq    %rax, %rcx\n"
	  "__internal_switch_next:\n"
	  "        lodsq\n"
	  "        movq    %rax, %rdx\n"
	  "        lodsq\n"
	  "        cmpq    %rdx, %rbx\n"
	  "        jnz     __internal_switch_no\n"
	  "        popq    %rsi\n"
	  "        jmp     *%rax\n"
	  "__internal_switch_no:\n"
	  "        loop    __internal_switch_next\n"
	  "        lodsq\n"
	  "        popq    %rsi\n"
    "        jmp     *%rax\n"
    "\n", Outfile);
    Generated_case_eval_code = 1;
  }
}

void cgfuncpreamble(struct symtable *sym) {
  ASSERT(sym->stype == S_FUNCTION);
  cgcommentsource("cgfuncpreamble");
  struct symtable *parm, *locvar;
  int cnt;

  char *name = sym->name;
  int paramOffset = 16;         // Any pushed params start at this stack offset
  int paramReg = FIRSTPARAMREG; // Index to the first param register in above reg lists

  cgtextseg();
  localOffset = 0;
  cgdebug("Generating func preamble for %s", name);

  if (sym->class == C_GLOBAL) {
    fprintf(Outfile, "\t.globl\t%s\n", name);
  }

  fprintf(Outfile,
          "\t.type\t%s, @function\n"
          "%s:\n"
          "\tpushq\t%%rbp\n"
          "\tmovq\t%%rsp, %%rbp\n",
          name, name);

  // Copy any in-register parameters to the stack, up to six of them
  // The remaining parameters are already on the stack
  for (parm = sym->member, cnt = 1; parm != NULL; parm = parm->next, cnt++) {
    if (cnt > 6) {
      // FIXME: what if it's bigger than 8 bytes, like a struct?
      ASSERT(primtype(parm->type));
      parm->posn = paramOffset;
      paramOffset += 8;
      cgdebug("Generating parameter spill push for param %s, posn: %d", parm->name, parm->posn);
    } else {
      parm->posn = cggetlocaloffset(parm);
      cgdebug("Generating parameter reg load for param %s, posn: %d", parm->name, parm->posn);
      cgstorlocal(paramReg--, parm);
    }
  }

  // For the remainder, if they are a parameter then they are
  // already on the stack. If only a local, make a stack position.
  for (locvar = Localshead; locvar != NULL; locvar = locvar->next) {
    locvar->posn = cggetlocaloffset(locvar);
    cgdebug("Generating local variable offset for var %s, posn: %d", locvar->name, locvar->posn);
  }

  // Align the stack pointer to be a multiple of 16
  // less than its previous value
  stackOffset = (localOffset + 15) & ~15;
  fprintf(Outfile, "\taddq\t$%d, %%rsp\n", -stackOffset);
}

// Print out the assembly postamble
void cgfuncpostamble(struct symtable *sym) {
  cglabel(sym->endlabel);
  /*fprintf(Outfile, "\taddq\t$%d,%%rsp\n", stackOffset);*/
  fprintf(Outfile, "\tleaq\t0(%%rbp), %%rsp\n");
  fputs(
    "\tpopq	%rbp\n"
    "\tret\n",
    Outfile
  );
}

// Load an integer literal value into a register.
// Return the number of the register
int cgloadint(int value, int type) {
  cgcommentsource("cgloadint");
  int size, r;
  size = cgprimsize(type);

  r = alloc_register();

  switch (size) {
    case CHARSZ:
    case INTSZ:
    case LONGSZ:
      // Print out the code to initialise it
      fprintf(Outfile, "\tmovq\t$%d, %s\n", value, reglist[r]);
      break;
    default:
      fatalv("Unexpected type in cgloadint: %s (%d)", typename(type, NULL), type);
  }
  return (r);
}

// Add two registers together and return
// the number of the register with the result
int cgadd(int r1, int r2) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  cgcommentsource("cgadd");
  fprintf(Outfile, "\taddq\t%s, %s\n", reglist[r2], reglist[r1]);
  free_register(r2);
  return (r1);
}

// Subtract the second register from the first and
// return the number of the register with the result
int cgsub(int r1, int r2) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  cgcommentsource("cgsub");
  fprintf(Outfile, "\tsubq\t%s, %s\n", reglist[r2], reglist[r1]);
  free_register(r2);
  return(r1);
}

// Multiply two registers together and return
// the number of the register with the result
int cgmul(int r1, int r2) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  cgcommentsource("cgmul");
  fprintf(Outfile, "\timulq\t%s, %s\n", reglist[r2], reglist[r1]);
  free_register(r2);
  return(r1);
}

// Divide the first register by the second and
// return the number of the register with the result
int cgdiv(int r1, int r2) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  cgcommentsource("cgdiv");
  fprintf(Outfile, "\tmovq\t%s,%%rax\n", reglist[r1]);
  fprintf(Outfile, "\tcqo\n");
  fprintf(Outfile, "\tidivq\t%s\n", reglist[r2]);
  fprintf(Outfile, "\tmovq\t%%rax,%s\n", reglist[r1]);
  free_register(r2);
  return(r1);
}

int cgmod(int r1, int r2) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  cgcommentsource("cgmod");
  fprintf(Outfile, "\tmovq\t%s,%%rax\n", reglist[r1]);
  fprintf(Outfile, "\tcqo\n");
  fprintf(Outfile, "\tidivq\t%s\n", reglist[r2]);
  fprintf(Outfile, "\tmovq\t%%rdx,%s\n", reglist[r1]);
  free_register(r2);
  return (r1);
}

// Call printint() with the given register
void cgprintint(int r) {
  ASSERT_REG(r);
  cgcommentsource("cgprintint");
  fprintf(Outfile, "\tmovq\t%s, %%rdi\n", reglist[r]);
  fprintf(Outfile, "\tcall\tprintint\n");
  free_register(r);
}

int cgloadglob(struct symtable *sym, int op) {
  // Get a new register
  int r = alloc_register();

  int size = cgprimsize(sym->type);
  int is_ptr_type = ptrtype(sym->type);
  int elem_size = 0;
  if (is_ptr_type) {
    elem_size = typesize(value_at(sym->type), sym->ctype);
  }
  int i;

  if (sym->type == S_ARRAY) {
    ASSERT(size == PTRSZ); // must be pointer type
  }

  // Print out the code to initialise it
  switch (size) {
    case CHARSZ:
      fprintf(Outfile, "\tmovzbq\t%s(%%rip), %s\n", sym->name, reglist[r]);
      break;
    case INTSZ:
      fprintf(Outfile, "\tmovslq\t%s(%%rip), %s\n", sym->name, reglist[r]);
      break;
    case LONGSZ:
      if (sym->stype == S_ARRAY) {
        fprintf(Outfile, "\tleaq\t%s(%%rip), %s\n", sym->name, reglist[r]);
      } else {
        fprintf(Outfile, "\tmovq\t%s(%%rip), %s\n", sym->name, reglist[r]);
      }
      break;
    default:
      fatalv("Bad type in cgloadglob: %s (%d)",
          typename(sym->type, sym->ctype), sym->type);
  }
  return (r);
}

// Load a value from a local variable into a register.
// Return the number of the register. If the
// operation is pre- or post-increment/decrement,
// also perform this action.
int cgloadlocal(struct symtable *sym, int op) {
  // Get a new register
  int r;
  ASSERT(sym->class == C_LOCAL || sym->class == C_PARAM);
  cgcommentsource("cgloadlocal");

  if (!primtype(sym->type)) {
    return (cgaddress(sym));
  }

  int size = cgprimsize(sym->type);
  int is_ptr_type = ptrtype(sym->type);
  int elem_size = 0;
  int i;
  if (is_ptr_type) {
    elem_size = typesize(value_at(sym->type), sym->ctype);
  }
  r = alloc_register();

  // Print out the code to initialise it
  switch (size) {
    case CHARSZ:
      fprintf(Outfile, "\tmovzbq\t%d(%%rbp), %s\n", sym->posn,
          reglist[r]);
      break;
    case INTSZ:
      fprintf(Outfile, "\tmovslq\t%d(%%rbp), %s\n", sym->posn,
          reglist[r]);
      break;
    case LONGSZ:
      fprintf(Outfile, "\tmovq\t%d(%%rbp), %s\n", sym->posn,
          reglist[r]);
      break;
    default:
      fatalv("Bad type in cgloadlocal: %s (%d)",
          typename(sym->type, sym->ctype), sym->type);
  }
  return (r);
}

// Store a register's value into a variable
int cgstorglob(int r, struct symtable *sym) {
  ASSERT_REG(r);
  int type = sym->type;
  int size = cgprimsize(type);
  ASSERT(sym->stype != S_ARRAY);
  switch (size) {
    case CHARSZ:
      fprintf(Outfile, "\tmovb\t%s, %s(%%rip)\n", breglist[r], sym->name);
      break;
    case INTSZ:
      fprintf(Outfile, "\tmovl\t%s, %s(%%rip)\n", dreglist[r], sym->name);
      break;
    case LONGSZ:
      fprintf(Outfile, "\tmovq\t%s, %s(%%rip)\n", reglist[r], sym->name);
      break;
    default:
      fatalv("Bad type in cgstorglob: %s (%d)", typename(type, sym->ctype), type);
  }
  return (r);
}

// Store a register's value into a local variable
int cgstorlocal(int r, struct symtable *sym) {
  ASSERT_REG(r);
  cgcommentsource("cgstorlocal");
  int size = typesize(sym->type, sym->ctype);

  switch (size) {
    case CHARSZ:
      fprintf(Outfile, "\tmovb\t%s, %d(%%rbp)\n", breglist[r],
          sym->posn);
      break;
    case INTSZ:
      fprintf(Outfile, "\tmovl\t%s, %d(%%rbp)\n", dreglist[r],
          sym->posn);
      break;
    case LONGSZ:
      fprintf(Outfile, "\tmovq\t%s, %d(%%rbp)\n", reglist[r],
          sym->posn);
      break;
    default:
      fatalv("Bad type in cgstorlocal: %s (%d)", typename(sym->type, sym->ctype), sym->type);
  }
  return (r);
}

// Generate a global symbol
void cgglobsym(struct symtable *sym) {
  int initvalue;
  int size;
  int type;
  int i, j;

  if (sym->stype == S_FUNCTION || sym->class == C_EXTERN) return;

  ASSERT(sym->class == C_GLOBAL || sym->class == C_STATIC);
  cgcommentsource("cgglobsym");

  // Get the size of the variable (or its elements if an array)
  // and the type of the variable
  if (sym->stype == S_ARRAY) {
    size = typesize(value_at(sym->type), sym->ctype); // size of each element
    type = value_at(sym->type); // element type
  } else {
    size = sym->size;
    type = sym->type;
  }

  cgdebug("Generating %s symbol %s (type=%s) with size %d",
      sym->class == C_STATIC ? "static" : "global",
      sym->name, typename(sym->type, sym->ctype), sym->size);

  ASSERT(size > 0);
  ASSERT(sym->nelems > 0);

  cgdataseg();
  /*if (sym->ctype && arysz == 1) {*/
    /*fprintf(Outfile, "\t.comm %s,%d,%d\n", sym->name, typesz, typesz);*/
    /*return;*/
  /*}*/
  if (sym->class == C_GLOBAL) {
    fprintf(Outfile, "\t.globl\t%s\n", sym->name);
  }
  fprintf(Outfile, "%s:\n", sym->name);

  for (i = 0; i < sym->nelems; i++) {
    initvalue = 0;
    if (sym->initlist) {
      initvalue = sym->initlist[i];
    }
    switch (size) {
      case CHARSZ:
        fprintf(Outfile, "\t.byte\t%d\n", initvalue);
        break;
      case INTSZ:
        fprintf(Outfile, "\t.long\t%d\n", initvalue);
        break;
      case LONGSZ:
        // Generate the pointer to a string literal
        if (sym->initlist != NULL && type == pointer_to(P_CHAR) && initvalue != 0) {
          fprintf(Outfile, "\t.quad\tL%d\n", initvalue); // label to string literal
        } else {
          fprintf(Outfile, "\t.quad\t%d\n", initvalue);
        }
        break;
      default:
        for (j = 0; j < size; j++) {
          fprintf(Outfile, "\t.byte\t0\n");
        }
    }
  }
}

// List of comparison instructions,
// in AST order: A_EQ, A_NE, A_LT, A_GT, A_LE, A_GE
static char *cmplist[] =
  { "sete", "setne", "setl", "setg", "setle", "setge" };

// Compare two registers.
int cgcompare_and_set(int ASTop, int r1, int r2, int type) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  int size = cgprimsize(type);
  cgcommentsource("cgcompare_and_set");
  // Check the range of the AST operation
  if (ASTop < A_EQ || ASTop > A_GE) {
    fatal("Bad ASTop in cgcompare_and_set()");
  }

  switch (size) {
    case CHARSZ:
      fprintf(Outfile, "\tcmpb\t%s, %s\n", breglist[r2], breglist[r1]);
      break;
    case INTSZ:
      fprintf(Outfile, "\tcmpl\t%s, %s\n", dreglist[r2], dreglist[r1]);
      break;
    case LONGSZ:
      fprintf(Outfile, "\tcmpq\t%s, %s\n", reglist[r2], reglist[r1]);
      break;
    default:
      fatalv("Bad type in cgcompare_and_set: %s (%d)", typename(type, NULL), type);
  }
  fprintf(Outfile, "\t%s\t%s\n", cmplist[ASTop - A_EQ], breglist[r2]);
  fprintf(Outfile, "\tmovzbq\t%s, %s\n", breglist[r2], reglist[r2]);
  free_register(r1);
  return (r2);
}

// Generate a label
void cglabel(int l) {
  fprintf(Outfile, "L%d:\n", l);
}

// Generate a jump to a label
void cgjump(int l) {
  fprintf(Outfile, "\tjmp\tL%d\n", l);
}

// List of inverted jump instructions,
// in AST order: A_EQ, A_NE, A_LT, A_GT, A_LE, A_GE
static char *invcmplist[] = { "jne", "je", "jge", "jle", "jg", "jl" };

// Compare two registers and jump if false.
int cgcompare_and_jump(int ASTop, int r1, int r2, int label, int type) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  cgcommentsource("cgcompare_and_jump");
  int size = cgprimsize(type);

  // Check the range of the AST operation
  if (ASTop < A_EQ || ASTop > A_GE)
    fatal("Bad ASTop in cgcompare_and_set()");

  switch (size) {
    case CHARSZ:
      fprintf(Outfile, "\tcmpb\t%s, %s\n", breglist[r2], breglist[r1]);
      break;
    case INTSZ:
      fprintf(Outfile, "\tcmpl\t%s, %s\n", dreglist[r2], dreglist[r1]);
      break;
    case LONGSZ:
      fprintf(Outfile, "\tcmpq\t%s, %s\n", reglist[r2], reglist[r1]);
      break;
    default:
      fatalv("Bad type in cgcompare_and_jump: %s (%d)", typename(type, NULL), type);
  }

  fprintf(Outfile, "\t%s\tL%d\n", invcmplist[ASTop - A_EQ], label);
  free_register(r1);
  free_register(r2);
  return (NOREG);
}

// Widen the value in the register from the old
// to the new type, and return a register with
// this new value
int cgwiden(int r, int oldtype, int newtype) {
  ASSERT_REG(r);
  // Nothing to do
  return (r);
}

int cgprimsize(int ptype) {
  char *x = 0;
  ASSERT(primtype(ptype));
  if (ptrtype(ptype)) return (PTRSZ);
  switch (ptype) {
    case P_CHAR: return (CHARSZ);
    case P_INT:  return (INTSZ);
    case P_LONG: return (LONGSZ);
    default:
      fatalv("Bad type in cgprimsize: %s (%d)", typename(ptype, NULL), ptype);
  }
  return (0);                   // Keep -Wall happy
}

// NOTE: argnum 1 is first argument to function. Frees the given
// register after the value is copied to its destination.
void cgcopyarg(struct symtable *func, int r, int argnum) {
  ASSERT_REG(r);
  cgcommentsource("cgcopyarg");
  // varargs function
  if (func->size < 0 && argnum > func->nelems) {
    fprintf(Outfile, "\tpushq\t%s # varargs argument %d\n", reglist[r], argnum); // last argument must be pushed first
    free_register(r);
    return;
  }
  // If this is above the sixth argument, simply push the
  // register on the stack. We rely on being called with
  // successive arguments in the correct order for x86-64
  if (argnum > 6) {
    fprintf(Outfile, "\tpushq\t%s # spilled argument %d\n", reglist[r], argnum); // last argument must be pushed first
  } else {
    // Otherwise, copy the value into one of the six registers
    // used to hold parameter values
    fprintf(Outfile, "\tmovq\t%s, %s # standard argument %d for %s\n", reglist[r],
            reglist[FIRSTPARAMREG - argnum + 1], argnum, func->name);
  }

  free_register(r);
}

// Call a function and return the register with the result.
// The arguments must be loaded into registers with `cgcopyarg`.
int cgcall(struct symtable *sym, int numargs) {
  cgcommentsource("cgcall");
  int outr;
  int num_args_spilled = 0;
  if (sym->size < 0 && numargs > sym->nelems) { // varargs
    num_args_spilled = numargs - sym->nelems;
  } else if (numargs > 6) {
    num_args_spilled = numargs - 6;
  }
  if (num_args_spilled > 0 && num_args_spilled % 2 != 0) {
    num_args_spilled++; // %rsp requires 16-byte alignment before calls to SSE functions (like printf)
  }
  fprintf(Outfile, "\tmovq $0, %%rax # 0 out rax for vector registers (varargs)\n");
  fprintf(Outfile, "\tcall\t%s\n", sym->name);
  if (num_args_spilled > 0) {
    // restore spilled argument stack space (assume each argument is a word in size)
    fprintf(Outfile, "\taddq\t$%d, %%rsp # restore spilled argument stack space\n", 8*num_args_spilled);
  }
  unspill_all_regs();
  // Get a new register
  outr = alloc_register();
  fprintf(Outfile, "\tmovq\t%%rax, %s\n", reglist[outr]);
  return (outr);
}

// XXX: doesn't work for returning structs as values
void cgreturn(int reg, struct symtable *func) {
  cgcommentsource("cgreturn");
  if (reg == NOREG) {
    cgjump(func->endlabel);
    return;
  }
  ASSERT_REG(reg);
  // Generate code depending on the function's type
  int typesz = typesize(func->type, func->ctype);
  switch (typesz) {
    case CHARSZ:
      fprintf(Outfile, "\tmovzbl\t%s, %%eax\n", breglist[reg]);
      break;
    case INTSZ:
      fprintf(Outfile, "\tmovl\t%s, %%eax\n", dreglist[reg]);
      break;
    case LONGSZ:
      fprintf(Outfile, "\tmovq\t%s, %%rax\n", reglist[reg]);
      break;
    default:
      fatalv("Bad function return type size in cgreturn for function: %s, type size: %d",
          func->name, typesz);
  }
  cgjump(func->endlabel);
}

int cg_builtin_vararg_addr_setup(void) {
  ASSERT(CurFunctionSym);
  int required_args = CurFunctionSym->nelems;
  int spilled_args = 0;
  if (CurFunctionSym->size >= 0) { // not a vararg function
    fatalv("%s is not a vararg function, can't use va_start", CurFunctionSym->name);
  }
  if (required_args > 6) {
    spilled_args = 6 - required_args;
  }
  int base_ptr_offset = (spilled_args*8)+16; // first vararg parameter is pushed last
  int r = alloc_register();
  fprintf(Outfile, "\tleaq\t%d(%%rbp), %s # __builtin_vararg_addr_setup\n", base_ptr_offset, reglist[r]);
  return (r);
}

// Generate code to load the address of a global or local
// identifier into a variable. Return a new register
int cgaddress(struct symtable *sym) {
  int r = alloc_register();
  cgcommentsource("cgaddress");

  ASSERT(sym);

  if (sym->class == C_LOCAL || sym->class == C_PARAM) {
    fprintf(Outfile, "\tleaq\t%d(%%rbp), %s\n", sym->posn, reglist[r]);
  } else {
    fprintf(Outfile, "\tleaq\t%s(%%rip), %s\n", sym->name, reglist[r]);
  }
  return (r);
}

// Dereference a pointer to get the value it
// pointing at into the same register
int cgderef(int r, int type) {
  ASSERT_REG(r);
  cgcommentsource("cgderef");
  int newtype = value_at(type);
  int size = cgprimsize(newtype);

  switch (size) {
    case CHARSZ:
      fprintf(Outfile, "\tmovzbq\t(%s), %s\n", reglist[r], reglist[r]);
      if (AsmComments) {
        fprintf(Outfile, " # %s = *%s", reglist[r], reglist[r]);
      }
      fprintf(Outfile, "\n");
      break;
    case 2:
      fprintf(Outfile, "\tmovslq\t(%s), %s\n", reglist[r], reglist[r]);
      if (AsmComments) {
        fprintf(Outfile, " # %s = *%s", reglist[r], reglist[r]);
      }
      fprintf(Outfile, "\n");
      break;
    case INTSZ:
      fprintf(Outfile, "\tmovl\t(%s), %s\n", reglist[r], dreglist[r]);
      if (AsmComments) {
        fprintf(Outfile, " # %s = *%s", dreglist[r], dreglist[r]);
      }
      fprintf(Outfile, "\n");
      break;
    case LONGSZ:
      fprintf(Outfile, "\tmovq\t(%s), %s", reglist[r], reglist[r]);
      if (AsmComments) {
        fprintf(Outfile, " # %s = *%s", reglist[r], reglist[r]);
      }
      fprintf(Outfile, "\n");
      break;
    default:
      fatalv("Bad type in cgderef: %s (%d)", typename(type, NULL), type);
  }
  return (r);
}

// Store through a dereferenced pointer
int cgstorderef(int r1, int r2, int type) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  cgcommentsource("cgstorderef");
  int size = cgprimsize(type);
  switch (size) {
    case CHARSZ:
      fprintf(Outfile, "\tmovb\t%s, (%s)\n", breglist[r1], reglist[r2]);
      break;
    case INTSZ:
      fprintf(Outfile, "\tmovl\t%s, (%s)\n", dreglist[r1], reglist[r2]);
      break;
    case PTRSZ:
      fprintf(Outfile, "\tmovq\t%s, (%s)\n", reglist[r1], reglist[r2]);
      break;
    default:
      fatalv("Can't cgstoderef on type: %s (%d)", typename(type, NULL), type);
  }
  return (r1);
}

// Shift a register left by a constant
int cgshlconst(int r, int val) {
  ASSERT_REG(r);
  cgcommentsource("cgshlconst");
  fprintf(Outfile, "\tsalq\t$%d, %s\n", val, reglist[r]);
  return(r);
}

// Generate a global string and its start label
void cgglobstr(int label, char *strval) {
  char *cptr;
  cglabel(label);
  for (cptr= strval; *cptr; cptr++) {
    fprintf(Outfile, "\t.byte\t%d\n", *cptr);
  }
  fprintf(Outfile, "\t.byte\t0\n");
}

// Given the label number of a global string,
// load its address into a new register
int cgloadglobstr(int label) {
  // Get a new register
  int r = alloc_register();
  cgcommentsource("cgloadglobstr");
  fprintf(Outfile, "\tleaq\tL%d(%%rip), %s", label, reglist[r]);
  if (AsmComments) {
    fprintf(Outfile, " # %s = (char*)&L%d", reglist[r], label);
  }
  fprintf(Outfile, "\n");
  return (r);
}

// Load a boolean value (only 0 or 1)
// into the given register
void cgloadboolean(int r, int val) {
  ASSERT_REG(r);
  ASSERT(val == 0 || val == 1);
  fprintf(Outfile, "\tmovq\t$%d, %s\n", val, reglist[r]);
}

int cgbitand(int r1, int r2) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  cgcommentsource("cgbitand");
  fprintf(Outfile, "\tandq\t%s, %s\n", reglist[r1], reglist[r2]);
  free_register(r1);
  return (r2);
}

int cgbitor(int r1, int r2) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  cgcommentsource("cgbitor");
  fprintf(Outfile, "\torq\t%s, %s\n", reglist[r1], reglist[r2]);
  free_register(r1);
  return (r2);
}

int cgbitxor(int r1, int r2) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  cgcommentsource("cgbitxor");
  fprintf(Outfile, "\txorq\t%s, %s\n", reglist[r1], reglist[r2]);
  free_register(r1);
  return (r2);
}

// Negate a register's value
int cgnegate(int r) {
  ASSERT_REG(r);
  cgcommentsource("cgnegate");
  fprintf(Outfile, "\tnegq\t%s\n", reglist[r]);
  return (r);
}

// Invert a register's value (bitwise not, or `~`)
int cginvert(int r) {
  ASSERT_REG(r);
  cgcommentsource("cginvert");
  fprintf(Outfile, "\tnotq\t%s\n", reglist[r]);
  return (r);
}

int cgshl(int r1, int r2) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  cgcommentsource("cgshl");
  fprintf(Outfile, "\tmovb\t%s, %%cl\n", breglist[r2]);
  fprintf(Outfile, "\tshlq\t%%cl, %s\n", reglist[r1]);
  free_register(r2);
  return (r1);
}

int cgshr(int r1, int r2) {
  ASSERT_REG(r1);
  ASSERT_REG(r2);
  cgcommentsource("cgshr");
  fprintf(Outfile, "\tmovb\t%s, %%cl\n", breglist[r2]);
  fprintf(Outfile, "\tshrq\t%%cl, %s\n", reglist[r1]);
  free_register(r2);
  return (r1);
}

// Logically negate a register's value
int cglognot(int r) {
  ASSERT_REG(r);
  cgcommentsource("cglognot");
  fprintf(Outfile, "\ttest\t%s, %s\n", reglist[r], reglist[r]);
  fprintf(Outfile, "\tsete\t%s\n", breglist[r]);
  fprintf(Outfile, "\tmovzbq\t%s, %s\n", breglist[r], reglist[r]);
  return (r);
}

void cgjumpif(int r, int label) {
  ASSERT_REG(r);
  cgcommentsource("cgjumpif");
  fprintf(Outfile, "\ttest\t%s, %s\n", reglist[r], reglist[r]); // set ZF to 1 if `r` == 0
  fprintf(Outfile, "\tjne\tL%d\n", label); // Jump if ZF == 0
}

void cgjumpunless(int r, int label) {
  ASSERT_REG(r);
  cgcommentsource("cgjumpunless");
  fprintf(Outfile, "\ttest\t%s, %s\n", reglist[r], reglist[r]); // set ZF to 1 if `r` == 0
  fprintf(Outfile, "\tje\tL%d\n", label); // Jump if ZF == 1
}

// Convert an integer value to a boolean value.
// Jump to end label if `op` is an IF, WHILE or TERNARY operation and `r` is 0
int cgboolean(int r, int op, int endlabel) {
  ASSERT_REG(r);
  cgcommentsource("cgboolean");
  fprintf(Outfile, "\ttest\t%s, %s\n", reglist[r], reglist[r]); // set ZF to 1 if `r` == 0
  if (op == A_IF || op == A_WHILE || op == A_FOR || op == A_TERNARY ||
      op == A_LOGOR || op == A_LOGAND) { // NOTE: 'for' constructs are turned into A_WHILEs
    ASSERT(endlabel != NOLABEL);
    if (op == A_LOGOR) {
      fprintf(Outfile, "\tjne\tL%d\n", endlabel); // Jump if ZF == 0 (`r` is true)
    } else {
      fprintf(Outfile, "\tje\tL%d\n", endlabel); // Jump if ZF == 1 (`r` is false)
    }
  } else {
    fprintf(Outfile, "\tsetnz\t%s\n", breglist[r]);
    fprintf(Outfile, "\tmovzbq\t%s, %s\n", breglist[r], reglist[r]);
  }
  return (r);
}

// NOTE: should not be called for parameters that are more than the sixth
// parameter to a function.
int cggetlocaloffset(struct symtable *sym) {
  ASSERT(sym->class == C_LOCAL || sym->class == C_PARAM);
  int type = sym->type;
  int stype = sym->stype;
  struct symtable *ctype = sym->ctype;
  if (stype == S_ARRAY) {
    int arysize = sym->size;
    ASSERT(arysize > 0);
    // here, `type` is the pointer to the actual element type
    int fullsize = typesize(value_at(type), ctype)*arysize;
    if (fullsize > 4) {
      localOffset += fullsize;
    } else {
      localOffset += 4;
    }
    return (-localOffset);
  } else if (stype == S_VARIABLE) {
    // Decrement the offset by a minimum of 4 bytes
    // and allocate on the stack
    if (typesize(type, ctype) > 4) {
      localOffset += typesize(type, ctype);
    } else {
      localOffset += 4;
    }
    return (-localOffset);
  }
  ASSERT(0);
  return (-1);
}

// Given a scalar type, an existing memory offset
// (which hasn't been allocated to anything yet)
// and a direction (1 is up, -1 is down), calculate
// and return a suitably aligned memory offset
// for this scalar type. This could be the original
// offset, or it could be above/below the original
int cgalign(int type, int offset, int direction) {
  int alignment = cgalignment(type);
  if (alignment == 1) return (offset);

  // Here we have an int or a long. Align it on a 4-byte offset
  // I put the generic code here so it can be reused elsewhere.
  offset = (offset + direction * (alignment - 1)) & ~(alignment - 1);
  return (offset);
}

int cgalignment(int type) {
  if (ptrtype(type)) return (4);
  switch (type) {
    case P_CHAR:
      return (1);
    case P_INT:
    case P_LONG:
      return (4);
    default:
      fatalv("Bad type in cgalignment: %s (%d)", typename(type, NULL), type);
  }
  return (-1);
}

// Generate a switch jump table and the code to
// load the registers and call the __internal_switch code
/*
 * Example jump table:
    L14:                                # Switch jump table
        .quad   3                       # Three case values
        .quad   1, L10                  # case 1: jump to L10
        .quad   2, L11                  # case 2: jump to L11
        .quad   3, L12                  # case 3: jump to L12
        .quad   L13                     # default: jump to L13
 */
void cgswitch(int reg, int casecount, int internal_switch_dispatch_label,
    int *caselabels, int *casevals, int defaultlabel) {
  ASSERT_REG(reg);
  cgcommentsource("cgswitch");

  Needs_case_eval_code = 1; // output this in postamble

  int i, label;

  // Get a label for the jump table
  label = genlabel();
  cglabel(label);

  // Heuristic. If we have no cases, create one case
  // which points to the default case
  if (casecount == 0) {
    casevals[0] = 0; // unused
    caselabels[0] = defaultlabel;
    casecount = 1;
  }

  // Generate the jump table.
  fprintf(Outfile, "\t.quad\t%d\n", casecount);
  for (i = 0; i < casecount; i++) {
    fprintf(Outfile, "\t.quad\t%d, L%d\n", casevals[i], caselabels[i]);
  }
  fprintf(Outfile, "\t.quad\tL%d\n", defaultlabel);

  // Load the specific registers
  cglabel(internal_switch_dispatch_label);
  // %rax has switch value
  fprintf(Outfile, "\tmovq\t%s, %%rax\n", reglist[reg]);
  // %rdx has address of jump table
  fprintf(Outfile, "\tleaq\tL%d(%%rip), %%rdx\n", label);
  // jump to case dispatch
  fprintf(Outfile, "\tjmp\t__internal_switch\n");
}

void cgpush0() {
  fprintf(Outfile, "\tpushq $0 # padding\n");
}

void cgmove(int srcreg, int dstreg) {
  cgcommentsource("cgmove");
  ASSERT_REG(srcreg);
  ASSERT_REG(dstreg);
  fprintf(Outfile, "\tmovq %s, %s\n", reglist[srcreg], reglist[dstreg]);
}

void cggotolabel(struct symtable *sym) {
  fprintf(Outfile, "%s%d:\n", sym->name, sym->size);
}

void cggoto(struct symtable *sym) {
  cgcommentsource("cggoto");
  fprintf(Outfile, "\tjmp %s%d\n", sym->name, sym->size);
}

void cgcommentsource(char *func) {
  if (GenNode) {
    cgcomment("from %s on line %d:%d", func, GenNode->line, GenNode->col);
  } else {
    cgcomment("from %s", func);
  }
}

void cgcomment(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(Outfile, "# ");
  vfprintf(Outfile, fmt, ap);
  if (fmt[strlen(fmt) - 1] != '\n') {
    fprintf(Outfile, "\n");
  }
  va_end(ap);
}
