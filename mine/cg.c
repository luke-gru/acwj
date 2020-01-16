#include <assert.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Code generator for x86-64
// Copyright (c) 2019 Warren Toomey, GPL3

#define CHARSZ 1
#define INTSZ 4
#define LONGSZ 8
#define PTRSZ 8

#define NUMFREEREGS 4
#define FIRSTPARAMREG 9
#define MAXREGISTERPARAMS 6

#ifdef NDEBUG
#define
#define cgdebug(fmt, ...) (void)0
#else
#define cgdebug(fmt, ...) debugnoisy("cg", fmt, __VA_ARGS__)
#endif

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

// Set all registers as available
void freeall_registers(void)
{
  freereg[0]= freereg[1]= freereg[2]= freereg[3]= 1;
}

// Allocate a free register. Return the number of
// the register. Die if no available registers.
static int alloc_register(void)
{
  for (int i=0; i<NUMFREEREGS; i++) {
    if (freereg[i]) {
      freereg[i]= 0;
      return(i);
    }
  }
  fprintf(stderr, "Out of registers!\n");
  exit(1);
}

// Return a register to the list of available registers.
// Check to see if it's not already there.
static void free_register(int reg)
{
  if (freereg[reg] != 0) {
    fprintf(stderr, "Error trying to free register %d\n", reg);
    exit(1);
  }
  freereg[reg]= 1;
}

// Print out the assembly preamble
void cgpreamble()
{
  freeall_registers();
}

// Print out the assembly postamble
void cgpostamble() {
  (void)0; // do nothing
}

void cgfuncpreamble(struct symtable *sym) {
  assert(sym->stype == S_FUNCTION);
  struct symtable *parm, *locvar;
  int cnt;

  char *name = sym->name;
  int paramOffset = 16;         // Any pushed params start at this stack offset
  int paramReg = FIRSTPARAMREG; // Index to the first param register in above reg lists

  cgtextseg();
  localOffset = 0;
  cgdebug("Generating func preamble for %s", name);

  fprintf(Outfile,
          "\t.globl\t%s\n"
          "\t.type\t%s, @function\n"
          "%s:\n"
          "\tpushq\t%%rbp\n"
          "\tmovq\t%%rsp, %%rbp\n",
          name, name, name);

  // Copy any in-register parameters to the stack, up to six of them
  // The remaining parameters are already on the stack
  for (parm = sym->member, cnt = 1; parm != NULL; parm = parm->next, cnt++) {
    if (cnt > 6) {
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
  fprintf(Outfile, "\taddq\t$%d,%%rsp\n", -stackOffset);
}

// Print out the assembly postamble
void cgfuncpostamble(struct symtable *sym) {
  cglabel(sym->endlabel);
  fprintf(Outfile, "\taddq\t$%d,%%rsp\n", stackOffset);
  fputs(
    "\tpopq	%rbp\n"
    "\tret\n",
    Outfile
  );
}

// Load an integer literal value into a register.
// Return the number of the register
int cgloadint(int value) {

  // Get a new register
  int r= alloc_register();

  // Print out the code to initialise it
  fprintf(Outfile, "\tmovq\t$%d, %s\n", value, reglist[r]);
  return(r);
}

// Add two registers together and return
// the number of the register with the result
int cgadd(int r1, int r2) {
  fprintf(Outfile, "\taddq\t%s, %s\n", reglist[r1], reglist[r2]);
  free_register(r1);
  return(r2);
}

// Subtract the second register from the first and
// return the number of the register with the result
int cgsub(int r1, int r2) {
  fprintf(Outfile, "\tsubq\t%s, %s\n", reglist[r2], reglist[r1]);
  free_register(r2);
  return(r1);
}

// Multiply two registers together and return
// the number of the register with the result
int cgmul(int r1, int r2) {
  fprintf(Outfile, "\timulq\t%s, %s\n", reglist[r1], reglist[r2]);
  free_register(r1);
  return(r2);
}

// Divide the first register by the second and
// return the number of the register with the result
int cgdiv(int r1, int r2) {
  fprintf(Outfile, "\tmovq\t%s,%%rax\n", reglist[r1]);
  fprintf(Outfile, "\tcqo\n");
  fprintf(Outfile, "\tidivq\t%s\n", reglist[r2]);
  fprintf(Outfile, "\tmovq\t%%rax,%s\n", reglist[r1]);
  free_register(r2);
  return(r1);
}

// Call printint() with the given register
void cgprintint(int r) {
  fprintf(Outfile, "\tmovq\t%s, %%rdi\n", reglist[r]);
  fprintf(Outfile, "\tcall\tprintint\n");
  free_register(r);
}

int cgloadglob(struct symtable *sym, int op) {
  // Get a new register
  int r = alloc_register();

  int size = cgprimsize(sym->type);

  // Print out the code to initialise it
  switch (size) {
    case CHARSZ:
      if (op == A_PREINC)
        fprintf(Outfile, "\tincb\t%s(\%%rip)\n", sym->name);
      if (op == A_PREDEC)
        fprintf(Outfile, "\tdecb\t%s(\%%rip)\n", sym->name);
      fprintf(Outfile, "\tmovzbq\t%s(%%rip), %s\n", sym->name, reglist[r]);
      if (op == A_POSTINC)
        fprintf(Outfile, "\tincb\t%s(\%%rip)\n", sym->name);
      if (op == A_POSTDEC)
        fprintf(Outfile, "\tdecb\t%s(\%%rip)\n", sym->name);
      break;
    case INTSZ:
      if (op == A_PREINC)
        fprintf(Outfile, "\tincl\t%s(\%%rip)\n", sym->name);
      if (op == A_PREDEC)
        fprintf(Outfile, "\tdecl\t%s(\%%rip)\n", sym->name);
      fprintf(Outfile, "\tmovslq\t%s(\%%rip), %s\n", sym->name, reglist[r]);
      if (op == A_POSTINC)
        fprintf(Outfile, "\tincl\t%s(\%%rip)\n", sym->name);
      if (op == A_POSTDEC)
        fprintf(Outfile, "\tdecl\t%s(\%%rip)\n", sym->name);
      break;
    case LONGSZ:
      if (op == A_PREINC)
        fprintf(Outfile, "\tincq\t%s(\%%rip)\n", sym->name);
      if (op == A_PREDEC)
        fprintf(Outfile, "\tdecq\t%s(\%%rip)\n", sym->name);
      fprintf(Outfile, "\tmovq\t%s(\%%rip), %s\n", sym->name, reglist[r]);
      if (op == A_POSTINC)
        fprintf(Outfile, "\tincq\t%s(\%%rip)\n", sym->name);
      if (op == A_POSTDEC)
        fprintf(Outfile, "\tdecq\t%s(\%%rip)\n", sym->name);
      break;
    default:
      fatald("Bad type in cgloadglob:", sym->type);
  }
  return (r);
}

// Load a value from a local variable into a register.
// Return the number of the register. If the
// operation is pre- or post-increment/decrement,
// also perform this action.
int cgloadlocal(struct symtable *sym, int op) {
  // Get a new register
  int r = alloc_register();

  int size = cgprimsize(sym->type);

  // Print out the code to initialise it
  switch (size) {
    case CHARSZ:
      if (op == A_PREINC)
        fprintf(Outfile, "\tincb\t%d(%%rbp)\n", sym->posn);
      if (op == A_PREDEC)
        fprintf(Outfile, "\tdecb\t%d(%%rbp)\n", sym->posn);
      fprintf(Outfile, "\tmovzbq\t%d(%%rbp), %s\n", sym->posn,
          reglist[r]);
      if (op == A_POSTINC)
        fprintf(Outfile, "\tincb\t%d(%%rbp)\n", sym->posn);
      if (op == A_POSTDEC)
        fprintf(Outfile, "\tdecb\t%d(%%rbp)\n", sym->posn);
      break;
    case INTSZ:
      if (op == A_PREINC)
        fprintf(Outfile, "\tincl\t%d(%%rbp)\n", sym->posn);
      if (op == A_PREDEC)
        fprintf(Outfile, "\tdecl\t%d(%%rbp)\n", sym->posn);
      fprintf(Outfile, "\tmovslq\t%d(%%rbp), %s\n", sym->posn,
          reglist[r]);
      if (op == A_POSTINC)
        fprintf(Outfile, "\tincl\t%d(%%rbp)\n", sym->posn);
      if (op == A_POSTDEC)
        fprintf(Outfile, "\tdecl\t%d(%%rbp)\n", sym->posn);
      break;
    case LONGSZ:
      if (op == A_PREINC)
        fprintf(Outfile, "\tincq\t%d(%%rbp)\n", sym->posn);
      if (op == A_PREDEC)
        fprintf(Outfile, "\tdecq\t%d(%%rbp)\n", sym->posn);
      fprintf(Outfile, "\tmovq\t%d(%%rbp), %s\n", sym->posn,
          reglist[r]);
      if (op == A_POSTINC)
        fprintf(Outfile, "\tincq\t%d(%%rbp)\n", sym->posn);
      if (op == A_POSTDEC)
        fprintf(Outfile, "\tdecq\t%d(%%rbp)\n", sym->posn);
      break;
    default:
      fatald("Bad type in cgloadlocal:", sym->type);
  }
  return (r);
}

// Store a register's value into a variable
int cgstorglob(int r, struct symtable *sym) {
  int type = sym->type;
  int size = cgprimsize(type);
  switch (size) {
    case CHARSZ:
      fprintf(Outfile, "\tmovb\t%s, %s(\%%rip)\n", breglist[r], sym->name);
      break;
    case INTSZ:
      fprintf(Outfile, "\tmovl\t%s, %s(\%%rip)\n", dreglist[r], sym->name);
      break;
    case LONGSZ:
      fprintf(Outfile, "\tmovq\t%s, %s(\%%rip)\n", reglist[r], sym->name);
      break;
    default:
      fatald("Bad type in cgstorglob:", type);
  }
  return (r);
}

// Store a register's value into a local variable
int cgstorlocal(int r, struct symtable *sym) {
  int size = cgprimsize(sym->type);

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
      fatald("Bad type in cgstorlocal:", sym->type);
  }
  return (r);
}

// Generate a global symbol
void cgglobsym(struct symtable *sym) {
  assert(sym->class == C_GLOBAL);
  if (sym->stype == S_FUNCTION) return;

  int type = sym->type;
  int typesz = typesize(sym->type, sym->ctype);
  int arysz = sym->size;

  cgdataseg();
  fprintf(Outfile, "\t.globl\t%s\n", sym->name);
  fprintf(Outfile, "%s:\n", sym->name);
  assert(typesz > 0);
  assert(arysz > 0);

  for (int i = 0; i < arysz; i++) {
    switch (typesz) {
      case CHARSZ:
        fprintf(Outfile, "\t.byte\t0\n");
        break;
      case INTSZ:
        fprintf(Outfile, "\t.long\t0\n");
        break;
      case LONGSZ:
        fprintf(Outfile, "\t.quad\t0\n");
        break;
      default:
        for (int j = 0; j < typesz; j++) {
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
int cgcompare_and_set(int ASTop, int r1, int r2) {
  // Check the range of the AST operation
  if (ASTop < A_EQ || ASTop > A_GE) {
    fatal("Bad ASTop in cgcompare_and_set()");
  }

  fprintf(Outfile, "\tcmpq\t%s, %s\n", reglist[r2], reglist[r1]);
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
int cgcompare_and_jump(int ASTop, int r1, int r2, int label) {

  // Check the range of the AST operation
  if (ASTop < A_EQ || ASTop > A_GE)
    fatal("Bad ASTop in cgcompare_and_set()");

  fprintf(Outfile, "\tcmpq\t%s, %s\n", reglist[r2], reglist[r1]);
  fprintf(Outfile, "\t%s\tL%d\n", invcmplist[ASTop - A_EQ], label);
  freeall_registers();
  return (NOREG);
}

// Widen the value in the register from the old
// to the new type, and return a register with
// this new value
int cgwiden(int r, int oldtype, int newtype) {
  // Nothing to do
  return (r);
}

int cgprimsize(int ptype) {
  char *x = 0;
  if (ptrtype(ptype)) return (PTRSZ);
  switch (ptype) {
    case P_CHAR: return (CHARSZ);
    case P_INT:  return (INTSZ);
    case P_LONG: return (LONGSZ);
    default:
      fatald("Bad type in cgprimsize:", ptype);
  }
  return (0);                   // Keep -Wall happy
}

// argnum 1 is first argument to function
void cgcopyarg(int r, int argnum) {
  // If this is above the sixth argument, simply push the
  // register on the stack. We rely on being called with
  // successive arguments in the correct order for x86-64
  if (argnum > 6) {
    fprintf(Outfile, "\tpushq\t%s\n", reglist[r]); // last argument must be pushed first
  } else {
    // Otherwise, copy the value into one of the six registers
    // used to hold parameter values
    fprintf(Outfile, "\tmovq\t%s, %s\n", reglist[r],
            reglist[FIRSTPARAMREG - argnum + 1]);
  }

  free_register(r);
}

// Call a function and return the register with the result.
// The arguments must be loaded into registers with `cgloadarg`.
int cgcall(struct symtable *sym, int numargs) {
  // Get a new register
  int outr = alloc_register();
  fprintf(Outfile, "\tcall\t%s\n", sym->name);
  if (numargs > 6) {
    // restore spilled argument stack space
    fprintf(Outfile, "\taddq\t$%d, %%rsp\n", 8*(numargs-6));
  }
  fprintf(Outfile, "\tmovq\t%%rax, %s\n", reglist[outr]);
  return (outr);
}

void cgreturn(int reg, struct symtable *sym) {
  // Generate code depending on the function's type
  switch (sym->type) {
    case P_CHAR:
      fprintf(Outfile, "\tmovzbl\t%s, %%eax\n", breglist[reg]);
      break;
    case P_INT:
      fprintf(Outfile, "\tmovl\t%s, %%eax\n", dreglist[reg]);
      break;
    case P_LONG:
      fprintf(Outfile, "\tmovq\t%s, %%rax\n", reglist[reg]);
      break;
    default:
      fatald("Bad function type in cgreturn:", sym->type);
  }
  cgjump(sym->endlabel);
}

// Generate code to load the address of a global
// identifier into a variable. Return a new register
int cgaddress(struct symtable *sym) {
  int r = alloc_register();

  assert(sym->class != C_PARAM);

  if (sym->class == C_LOCAL) {
    fprintf(Outfile, "\tleaq\t%d(%%rbp), %s\n", sym->posn, reglist[r]);
  } else {
    fprintf(Outfile, "\tleaq\t%s(%%rip), %s\n", sym->name, reglist[r]);
  }
  return (r);
}

// Dereference a pointer to get the value it
// pointing at into the same register
int cgderef(int r, int type) {
  int newtype = value_at(type);
  int size = cgprimsize(newtype);

  switch (size) {
    case 1:
      fprintf(Outfile, "\tmovzbq\t(%s), %s\n", reglist[r], reglist[r]);
      break;
    case 2:
      fprintf(Outfile, "\tmovslq\t(%s), %s\n", reglist[r], reglist[r]);
      break;
    case 4:
    case 8:
      fprintf(Outfile, "\tmovq\t(%s), %s\n", reglist[r], reglist[r]);
      break;
    default:
      fatald("Bad type in cgderef", type);
  }
  return (r);
}

// Store through a dereferenced pointer
int cgstorderef(int r1, int r2, int type) {
  switch (type) {
    case P_CHAR:
      fprintf(Outfile, "\tmovb\t%s, (%s)\n", breglist[r1], reglist[r2]);
      break;
    case P_INT:
      fprintf(Outfile, "\tmovq\t%s, (%s)\n", reglist[r1], reglist[r2]);
      break;
    case P_LONG:
      fprintf(Outfile, "\tmovq\t%s, (%s)\n", reglist[r1], reglist[r2]);
      break;
    default:
      fatald("Can't cgstoderef on type:", type);
  }
  return (r1);
}

// Shift a register left by a constant
int cgshlconst(int r, int val) {
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
  fprintf(Outfile, "\tleaq\tL%d(\%%rip), %s\n", label, reglist[r]);
  return (r);
}

int cgbitand(int r1, int r2) {
  fprintf(Outfile, "\tandq\t%s, %s\n", reglist[r1], reglist[r2]);
  free_register(r1);
  return (r2);
}

int cgbitor(int r1, int r2) {
  fprintf(Outfile, "\torq\t%s, %s\n", reglist[r1], reglist[r2]);
  free_register(r1);
  return (r2);
}

int cgbitxor(int r1, int r2) {
  fprintf(Outfile, "\txorq\t%s, %s\n", reglist[r1], reglist[r2]);
  free_register(r1);
  return (r2);
}

// Negate a register's value
int cgnegate(int r) {
  fprintf(Outfile, "\tnegq\t%s\n", reglist[r]);
  return (r);
}

// Invert a register's value (bitwise not, or `~`)
int cginvert(int r) {
  fprintf(Outfile, "\tnotq\t%s\n", reglist[r]);
  return (r);
}

int cgshl(int r1, int r2) {
  fprintf(Outfile, "\tmovb\t%s, %%cl\n", breglist[r2]);
  fprintf(Outfile, "\tshlq\t%%cl, %s\n", reglist[r1]);
  free_register(r2);
  return (r1);
}

int cgshr(int r1, int r2) {
  fprintf(Outfile, "\tmovb\t%s, %%cl\n", breglist[r2]);
  fprintf(Outfile, "\tshrq\t%%cl, %s\n", reglist[r1]);
  free_register(r2);
  return (r1);
}

// Logically negate a register's value
int cglognot(int r) {
  fprintf(Outfile, "\ttest\t%s, %s\n", reglist[r], reglist[r]);
  fprintf(Outfile, "\tsete\t%s\n", breglist[r]);
  fprintf(Outfile, "\tmovzbq\t%s, %s\n", breglist[r], reglist[r]);
  return (r);
}

// Convert an integer value to a boolean value. Jump if
// it's an IF or WHILE operation
int cgboolean(int r, int op, int label) {
  fprintf(Outfile, "\ttest\t%s, %s\n", reglist[r], reglist[r]);
  if (op == A_IF || op == A_WHILE)
    fprintf(Outfile, "\tje\tL%d\n", label);
  else {
    fprintf(Outfile, "\tsetnz\t%s\n", breglist[r]);
    fprintf(Outfile, "\tmovzbq\t%s, %s\n", breglist[r], reglist[r]);
  }
  return (r);
}

// NOTE: should not be called for parameters that are more than the sixth
// parameter to a function.
int cggetlocaloffset(struct symtable *sym) {
  assert(sym->class == C_LOCAL || sym->class == C_PARAM);
  int type = sym->type;
  int stype = sym->stype;
  if (stype == S_ARRAY) {
    int arysize = sym->size;
    assert(arysize > 0);
    // here, `type` is the pointer to the actual element type
    int fullsize = cgprimsize(value_at(type))*arysize;
    localOffset += (fullsize > 4) ? fullsize : 4;
    return (-localOffset);
  } else if (stype == S_VARIABLE) {
    // Decrement the offset by a minimum of 4 bytes
    // and allocate on the stack
    localOffset += (cgprimsize(type) > 4) ? cgprimsize(type) : 4;
    return (-localOffset);
  } else {
    assert(0);
  }
}

// Given a scalar type, an existing memory offset
// (which hasn't been allocated to anything yet)
// and a direction (1 is up, -1 is down), calculate
// and return a suitably aligned memory offset
// for this scalar type. This could be the original
// offset, or it could be above/below the original
int cgalign(int type, int offset, int direction) {
  int alignment;

  // We don't need to do this on x86-64, but let's
  // align chars on any offset and align ints/pointers
  // on a 4-byte alignment
  switch(type) {
    case P_CHAR: return (offset);
    case P_INT:
    case P_LONG: break;
    default:
      fatald("Bad type in calc_aligned_offset:", type);
  }

  // Here we have an int or a long. Align it on a 4-byte offset
  // I put the generic code here so it can be reused elsewhere.
  alignment= 4;
  offset = (offset + direction * (alignment-1)) & ~(alignment-1);
  return (offset);
}
