#include <assert.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Code generator for x86-64
// Copyright (c) 2019 Warren Toomey, GPL3

#define CHARSZ 1
#define INTSZ 4
#define LONGSZ 8
#define PTRSIZE 8

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

// Array of type sizes in P_XXX order.
// 0 means no size.
static int psize[] = {
  // P_NONE, P_VOID, P_CHAR, P_INT, P_LONG
     0,       0,     CHARSZ, INTSZ, LONGSZ,
  // P_VOIDPTR, P_CHARPTR, P_INTPTR, P_LONGPTR
     PTRSIZE,   PTRSIZE,   PTRSIZE,  PTRSIZE
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

void cgfuncpreamble(int sym_id) {
  assert(Gsym[sym_id].stype == S_FUNCTION);

  char *name = Gsym[sym_id].name;
  int i;
  int paramOffset = 16;         // Any pushed params start at this stack offset
  int paramReg = FIRSTPARAMREG; // Index to the first param register in above reg lists

  cgtextseg();
  cgdebug("Generating func preamble for %s", name);

  fprintf(Outfile,
          "\t.globl\t%s\n"
          "\t.type\t%s, @function\n"
          "%s:\n"
          "\tpushq\t%%rbp\n"
          "\tmovq\t%%rsp, %%rbp\n",
          name, name, name);

  // Copy any in-register parameters to the stack
  // Stop after no more than six parameter registers
  for (i = NSYMBOLS - 1; i > Locls; i--) {
    if (Symtable[i].class != C_PARAM) // parameters appear at end of locals list
      break;
    if (i < NSYMBOLS - 6)
      break;
    Symtable[i].posn = cggetlocaloffset(i, 1);
    cgdebug("Generating parameter reg load for param %s, posn: %d", Symtable[i].name, Symtable[i].posn);
    cgstorlocal(paramReg--, i);
  }

  // For the remainder, if they are a parameter then they are
  // already on the stack. If only a local, make a stack position.
  for (; i > Locls; i--) {
    if (Symtable[i].class == C_PARAM) {
      Symtable[i].posn = paramOffset;
      paramOffset += 8;
      cgdebug("Generating parameter spill push for param %s, posn: %d", Symtable[i].name, Symtable[i].posn);
    } else {
      Symtable[i].posn = cggetlocaloffset(i, 0);
      cgdebug("Generating local variable offset for var %s, posn: %d", Symtable[i].name, Symtable[i].posn);
    }
  }

  // Align the stack pointer to be a multiple of 16
  // less than its previous value
  stackOffset = (localOffset + 15) & ~15;
  fprintf(Outfile, "\taddq\t$%d,%%rsp\n", -stackOffset);
}

// Print out the assembly postamble
void cgfuncpostamble(int sym_id) {
  cglabel(Gsym[sym_id].endlabel);
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

int cgloadglob(int id, int op) {
  // Get a new register
  int r = alloc_register();

  // Print out the code to initialise it
  switch (Gsym[id].type) {
    case P_CHAR:
      if (op == A_PREINC)
        fprintf(Outfile, "\tincb\t%s(\%%rip)\n", Gsym[id].name);
      if (op == A_PREDEC)
        fprintf(Outfile, "\tdecb\t%s(\%%rip)\n", Gsym[id].name);
      fprintf(Outfile, "\tmovzbq\t%s(%%rip), %s\n", Gsym[id].name, reglist[r]);
      if (op == A_POSTINC)
        fprintf(Outfile, "\tincb\t%s(\%%rip)\n", Gsym[id].name);
      if (op == A_POSTDEC)
        fprintf(Outfile, "\tdecb\t%s(\%%rip)\n", Gsym[id].name);
      break;
    case P_INT:
      if (op == A_PREINC)
        fprintf(Outfile, "\tincl\t%s(\%%rip)\n", Gsym[id].name);
      if (op == A_PREDEC)
        fprintf(Outfile, "\tdecl\t%s(\%%rip)\n", Gsym[id].name);
      fprintf(Outfile, "\tmovslq\t%s(\%%rip), %s\n", Gsym[id].name, reglist[r]);
      if (op == A_POSTINC)
        fprintf(Outfile, "\tincl\t%s(\%%rip)\n", Gsym[id].name);
      if (op == A_POSTDEC)
        fprintf(Outfile, "\tdecl\t%s(\%%rip)\n", Gsym[id].name);
      break;
    case P_LONG:
    case P_CHARPTR:
    case P_INTPTR:
    case P_LONGPTR:
      if (op == A_PREINC)
        fprintf(Outfile, "\tincq\t%s(\%%rip)\n", Gsym[id].name);
      if (op == A_PREDEC)
        fprintf(Outfile, "\tdecq\t%s(\%%rip)\n", Gsym[id].name);
      fprintf(Outfile, "\tmovq\t%s(\%%rip), %s\n", Gsym[id].name, reglist[r]);
      if (op == A_POSTINC)
        fprintf(Outfile, "\tincq\t%s(\%%rip)\n", Gsym[id].name);
      if (op == A_POSTDEC)
        fprintf(Outfile, "\tdecq\t%s(\%%rip)\n", Gsym[id].name);
      break;
    default:
      fatald("Bad type in cgloadglob:", Gsym[id].type);
  }
  return (r);
}

// Load a value from a local variable into a register.
// Return the number of the register. If the
// operation is pre- or post-increment/decrement,
// also perform this action.
int cgloadlocal(int id, int op) {
  // Get a new register
  int r = alloc_register();

  // Print out the code to initialise it
  switch (Symtable[id].type) {
    case P_CHAR:
      if (op == A_PREINC)
        fprintf(Outfile, "\tincb\t%d(%%rbp)\n", Symtable[id].posn);
      if (op == A_PREDEC)
        fprintf(Outfile, "\tdecb\t%d(%%rbp)\n", Symtable[id].posn);
      fprintf(Outfile, "\tmovzbq\t%d(%%rbp), %s\n", Symtable[id].posn,
          reglist[r]);
      if (op == A_POSTINC)
        fprintf(Outfile, "\tincb\t%d(%%rbp)\n", Symtable[id].posn);
      if (op == A_POSTDEC)
        fprintf(Outfile, "\tdecb\t%d(%%rbp)\n", Symtable[id].posn);
      break;
    case P_INT:
      if (op == A_PREINC)
        fprintf(Outfile, "\tincl\t%d(%%rbp)\n", Symtable[id].posn);
      if (op == A_PREDEC)
        fprintf(Outfile, "\tdecl\t%d(%%rbp)\n", Symtable[id].posn);
      fprintf(Outfile, "\tmovslq\t%d(%%rbp), %s\n", Symtable[id].posn,
          reglist[r]);
      if (op == A_POSTINC)
        fprintf(Outfile, "\tincl\t%d(%%rbp)\n", Symtable[id].posn);
      if (op == A_POSTDEC)
        fprintf(Outfile, "\tdecl\t%d(%%rbp)\n", Symtable[id].posn);
      break;
    case P_LONG:
    case P_CHARPTR:
    case P_INTPTR:
    case P_LONGPTR:
      if (op == A_PREINC)
        fprintf(Outfile, "\tincq\t%d(%%rbp)\n", Symtable[id].posn);
      if (op == A_PREDEC)
        fprintf(Outfile, "\tdecq\t%d(%%rbp)\n", Symtable[id].posn);
      fprintf(Outfile, "\tmovq\t%d(%%rbp), %s\n", Symtable[id].posn,
          reglist[r]);
      if (op == A_POSTINC)
        fprintf(Outfile, "\tincq\t%d(%%rbp)\n", Symtable[id].posn);
      if (op == A_POSTDEC)
        fprintf(Outfile, "\tdecq\t%d(%%rbp)\n", Symtable[id].posn);
      break;
    default:
      fatald("Bad type in cgloadlocal:", Symtable[id].type);
  }
  return (r);
}

// Store a register's value into a variable
int cgstorglob(int r, int slot) {
  int type = Gsym[slot].type;
  switch (type) {
    case P_CHAR:
      fprintf(Outfile, "\tmovb\t%s, %s(\%%rip)\n", breglist[r], Gsym[slot].name);
      break;
    case P_INT:
      fprintf(Outfile, "\tmovl\t%s, %s(\%%rip)\n", dreglist[r], Gsym[slot].name);
      break;
    case P_LONG:
    case P_CHARPTR:
    case P_INTPTR:
    case P_LONGPTR:
      fprintf(Outfile, "\tmovq\t%s, %s(\%%rip)\n", reglist[r], Gsym[slot].name);
      break;
    default:
      fatald("Bad type in cgstorglob:", type);
  }
  return (r);
}

// Store a register's value into a local variable
int cgstorlocal(int r, int id) {
  switch (Symtable[id].type) {
    case P_CHAR:
      fprintf(Outfile, "\tmovb\t%s, %d(%%rbp)\n", breglist[r],
          Symtable[id].posn);
      break;
    case P_INT:
      fprintf(Outfile, "\tmovl\t%s, %d(%%rbp)\n", dreglist[r],
          Symtable[id].posn);
      break;
    case P_LONG:
    case P_CHARPTR:
    case P_INTPTR:
    case P_LONGPTR:
      fprintf(Outfile, "\tmovq\t%s, %d(%%rbp)\n", reglist[r],
          Symtable[id].posn);
      break;
    default:
      fatald("Bad type in cgstorlocal:", Symtable[id].type);
  }
  return (r);
}

// Generate a global symbol
void cgglobsym(int slot) {
  assert(Symtable[slot].class == C_GLOBAL);
  if (Symtable[slot].stype == S_FUNCTION)
    return;

  int type = Symtable[slot].type;
  int typesize = cgprimsize(type);

  cgdataseg();
  fprintf(Outfile, "\t.globl\t%s\n", Gsym[slot].name);
  fprintf(Outfile, "%s:", Gsym[slot].name);
  assert(Gsym[slot].size > 0);

  for (int i =0; i < Gsym[slot].size; i++) {
    switch(typesize) {
      case 1:
        fprintf(Outfile, "\t.byte\t0\n");
        break;
      case 4:
        fprintf(Outfile, "\t.long\t0\n");
        break;
      case 8:
        fprintf(Outfile, "\t.quad\t0\n");
        break;
      default:
        fatald("Unknown typesize in cgglobsym: ", typesize);
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
  // Check the type is valid
  if (ptype < P_NONE || ptype >= P_LAST)
    fatald("Bad type in cgprimsize()", ptype);
  return (psize[ptype]);
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
int cgcall(int sym_id, int numargs) {
  // Get a new register
  int outr = alloc_register();
  fprintf(Outfile, "\tcall\t%s\n", Gsym[sym_id].name);
  if (numargs > 6) {
    // restore spilled argument stack space
    fprintf(Outfile, "\taddq\t$%d, %%rsp\n", 8*(numargs-6));
  }
  fprintf(Outfile, "\tmovq\t%%rax, %s\n", reglist[outr]);
  return (outr);
}

void cgreturn(int reg, int sym_id) {
  // Generate code depending on the function's type
  switch (Gsym[sym_id].type) {
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
      fatald("Bad function type in cgreturn:", Gsym[sym_id].type);
  }
  cgjump(Gsym[sym_id].endlabel);
}

// Generate code to load the address of a global
// identifier into a variable. Return a new register
int cgaddress(int slot) {
  int r = alloc_register();

  if (Symtable[slot].class == C_LOCAL) {
    fprintf(Outfile, "\tleaq\t%d(%%rbp), %s\n", Symtable[slot].posn, reglist[r]);
  } else {
    fprintf(Outfile, "\tleaq\t%s(%%rip), %s\n", Gsym[slot].name, reglist[r]);
  }
  return (r);
}

// Dereference a pointer to get the value it
// pointing at into the same register
int cgderef(int r, int type) {
  switch (type) {
    case P_CHARPTR:
      fprintf(Outfile, "\tmovzbq\t(%s), %s\n", reglist[r], reglist[r]);
      break;
    case P_INTPTR:
    case P_LONGPTR:
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
int cggetlocaloffset(int slot, int isparam) {
  assert(Symtable[slot].class == C_LOCAL || Symtable[slot].class == C_PARAM);
  int type = Symtable[slot].type;
  int stype = Symtable[slot].stype;
  if (stype == S_ARRAY) {
    int arysize = Symtable[slot].size;
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
