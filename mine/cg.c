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

// List of available registers
// and their names
static int freereg[4];
static char *reglist[4]  = { "%r8",  "%r9",  "%r10",  "%r11" };
static char *breglist[4] = { "%r8b", "%r9b", "%r10b", "%r11b" };
static char *dreglist[4] = { "%r8d", "%r9d", "%r10d", "%r11d" };

// Array of type sizes in P_XXX order.
// 0 means no size.
static int psize[] = {
  // P_NONE, P_VOID, P_CHAR, P_INT, P_LONG
     0,       0,     CHARSZ, INTSZ, LONGSZ,
  // P_VOIDPTR, P_CHARPTR, P_INTPTR, P_LONGPTR
     PTRSIZE,   PTRSIZE,   PTRSIZE,  PTRSIZE
};

// Set all registers as available
void freeall_registers(void)
{
  freereg[0]= freereg[1]= freereg[2]= freereg[3]= 1;
}

// Allocate a free register. Return the number of
// the register. Die if no available registers.
static int alloc_register(void)
{
  for (int i=0; i<4; i++) {
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
  fputs(
	"\t.text\n"
	".LC0:\n"
	"\t.string\t\"%d\\n\"\n"
	"printint:\n"
	"\tpushq\t%rbp\n"
	"\tmovq\t%rsp, %rbp\n"
	"\tsubq\t$16, %rsp\n"
	"\tmovl\t%edi, -4(%rbp)\n" // the int param is saved to stack
	"\tmovl\t-4(%rbp), %eax\n"
	"\tmovl\t%eax, %esi\n" // 2nd param to printf()
	"\tleaq	.LC0(%rip), %rdi\n" // 1st param to printf()
	"\tmovl	$0, %eax\n" // printf() is variadic, must specify # of vector registers (here, 0)
	"\tcall	printf@PLT\n" // plt = procedure link table
	"\tnop\n"
	"\tleave\n"
	"\tret\n",
	/*"\n"*/
	/*"\t.globl\tmain\n"*/
	/*"\t.type\tmain, @function\n"*/
	/*"main:\n"*/
	/*"\tpushq\t%rbp\n"*/
	/*"\tmovq	%rsp, %rbp\n",*/
  Outfile);
}

void cgfuncpreamble(int sym_id) {
  char *name = Gsym[sym_id].name;
  fprintf(Outfile,
          "\t.text\n"
          "\t.globl\t%s\n"
          "\t.type\t%s, @function\n"
          "%s:\n"
          "\tpushq\t%%rbp\n"
          "\tmovq\t%%rsp, %%rbp\n",
          name, name, name);
}

// Print out the assembly postamble
void cgfuncpostamble(int sym_id) {
  cglabel(Gsym[sym_id].endlabel);
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

// Load a value from a variable into a register.
// Return the number of the register
int cgloadglob(int slot) {
  // Get a new register
  int r = alloc_register();
  int type = Gsym[slot].type;
  switch (type) {
    case P_CHAR:
      fprintf(Outfile, "\tmovzbq\t%s(\%%rip), %s\n", Gsym[slot].name, reglist[r]);
      break;
    case P_INT:
      fprintf(Outfile, "\tmovzbl\t%s(\%%rip), %s\n", Gsym[slot].name, dreglist[r]);
      break;
    case P_LONG:
    case P_CHARPTR:
    case P_INTPTR:
    case P_LONGPTR:
      fprintf(Outfile, "\tmovq\t%s(\%%rip), %s\n", Gsym[slot].name, reglist[r]);
      break;
    default:
      fatald("Bad type in cgloadglob:", type);
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

// Generate a global symbol
void cgglobsym(int slot) {
  int type = Gsym[slot].type;
  int typesize = cgprimsize(Gsym[slot].type);
  fprintf(Outfile, "\t.comm\t%s,%d,%d\n", Gsym[slot].name, typesize, typesize);
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

int cgcall(int r, int sym_id) {
  // Get a new register
  int outr = alloc_register();
  fprintf(Outfile, "\tmovq\t%s, %%rdi\n", reglist[r]);
  fprintf(Outfile, "\tcall\t%s\n", Gsym[sym_id].name);
  fprintf(Outfile, "\tmovq\t%%rax, %s\n", reglist[outr]);
  free_register(r);
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

  fprintf(Outfile, "\tleaq\t%s(%%rip), %s\n", Gsym[slot].name, reglist[r]);
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

