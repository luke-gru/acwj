#include "defs.h"
#include "data.h"
#include "decl.h"

// Symbol table functions
// Copyright (c) 2019 Warren Toomey, GPL3

// Determine if the symbol s is in the global symbol table.
// Return its slot position or -1 if not found.
int findglob(char *s) {
  int i;

  for (i = 0; i < Globs; i++) {
    if (Symtable[i].class == C_PARAM) continue;
    if (*s == *Gsym[i].name && !strcmp(s, Gsym[i].name)) {
      return (i);
    }
  }
  return (-1);
}

// Determine if the symbol s is in the local symbol table.
// Return its slot position or -1 if not found.
int findlocl(char *s) {
  int i;

  for (i = Locls+1; i < NSYMBOLS; i++) {
    if (*s == *Gsym[i].name && !strcmp(s, Gsym[i].name)) {
      return (i);
    }
  }
  return (-1);
}

// Get the position of a new global symbol slot, or die
// if we've run out of positions.
static int newglob(void) {
  int p;

  if ((p = Globs++) >= Locls) {
    fatal("Too many global symbols");
  }
  return (p);
}

static int newlocl(void) {
  int p;

  if ((p = Locls--) <= Globs)
    fatal("Too many local symbols");
  return (p);
}

// Determine if the symbol s is in the symbol table.
// Return its slot position or -1 if not found.
int findsymbol(char *s) {
  int slot;

  slot = findlocl(s);
  if (slot == -1)
    slot = findglob(s);
  return (slot);
}

// Add a global symbol to the symbol table.
// Return the slot number in the symbol table
int addglob(char *name, int ptype, int stype, int size) {
  int y;

  // If this is already in the symbol table, return the existing slot
  if ((y = findglob(name)) != -1)
    return (y);

  // Otherwise get a new slot, fill it in and
  // return the slot number
  y = newglob();
  Gsym[y].name = strdup(name);
  Gsym[y].class = C_GLOBAL;
  Gsym[y].posn = 0; // ignored
  Gsym[y].type = ptype;
  Gsym[y].stype = stype;
  Gsym[y].size = size;

  genglobsym(y);

  return (y);
}

int addparam(char *name, int ptype, int stype, int size) {
  int param = newglob();
  Gsym[param].name = strdup(name);
  Gsym[param].class = C_PARAM;
  Gsym[param].type = ptype;
  Gsym[param].stype = stype;
  Gsym[param].size = size;
  Gsym[param].posn = 0;
}

// Add a global symbol to the symbol table.
// Return the slot number in the symbol table
int addlocl(char *name, int ptype, int stype, int isParam, int size) {
  int y;
  int param;

  // If this is already in the symbol table, return the existing slot
  if ((y = findlocl(name)) != -1)
    return (y);

  // Otherwise get a new slot, fill it in and
  // return the slot number
  y = newlocl();
  Gsym[y].name = strdup(name);
  Gsym[y].class = isParam ? C_PARAM : C_LOCAL;
  Gsym[y].type = ptype;
  Gsym[y].stype = stype;
  Gsym[y].size = size;
  // NOTE: `cggetlocaloffset` must be called after all other fields are set
  Gsym[y].posn = 0;

  // if it's a function parameter, add a new global symbol for it right after
  // the function itself, for type checking purposes, right after the global
  // symbol for the function.

  if (isParam) {
    param = newglob();
    Gsym[param].name = strdup(name);
    Gsym[param].class = C_PARAM;
    Gsym[param].type = ptype;
    Gsym[param].stype = stype;
    Gsym[param].size = size;
    Gsym[param].posn = 0;
  }

  return (y);
}

// Clear all the entries in the local symbol table
void freeloclsyms(void) {
  Locls = NSYMBOLS - 1;
}
