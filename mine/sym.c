#include <assert.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Symbol table functions
// Copyright (c) 2019 Warren Toomey, GPL3

// Append a node to the singly-linked list pointed to by head or tail
void appendsym(struct symtable **head, struct symtable **tail,
	       struct symtable *node) {

  // Check for valid pointers
  if (head == NULL || tail == NULL || node == NULL)
    fatal("Either head, tail or node is NULL in appendsym");

  // Append to the list
  if (*tail) {
    (*tail)->next = node;
    *tail = node;
  } else
    *head = *tail = node;
  node->next = NULL;
}

// Search for a symbol in a specific list.
// Return a pointer to the found node or NULL if not found.
static struct symtable *findsyminlist(char *s, struct symtable *list) {
  for (; list != NULL; list = list->next)
    if ((list->name != NULL) && !strcmp(s, list->name))
      return (list);
  return (NULL);
}

// Determine if the symbol s is in the global symbol table.
// Return a pointer to the found node or NULL if not found.
struct symtable *findglob(char *s) {
  return (findsyminlist(s, Globalshead));
}

// Determine if the symbol s is in the local symbol table.
// Return a pointer to the found node or NULL if not found.
struct symtable *findlocl(char *s) {
  struct symtable *node;

  // Look for a parameter if we are in a function's body
  if (CurFunctionSym) {
    node = findsyminlist(s, CurFunctionSym->member);
    if (node)
      return (node);
  }
  return (findsyminlist(s, Localshead));
}

// Determine if the symbol s is in the symbol table.
// Return a pointer to the found node or NULL if not found.
struct symtable *findsymbol(char *s) {
  struct symtable *node;

  // Look for a parameter if we are in a function's body
  if (CurFunctionSym) {
    node = findsyminlist(s, CurFunctionSym->member);
    if (node)
      return (node);
  }
  // Otherwise, try the local and global symbol lists
  node = findsyminlist(s, Localshead);
  if (node)
    return (node);
  return (findsyminlist(s, Globalshead));
}

// Create a symbol node to be added to a symbol table list.
// Set up the node's:
// + type: char, int etc.
// + structural type: var, function, array etc.
// + size: number of elements, or endlabel: end label for a function
// + posn: Position information for local symbols
// Return a pointer to the new node
struct symtable *newsym(char *name, int type, struct symtable *ctype, int stype, int class,
			int size, int posn) {

  // Get a new node
  struct symtable *node = (struct symtable *) malloc(sizeof(struct symtable));
  if (node == NULL)
    fatal("Unable to malloc a symbol table node in newsym");

  // Fill in the values
  node->name = strdup(name);
  node->type = type;
  node->ctype = ctype;
  node->stype = stype;
  node->class = class;
  node->size = size;
  node->posn = posn;
  node->next = NULL;
  node->member = NULL;

  // Generate any global space
  if (class == C_GLOBAL)
    genglobsym(node);
  return (node);
}

// Add a global symbol to the symbol table.
// Return the slot number in the symbol table.
// Also, generates asm code for the symbol.
struct symtable *addglob(char *name, int ptype, struct symtable *ctype, int stype, int size) {
  struct symtable *sym = newsym(name, ptype, ctype, stype, C_GLOBAL, size, 0);
  appendsym(&Globalshead, &Globalstail, sym);
  return (sym);
}

// Add a symbol to the parameter symbol list
struct symtable *addparam(char *name, int ptype, struct symtable *ctype, int stype, int size) {
  struct symtable *sym = newsym(name, ptype, ctype, stype, C_PARAM, size, 0);
  appendsym(&Paramshead, &Paramstail, sym);
  return (sym);
}

// Add a symbol to the local symbol list
struct symtable *addlocl(char *name, int ptype, struct symtable *ctype, int stype, int size) {
  struct symtable *sym = newsym(name, ptype, ctype, stype, C_LOCAL, size, 0);
  appendsym(&Localshead, &Localstail, sym);
  return (sym);
}

// Add a symbol to the struct types list
struct symtable *addstruct(char *name, int ptype, struct symtable *ctype, int stype, int size) {
  struct symtable *sym = newsym(name, ptype, ctype, stype, C_STRUCT, size, 0);
  appendsym(&Structshead, &Structstail, sym);
  return (sym);
}

// Add a struct member to the temp members list
struct symtable *addmember(char *name, int ptype, struct symtable *ctype, int stype, int size) {
  struct symtable *sym = newsym(name, ptype, ctype, stype, C_MEMBER, size, 0);
  appendsym(&Membershead, &Memberstail, sym);
  return (sym);
}

// Find a struct type.
// Return a pointer to the found node or NULL if not found.
struct symtable *findstruct(char *s) {
  return (findsyminlist(s, Structshead));
}
// Find a member of the currently parsed struct.
// Return a pointer to the found node or NULL if not found.
struct symtable *findmember(char *s) {
  return (findsyminlist(s, Membershead));
}

// Clear all the entries in the local symbol table
void freeloclsyms(void) {
  Localshead = Localstail = NULL;
  Paramshead = Paramstail = NULL;
  CurFunctionSym = NULL;
}

// Reset the contents of the symbol table
void clear_symtable(void) {
  Globalshead = Globalstail = NULL;
  Localshead = Localstail = NULL;
  Paramshead = Paramstail = NULL;
  Structshead = Structstail = NULL;
  Membershead = Memberstail = NULL;
}
