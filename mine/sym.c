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
static struct symtable *findsyminlist(char *s, struct symtable *list, int class) {
  for (; list != NULL; list = list->next) {
    if (class && class != list->class) continue;
    if ((list->name != NULL) && !strcmp(s, list->name)) {
      return (list);
    }
  }
  return (NULL);
}

// Create a symbol node to be added to a symbol table list.
// Set up the node's:
// + type: char, int etc.
// + structural type: var, function, array etc.
// + size: number of elements, or endlabel: end label for a function
// + posn: Position information for local symbols
// Return a pointer to the new node
struct symtable *newsym(char *name, int type, struct symtable *ctype, int stype, int class,
			int nelems, int posn) {

  // Get a new node
  struct symtable *node = (struct symtable *) malloc(sizeof(struct symtable));
  if (node == NULL)
    fatal("Unable to malloc a symbol table node in newsym");

  // Fill in the values
  node->name = name ? strdup(name) : NULL;
  node->type = type;
  node->ctype = ctype;
  node->stype = stype;
  node->class = class;
  node->nelems = nelems;

  // For pointers and integer types, set the size
  // of the symbol. structs and union declarations
  // manually set this up themselves.
  if (ptrtype(type) || inttype(type))
    node->size = nelems * typesize(type, ctype);

  node->posn = posn;
  node->next = NULL;
  node->member = NULL;
  node->initlist = NULL;

  return (node);
}

// Add a global symbol to the symbol table.
// Return the slot number in the symbol table.
// Also, generates asm code for the symbol.
struct symtable *addglob(char *name, int ptype, struct symtable *ctype, int stype, int class, int nelems) {
  struct symtable *sym = newsym(name, ptype, ctype, stype, class, nelems, 0);
  appendsym(&Globalshead, &Globalstail, sym);
  return (sym);
}

// Add a symbol to the parameter symbol list
struct symtable *addparam(char *name, int ptype, struct symtable *ctype, int stype, int nelems) {
  struct symtable *sym = newsym(name, ptype, ctype, stype, C_PARAM, nelems, 0);
  appendsym(&Paramshead, &Paramstail, sym);
  return (sym);
}

// Add a symbol to the local symbol list
struct symtable *addlocl(char *name, int ptype, struct symtable *ctype, int stype, int nelems) {
  struct symtable *sym = newsym(name, ptype, ctype, stype, C_LOCAL, nelems, 0);
  appendsym(&Localshead, &Localstail, sym);
  return (sym);
}

// Add a symbol to the struct types list
struct symtable *addstruct(char *name, int ptype, struct symtable *ctype, int stype, int nelems) {
  struct symtable *sym = newsym(name, ptype, ctype, stype, C_STRUCT, nelems, 0);
  appendsym(&Structshead, &Structstail, sym);
  return (sym);
}

// Add a symbol to the union types list
struct symtable *addunion(char *name, int ptype, struct symtable *ctype, int stype, int nelems) {
  struct symtable *sym = newsym(name, ptype, ctype, stype, C_UNION, nelems, 0);
  appendsym(&Unionshead, &Unionstail, sym);
  return (sym);
}

// Add a struct member to the temp members list
struct symtable *addmember(char *name, int ptype, struct symtable *ctype, int stype, int nelems) {
  struct symtable *sym = newsym(name, ptype, ctype, stype, C_MEMBER, nelems, 0);
  appendsym(&Membershead, &Memberstail, sym);
  return (sym);
}

// Add an enum type or value to the enum list.
// Class is C_ENUMTYPE or C_ENUMVAL.
// Use posn to store the int value for C_ENUMVAL.
struct symtable *addenum(char *name, int class, int value) {
  struct symtable *sym = newsym(name, P_INT, NULL, 0, class, 1, value);
  appendsym(&Enumshead, &Enumstail, sym);
  return (sym);
}

// Add a typedef to the typedef list
struct symtable *addtypedef(char *name, int type, struct symtable *ctype,
			   int stype, int size) {
  struct symtable *sym = newsym(name, type, ctype, stype, C_TYPEDEF, size, 0);
  appendsym(&Typeshead, &Typestail, sym);
  return (sym);
}

// Determine if the symbol s is in the global symbol table.
// Return a pointer to the found node or NULL if not found.
struct symtable *findglob(char *s) {
  return (findsyminlist(s, Globalshead, 0));
}

// Determine if the symbol s is in the local symbol table.
// Return a pointer to the found node or NULL if not found.
struct symtable *findlocl(char *s) {
  struct symtable *node;

  // Look for a parameter if we are in a function's body
  if (CurFunctionSym) {
    node = findsyminlist(s, CurFunctionSym->member, 0);
    if (node)
      return (node);
  }
  return (findsyminlist(s, Localshead, 0));
}

// Determine if the symbol s is in the symbol table.
// Return a pointer to the found node or NULL if not found.
struct symtable *findsymbol(char *s) {
  struct symtable *node;

  // Look for a parameter if we are in a function's body
  if (CurFunctionSym) {
    node = findsyminlist(s, CurFunctionSym->member, 0);
    if (node) return (node);
  }
  // Otherwise, try the locals
  node = findsyminlist(s, Localshead, 0);
  if (node) return (node);
  // Otherwise, look for global enum values
  node = findenumval(s);
  if (node) return (node);
  // Otherwise, try the globals
  return (findsyminlist(s, Globalshead, 0));
}

// Find a struct type.
// Return a pointer to the found node or NULL if not found.
struct symtable *findstruct(char *s) {
  return (findsyminlist(s, Structshead, 0));
}

// Find a union type.
// Return a pointer to the found node or NULL if not found.
struct symtable *findunion(char *s) {
  return (findsyminlist(s, Unionshead, 0));
}

// Find a member of the currently parsed struct/union.
// Return a pointer to the found node or NULL if not found.
struct symtable *findmember(char *s) {
  return (findsyminlist(s, Membershead, 0));
}

// Find an enum type in the enum list
// Return a pointer to the found node or NULL if not found.
struct symtable *findenumtype(char *s) {
  return (findsyminlist(s, Enumshead, C_ENUMTYPE));
}

// Find an enum value in the enum list
// Return a pointer to the found node or NULL if not found.
struct symtable *findenumval(char *s) {
  return (findsyminlist(s, Enumshead, C_ENUMVAL));
}

// Find a type in the tyedef list
// Return a pointer to the found node or NULL if not found.
struct symtable *findtypedef(char *s) {
  return (findsyminlist(s, Typeshead, 0));
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
  Unionshead = Unionstail = NULL;
  Membershead = Memberstail = NULL;
  Enumshead = Enumstail = NULL;
  Typeshead = Typestail = NULL;
}
