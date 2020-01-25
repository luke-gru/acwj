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

  if (stype == S_VARIABLE || stype == S_ARRAY) {
    if (class == C_EXTERN) {
      node->size = 0;
    } else {
      ASSERT(nelems > 0);
      node->size = nelems * typesize(type, ctype);
      ASSERT(node->size > 0);
    }
  } else {
    // Could be a struct or union type, where size will get calculated elsewhere.
    // Otherwise, could be a function symbol, no size, or extern variable
    node->size = 0;
  }

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

struct symtable *addlabel(char *name) {
  int lblnum = genlabel();
  struct symtable *sym = newsym(name, 0, 0, 0, 0, 0, lblnum);
  appendsym(&Labelshead, &Labelstail, sym);
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

struct symtable *findlabel(char *s) {
  return (findsyminlist(s, Labelshead, 0));
}

struct symtable *add_or_find_label(char *s) {
  struct symtable *lbl = findlabel(s);
  if (lbl) return (lbl);
  return (addlabel(s));
}

int isglobalsym(struct symtable *sym) {
  return (sym->class == C_GLOBAL || sym->class == C_STATIC || sym->class == C_EXTERN);
}

int issizedsym(struct symtable *sym) {
  return (sym->size > 0 && sym->class != C_EXTERN);
}

// Given a pointer to a symbol that may already exist,
// return true if this symbol doesn't exist. We use
// this function to convert externs into globals
int is_new_symbol(struct symtable *sym, int class,
                  int type, struct symtable *ctype) {

  // There is no existing symbol, thus is new
  if (sym==NULL) return(1);

  // global versus extern: if they match that it's not new
  // and we can convert the class to global
  if ((sym->class== C_GLOBAL && class== C_EXTERN)
      || (sym->class== C_EXTERN && class== C_GLOBAL)) {

      // If the types don't match, there's a problem
      if (type != sym->type)
        fatals("Type mismatch between global/extern", sym->name);

      // Struct/unions, also compare the ctype
      if (type >= P_STRUCT && ctype != sym->ctype)
        fatals("Type mismatch between global/extern", sym->name);

      // If we get to here, the types match, so mark the symbol
      // as global
      sym->class= C_GLOBAL;
      // Return that symbol is not new
      return(0);
  }

  // It must be a duplicate symbol if we get here
  fatals("Duplicate global variable declaration", sym->name);
  return(-1);   // Keep -Wall happy
}

// Clear all the entries in the local symbol table, and clears goto labels for
// current function.
void freeloclsyms(void) {
  Localshead = Localstail = NULL;
  Paramshead = Paramstail = NULL;
  Labelshead = Labelstail = NULL;
  CurFunctionSym = NULL;
}

void freestaticsyms(void) {
  // g points at current node, prev at the previous one
  struct symtable *g, *prev= NULL;

  // Walk the global table looking for static entries
  for (g= Globalshead; g != NULL; g= g->next) {
    if (g->class == C_STATIC) {

      // If there's a previous node, rearrange the prev pointer
      // to skip over the current node. If not, g is the head,
      // so do the same to Globhead
      if (prev != NULL) prev->next= g->next;
      else Globalshead->next= g->next;

      // If g is the tail, point Globtail at the previous node
      // (if there is one), or Globhead
      if (g == Globalstail) {
        if (prev != NULL) Globalstail= prev;
        else Globalstail= Globalshead;
      }
    }
  }

  // Point prev at g before we move up to the next node
  prev= g;
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
  Labelshead = Labelstail = NULL;
}

#define TABS "\t\t\t\t\t"
void dump_sym_lvl(struct symtable *sym, FILE *f, int lvl) {
  struct symtable *m;
  fprintf(f, "%.*ssym %s:\n", lvl, TABS, sym->name ? sym->name : "(anon)");
  lvl++;
  if (sym->class == C_STRUCT) {
    fprintf(f, "%.*stype: STRUCT TYPE (%d)\n", lvl, TABS, sym->type);
  } else if (sym->class == C_UNION) {
    fprintf(f, "%.*stype: UNION TYPE (%d)\n", lvl, TABS, sym->type);
  } else if (sym->stype == S_FUNCTION || sym->stype == S_PROTO) {
    // function returning this type
    fprintf(f, "%.*stype: FUNCTION -> %s (%d)\n", lvl, TABS, typename(sym->type, sym->ctype), sym->type);
  } else {
    // variable
    fprintf(f, "%.*stype: %s (%d)\n", lvl, TABS, typename(sym->type, sym->ctype), sym->type);
  }
  fprintf(f, "%.*sclass: %s (%d)\n", lvl, TABS, classname(sym->class), sym->class);
  fprintf(f, "%.*sstype: %s (%d)\n", lvl, TABS, stypename(sym->stype), sym->stype);
  fprintf(f, "%.*snelems: %d\n", lvl, TABS, sym->nelems);
  fprintf(f, "%.*ssize: %d\n", lvl, TABS, sym->size);
  fprintf(f, "%.*sposn: %d\n", lvl, TABS, sym->posn);
  fprintf(f, "%.*snext? %s\n", lvl, TABS, sym->next ? "t" : "f");
  // a symbol for a composite type or function, print its members
  if (sym->member) {
    fprintf(f, "%.*smembers: (%d)\n", lvl, TABS, sym->nelems);
    for (m = sym->member; m; m = m->next) {
      dump_sym_lvl(m, f, lvl+1);
    }
  }
}

void dump_sym(struct symtable *sym, FILE *f) {
  dump_sym_lvl(sym, f, 0);
}

void dump_sym_list(struct symtable *list, FILE *f) {
  struct symtable *cur = list;
  while (cur) {
    dump_sym(cur, f);
    cur = cur->next;
  }
}

void dumptable(struct symtable *head, char *name, int indent);

// Dump a single symbol
static void dumpsym(struct symtable *sym, int indent) {
  int i;

  for (i = 0; i < indent; i++)
    printf(" ");
  switch (sym->type & (~0xf)) {
  case P_VOID:
    printf("void ");
    break;
  case P_CHAR:
    printf("char ");
    break;
  case P_INT:
    printf("int ");
    break;
  case P_LONG:
    printf("long ");
    break;
  case P_STRUCT:
    if (sym->ctype != NULL)
      printf("struct %s ", sym->ctype->name);
    else
      printf("struct %s ", sym->name);
    break;
  case P_UNION:
    if (sym->ctype != NULL)
      printf("union %s ", sym->ctype->name);
    else
      printf("union %s ", sym->name);
    break;
  default:
    printf("unknown type ");
  }

  for (i = 0; i < (sym->type & 0xf); i++)
    printf("*");
  printf("%s", sym->name);

  switch (sym->stype) {
  case S_VARIABLE:
  case 0:
    break;
  case S_FUNCTION:
  case S_PROTO:
    printf("()");
    break;
  case S_ARRAY:
    printf("[]");
    break;
  default:
    printf(" unknown stype: %d", sym->stype);
  }

  switch (sym->class) {
  case C_GLOBAL:
    printf(": global");
    break;
  case C_LOCAL:
    printf(": local");
    break;
  case C_PARAM:
    printf(": param");
    break;
  case C_EXTERN:
    printf(": extern");
    break;
  case C_STATIC:
    printf(": static");
    break;
  case C_STRUCT:
    printf(": struct");
    break;
  case C_UNION:
    printf(": union");
    break;
  case C_MEMBER:
    printf(": member");
    break;
  case C_ENUMTYPE:
    printf(": enumtype");
    break;
  case C_ENUMVAL:
    printf(": enumval");
    break;
  case C_TYPEDEF:
    printf(": typedef");
    break;
  default:
    printf(": unknown class: %d", sym->class);
  }

  switch (sym->stype) {
  case S_VARIABLE:
  case S_NONE:
    if (sym->class == C_ENUMVAL)
      printf(", value %d\n", sym->posn);
    else
      printf(", size %d\n", sym->size);
    break;
  case S_FUNCTION:
  case S_PROTO:
    printf(", %d params\n", sym->nelems);
    break;
  case S_ARRAY:
    printf(", %d elems, size %d\n", sym->nelems, sym->size);
    break;
  }

  switch (sym->type & (~0xf)) {
  case P_STRUCT:
  case P_UNION:
    dumptable(sym->member, NULL, 4);
  }

  switch (sym->stype) {
  case S_FUNCTION:
  case S_PROTO:
    dumptable(sym->member, NULL, 4);
  }
}

// Dump one symbol table
void dumptable(struct symtable *head, char *name, int indent) {
  struct symtable *sym;

  if (head != NULL && name != NULL)
    printf("%s\n--------\n", name);
  for (sym = head; sym != NULL; sym = sym->next)
    dumpsym(sym, indent);
}

void dumpsymtables(void) {
  dumptable(Globalshead, "Global", 0);
  printf("\n");
  dumptable(Enumshead, "Enums", 0);
  printf("\n");
  dumptable(Typeshead, "Typedefs", 0);
}
