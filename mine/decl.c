#include "defs.h"
#include "data.h"
#include "decl.h"

// Parsing of declarations
// Copyright (c) 2019 Warren Toomey, GPL3
//
struct symtable *composite_declaration(int comptype);
void enum_declaration(void);
int typedef_declaration(struct symtable **ctype);
int parse_literal(int type);

#define MAX_COMPOSITE_NESTING 100
static int ParseCompositeLevel = 0;
static int ParseCompositeLevels[MAX_COMPOSITE_NESTING];

#define SET_PARSING_COMPOSITE(ptype) ParseCompositeLevels[ParseCompositeLevel++] = ptype
#define UNSET_PARSING_COMPOSITE() ASSERT(ParseCompositeLevel > 0), ParseCompositeLevel--

#define IS_PARSING_STRUCT (ParseCompositeLevel > 0 && ParseCompositeLevels[ParseCompositeLevel-1] == P_STRUCT)
#define IS_PARSING_UNION  (ParseCompositeLevel > 0 && ParseCompositeLevels[ParseCompositeLevel-1] == P_UNION)

#define TYPE_DEFN (-1)
#define VOID_PARAMS (-2)

static int type_of_typedef(char *name, struct symtable **ctype, int fail) {
  struct symtable *t;
  ASSERT(Token.token == T_IDENT);

  // Look up the typedef in the list
  t = findtypedef(name);
  if (t == NULL) {
    if (fail) {
      fatals("unknown type", name);
    } else {
      return -1;
    }
  }
  *ctype = t->ctype;
  return (t->type);
}

// Given a typedef name, return the type it represents. Errors out if none
// found.
int type_of_typedef_fail(char *name, struct symtable **ctype) {
  return type_of_typedef(name, ctype, 1);
}

// Given a typedef name, return the type it represents
// Returns -1 if no type found
int type_of_typedef_nofail(char *name, struct symtable **ctype) {
  return type_of_typedef(name, ctype, 0);
}

// Parse the current token and
// return a primitive type enum value
int parse_base_type(int t, struct symtable **ctype, int *class) {
  int type = P_NONE;
  int extern_static = 1;

  while (extern_static) {
    switch (Token.token) {
      case T_EXTERN:
        if (*class == C_STATIC)
          fatal("Illegal to have extern and static at the same time");
        *class = C_EXTERN;
        scan(&Token);
        break;
      case T_STATIC:
        if (*class == C_LOCAL)
          fatal("Compiler doesn't support static local declarations");
        if (*class == C_EXTERN)
          fatal("Illegal to have extern and static at the same time");
        *class = C_STATIC;
        scan(&Token);
        break;
      default:
        extern_static = 0;
        break;
    }
  }

  t = Token.token;
  switch (t) {
    case T_CHAR:
      type = P_CHAR;
      break;
    case T_INT:
      type = P_INT;
      break;
    case T_LONG:
      type = P_LONG;
      break;
    case T_VOID:
      type = P_VOID;
      break;
    case T_STRUCT:
      type = P_STRUCT;
      *ctype = composite_declaration(P_STRUCT);
      if (Token.token == T_SEMI) type = TYPE_DEFN;
      return type;
    case T_UNION:
      type = P_UNION;
      *ctype = composite_declaration(P_UNION);
      if (Token.token == T_SEMI) type = TYPE_DEFN;
      return type;
    case T_ENUM:
      type = P_INT;
      enum_declaration();
      if (Token.token == T_SEMI) type = TYPE_DEFN;
      return type;
    case T_TYPEDEF:
      type = typedef_declaration(ctype);
      if (Token.token == T_SEMI) type = TYPE_DEFN;
      return type;
    case T_IDENT: // might be typedef
      type = type_of_typedef_fail(Text, ctype);
      ident();
      return type;
    default:
      fatals("Expected a type, found token", tokenname(t));
  }
  scan(&Token);
  return type;
}

// Parse 0 or more '*'s after base type
// TODO: get array types working
int parse_pointer_array_type(int basetype) {
  int type = basetype;
  while (Token.token == T_STAR) {
    scan(&Token);
    type = pointer_to(type);
  }
  return (type);
}

int parse_full_type(int t, struct symtable **ctype, int *class) {
  int type = parse_base_type(t, ctype, class);
  return parse_pointer_array_type(type);
}

static int param_declaration_list(struct symtable *oldfuncsym,
    struct symtable *newfuncsym) {
  int type;
  int paramcnt = 0;
  struct symtable *ctype = NULL;
  struct symtable *protoptr = NULL;

  if (oldfuncsym) {
    protoptr = oldfuncsym->member;
    ASSERT(oldfuncsym->stype == S_PROTO);
  }

  // Loop until the final right parentheses. Current token starts
  // the loop right after T_LPAREN.
  while (Token.token != T_RPAREN) {
    type = declaration_list(&ctype, C_PARAM, T_COMMA, T_RPAREN, NULL);
    if (type == TYPE_DEFN) {
      fatal("Bad type in parameter list");
    }
    if (type == VOID_PARAMS) {
      break;
    }
    // We have an existing prototype.
    // Check that this type matches the prototype.
    if (protoptr) {
      if (type != protoptr->type) { // TODO: check struct types
        fatalv("Parameter type doesn't match prototype for parameter %d "
               "(expected %s got %s)",
               paramcnt+1, typename(protoptr->type, protoptr->ctype), typename(type, ctype));
      }
      // no need for `var_declaration()` here, we copy the prototype's parameters to locals later
      // if this is indeed a function definition. We do need to update its
      // parameter names though, in case it is a definition.
      ASSERT(protoptr->class == C_PARAM);
      protoptr->name = strdup(Text); // update name of variable
      protoptr = protoptr->next;
    }
    paramcnt++;

    if (Token.token == T_RPAREN) break;
    comma();
  }

  // Check that the number of parameters in this list matches
  // any existing prototype
  if (oldfuncsym != NULL && (paramcnt != oldfuncsym->nelems)) {
    fatals("Parameter count mismatch for function", oldfuncsym->name);
  }
  // Return the count of parameters
  return (paramcnt);
}

struct symtable *function_declaration(char *name, int type, struct symtable *ctype, int class) {
  struct ASTnode *tree, *finalstmt;
  struct symtable *oldfuncsym, *newfuncsym = NULL;
  int endlabel, paramcnt = 0;

  // Clear out the parameter list
  Paramshead = Paramstail = NULL;

  oldfuncsym = findglob(name);
  if (oldfuncsym && oldfuncsym->stype == S_PROTO) {
    // proto exists, check return type
    // TODO: check that struct return types match if necessary
    if (type != oldfuncsym->type) {
      fatalv("Function return value must match prototype for function %s", name);
    }
    CurFunctionSym = oldfuncsym;
  // function definition already exists
  } else if (oldfuncsym && oldfuncsym->stype == S_FUNCTION) {
    // TODO: allow prototypes after function definitions
    fatalv("Cannot redefine or redeclare a defined function %s", name);
  // global symbol conflict
  } else if (oldfuncsym) {
    fatalv("Identifier %s already exists, cannot be a function", name);
  } else {
    newfuncsym = addglob(name, type, ctype, S_FUNCTION, class, 0);
    CurFunctionSym = newfuncsym;
  }
  lparen();
  paramcnt = param_declaration_list(oldfuncsym, newfuncsym);
  rparen();

  if (newfuncsym) {
    newfuncsym->nelems = paramcnt;
    newfuncsym->member = Paramshead;
    oldfuncsym = newfuncsym;
  }
  // Clear out the parameter list
  Paramshead = Paramstail = NULL;

  if (Token.token == T_SEMI) {
    oldfuncsym->stype = S_PROTO; // actually a prototype
    scan(&Token);
    return oldfuncsym;
  }
  oldfuncsym->stype = S_FUNCTION; // turn proto into real function

  cgresetlocals();
  lbrace();
  tree = compound_statement(0);
  rbrace();
  // If the function type isn't P_VOID, check that
  // the last AST operation in the compound statement
  // was a return statement
  if (type != P_VOID) {
    // Error if no statements in the function
    if (tree == NULL) {
      fatal("No statements in function with non-void type");
    }

    finalstmt = (tree->op == A_GLUE) ? tree->right : tree;
    if (finalstmt == NULL || finalstmt->op != A_RETURN) {
      fatal("No return for function with non-void type");
    }
  }
  tree = mkuastunary(A_FUNCTION, type, tree, oldfuncsym, 0);

  tree = optimise(tree);

  // Generate the assembly code for it
  if (O_dumpAST) {
    dumpAST(tree, NOLABEL, 0);
    fprintf(stdout, "\n\n");
  }
  genAST(tree, NOREG, NOLABEL, NOLABEL, 0);
  freeloclsyms();
  return (oldfuncsym);
}

struct symtable *array_declaration(char *varname, int type,
					  struct symtable *ctype, int class, struct ASTnode **stmt) {
  struct symtable *sym;
  int nelems= -1;       // Assume the number of elements won't be given
  int *initlist = NULL;
  int i, j;
  int maxelems;
  int gotelems = 0;
  // Skip past the '['
  match(T_LBRACKET, "[");

  if (Token.token != T_RBRACKET) {
    gotelems = 1;
    nelems = parse_literal(P_INT);
  }
  if (gotelems && nelems <= 0) {
    fatald("Array size is illegal", nelems);
  }

  // Add this as a known array
  // We treat the array as a pointer to its elements' type
  switch (class) {
    case C_EXTERN:
    case C_STATIC:
    case C_GLOBAL:
      sym = addglob(varname, pointer_to(type), ctype, S_ARRAY, class,
          Token.intvalue);
      break;
    case C_LOCAL:
      sym = addlocl(varname, pointer_to(type), ctype, S_ARRAY,
          Token.intvalue);
      break;
    case C_PARAM:
    case C_MEMBER:
      fatal("For now, declaration of member or parameter arrays is not implemented");
    default:
      ASSERT(0);
  }
  match(T_RBRACKET, "]");

  if (Token.token == T_ASSIGN) {
    if (class != C_GLOBAL && class != C_STATIC)
      fatals("Variable can not be initialised", varname);
    scan(&Token);

#define TABLE_INCREMENT (10)
    lbrace();
    if (nelems != -1) {
      maxelems = nelems;
    } else {
      maxelems = TABLE_INCREMENT;
    }
    initlist = (int*)malloc(maxelems * sizeof(int));
    while (1) {
      // Check we can add the next value, then parse and add it
      if (nelems != -1 && i == maxelems)
        fatal("Too many values given in initialisation list");
      initlist[i++] = parse_literal(type);

      // Increase the list size if the original size was
      // not set and we have hit the end of the current list
      if (nelems == -1 && i == maxelems) {
        maxelems += TABLE_INCREMENT;
        initlist= (int*)realloc(initlist, maxelems * sizeof(int));
      }

      // Leave when we hit the right curly bracket
      if (Token.token == T_RBRACE) {
        rbrace();
        break;
      }
      comma();
    }
    // Zero any unused elements in the initlist.
    // Attach the list to the symbol table entry
    for (j=i; j < sym->nelems; j++) {
      initlist[j] = 0;
    }
    if (i > nelems) nelems = i;
    sym->initlist = initlist;
  }
#undef TABLE_INCREMENT

  if (nelems > 0) { // extern declarations have nelems = 0
    sym->nelems = nelems;
    sym->size = sym->nelems * typesize(type, ctype);
  } else {
    ASSERT(sym->class == C_EXTERN);
  }

  if (isglobalsym(sym)) {
    genglobsym(sym);
  }
  return (sym);
}

// Parse a type which appears inside a cast. '(' has already been parsed,
// this parses up to the ')'
int parse_cast_type(struct symtable **ctype) {
  int type, class;
  type = parse_full_type(Token.token, ctype, &class);

  // Do some error checking. I'm sure more can be done
  if (type == P_STRUCT || type == P_UNION || type == P_VOID) {
    fatal("Cannot cast to a struct, union or void type");
  }
  return (type);
}

// Given a type, parse an expression of literals and ensure
// that the type of this expression matches the given type.
// Parse any type cast that precedes the expression.
// If an integer literal, return this value.
// If a string literal, return the label number of the string.
int parse_literal(int type) {
  struct ASTnode *tree;

  tree = optimise(binexpr(0));

  // If there's a cast, get the child and
  // mark it as having the type from the cast
  if (tree->op == A_CAST) {
    tree->left->type= tree->type;
    tree= tree->left;
  }

  // The tree must now have an integer or string literal
  if (tree->op != A_INTLIT && tree->op != A_STRLIT)
    fatal("Cannot initialise globals with a general expression");

  // If the type is char * and
  if (type == pointer_to(P_CHAR)) {
    // We have a string literal, return the label number
    if (tree->op == A_STRLIT)
      return (tree->intvalue);
    // We have a zero int literal, so that's a NULL
    if (tree->op == A_INTLIT && tree->intvalue==0)
      return (0);
  }

  // We only get here with an integer literal. The input type
  // is an integer type and is wide enough to hold the literal value
  if (inttype(type) && typesize(type, NULL) >= typesize(tree->type, NULL))
    return (tree->intvalue);

  fatal("Type mismatch: literal vs. variable");
  return(0);    // Keep -Wall happy
}

struct symtable *scalar_declaration(char *varname, int type,
    struct symtable *ctype, int class, struct ASTnode **assign_expr) {

  struct symtable *sym, *cast_ctype = NULL;
  struct ASTnode *varnode, *exprnode;
  int casttype;

  // Add this as a known scalar
  switch (class) {
    case C_EXTERN:
    case C_GLOBAL:
    case C_STATIC:
      sym = addglob(varname, type, ctype, S_VARIABLE, class, 1);
      break;
    case C_LOCAL:
      sym = addlocl(varname, type, ctype, S_VARIABLE, 1);
      break;
    case C_PARAM:
        debugnoisy("parse", "param %s for function %s has type %s",
            varname, CurFunctionSym->name, typename(type, ctype));
      sym = addparam(varname, type, ctype, S_VARIABLE, 1);
      break;
    case C_MEMBER:
      if (IS_PARSING_STRUCT) {
        debugnoisy("parse", "member %s for struct %s has type %s",
            varname, Structstail->name, typename(type, ctype));
      } else {
        debugnoisy("parse", "member %s for union %s has type %s",
            varname, Unionstail->name, typename(type, ctype));
        ASSERT(IS_PARSING_UNION);
      }
      sym = addmember(varname, type, ctype, S_VARIABLE, 1);
      break;
    default:
      ASSERT(0);
  }

  if (class != C_EXTERN) {
    ASSERT(sym->size > 0);
  }

  if (Token.token == T_ASSIGN) {
    // Only possible for a global or local
    if (class != C_GLOBAL && class != C_LOCAL && class != C_STATIC)
      fatals("Variable can not be initialised", varname);
    scan(&Token); // '='

    // Globals
    if (class == C_GLOBAL || class == C_STATIC) {
      // Create one initial value for the variable and
      // parse this value
      sym->initlist= (int *)malloc(sizeof(int));
      sym->initlist[0]= parse_literal(type);
    }                           // No else code yet, soon
    if (class == C_LOCAL) {
      // Make an A_IDENT AST node with the variable
      varnode = mkastleaf(A_IDENT, sym->type, sym, 0);

      // Get the expression for the assignment, make into a rvalue
      exprnode = binexpr(0);
      exprnode->rvalue = 1;

      // Ensure the expression's type matches the variable
      exprnode = modify_type(exprnode, varnode->type, 0);
      if (exprnode == NULL)
        fatal("Incompatible expression in assignment");

      // Make an assignment AST tree
      ASSERT(assign_expr);
      *assign_expr = mkastnode(A_ASSIGN, exprnode->type, exprnode,
                                        NULL, varnode, NULL, 0);
    }
  }

  if (isglobalsym(sym)) {
    genglobsym(sym);
  }

  return (sym);
}

// Either declaring a struct variable or defining a new struct.
struct symtable *composite_declaration(int comptype) {
  struct symtable *ctype = NULL;
  struct symtable *m = NULL;
  char *compname = NULL;
  int offset;
  int t;
  int membcount = 0;
  int fwddecl = 0;
  const char *comptypename;
  struct symtable *oldmembtail;
  struct symtable *oldmembhead;

  switch (comptype) {
    case P_STRUCT:
      // Skip the struct keyword
      match(T_STRUCT, "struct");
      comptypename = "struct";
      break;
    case P_UNION:
      // Skip the union keyword
      match(T_UNION, "union");
      comptypename = "union";
      break;
    default:
      ASSERT(0);
  }

  // See if there is a following struct/union name
  if (Token.token == T_IDENT) {
    // Find any matching composite type
    ctype = comptype == P_STRUCT ? findstruct(Text) : findunion(Text);
    compname = strdup(Text);
    scan(&Token); // the identifier
  }

  // If the next token isn't an LBRACE, this is the usage of an existing struct/union type.
  // Return the pointer to the type.
  if (Token.token != T_LBRACE) {
    if (Token.token == T_SEMI) {
      fwddecl = 1;
    }
    if (!fwddecl && ctype == NULL) {
      fatalv("unknown %s type: %s", comptypename, Text);
    } else if (!fwddecl) {
      return (ctype); // return existing struct for declaration like `struct foo f;` (size=0 for now)
    }
  }

  if (ctype == NULL) { // new composite type
    debugnoisy("parse", "%s new %s: %s",
        fwddecl ? "declaring (fwd)" : "defining",
        comptypename, compname);
    // Build the struct node and skip the left brace
    if (comptype == P_STRUCT) {
      ctype = addstruct(compname, P_STRUCT, NULL, 0, 0);
    } else {
      ASSERT(comptype == P_UNION);
      ctype = addunion(compname, P_UNION, NULL, 0, 0);
    }
  }

  if (fwddecl) {
    ASSERT(ctype);
    return ctype;
  }
  lbrace();

  SET_PARSING_COMPOSITE(comptype);

  oldmembhead = Membershead;
  oldmembtail = Memberstail;
  Membershead = NULL;
  Memberstail = NULL;
  // Scan in the list of members
  while (1) {
    m = NULL;
    // Get the next member. m is used as a dummy
    t = declaration_list(&m, C_MEMBER, T_SEMI, T_RBRACE, NULL);
    if (t == -1) {
      fatalv("Bad type in member list for %s %s", comptypename, compname);
    }
    membcount++;
    if (Token.token == T_SEMI)
      scan(&Token);
    if (Token.token == T_RBRACE)
      break;
  }

  rbrace();
  if (Membershead==NULL)
    fatalv("No members in %s", comptypename, ctype->name);

  if (!ctype->member) {
    ctype->member = Membershead;
  }
  ctype->nelems = membcount;
  ASSERT(ctype->member); // TODO: error out if there are no members for struct
  Membershead = oldmembhead;
  Memberstail = oldmembtail;
  UNSET_PARSING_COMPOSITE();

  // Set the offset of the initial member
  // and find the first free byte after it
  m = ctype->member;
  m->posn = 0;
  if (comptype == P_STRUCT) {
    offset = typesize(m->type, m->ctype);
  } else {
    offset = typesize(m->type, NULL);
  }

  // Set the position of each successive member in the struct
  for (m = m->next; m != NULL; m = m->next) {
    // Set the offset for this member
    if (comptype == P_STRUCT) {
      if (m->ctype && !ptrtype(m->type)) { // member is itself a composite (nested composite)
        m->posn = genalign(P_LONG, offset, 1); // NOTE: P_LONG here just means align it to nearest 4-byte boundary
        offset += m->ctype->size;
      } else {
        m->posn = genalign(m->type, offset, 1);
        // Get the offset of the next free byte after this member
        offset += typesize(m->type, m->ctype);
      }
    } else {
      m->posn = 0;
      int typesz = typesize(m->type, m->ctype);
      if (typesz > offset) offset = typesz;
    }
  }

  // Set the overall size of the struct/union
  ctype->size = offset;

  /*dump_sym(ctype, stdout);*/

  return (ctype);
}

void enum_declaration(void) {
  struct symtable *etype = NULL, *enumval = NULL;
  char *name = NULL;
  int intval = 0;

  match(T_ENUM, "enum");

  // If there's a following enum type name, get a
  // pointer to any existing enum type node.
  if (Token.token == T_IDENT) {
    etype = findenumtype(Text);
    name = strdup(Text);        // As it gets tromped soon
    scan(&Token); // ident
  }

  // If the next token isn't a LBRACE, check
  // that we have an enum type name, then return
  if (Token.token != T_LBRACE) {
    if (etype == NULL) {
      fatals("undeclared enum type:", name);
    }
    return;
  }

  // we're defining a new enum
  lbrace();
  if (etype != NULL) {
    fatals("enum type redeclared:", etype->name);
  }
  // Build an enum type node for this identifier
  etype = addenum(name, C_ENUMTYPE, 0);
  // Loop to get all the enum values
  while (1) {
    // Ensure we have an identifier
    // Copy it in case there's an int literal coming up
    ident();
    name = strdup(Text);

    // Ensure this enum value hasn't been declared before
    enumval = findenumval(name);
    if (enumval != NULL)
      fatals("enum value redeclared:", name);

    // If the next token is an '=', skip it and
    // get the following int literal
    if (Token.token == T_ASSIGN) {
      scan(&Token);
      if (Token.token != T_INTLIT)
        fatal("Expected int literal after '='");
      intval = Token.intvalue;
      scan(&Token); // the int literal
    }
    // Build an enum value node for this identifier.
    // Increment the value for the next enum identifier.
    etype = addenum(name, C_ENUMVAL, intval++);
    // Bail out on a right curly bracket, else get a comma
    if (Token.token == T_RBRACE)
      break;
    comma();
  }
  rbrace();
}

/*
  typedef_declaration: 'typedef' existing_type identifier
                       ;
*/
// Parse a typedef declaration and return the type
// and ctype that it represents
int typedef_declaration(struct symtable **ctype) {
  int type;
  int class; // unused

  match(T_TYPEDEF, "typedef");

  // Get the actual type following the keyword
  type = parse_full_type(Token.token, ctype, &class);

  ASSERT(Token.token == T_IDENT);
  // See if the typedef identifier already exists
  if (findtypedef(Text) != NULL)
    fatals("redefinition of typedef", Text);

  // It doesn't exist so add it to the typedef list
  addtypedef(Text, type, *ctype, 0, 0);
  ident();
  ASSERT(Token.token == T_SEMI);
  return (type);
}

// Parse the declaration of a variable or function.
// The type and any following '*'s have been scanned, and we
// have the identifier in the Token variable.
// The class argument is the variable's class.
// Return a pointer to the symbol's entry in the symbol table
struct symtable *symbol_declaration(int type, struct symtable *ctype,
					   int class, struct ASTnode **assign_expr) {
  struct symtable *sym = NULL;
  char *varname;
  int stype = S_VARIABLE;
  ASSERT(Token.token == T_IDENT);

  varname = strdup(Text);
  // Assume it will be a scalar variable.
  // Ensure that we have an identifier.
  // We copied it above so we can scan more tokens in, e.g.
  // an assignment expression for a local variable.
  ident();

  // Deal with function declarations
  if (Token.token == T_LPAREN) {
    return (function_declaration(varname, type, ctype, class));
  }
  // See if this array or scalar variable has already been declared
  switch (class) {
    case C_EXTERN:
    case C_STATIC:
    case C_GLOBAL:
      if (findglob(varname) != NULL)
        fatals("Duplicate global variable declaration", varname);
      break;
    case C_LOCAL:
    case C_PARAM:
      if (findlocl(varname) != NULL)
        fatals("Duplicate local variable declaration", varname);
      break;
    case C_MEMBER:
      if (findmember(varname) != NULL)
        fatals("Duplicate struct/union member declaration", varname);
      break;
    default:
      ASSERT(0);
  }

  // Add the array or scalar variable to the symbol table
  if (Token.token == T_LBRACKET) {
    sym = array_declaration(varname, type, ctype, class, assign_expr);
    stype = S_ARRAY;
  } else {
    sym = scalar_declaration(varname, type, ctype, class, assign_expr);
  }

  return (sym);
}

// Parse a list of symbols where there is an initial type.
// Return the full type of the last symbol. et1 and et2 are end tokens.
int declaration_list(struct symtable **ctype, int class, int et1, int et2,
    struct ASTnode **assign_exprs) {
  int inittype, type;
  struct symtable *sym;
  struct ASTnode *assign_expr = NULL;
  if (assign_exprs)
    *assign_exprs = NULL;

  // Get the initial type. If TYPE_DEFN, it was
  // a type definition, return this.
  if ((inittype = parse_base_type(Token.token, ctype, &class)) == TYPE_DEFN)
    if (inittype == TYPE_DEFN && IS_PARSING_STRUCT && (*ctype) && (*ctype)->type == P_UNION) {
      debugnoisy("parse", "found anon union in struct %s, adding it as member", Structstail->name);
      ASSERT((*ctype)->member);
      addmember(NULL, P_UNION, *ctype, S_VARIABLE, 1);
      return P_UNION; // anonymous union inside struct
    } else {
      return (TYPE_DEFN);
    }

  // Now parse the list of symbols
  while (1) {
    // See if this symbol is a pointer
    type = parse_pointer_array_type(inittype);

    // don't add a new parameter symbol if it's a prototype, just re-use them
    if (class == C_PARAM && CurFunctionSym && CurFunctionSym->stype == S_PROTO) {
      ident();
      return type;
    }

    if (class == C_PARAM && type == P_VOID && Token.token == T_RPAREN) {
      return VOID_PARAMS;
    }

    // Parse this symbol
    sym = symbol_declaration(type, *ctype, class, &assign_expr);
    ASSERT(sym);

    // Build assignment tree statements, store them to inpointer `assign_exprs`.
    // This is for local var declarations with initialization expressions
    if (assign_exprs && assign_expr) {
      if (*assign_exprs == NULL) {
        *assign_exprs = assign_expr;
      } else {
        *assign_exprs = mkastnode(A_GLUE, P_NONE, *assign_exprs, NULL, assign_expr, NULL, 0);
      }
    }

    // We parsed a function, there is no list so leave
    if (sym->stype == S_FUNCTION || sym->stype == S_PROTO) {
      if (!isglobalsym(sym))
        fatal("Function definition not at global level");
      return (type);
    }

    // We are at the end of the list, leave
    if (Token.token == et1 || Token.token == et2)
      return (type);

    // Otherwise, we need a comma as separator
    comma();
  }
  ASSERT(0);
}

void global_declarations(void) {
  struct symtable *ctype = NULL;

  while (Token.token != T_EOF) {
    declaration_list(&ctype, C_GLOBAL, T_SEMI, T_EOF, NULL);
    if (Token.token == T_SEMI) {
      semi();
    }
  }
}
