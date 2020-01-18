#include <assert.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Parsing of declarations
// Copyright (c) 2019 Warren Toomey, GPL3
//
struct symtable *composite_declaration(int comptype);
void enum_declaration(void);
int typedef_declaration(struct symtable **ctype);

static int ParsingStructDefn = 0;
static int ParsingUnionDefn = 0;

static int type_of_typedef(char *name, struct symtable **ctype, int fail) {
  struct symtable *t;
  assert(Token.token == T_IDENT);

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
        *class = C_EXTERN;
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
      if (Token.token == T_SEMI) type = -1;
      return type;
    case T_UNION:
      type = P_UNION;
      *ctype = composite_declaration(P_UNION);
      if (Token.token == T_SEMI) type = -1;
      return type;
    case T_ENUM:
      type = P_INT;
      enum_declaration();
      if (Token.token == T_SEMI) type = -1;
      return type;
    case T_TYPEDEF:
      type = typedef_declaration(ctype);
      if (Token.token == T_SEMI) type = -1;
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
    assert(oldfuncsym->stype == S_PROTO);
  }

  // Loop until the final right parentheses. Current token starts
  // the loop right after T_LPAREN.
  while (Token.token != T_RPAREN) {
    type = declaration_list(&ctype, C_PARAM, T_COMMA, T_RPAREN);
    if (type == -1) {
      fatal("Bad type in parameter list");
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
      assert(protoptr->class == C_PARAM);
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
					  struct symtable *ctype, int class) {
  struct symtable *sym;
  // Skip past the '['
  match(T_LBRACKET, "[");

  // Check we have an array size
  if (Token.token == T_INTLIT) {
    // Add this as a known array
    // We treat the array as a pointer to its elements' type
    switch (class) {
      case C_EXTERN:
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
        assert(0);
    }
    scan(&Token); // integer
  }
  match(T_RBRACKET, "]");
  return (sym);
}

struct symtable *scalar_declaration(char *varname, int type,
    struct symtable *ctype, int class) {

  // Add this as a known scalar
  switch (class) {
    case C_EXTERN:
    case C_GLOBAL:
      return (addglob(varname, type, ctype, S_VARIABLE, class, 1));
    case C_LOCAL:
      return (addlocl(varname, type, ctype, S_VARIABLE, 1));
    case C_PARAM:
        debugnoisy("parse", "param %s for function %s has type %s",
            varname, CurFunctionSym->name, typename(type, ctype));
      return (addparam(varname, type, ctype, S_VARIABLE, 1));
    case C_MEMBER:
      if (ParsingStructDefn) {
        debugnoisy("parse", "member %s for struct %s has type %s",
            varname, Structstail->name, typename(type, ctype));
      } else {
        assert(ParsingUnionDefn);
        debugnoisy("parse", "member %s for union %s has type %s",
            varname, Unionstail->name, typename(type, ctype));
      }
      return (addmember(varname, type, ctype, S_VARIABLE, 1));
    default:
      assert(0);
  }
  return (NULL);		// Keep -Wall happy
}

void array_initialisation(struct symtable *sym, int type,
				 struct symtable *ctype, int class) {
  fatal("No array initialisation yet!");
}

// Either declaring a struct variable or defining a new struct.
struct symtable *composite_declaration(int comptype) {
  struct symtable *ctype = NULL;
  struct symtable *m = NULL;
  char *compname = NULL;
  int offset;
  int t;

  switch(comptype) {
    case P_STRUCT:
      // Skip the struct keyword
      match(T_STRUCT, "struct");
      break;
    case P_UNION:
      // Skip the union keyword
      match(T_UNION, "union");
      break;
    default:
      assert(0);
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
    if (ctype == NULL) {
      fatalv("unknown %s type: %s", comptype == P_STRUCT ? "struct" : "union", Text);
    }
    return (ctype);
  }

  // Build the struct node and skip the left brace
  if (comptype == P_STRUCT) {
    ParsingStructDefn = 1;
    ctype = addstruct(compname, P_STRUCT, NULL, 0, 0);
  } else {
    ParsingUnionDefn = 1;
    ctype = addunion(compname, P_UNION, NULL, 0, 0);
  }
  lbrace();

  // Scan in the list of members
  while (1) {
    // Get the next member. m is used as a dummy
    t = declaration_list(&m, C_MEMBER, T_SEMI, T_RBRACE);
    if (t== -1) {
      fatal("Bad type in member list");
    }
    if (Token.token == T_SEMI)
      scan(&Token);
    if (Token.token == T_RBRACE)
      break;
  }

  rbrace();
  if (Membershead==NULL)
    fatals("No members in struct", ctype->name);

  ctype->member = Membershead;
  assert(ctype->member); // TODO: error out if thera are no members for struct
  Membershead = Memberstail = NULL;
  ParsingUnionDefn = ParsingStructDefn = 0;

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
      m->posn = genalign(m->type, offset, 1);
      // Get the offset of the next free byte after this member
      offset += typesize(m->type, m->ctype);
    } else {
      m->posn = 0;
      int typesz = typesize(m->type, m->ctype);
      if (typesz > offset) offset = typesz;
    }
  }

  // Set the overall size of the struct/union
  ctype->size = offset;

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

  assert(Token.token == T_IDENT);
  // See if the typedef identifier already exists
  if (findtypedef(Text) != NULL)
    fatals("redefinition of typedef", Text);

  // It doesn't exist so add it to the typedef list
  addtypedef(Text, type, *ctype, 0, 0);
  ident();
  assert(Token.token == T_SEMI);
  return (type);
}

// Parse the declaration of a variable or function.
// The type and any following '*'s have been scanned, and we
// have the identifier in the Token variable.
// The class argument is the variable's class.
// Return a pointer to the symbol's entry in the symbol table
struct symtable *symbol_declaration(int type, struct symtable *ctype,
					   int class) {
  struct symtable *sym = NULL;
  char *varname;
  int stype = S_VARIABLE;
  assert(Token.token == T_IDENT);

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
      assert(0);
  }

  // Add the array or scalar variable to the symbol table
  if (Token.token == T_LBRACKET) {
    sym = array_declaration(varname, type, ctype, class);
    stype = S_ARRAY;
  } else {
    sym = scalar_declaration(varname, type, ctype, class);
  }

  // The array or scalar variable is being initialised
  if (Token.token == T_ASSIGN) {
    // Not possible for a parameter or member
    if (class == C_PARAM)
      fatals("Initialisation of a parameter not permitted", varname);
    if (class == C_MEMBER)
      fatals("Initialisation of a member not permitted", varname);
    scan(&Token);

    // Array initialisation
    if (stype == S_ARRAY)
      array_initialisation(sym, type, ctype, class);
    else {
      fatal("Scalar variable initialisation not done yet");
      // Variable initialisation
      // if (class== C_LOCAL)
      // Local variable, parse the expression
      // expr= binexpr(0);
      // else write more code!
    }
  }
  // Generate the storage for the array or scalar variable. SOON.
  // genstorage(sym, expr);
  return (sym);
}

// Parse a list of symbols where there is an initial type.
// Return the type of the symbols. et1 and et2 are end tokens.
int declaration_list(struct symtable **ctype, int class, int et1, int et2) {
  int inittype, type;
  struct symtable *sym;

  // Get the initial type. If -1, it was
  // a composite type definition, return this
  if ((inittype = parse_base_type(Token.token, ctype, &class)) == -1)
    return (-1);

  // Now parse the list of symbols
  while (1) {
    // See if this symbol is a pointer
    type = parse_pointer_array_type(inittype);

    // don't add a new parameter symbol if it's a prototype, just re-use them
    if (class == C_PARAM && CurFunctionSym && CurFunctionSym->stype == S_PROTO) {
      ident();
      return type;
    }

    // Parse this symbol
    sym = symbol_declaration(type, *ctype, class);
    assert(sym);

    // We parsed a function, there is no list so leave
    if (sym->stype == S_FUNCTION || sym->stype == S_PROTO) {
      if (class != C_GLOBAL)
        fatal("Function definition not at global level");
      return (type);
    }

    // We are at the end of the list, leave
    if (Token.token == et1 || Token.token == et2)
      return (type);

    // Otherwise, we need a comma as separator
    comma();
  }
  assert(0);
}

void global_declarations(void) {
  struct symtable *ctype = NULL;

  while (Token.token != T_EOF) {
    declaration_list(&ctype, C_GLOBAL, T_SEMI, T_EOF);
    if (Token.token == T_SEMI) {
      semi();
    }
  }
}
