#include <assert.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Parsing of declarations
// Copyright (c) 2019 Warren Toomey, GPL3
//
struct symtable *struct_declaration(void);

// Parse the current token and
// return a primitive type enum value
int parse_base_type(int t, struct symtable **ctype) {
  int type = P_NONE;
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
      *ctype = struct_declaration();
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

int parse_full_type(int t, struct symtable **ctype) {
  int type = parse_base_type(t, ctype);
  return parse_pointer_array_type(type);
}

/**
    variable_declaration: type identifier ';'
      | type identifier '[' P_INTLIT ']' ';'
      ;
*/

// Parse the declaration of 1 scalar variable or array variable with a given size.
// The identifier has been scanned and we have the type.
// Return the pointer to variable's entry in the symbol table
struct symtable *var_declaration(int type, struct symtable *ctype, int class) {
  struct symtable *sym = NULL;

  // See if this has already been declared
  switch (class) {
    case C_GLOBAL:
      if (findglob(Text) != NULL)
        fatals("Duplicate global variable declaration", Text);
    case C_LOCAL:
    case C_PARAM:
      if (findlocl(Text) != NULL)
        fatals("Duplicate local variable declaration", Text);
    case C_MEMBER:
      if (findmember(Text) != NULL)
        fatals("Duplicate struct member name", Text);
  }

  // Text now has the identifier's name.
  // If the next token is a '['
  if (Token.token == T_LBRACKET) {
    // Skip past the '['
    scan(&Token);

    // Check we have an array size
    if (Token.token == T_INTLIT) {
      // Add this as a known array and generate its space in assembly.
      // We treat the array as a pointer to its elements' type
      switch (class) {
        case C_GLOBAL:
          sym = addglob(Text, pointer_to(type), ctype, S_ARRAY, Token.intvalue);
          break;
        case C_LOCAL:
          sym = addlocl(Text, pointer_to(type), ctype, S_ARRAY, Token.intvalue);
          break;
        case C_PARAM:
          sym = addparam(Text, pointer_to(type), ctype, S_ARRAY, Token.intvalue);
          break;
        case C_MEMBER:
          fatal("For now, declaration of non-global structs is not implemented");
          sym = addmember(Text, pointer_to(type), ctype, S_ARRAY, Token.intvalue);
          break;
        default:
          fatald("Unknown class", class);
      }
    }
    // Ensure we have a following ']'
    scan(&Token);
    match(T_RBRACKET, "]");
  } else {
    // Add this as a known scalar
    // and generate its space in assembly
    switch (class) {
      case C_GLOBAL:
        sym = addglob(Text, type, ctype, S_VARIABLE, 1);
        break;
      case C_LOCAL:
        sym = addlocl(Text, type, ctype, S_VARIABLE, 1);
        break;
      case C_PARAM:
        sym = addparam(Text, type, ctype, S_VARIABLE, 1);
        break;
      case C_MEMBER:
        sym = addmember(Text, type, ctype, S_VARIABLE, 1);
        break;
      default:
        fatald("Unknown class", class);
    }
  }
  return (sym);
}

// var_declaration_list: <null>
//           | variable_declaration
//           | variable_declaration separate_token var_declaration_list ;
//
// When called to parse function parameters, separate_token is ','.
// When called to parse members of a struct/union, separate_token is ';'.
//
// Parse a list of variables.
// Add them as symbols to one of the symbol table lists, and return the
// number of variables. If funcsym is not NULL, there is an existing function
// prototype, so compare each variable's type against this prototype.

static int var_declaration_list(struct symtable *funcsym, int class,
       int separate_token, int end_token) {
  int type;
  int paramcnt = 0;
  struct symtable *protoptr = NULL;
  struct symtable *ctype = NULL;

  if (funcsym) {
    protoptr = funcsym->member;
    assert(funcsym->stype == S_PROTO);
  }

  // Loop until the final right parentheses. Current token starts
  // the loop right after T_LPAREN.
  while (Token.token != end_token) {
    type = parse_full_type(Token.token, &ctype);
    ident();
    // We have an existing prototype.
    // Check that this type matches the prototype.
    if (protoptr) {
      if (type != protoptr->type) {
        fatalv("Parameter type doesn't match prototype for parameter %d "
               "(expected %s got %s)",
               paramcnt+1, typename(protoptr->type), typename(type));
      }
      // no need for `var_declaration()` here, we copy the prototype's parameters to locals later
      // if this is indeed a function definition. We do need to update its
      // parameter names though, in case it is a definition.
      assert(protoptr->class == C_PARAM);
      protoptr->name = strdup(Text);
      protoptr = protoptr->next;
    } else {
      if (class == C_PARAM) {
        debugnoisy("parse", "param %d for function %s has type %s",
            paramcnt+1, CurFunctionSym->name, typename(type));
      } else if (ctype) { // struct member
        debugnoisy("parse", "member %d for struct %s has type %s",
            paramcnt+1, ctype->name, typename(type));
      }
      var_declaration(type, ctype, class);
    }
    paramcnt++;

    // Must have a separate_token or ')' at this point
    if ((Token.token != separate_token) && (Token.token != end_token)) {
      fatalv("Unexpected token in var declaration list: %s", tokenname(Token.token));
    }
    if (Token.token == separate_token) {
      scan(&Token);
    }
  }

  // Check that the number of parameters in this list matches
  // any existing prototype
  if (funcsym != NULL && (paramcnt != funcsym->nelems)) {
    fatals("Parameter count mismatch for function", funcsym->name);
  }
  // Return the count of parameters
  return (paramcnt);
}

struct ASTnode *function_declaration(int type, struct symtable *ctype) {
  struct ASTnode *tree, *finalstmt;
  struct symtable *oldfuncsym, *newfuncsym = NULL;
  int endlabel, paramcnt = 0;

  // Clear out the parameter list
  Paramshead = Paramstail = NULL;

  oldfuncsym = findglob(Text);
  if (oldfuncsym && oldfuncsym->stype == S_PROTO) {
    // proto exists, check return type
    // TODO: check that struct return types match if necessary
    if (type != oldfuncsym->type) {
      fatalv("Function return value must match prototype for function %s", Text);
    }
  // function definition already exists
  } else if (oldfuncsym && oldfuncsym->stype == S_FUNCTION) {
    // TODO: allow prototypes after function definitions
    fatalv("Cannot redefine or redeclare a defined function %s", Text);
  // global symbol conflict
  } else if (oldfuncsym) {
    fatalv("Identifier %s already exists, cannot be a function", Text);
  } else {
    newfuncsym = addglob(Text, type, ctype, S_FUNCTION, 0);
    CurFunctionSym = newfuncsym;
  }
  lparen();
  paramcnt = var_declaration_list(oldfuncsym, C_PARAM, T_COMMA, T_RPAREN);
  rparen();

  if (newfuncsym) {
    newfuncsym->nelems = paramcnt;
    newfuncsym->member = Paramshead;
    oldfuncsym = newfuncsym;
  }
  // Clear out the parameter list
  Paramshead = Paramstail = NULL;

  CurFunctionSym = oldfuncsym;    // set currently parsed/generated function

  if (Token.token == T_SEMI) {
    oldfuncsym->stype = S_PROTO; // actually a prototype
    scan(&Token);
    return NULL;
  }
  oldfuncsym->stype = S_FUNCTION; // turn proto into real function

  cgresetlocals();
  tree = compound_statement();
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
  return (mkuastunary(A_FUNCTION, type, tree, oldfuncsym, 0));
}

// Either declaring a struct variable or defining a new struct.
struct symtable *struct_declaration(void) {
  struct symtable *ctype = NULL;
  struct symtable *m;
  int offset;

  // Skip the struct keyword
  match(T_STRUCT, "struct");

  // See if there is a following struct name
  if (Token.token == T_IDENT) {
    // Find any matching composite type
    ctype = findstruct(Text);
    scan(&Token);
  }

  // If the next token isn't an LBRACE, this is the usage of an existing struct type.
  // Return the pointer to the type.
  if (Token.token != T_LBRACE) {
    if (ctype == NULL) fatals("unknown struct type", Text);
    return (ctype);
  }

  // Build the struct node and skip the left brace
  ctype = addstruct(Text, P_STRUCT, NULL, 0, 0);
  scan(&Token);

  // Scan in the list of members and attach
  // to the struct type's node
  var_declaration_list(NULL, C_MEMBER, T_SEMI, T_RBRACE);
  rbrace();

  ctype->member = Membershead;
  assert(ctype->member); // TODO: error out if thera are no members for struct
  Membershead = Memberstail = NULL;

  // Set the offset of the initial member
  // and find the first free byte after it
  m = ctype->member;
  m->posn = 0;
  offset = typesize(m->type, m->ctype);

  // Set the position of each successive member in the struct
  for (m = m->next; m != NULL; m = m->next) {
    // Set the offset for this member
    m->posn = genalign(m->type, offset, 1);

    // Get the offset of the next free byte after this member
    offset += typesize(m->type, m->ctype);
  }

  // Set the overall size of the struct
  ctype->size = offset;

  ident();
  ctype->name = strdup(Text); // update the struct's name

  return ctype;
}

void global_declarations(void) {
  struct ASTnode *tree;
  int type, basetype;
  struct symtable *ctype = NULL;

  while (1) {
    basetype = parse_base_type(Token.token, &ctype);
    type = parse_pointer_array_type(basetype);
found_type:
    // struct definition
    if (type == P_STRUCT && Token.token == T_SEMI) {
      scan(&Token);
      continue;
    }
    ident();
    if (Token.token == T_LPAREN) {
      // Parse the function declaration and
      // generate the assembly code for it
      tree = function_declaration(type, ctype);
      if (tree) {
        if (O_dumpAST) {
          dumpAST(tree, 0, 0);
          fprintf(stdout, "\n");
        }
        if (!O_parseOnly) {
          genAST(tree, NOREG, 0);
        }
      } else {
        // prototype, do nothing
      }
      freeloclsyms();
    } else {
      // setup the symbol table
      var_declaration(type, ctype, C_GLOBAL);
      if (Token.token == T_COMMA) {
        scan(&Token);
        type = parse_pointer_array_type(basetype);
        goto found_type;
      }
      semi();
    }

    if (Token.token == T_EOF)
      break;
  }
}
