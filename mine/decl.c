#include <assert.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Parsing of declarations
// Copyright (c) 2019 Warren Toomey, GPL3

// Parse the current token and
// return a primitive type enum value
int parse_base_type(int t) {
  int type = P_NONE;
  if (t == T_CHAR) type = P_CHAR;
  if (t == T_INT)  type = P_INT;
  if (t == T_LONG) type = P_LONG;
  if (t == T_VOID) type = P_VOID;
  if (type == P_NONE)
    fatals("Expected a type, found token", tokenname(t));
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

int parse_full_type(int t) {
  int type = parse_base_type(t);
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
struct symtable *var_declaration(int type, int class) {
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
          sym = addglob(Text, pointer_to(type), S_ARRAY, Token.intvalue);
          break;
        case C_LOCAL:
        case C_PARAM:
          // TODO
          fatal("For now, declaration of local arrays is not implemented");
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
        sym = addglob(Text, type, S_VARIABLE, 1);
        break;
      case C_LOCAL:
        sym = addlocl(Text, type, S_VARIABLE, 1);
        break;
      case C_PARAM:
        sym = addparam(Text, type, S_VARIABLE, 1);
        break;
    }
  }
  return (sym);
}

// param_declaration: <null>
//           | variable_declaration
//           | variable_declaration ',' param_declaration
//
// Parse the parameters in parentheses after the function name.
// Add them as symbols to the symbol table and return the number
// of parameters. If funcsym is not NULL, there is an existing function
// prototype, and the function has this symbol table pointer.

static int param_declaration(struct symtable *funcsym) {
  int type;
  int proto_paramcnt = -1; // no proto
  int paramcnt = 0;
  struct symtable *protoptr = NULL;

  if (funcsym) {
    protoptr = funcsym->member;
    assert(funcsym->stype == S_PROTO);
  }

  // Loop until the final right parentheses. Current token starts
  // the loop right after T_LPAREN.
  while (Token.token != T_RPAREN) {
    type = parse_full_type(Token.token);
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
      debugnoisy("parse", "param %d for function %s has type %s",
          paramcnt+1, CurFunctionSym->name, typename(type));
      var_declaration(type, C_PARAM);
    }
    paramcnt++;

    switch (Token.token) {
      case T_COMMA:
        scan(&Token);
        break;
      case T_RPAREN:
        break;
      default:
        fatalv("Unexpected token in parameter list: %s", tokenname(Token.token));
    }
  }

  // Check that the number of parameters in this list matches
  // any existing prototype
  if (funcsym != NULL && (paramcnt != funcsym->nelems)) {
    fatals("Parameter count mismatch for function", funcsym->name);
  }
  return (paramcnt);
}

struct ASTnode *function_declaration(int type) {
  struct ASTnode *tree, *finalstmt;
  struct symtable *oldfuncsym, *newfuncsym = NULL;
  int endlabel, paramcnt = 0;

  // Clear out the parameter list
  Paramshead = Paramstail = NULL;

  oldfuncsym = findglob(Text);
  if (oldfuncsym && oldfuncsym->stype == S_PROTO) {
    // proto exists, check return type
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
    newfuncsym = addglob(Text, type, S_FUNCTION, 0);
    CurFunctionSym = newfuncsym;
  }
  lparen();
  paramcnt = param_declaration(oldfuncsym);
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

void global_declarations(void) {
  struct ASTnode *tree;
  int type, basetype;

  while (1) {
    basetype = parse_base_type(Token.token);
    type = parse_pointer_array_type(basetype);
found_type:
    ident();
    if (Token.token == T_LPAREN) {
      // Parse the function declaration and
      // generate the assembly code for it
      tree = function_declaration(type);
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
      var_declaration(type, C_GLOBAL);
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
