#include <assert.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Parsing of declarations
// Copyright (c) 2019 Warren Toomey, GPL3

// Parse the current token and
// return a primitive type enum value
// TODO: allow type to be an array type without size given, like for
// parameters: `char[] fmt`
int parse_type(int t) {
  int type = P_NONE;
  if (t == T_CHAR) type = P_CHAR;
  if (t == T_INT)  type = P_INT;
  if (t == T_LONG) type = P_LONG;
  if (t == T_VOID) type = P_VOID;
  if (type == P_NONE)
    fatals("Expected a type, found token", tokenname(t));

  while (1) {
    scan(&Token);
    if (Token.token != T_STAR) {
      break;
    }
    type = pointer_to(type);
  }

  return (type);
}

/**
    variable_declaration: type identifier ';'
      | type identifier '[' P_INTLIT ']' ';'
      ;
*/

// Parse the declaration of 1 or more scalar variable and/or array variables with a given size.
// The identifier has been scanned and we have the type. If `isParam` is true,
// scan only 1 parameter.
void var_declaration(int type, int isLocal, int isParam) {
  int id;
  while (1) {
    // array variable
    if (Token.token == T_LBRACKET) {
      scan(&Token);
      if (Token.token == T_INTLIT) {
        // Add this as a known array and generate its space in assembly.
        // We treat the array as a pointer to its elements' type
        if (isLocal) {
          if (isParam) {
            // FIXME: just make them pointers to their underlying type
            fatal("Array parameters are not yet supported");
          }
          addlocl(Text, pointer_to(type), S_ARRAY, isParam ? C_PARAM : C_LOCAL, Token.intvalue);
        } else {
          addglob(Text, pointer_to(type), S_ARRAY, Token.intvalue);
        }
      } else {
        fatal("Missing array size in array variable declaration");
      }
      scan(&Token); // integer literal array size
      match(T_RBRACKET, "]");
    // scalar variable
    } else {
      if (isParam) {
        if (addparam(Text, type, S_VARIABLE, 1) == -1) {
          fatalv("Internal error: could not create parameter %s", Text);
        }
      } else if (isLocal) {
        if (addlocl(Text, type, S_VARIABLE, C_LOCAL, 1) == -1) {
          fatals("Duplicate local variable declaration", Text);
        }
      } else {
        addglob(Text, type, S_VARIABLE, 1);
      }
    }

    if (isParam) return;

    if (Token.token == T_SEMI) {
      semi();
      return;
    } else if (Token.token == T_COMMA) {
      scan(&Token);
      ident();
      continue;
    } else {
      fatalv("Unexpected token after variable declaration: %s", tokenname(Token.token));
    }
  }
}

// param_declaration: <null>
//           | variable_declaration
//           | variable_declaration ',' param_declaration
//
// Parse the parameters in parentheses after the function name.
// Add them as symbols to the symbol table and return the number
// of parameters.
static int param_declaration(int proto_id) {
  int type;
  int proto_paramcnt = -1; // no proto
  int paramcnt = 0;

  if (proto_id != -1) {
    proto_paramcnt = Symtable[proto_id].size;
  }

  // Loop until the final right parentheses. Current token starts
  // the loop right after T_LPAREN.
  while (Token.token != T_RPAREN) {
    type = parse_type(Token.token);
    ident();
    // We have an existing prototype.
    // Check that this type matches the prototype.
    if (proto_id != -1) {
      if (type != Symtable[proto_id+paramcnt+1].type) {
        fatalv("Parameter type doesn't match prototype for parameter %d", paramcnt+1);
      }
      // no need for `var_declaration()` here, we copy the prototype's parameters to locals later
      // if this is indeed a function definition. We do need to update its
      // parameter names though, in case it is a definition.
      assert(Symtable[proto_id+paramcnt+1].class == C_PARAM);
      Symtable[proto_id+paramcnt+1].name = strdup(Text);
    } else {
      var_declaration(type, 1, 1);
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
  if ((proto_id != -1) && (paramcnt != proto_paramcnt)) {
    fatals("Parameter count mismatch for function", Symtable[proto_id].name);
  }
  return (paramcnt);
}

struct ASTnode *function_declaration(int type) {
  struct ASTnode *tree, *finalstmt;
  int funcslot;
  int protoslot = -1;
  int paramcnt = 0;

  protoslot = funcslot = findglob(Text);
  if (protoslot != -1 && Symtable[protoslot].stype == S_PROTO) {
    // proto exists, check return type
    if (type != Symtable[protoslot].type) {
      fatalv("Function return value must match prototype for function %s", Text);
    }
  // function definition already exists
  } else if (protoslot != -1 && Symtable[protoslot].stype == S_FUNCTION) {
    // TODO: allow prototypes after function definitions
    fatalv("Cannot redefine or redeclare a defined function %s", Text);
  // global symbol conflict
  } else if (protoslot != -1) {
    fatalv("Identifier %s already exists, cannot be a function", Text);
  } else {
    funcslot = addglob(Text, type, S_FUNCTION, paramcnt);
    Functionid = funcslot; // set currently parsed/generated function
  }
  lparen();
  paramcnt = param_declaration(protoslot);
  rparen();
  Symtable[funcslot].size = paramcnt;

  if (Token.token == T_SEMI) {
    Symtable[funcslot].stype = S_PROTO; // actually a prototype
    scan(&Token);
    return NULL;
  }
  Symtable[funcslot].stype = S_FUNCTION; // turn proto into real function

  // This is not just a prototype.
  // Copy the global parameters to be local parameters
  copyfuncparams(funcslot);

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
  return (mkuastunary(A_FUNCTION, type, tree, funcslot));
}

void global_declarations(void) {
  struct ASTnode *tree;
  int type;

  while (1) {
    type = parse_type(Token.token);
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
        freeloclsyms();
      } else {
        // prototype, do nothing
      }
    } else {
      // Parse the global variable declaration
      var_declaration(type, 0, 0);
    }

    if (Token.token == T_EOF)
      break;
  }
}
