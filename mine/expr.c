#include <assert.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Parsing of expressions
// Copyright (c) 2019 Warren Toomey, GPL3

/*
 compound_statement: '{' '}'          // empty, i.e. no statement
      |      '{' statement '}'
      |      '{' statement statements '}'
      ;

 statement: declaration
      |     if_statement
      ;

 declaration: 'int' identifier ';'  ;

 if_statement: if_head
      |        if_head 'else' compound_statement
      ;

 if_head: 'if' '(' true_false_expression ')' compound_statement  ;

 identifier: T_IDENT ;
*/

// Parse a primary factor and return an
// AST node representing it.
static struct ASTnode *primary(void) {
  struct ASTnode *n;
  int id;

  switch (Token.token) {
  case T_INTLIT:
    // For an INTLIT token, make a leaf AST node for it.
    // Make it a P_CHAR if it's within the P_CHAR range
    if ((Token.intvalue) >= 0 && (Token.intvalue < 256)) {
      n = mkastleaf(A_INTLIT, P_CHAR, Token.intvalue);
    } else {
      n = mkastleaf(A_INTLIT, P_INT, Token.intvalue);
    }
    break;
  case T_IDENT:
    // This could be a variable or a function call.
    // Scan in the next token to find out
    scan(&Token);
    // It's a '(', so a function call
    if (Token.token == T_LPAREN) {
      return (funcall());
    }
    reject_token(&Token);
    // Check that this identifier exists
    id = findglob(Text);
    if (id == -1) {
      fatals("Unknown variable", Text);
    }

    // Make a leaf AST node for it
    n = mkastleaf(A_IDENT, Gsym[id].type, id);
    break;
  default:
    fatals("Syntax error in primary(), token", tokenname(Token.token));
  }

  // Scan in the next token and return the leaf node
  scan(&Token);
  return (n);
}


// Convert a binary operator token into an AST operation.
// We rely on a 1:1 mapping from token to AST operation
static int binastop(int tokentype) {
  if (tokentype > T_EOF && tokentype < T_INTLIT)
    return(tokentype);
  fatals("Syntax error in binastop(), token", tokenname(tokentype));
}

// Operator precedence for each token. Must
// match up with the order of tokens in defs.h
static int OpPrec[] = {
  0,  10,     // T_EOF, T_ASSIGN
  20, 20,     // T_PLUS, T_MINUS
  30, 30,     // T_STAR, T_SLASH
  40, 40,     // T_EQ, T_NE
  50, 50, 50, 50    // T_LT, T_GT, T_LE, T_GE
};

// Check that we have a binary operator and
// return its precedence.
static int op_precedence(int tokentype) {
  int prec = OpPrec[tokentype];
  if (prec == 0) {
    fatals("Syntax error in op_precedence(), token", tokenname(tokentype));
  }
  return (prec);
}

static int rightassoc(int tokentype) {
  if (tokentype == T_ASSIGN) // `a = b = c;`, `=` is right associative
    return(1);
  return(0);
}

// Return an AST tree whose root is a binary operator.
// Parameter ptp is the previous token's precedence.
struct ASTnode *binexpr(int ptp) {
  struct ASTnode *left, *right;
  struct ASTnode *ltemp, *rtemp;
  int ASTop;
  int tokentype;

  assert(Token.token != T_SEMI);
  // Get the primary tree on the left.
  // Fetch the next token at the same time.
  left = prefix();

  // If we hit a semicolon or ')', return just the left node
  tokentype = Token.token;
  if (tokentype == T_SEMI || tokentype == T_RPAREN) {
    left->rvalue = 1;
    return (left);
  }

  // While the precedence of this token is
  // more than that of the previous token precedence
  while ((op_precedence(tokentype) > ptp) ||
         (rightassoc(tokentype) && op_precedence(tokentype) == ptp)) {
    // Fetch in the next integer literal
    scan(&Token);

    // Recursively call binexpr() with the
    // precedence of our token to build a sub-tree
    right = binexpr(OpPrec[tokentype]);

    ASTop = binastop(tokentype);
    if (ASTop == A_ASSIGN) {
      right->rvalue = 1;
      right = modify_type(right, left->type, 0);
      if (right == NULL)
        fatal("Incompatible expression in assignment");

      // Make an assignment AST tree. However, switch
      // left and right around, so that the right expression's
      // code will be generated before the left expression
      ltemp= left; left= right; right= ltemp;
    } else {
      left->rvalue = 1;
      right->rvalue = 1;

      ltemp = modify_type(left, right->type, ASTop);
      rtemp = modify_type(right, left->type, ASTop);

      if (ltemp == NULL && rtemp == NULL)
        fatalv("Incompatible types %s and %s in binary expression", typename(left->type), typename(right->type));

      if (ltemp != NULL) left = ltemp;
      if (rtemp != NULL) right = rtemp;
    }

    // Join that sub-tree with ours. Convert the token
    // into an AST operation at the same time.
    left = mkastnode(ASTop, left->type, left, NULL, right, 0);

    // Update the details of the current token.
    // If we hit a semicolon or ')', return just the left node
    tokentype = Token.token;
    if (tokentype == T_SEMI || tokentype == T_RPAREN) {
      left->rvalue = 1;
      return (left);
    }
  }

  // Return the tree we have when the precedence
  // is the same or lower
  return (left);
}

struct ASTnode *funcall(void) {
  struct ASTnode *tree;
  int id;

  // Check that the identifier has been defined,
  // then make a leaf node for it.
  if ((id = findglob(Text)) == -1) {
    fatals("Undeclared function", Text);
  }
  if (Gsym[id].stype != S_FUNCTION) {
    fatals("Tried to call non-function", Text);
  }
  lparen();

  // Parse the following expression
  tree = binexpr(0);

  // Build the function call AST node. Store the
  // function's return type as this node's type.
  // Also record the function's symbol-id
  tree = mkuastunary(A_FUNCALL, Gsym[id].type, tree, id);

  rparen();
  return (tree);
}

/**
    prefix_expression: primary
        | '*' prefix_expression
        | '&' prefix_expression
        ;
 */
struct ASTnode *prefix(void) {
  struct ASTnode *tree;
  switch (Token.token) {
    case T_AMPER:
      scan(&Token);
      tree = prefix();

      // Ensure that it's an identifier
      if (tree->op != A_IDENT)
        fatal("& operator must be followed by an identifier");

      // Now change the operator to A_ADDR and the type to
      // a pointer to the original type
      tree->op = A_ADDR;
      tree->type = pointer_to(tree->type);
      break;
    case T_STAR:
      scan(&Token);
      tree = prefix();
      // For now, ensure it's either another deref or an
      // identifier
      if (tree->op != A_IDENT && tree->op != A_DEREF)
        fatal("* operator must be followed by an identifier or *");

      // Prepend an A_DEREF operation to the tree
      tree = mkuastunary(A_DEREF, value_at(tree->type), tree, 0);
      break;
    default:
      assert(Token.token != T_SEMI);
      tree = primary();
      break;
  }
  return (tree);
}
