#include "defs.h"
#include "data.h"
#include "decl.h"

// Parsing of expressions
// Copyright (c) 2019 Warren Toomey, GPL3

// Parse a primary factor and return an
// AST node representing it.
static struct ASTnode *primary(void) {
  struct ASTnode *n;
  int id;

  switch (Token.token) {
  case T_INTLIT:
    // For an INTLIT token, make a leaf AST node for it.
    n = mkastleaf(A_INTLIT, Token.intvalue);
    break;

  case T_IDENT:
    // Check that this identifier exists
    id = findglob(Text);
    if (id == -1)
      fatals("Unknown variable", Text);

    // Make a leaf AST node for it
    n = mkastleaf(A_IDENT, id);
    break;

  default:
    fatald("Syntax error, token", Token.token);
  }

  // Scan in the next token and return the leaf node
  scan(&Token);
  return (n);
}


// Convert a binary operator token into an AST operation.
// We rely on a 1:1 mapping from token to AST operation
static int arithop(int tokentype) {
  if (tokentype > T_EOF && tokentype < T_INTLIT)
    return(tokentype);
  fatald("Syntax error, token", tokentype);
}

// Operator precedence for each token. Must
// match up with the order of tokens in defs.h
static int OpPrec[] = {
  0, 10, 10,			// T_EOF, T_PLUS, T_MINUS
  20, 20,			// T_STAR, T_SLASH
  30, 30,			// T_EQ, T_NE
  40, 40, 40, 40		// T_LT, T_GT, T_LE, T_GE
};

// Check that we have a binary operator and
// return its precedence.
static int op_precedence(int tokentype) {
  int prec = OpPrec[tokentype];
  if (prec == 0)
    fatald("Syntax error, token", tokentype);
  return (prec);
}

// Return an AST tree whose root is a binary operator.
// Parameter ptp is the previous token's precedence.
struct ASTnode *binexpr(int ptp) {
  struct ASTnode *left, *right;
  int tokentype;

  // Get the primary tree on the left.
  // Fetch the next token at the same time.
  left = primary();

  // If we hit a semicolon, return just the left node
  tokentype = Token.token;
  if (tokentype == T_SEMI)
    return (left);

  // While the precedence of this token is
  // more than that of the previous token precedence
  while (op_precedence(tokentype) > ptp) {
    // Fetch in the next integer literal
    scan(&Token);

    // Recursively call binexpr() with the
    // precedence of our token to build a sub-tree
    right = binexpr(OpPrec[tokentype]);

    // Join that sub-tree with ours. Convert the token
    // into an AST operation at the same time.
    left = mkastnode(arithop(tokentype), left, right, 0);

    // Update the details of the current token.
    // If we hit a semicolon, return just the left node
    tokentype = Token.token;
    if (tokentype == T_SEMI)
      return (left);
  }

  // Return the tree we have when the precedence
  // is the same or lower
  return (left);
}
