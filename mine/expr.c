#include "defs.h"
#include "data.h"
#include "decl.h"

// Parsing of expressions
// Copyright (c) 2019 Warren Toomey, GPL3

/**
 *
  primary_expression
          : T_IDENT
          | T_INTLIT
          | T_STRLIT
          | '(' expression ')'
          ;

  postfix_expression
          : primary_expression
          | postfix_expression '[' expression ']'
          | postfix_expression '(' expression ')'
          | postfix_expression '++'
          | postfix_expression '--'
          ;

  prefix_expression
          : postfix_expression
          | '++' prefix_expression
          | '--' prefix_expression
          | prefix_operator prefix_expression
          ;

  prefix_operator
          : '&'
          | '*'
          | '-'
          | '~'
          | '!'
          ;

  multiplicative_expression
          : prefix_expression
          | multiplicative_expression '*' prefix_expression
          | multiplicative_expression '/' prefix_expression
          | multiplicative_expression '%' prefix_expression
          ;
*/

// expression_list: <null>
//        | expression
//        | expression ',' expression_list
//        ;

// Parse a list of zero or more comma-separated expressions and
// return an AST composed of A_GLUE nodes with the left-hand child
// being the sub-tree of previous expressions (or NULL) and the right-hand
// child being the next expression. Each A_GLUE node will have size field
// set to the number of expressions in the tree at this point. If no
// expressions are parsed, NULL is returned
struct ASTnode *expression_list(int endtoken) {
  struct ASTnode *child, *tree = NULL;
  int exprcount = 1;
  // Loop until the end token
  while (Token.token != endtoken) {

    // Parse the next expression
    child = binexpr(0);

    // Build an A_GLUE AST node ...
    tree = mkastnode(A_GLUE, P_NONE, tree, NULL, child, NULL, exprcount);
    exprcount++;

    // Stop when we reach the end token
    if (Token.token == endtoken) break;

    // Must have a ',' at this point
    match(T_COMMA, ",");
  }

  // Return the tree of expressions (can be NULL)
  return (tree);
}

struct ASTnode *array_access(void);
struct ASTnode *member_access(int withpointer);

// Parse a postfix expression and return an AST node representing it.
// The identifier is already in Text.
struct ASTnode *postfix(void) {
  struct ASTnode *n;
  struct symtable *varptr;
  struct symtable *enumptr;

  // If the identifier matches an enum value,
  // return an A_INTLIT node
  if ((enumptr = findenumval(Text)) != NULL) {
    ident();
    return (mkastleaf(A_INTLIT, P_INT, NULL, enumptr->posn));
  }

  // Scan in the next token to see if we have a postfix expression
  ident();

  // Function call
  if (Token.token == T_LPAREN)
    return (funcall());

  // An array reference
  if (Token.token == T_LBRACKET)
    return (array_access());

  if (Token.token == T_DOT)
    return (member_access(0));

  if (Token.token == T_ARROW)
    return (member_access(1));

  // A variable. Check that the variable exists.
  varptr = findsymbol(Text);
  if (varptr == NULL || (varptr->stype != S_VARIABLE && varptr->stype != S_ARRAY)) {
    fatals("Unknown variable", Text);
  }

  switch (Token.token) {
      // Post-increment: skip over the token
    case T_INC:
      scan(&Token);
      n = mkastleaf(A_POSTINC, varptr->type, varptr, 0);
      break;
      // Post-decrement: skip over the token
    case T_DEC:
      scan(&Token);
      n = mkastleaf(A_POSTDEC, varptr->type, varptr, 0);
      break;
      // Just a variable reference
    default:
      n = mkastleaf(A_IDENT, varptr->type, varptr, 0);
      break;
  }
  return (n);
}

// Only deals with array access after an identifier (variable). Ex: `print(a[4]);`
// current token is '[', the identifier has been scanned.
struct ASTnode *array_access(void) {
  struct ASTnode *left, *right;
  struct symtable *ary;

  // Check that the identifier has been defined as an array
  // then make a leaf node for it that points at the base
  if ((ary = findsymbol(Text)) == NULL || ary->stype != S_ARRAY) {
    fatals("Undeclared array", Text);
  }

  left = mkastleaf(A_ADDR, ary->type, ary, 0);

  // Get the '['
  scan(&Token);

  // Parse the following expression
  right = binexpr(0);

  // Get the ']'
  match(T_RBRACKET, "]");

  // Ensure that this is of int type
  if (!inttype(right->type)) {
    fatal("Array index is not of integer type");
  }

  // Scale the index by the size of the element's type
  right = modify_type(right, left->type, A_ADD);

  // Return an AST tree where the array's base has the offset
  // added to it, and dereference the element. Still an lvalue
  // at this point.
  left = mkastnode(A_ADD, ary->type, left, NULL, right, NULL, 0);
  left = mkuastunary(A_DEREF, value_at(left->type), left, NULL, 0);
  return (left);
}

// Parse the member reference of a struct (or union, soon)
// and return an AST tree for it. If withpointer is true,
// the access is through a pointer to the member.
struct ASTnode *member_access(int withpointer) {
  struct ASTnode *left, *right;
  struct symtable *compvar;
  struct symtable *typeptr;
  struct symtable *m;

  // Check that the identifer has been declared as a struct (or a union, later),
  // or a struct/union pointer
  if ((compvar = findsymbol(Text)) == NULL)
    fatals("Undeclared variable", Text);
  // TODO: allow multiple levels of indirection
  if (withpointer && compvar->type != pointer_to(P_STRUCT) && compvar->type != pointer_to(P_UNION))
    fatals("Undeclared variable", Text);
  if (!withpointer && compvar->type != P_STRUCT && compvar->type != P_UNION)
    fatals("Undeclared variable", Text);

  // If a pointer to a struct, get the pointer's value.
  // Otherwise, make a leaf node that points at the base
  // Either way, it's an rvalue
  if (withpointer) {
    left = mkastleaf(A_IDENT, pointer_to(compvar->type), compvar, 0);
  } else {
    left = mkastleaf(A_ADDR, compvar->type, compvar, 0);
  }
  left->rvalue = 1;

  // Get the details of the composite type
  typeptr = compvar->ctype;

  int initposn = 0;
  int hasanonunion;
  struct symtable *mu;

  while (1) {
    hasanonunion = 0;
    // Skip the '.' or '->' token and get the member's name
    scan(&Token);
    ident();

    // Find the matching member's name in the type
    // Die if we can't find it
    for (m = typeptr->member; m != NULL; m = m->next) {
      if (m->name && !strcmp(m->name, Text)) {
        break; // found
      } else if (typeptr->type == P_STRUCT && !m->name && m->ctype && m->ctype->type == P_UNION) {
        hasanonunion = 1;
      }
    }

    // anonymous union member search
    if (m == NULL && hasanonunion) {
      for (m = typeptr->member; m != NULL; m = m->next) {
        if (m->ctype && m->ctype->type == P_UNION && !m->name) {
          ASSERT(m->ctype->member);
          for (mu = m->ctype->member; mu != NULL; mu = mu->next) {
            if (!strcmp(mu->name, Text)) {
              debugnoisy("parse", "Found anonymous union member %s, adding offset %d\n", mu->name, initposn);
              m = mu;
              goto found_memb;
            }
          }
        }
        initposn += m->size;
      }
    }

found_memb:

    if (m == NULL) {
      fatalv("No member %s found in %s %s: ", Text,
          compvar->type == P_STRUCT ? "struct" : "union", typeptr->name);
    }

    if (Token.token == T_DOT) {
      if (m->ctype->type != P_STRUCT && m->ctype->type != P_UNION) {
        scan(&Token); ident();
        fatalv("Attempt to access member %s of non-composite", Text);
      }
      typeptr = m->ctype;
      initposn += m->posn;
      continue;
    }

    // Build an A_INTLIT node with the offset
    right = mkastleaf(A_INTLIT, P_LONG, NULL, initposn + m->posn);

    // Add the member's offset to the base of the struct and
    // dereference it. Still an lvalue at this point
    left = mkastnode(A_ADD, pointer_to(m->type), left, NULL, right, NULL, 0);
    left = mkuastunary(A_DEREF, m->type, left, NULL, 0);

    if (Token.token == T_ARROW) {
      typeptr = m->ctype;
      initposn = 0;
      continue;
    } else {
      break;
    }
  }

  return (left);
}

/**
  primary_expression
          : IDENTIFIER
          | CONSTANT
          | STRING_LITERAL
          | '(' expression ')'
          ;
*/
// Parse a primary factor and return an
// AST node representing it.
struct ASTnode *primary(void) {
  struct ASTnode *n;
  int id;
  int casttype = 0;
  struct symtable *ctype = NULL;

  switch (Token.token) {
  case T_LPAREN:
    // Beginning of a parenthesised expression, skip the '('
    scan(&Token);
    switch (Token.token) {
      case T_IDENT:
        // We have to see if the identifier matches a typedef.
        // If not, treat it as an expression.
        if (type_of_typedef_nofail(Text, &ctype) == -1) {
          n = binexpr(0);
          break;
        }
      case T_VOID:
      case T_CHAR:
      case T_INT:
      case T_LONG:
      case T_STRUCT:
      case T_UNION:
      case T_ENUM:
        // Get the type inside the parentheses
        casttype = parse_cast_type(&ctype);
        // Skip the closing ')' and then parse the following expression
        rparen();
        // fallthru
      default:
        n = binexpr(0); // Scan in the expression
    }
    if (casttype == 0) {
      rparen();
    } else {
      if (casttype == n->type) {
        // TODO: warn useless cast
      }
      // Otherwise, make a unary AST node for the cast
      n = mkuastunary(A_CAST, casttype, n, NULL, 0);
    }
    return (n);
  case T_INTLIT:
    // For an INTLIT token, make a leaf AST node for it.
    // Make it a P_CHAR if it's within the P_CHAR range
    if ((Token.intvalue) >= 0 && (Token.intvalue < 256)) {
      n = mkastleaf(A_INTLIT, P_CHAR, NULL, Token.intvalue);
    } else {
      n = mkastleaf(A_INTLIT, P_INT, NULL, Token.intvalue);
    }
    break;
  case T_IDENT:
    return (postfix());
  case T_STRLIT:
    id = genglobstr(Text);
    n = mkastleaf(A_STRLIT, pointer_to(P_CHAR), NULL, id);
    break;
  default:
    fatals("Expecting a primary expression, got token", tokenname(Token.token));
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
  0, 10, 20, 30,                // T_EOF, T_ASSIGN, T_LOGOR, T_LOGAND
  40, 50, 60,                   // T_OR, T_XOR, T_AMPER
  70, 70,                       // T_EQ, T_NE
  80, 80, 80, 80,               // T_LT, T_GT, T_LE, T_GE
  90, 90,                       // T_LSHIFT, T_RSHIFT
  100, 100,                     // T_PLUS, T_MINUS
  110, 110                      // T_STAR, T_SLASH
};

// Check that we have a binary operator and
// return its precedence.
static int op_precedence(int tokentype) {
  if (tokentype >= T_INTLIT) {
    fatals("Token with no precedence in op_precedence:", tokenname(tokentype));
  }
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

  if (Token.token == T_SEMI) {
    fatal("Unexpected semicolon");
  }
  // Get the primary tree on the left.
  // Fetch the next token at the same time.
  left = prefix();

  // If we hit a ';', ':', ')', ']' or ',' return just the left node
  tokentype = Token.token;
  if (tokentype == T_SEMI || tokentype == T_RPAREN || tokentype == T_RBRACKET || tokentype == T_COMMA || tokentype == T_COLON) {
    left->rvalue = 1;
    return (left);
  }

  // While the precedence of this token is
  // more than that of the previous token precedence
  while ((op_precedence(tokentype) > ptp) ||
         (rightassoc(tokentype) && op_precedence(tokentype) == ptp)) {
    scan(&Token); // the precedence operator token

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
        fatalv("Incompatible types %s and %s in binary expression", typename(left->type, NULL), typename(right->type, NULL));

      if (ltemp != NULL) left = ltemp;
      if (rtemp != NULL) right = rtemp;
    }

    // Join that sub-tree with ours. Convert the token
    // into an AST operation at the same time.
    left = mkastnode(ASTop, left->type, left, NULL, right, NULL, 0);

    // Update the details of the current token.
    // If we hit a terminating token, return just the left node
    tokentype = Token.token;
    if (tokentype == T_SEMI || tokentype == T_RPAREN ||
        tokentype == T_RBRACKET || tokentype == T_COMMA) {
      left->rvalue = 1;
      return (left);
    }
  }

  // Return the tree we have when the precedence
  // is the same or lower
  left->rvalue = 1;
  return (left);
}

// Identifier is parsed, current token is '('.
struct ASTnode *funcall(void) {
  struct ASTnode *tree = NULL; // expression list
  struct symtable *funcptr;

  // Check that the identifier has been defined,
  // then make a leaf node for it.
  if ((funcptr = findglob(Text)) == NULL) {
    fatals("Undeclared function", Text);
  }
  if (funcptr->stype != S_FUNCTION && funcptr->stype != S_PROTO) {
    fatals("Tried to call non-function", Text);
  }
  lparen();

  tree = expression_list(T_RPAREN);

  rparen();

  // XXX Check type of each argument against the function's prototype

  // Build the function call AST node. Store the
  // function's return type as this node's type.
  return mkuastunary(A_FUNCALL, funcptr->type, tree, funcptr, 0);
}

/**
    prefix_expression: primary
        | '*'  prefix_expression
        | '&'  prefix_expression
        | '-'  prefix_expression
        | '~'  prefix_expression
        | '!'  prefix_expression
        | '++' prefix_expression
        | '--' prefix_expression
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
      tree = mkuastunary(A_DEREF, value_at(tree->type), tree, NULL, 0);
      break;
    case T_MINUS:
      scan(&Token);
      tree = prefix();
      // Prepend a A_NEGATE operation to the tree and
      // make the child an rvalue. Because chars are unsigned,
      // also widen this to int so that it's signed
      tree->rvalue = 1;
      tree = modify_type(tree, P_INT, 0);
      tree = mkuastunary(A_NEGATE, tree->type, tree, NULL, 0);
      break;
    case T_INVERT:
      // Get the next token and parse it
      // recursively as a prefix expression
      scan(&Token);
      tree = prefix();

      // Prepend a A_INVERT operation to the tree and
      // make the child an rvalue.
      tree->rvalue = 1;
      tree = mkuastunary(A_INVERT, tree->type, tree, NULL, 0);
      break;
    case T_LOGNOT:
      // Get the next token and parse it
      // recursively as a prefix expression
      scan(&Token);
      tree = prefix();

      // Prepend a A_LOGNOT operation to the tree and
      // make the child an rvalue.
      tree->rvalue = 1;
      tree = mkuastunary(A_LOGNOT, tree->type, tree, NULL, 0);
      break;
    case T_INC:
      // Get the next token and parse it
      // recursively as a prefix expression
      scan(&Token);
      tree = prefix();

      // For now, ensure it's an identifier
      if (tree->op != A_IDENT)
        fatal("++ operator must be followed by an identifier");

      // Prepend an A_PREINC operation to the tree
      tree = mkuastunary(A_PREINC, tree->type, tree, NULL, 0);
      break;
    case T_DEC:
      // recursively as a prefix expression
      scan(&Token);
      tree = prefix();

      // For now, ensure it's an identifier
      if (tree->op != A_IDENT)
        fatal("-- operator must be followed by an identifier");

      // Prepend an A_PREDEC operation to the tree
      tree = mkuastunary(A_PREDEC, tree->type, tree, NULL, 0);
      break;
    default:
      ASSERT(Token.token != T_SEMI);
      tree = primary();
      break;
  }
  return (tree);
}
