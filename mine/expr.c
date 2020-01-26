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
          | postfix_expression '(' argument_list ')'
          | postfix_expression '->' T_IDENT
          | postfix_expression '.' T_IDENT
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

// fwd
struct ASTnode *array_access(struct ASTnode *left);
struct ASTnode *member_access(struct ASTnode *left, int withpointer);
struct ASTnode *primary(void);

// Parse a list of zero or more comma-separated expressions and
// return an AST composed of A_GLUE nodes with the left-hand child
// being the sub-tree of previous expressions (or NULL) and the right-hand
// child being the next expression. Each A_GLUE node will have size field
// set to the number of expressions in the tree at this point. If no
// expressions are parsed, NULL is returned
struct ASTnode *expression_list(int endtoken) {
  struct ASTnode *child;
  struct ASTnode *tree = NULL;
  int exprcount = 1;
  // Loop until the end token
  while (Token.token != endtoken) {

    // Parse the next expression
    child = binexpr(0);

    // Build an A_GLUE AST node ...
    tree = mkastnode(A_GLUE, P_NONE, NULL, tree, NULL, child, NULL, exprcount);
    exprcount++;

    // Stop when we reach the end token
    if (Token.token == endtoken) break;

    // Must have a ',' at this point
    match(T_COMMA, ",");
  }

  // Return the tree of expressions (can be NULL)
  return (tree);
}

// Parse a postfix expression and return an AST node representing it.
// The identifier is already in Text.
struct ASTnode *postfix(void) {
  struct ASTnode *n;
  struct symtable *varptr;
  struct symtable *enumptr;

  n = primary();

  while (1) {
    switch (Token.token) {
      // TODO: this should be a postfix operator, example: f()() should work.
      /*case T_LPAREN:*/
        /*n = funcall(n);*/
      case T_LBRACKET:
        n = array_access(n);
        break;
      case T_DOT:
        n = member_access(n, 0);
        break;
      case T_ARROW:
        n = member_access(n, 1);
        break;
      case T_INC:
        if (n->rvalue)
          fatal("Cannot ++ on rvalue");
        // Can't do it twice
        if (n->op == A_POSTINC || n->op == A_POSTDEC)
          fatal("Cannot ++ and/or -- more than once");
        scan(&Token);
        // change AST operation
        n->op = A_POSTINC;
        break;
      case T_DEC:
        if (n->rvalue)
          fatal("Cannot ++ on rvalue");
        // Can't do it twice
        if (n->op == A_POSTINC || n->op == A_POSTDEC)
          fatal("Cannot ++ and/or -- more than once");
        scan(&Token);
        // change AST operation
        n->op = A_POSTDEC;
        break;
      default:
        return (n);
    }
  }

  ASSERT(0);
  return (NULL);
}

// Only deals with array access after an identifier (variable). Ex: `print(a[4]);`
// current token is '[', the identifier has been scanned.
struct ASTnode *array_access(struct ASTnode *left) {
  struct ASTnode *right;
  struct symtable *ary;
  int ptr_type, elem_type;

  // Check that the sub-tree is a pointer
  if (!ptrtype(left->type))
    fatal("Not an array or pointer");

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

  left->rvalue = 1;
  // Scale the index by the size of the element's type
  right = modify_type(right, left->type, left->ctype, A_ADD);

  // Return an AST tree where the array's base has the offset
  // added to it, and dereference the element. Still an lvalue
  // at this point.
  left = mkastnode(A_ADD, left->type, left->ctype, left, NULL, right, NULL, 0);
  left = mkastunary(A_DEREF, value_at(left->type), left->ctype, left, NULL, 0);
  return (left);
}

// Parse the member reference of a struct (or union, soon)
// and return an AST tree for it. If withpointer is true,
// the access is through a pointer to the member.
struct ASTnode *member_access(struct ASTnode *left, int withpointer) {
  struct ASTnode *right;
  struct symtable *typeptr;
  struct symtable *m;

  // TODO: allow multiple levels of indirection
  if (withpointer && left->type != pointer_to(P_STRUCT) && left->type != pointer_to(P_UNION))
    fatal("Invalid member access with `->`, type mismatch");

  // Or, check that the left AST tree is a struct or union.
  // If so, change it from an A_IDENT to an A_ADDR so that
  // we get the base address, not the value at this address.
  if (!withpointer) {
    if (left->type == P_STRUCT || left->type == P_UNION)
      left->op = A_ADDR;
    else
      fatal("Invalid member access with `.`, type mismatch");
  }

  // Get the details of the composite type
  typeptr = left->ctype;

  left->rvalue = 1; // make sure idents get loaded into a register

  // Skip the '.' or '->' token and get the member's name
  scan(&Token);
  ident();

  int initposn = 0;
  int hasanonunion;
  struct symtable *mu;

  hasanonunion = 0;

  ASSERT(typeptr);

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
        typeptr->type == P_STRUCT ? "struct" : "union", typeptr->name);
  }

  // Build an A_INTLIT node with the offset
  right = mkastleaf(A_INTLIT, P_LONG, NULL, NULL, initposn + m->posn);

  // Add the member's offset to the base of the struct and
  // dereference it. Still an lvalue at this point
  left = mkastnode(A_ADD, pointer_to(m->type), NULL, left, NULL, right, NULL, 0);
  left = mkastunary(A_DEREF, m->type, m->ctype, left, NULL, 0);

  return (left);
}

struct ASTnode *paren_expression(void) {
  struct ASTnode *n;
  struct symtable *ctype = NULL;
  int casttype = P_NONE;

  ASSERT(Token.token == T_LPAREN);
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
  if (casttype == P_NONE) {
    rparen();
  } else {
    if (casttype == n->type) {
      // TODO: warn useless cast
    }
    // Otherwise, make a unary AST node for the cast
    n = mkastunary(A_CAST, casttype, ctype, n, NULL, 0);
  }
  return (n);
}

/**
  primary_expression
          : T_IDENT
          | T_INTLIT
          | T_STRLIT
          | '(' expression ')'
          | sizeof '(' type_decl ')'
          ;
*/
// Parse a primary factor and return an
// AST node representing it.
struct ASTnode *primary(void) {
  struct ASTnode *n;
  struct symtable *enumptr, *varptr;
  struct symtable *ctype = NULL;
  int id;
  int type = 0;
  int size, class;

  switch (Token.token) {
    case T_STATIC:
    case T_EXTERN:
    fatal("Compiler doesn't support static or extern local declarations");
  case T_SIZEOF:
    scan(&Token);
    if (Token.token != T_LPAREN)
      fatal("Left parenthesis expected after sizeof");
    scan(&Token);
    type = parse_full_type(Token.token, &ctype, &class);
    size = typesize(type, ctype);
    rparen();
    return (mkastleaf(A_INTLIT, P_INT, NULL, NULL, size));
  case T_LPAREN:
    return (paren_expression());
  case T_INTLIT:
    // For an INTLIT token, make a leaf AST node for it.
    // Make it a P_CHAR if it's within the P_CHAR range
    // FIXME: assume signed chars unless noted otherwise
    if ((Token.intvalue) >= 0 && (Token.intvalue <= 255)) {
      n = mkastleaf(A_INTLIT, P_CHAR, NULL, NULL, Token.intvalue);
    } else {
      n = mkastleaf(A_INTLIT, P_INT, NULL, NULL, Token.intvalue);
    }
    break;
  case T_IDENT:
    // If the identifier matches an enum value,
    // return an A_INTLIT node
    if ((enumptr = findenumval(Text)) != NULL) {
      n = mkastleaf(A_INTLIT, P_INT, NULL, NULL, enumptr->posn);
      break;
    }
    // See if this identifier exists as a symbol. For arrays, set rvalue to 1.
    if ((varptr = findsymbol(Text)) == NULL)
      fatals("Unknown variable or function", Text);
    switch (varptr->stype) {
      case S_VARIABLE:
        n = mkastleaf(A_IDENT, varptr->type, varptr->ctype, varptr, 0);
        break;
      case S_ARRAY:
        n = mkastleaf(A_ADDR, varptr->type, varptr->ctype, varptr, 0);
        n->rvalue = 1;
        break;
      case S_FUNCTION:
      case S_PROTO:
        // Function call, see if the next token is a left parenthesis
        ident();
        if (Token.token != T_LPAREN)
          fatals("Function name used without parentheses", Text);
        return (funcall());
      default:
        fatals("Identifier not a scalar or array variable", Text);
    }
    break;
  case T_STRLIT:
    id = genglobstr(Text);
    n = mkastleaf(A_STRLIT, pointer_to(P_CHAR), NULL, NULL, id);
    break;
  default:
    fatals("Expecting a primary expression, got token", tokenname(Token.token));
  }

  // Scan in the next token and return the leaf node
  scan(&Token);
  return (n);
}

int isbinastop(int op_or_token) {
  if (op_or_token > T_EOF && op_or_token < T_INTLIT)
    return (1);
  return (0);
}

// Convert a binary operator token into an AST operation.
// We rely on a 1:1 mapping from token to AST operation
static int binastop(int tokentype) {
  if (tokentype > T_EOF && tokentype < T_INTLIT)
    return(tokentype);
  fatals("Syntax error in binastop(), token", tokenname(tokentype));
  return(-1);
}


// Operator precedence for each token (low first). Must
// match up with the order of tokens in defs.h
static int OpPrec[] = {
  0, 5, 10,                     // T_EOF, T_COMMA, T_ASSIGN
  10, 10, 10, 10,               // T_AS_PLUS, T_AS_MINUS, T_AS_STAR, T_AS_SLASH
  15,                           // T_QUESTION,
  20, 30,                       // T_LOGOR, T_LOGAND
  40, 50, 60,                   // T_OR, T_XOR, T_AMPER
  70, 70,                       // T_EQ, T_NE
  80, 80, 80, 80,               // T_LT, T_GT, T_LE, T_GE
  90, 90,                       // T_LSHIFT, T_RSHIFT
  100, 100,                     // T_PLUS, T_MINUS
  110, 110, 110                 // T_STAR, T_SLASH, T_PERCENT
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
  // `a = b = c;`, `=` is right associative (a=(b=c)), same for '+=', '-=', '*=', '/='
  if (tokentype >= T_ASSIGN && tokentype <= T_AS_SLASH)
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
  if (tokentype == T_SEMI || tokentype == T_RPAREN || tokentype == T_RBRACKET ||
      tokentype == T_COLON || tokentype == T_RBRACE) {
    left->rvalue = 1;
    return (left);
  }
  // comma separator in parameter list
  if (CommaAsSeparator && tokentype == T_COMMA) {
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
    switch (ASTop) {
      case A_TERNARY:
        // condition expr in `left`, truexpr in `right`
        match(T_COLON, ":");
        ltemp = binexpr(0);
        // Build and return the AST for this statement. Use the middle
        // expression's type as the return type. XXX We should also
        // consider the third expression's type.
        return (mkastnode(A_TERNARY, right->type, right->ctype, left, right, ltemp, NULL, 0));
    }
    if (ASTop == A_ASSIGN) {
      right->rvalue = 1;
      rtemp = modify_type(right, left->type, left->ctype, A_ASSIGN);
      if (rtemp == NULL) {
        fatalv("Incompatible expression in assignment. Expected type %s, got %s",
            typename(left->type, NULL), typename(right->type, NULL));
      }
      right = rtemp;

      // Make an assignment AST tree. However, switch
      // left and right around, so that the right expression's
      // code will be generated before the left expression
      ltemp= left; left= right; right= ltemp;
    } else if (ASTop == A_SEQUENCE) {
      left->rvalue = 1; // first value thrown away
    } else {
      left->rvalue = 1;
      right->rvalue = 1;

      ltemp = modify_type(left, right->type, right->ctype, ASTop);
      rtemp = modify_type(right, left->type, left->ctype, ASTop);

      if (ltemp == NULL && rtemp == NULL)
        fatalv("Incompatible types %s and %s in binary expression for op %s",
            typename(left->type, NULL),
            typename(right->type, NULL), opname(ASTop));

      if (ltemp != NULL) left = ltemp;
      if (rtemp != NULL) right = rtemp;
    }

    // Join that sub-tree with ours. Convert the token
    // into an AST operation at the same time.
    left = mkastnode(ASTop, left->type, left->ctype, left, NULL, right, NULL, 0);

    // Some operators produce an int result regardless of their operands
    switch (binastop(tokentype)) {
      case A_LOGOR:
      case A_LOGAND:
      case A_EQ:
      case A_NE:
      case A_LT:
      case A_GT:
      case A_LE:
      case A_GE:
        left->type = P_INT;
    }

    // Update the details of the current token.
    // If we hit a terminating token, return just the left node
    tokentype = Token.token;
    if (tokentype == T_SEMI || tokentype == T_RPAREN ||
        tokentype == T_RBRACKET || tokentype == T_COLON ||
        tokentype == T_RBRACE) {
      left->rvalue = 1;
      return (left);
    }
    // comma separator in parameter list
    if (CommaAsSeparator && tokentype == T_COMMA) {
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

  ASSERT(Token.token == T_LPAREN);

  // Check that the identifier has been defined,
  // then make a leaf node for it.
  if ((funcptr = findglob(Text)) == NULL) {
    fatals("Undeclared function", Text);
  }
  if (funcptr->stype != S_FUNCTION && funcptr->stype != S_PROTO) {
    fatals("Tried to call non-function", Text);
  }
  lparen();

  CommaAsSeparator = 1;
  tree = expression_list(T_RPAREN);
  CommaAsSeparator = 0;

  rparen();

  // XXX Check type of each argument against the function's prototype

  // Build the function call AST node. Store the
  // function's return type as this node's type.
  return (mkastunary(A_FUNCALL, funcptr->type, funcptr->ctype, tree, funcptr, 0));
}

/**
    prefix_expression: postfix_expression
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
        fatalv("& operator must be followed by an identifier, has AST op: %d", tree->op);

      // Prevent '&' being performed on an array
      if (tree->sym->stype == S_ARRAY)
        fatal("& operator cannot be performed on an array");

      // Now change the operator to A_ADDR and the type to
      // a pointer to the original type
      tree->op = A_ADDR;
      tree->type = pointer_to(tree->type);
      break;
    case T_STAR:
      scan(&Token);
      tree = prefix();

      tree->rvalue = 1;
      // Prepend an A_DEREF operation to the tree
      tree = mkastunary(A_DEREF, value_at(tree->type), tree->ctype, tree, NULL, 0);
      break;
    case T_MINUS:
      scan(&Token);
      tree = prefix();
      // Prepend a A_NEGATE operation to the tree and
      // make the child an rvalue. Because chars are unsigned,
      // also widen this to int so that it's signed
      tree->rvalue = 1;
      tree = modify_type(tree, P_INT, NULL, 0);
      tree = mkastunary(A_NEGATE, tree->type, NULL, tree, NULL, 0);
      break;
    case T_INVERT:
      // Get the next token and parse it
      // recursively as a prefix expression
      scan(&Token);
      tree = prefix();

      // Prepend a A_INVERT operation to the tree and
      // make the child an rvalue.
      tree->rvalue = 1;
      tree = mkastunary(A_INVERT, tree->type, NULL, tree, NULL, 0);
      break;
    case T_LOGNOT:
      // Get the next token and parse it
      // recursively as a prefix expression
      scan(&Token);
      tree = prefix();

      // Prepend a A_LOGNOT operation to the tree and
      // make the child an rvalue.
      tree->rvalue = 1;
      tree = mkastunary(A_LOGNOT, tree->type, NULL, tree, NULL, 0);
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
      tree = mkastunary(A_PREINC, tree->type, tree->ctype, tree, NULL, 0);
      break;
    case T_DEC:
      // recursively as a prefix expression
      scan(&Token);
      tree = prefix();

      // For now, ensure it's an identifier
      if (tree->op != A_IDENT)
        fatal("-- operator must be followed by an identifier");

      // Prepend an A_PREDEC operation to the tree
      tree = mkastunary(A_PREDEC, tree->type, tree->ctype, tree, NULL, 0);
      break;
    default:
      ASSERT(Token.token != T_SEMI);
      tree = postfix();
      break;
  }
  return (tree);
}
