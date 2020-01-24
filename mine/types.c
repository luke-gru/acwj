#include "defs.h"
#include "decl.h"

// NOTE: allocates strings, potentially a lot
char *typename(int ptype, struct symtable *ctype) {
  if (ptrtype(ptype)) {
    int valuetype = value_at(ptype);
    char *valuename = typename(valuetype, ctype);
    return str_concat(valuename, "*");
  }
  switch (ptype) {
  case P_CHAR:
    return strdup("char");
  case P_INT:
    return strdup("int");
  case P_LONG:
    return strdup("long");
  case P_VOID:
    return strdup("void");
  case P_STRUCT:
    if (ctype && ctype->name)
      return str_concat("struct ", ctype->name);
    else
      return str_concat("struct ", "?");
  case P_UNION:
    if (ctype && ctype->name)
      return str_concat("union ", ctype->name);
    else
      return str_concat("union ", "?");
  default:
    fatald("Invalid typename", ptype);
  }
  return NULL;
}

int inttype(int ptype) {
  return ((ptype & P_PTR_BITS) == 0) && (ptype >= P_CHAR && ptype <= P_LONG);
}

int ptrtype(int ptype) {
  return ((ptype & P_PTR_BITS) != 0);
}

// Given an AST tree and a type which we want it to become,
// possibly modify the tree by widening or scaling so that
// it is compatible with this type. Return the original tree
// if no changes occurred, a modified tree, or NULL if the
// tree is not compatible with the given type.
// If this will be part of a binary operation, the AST op is not zero.
struct ASTnode *modify_type(struct ASTnode *tree, int rtype,
    struct symtable *rctype, int op) {
  int ltype;
  int lsize, rsize;

  // any types will do here, ex: `if (ptr->name && !strcmp(ptr->name, "John"))`
  // has types `char* and char (bool)`
  if (op == A_LOGOR || op == A_LOGAND) {
    return tree;
  }

  ltype = tree->type;

  // Compare scalar int types
  if (inttype(ltype) && inttype(rtype)) {

    // Both types same, nothing to do
    if (ltype == rtype) return (tree);

    // Get the sizes for each type
    lsize = genprimsize(ltype);
    rsize = genprimsize(rtype);

    // Tree's size is too big
    if (lsize > rsize) return (NULL);

    // Widen to the right
    if (rsize > lsize) return (mkastunary(A_WIDEN, rtype, rctype, tree, NULL, 0));
  }

  if (ptrtype(ltype) && ptrtype(rtype)) {
    // We can compare them
    if (op >= A_EQ && op <= A_GE)
      return (tree);

    if (op == A_ASSIGN && ltype == rtype) return tree;

    // A comparison of the same type for a non-binary operation is OK,
    // or when the left tree is of  `void *` type.
    if (!isbinastop(op) && (ltype == rtype || ltype == pointer_to(P_VOID)))
      return (tree);
  }

  // We can scale only on A_ADD or A_SUBTRACT operation
  if (op == A_ADD || op == A_SUBTRACT || op == A_AS_ADD || op == A_AS_SUBTRACT) {

    // Left is int type, right is pointer type and the size
    // of the original type is >1: scale the left
    if (inttype(ltype) && ptrtype(rtype)) {
      rsize = genprimsize(value_at(rtype));
      if (rsize > 1) {
        return (mkastunary(A_SCALE, rtype, rctype, tree, NULL, rsize));
      } else {
        return (tree); // Size 1, no need to scale
      }
    }
  }

  // so that char *ptr = 0 works
  if (op == A_ASSIGN && ptrtype(rtype) && inttype(ltype)) {
    if (tree->intvalue == 0) {
      return (mkastunary(A_WIDEN, P_LONG, NULL, tree, NULL, 0));
    }
  }

  // allow `if (ptr == 0) {...}` and `return NULL`
  if (op == A_EQ || op == A_NE || op == A_RETURN && (ptrtype(rtype) || ptrtype(ltype))) {
    if (inttype(rtype) || inttype(ltype)) {
      return (tree);
    }
  }

  // If we get here, the types are not compatible
  return (NULL);
}

// Given a primitive type, return
// the type which is a pointer to it
int pointer_to(int type) {
  // too many levels of indirection
  if ((type & P_PTR_BITS) == P_PTR_BITS) {
    fatald("Unrecognised in pointer_to: type", type);
  }
  return (type + 1);
}

int value_at(int type) {
  if ((type & 0xf) == 0x0) {
    fatald("Unrecognised in value_at: type", type);
  }
  return (type - 1);
}

// Given a type and a composite type pointer, return
// the size of this type in bytes
int typesize(int type, struct symtable *ctype) {
  if (type == P_STRUCT || type == P_UNION)
    return (ctype->size);
  return (genprimsize(type));
}
