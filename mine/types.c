#include "defs.h"
#include "decl.h"

// NOTE: allocates strings, potentially a lot
char *typename(int ptype, struct symtable *ctype) {
  if (ptrtype(ptype)) {
    int valuetype = value_at(ptype);
    char *valuename = typename(valuetype, ctype);
    return (str_concat(valuename, "*"));
  }
  switch (ptype) {
  case P_CHAR:
    return (strdup("char"));
  case P_INT:
    return (strdup("int"));
  case P_LONG:
    return (strdup("long"));
  case P_VOID:
    return (strdup("void"));
  case P_STRUCT:
    if (ctype && ctype->name)
      return (str_concat("struct ", ctype->name));
    else
      return (str_concat("struct ", "?"));
  case P_UNION:
    if (ctype && ctype->name)
      return (str_concat("union ", ctype->name));
    else
      return (str_concat("union ", "?"));
  default:
    fatald("Invalid typename", ptype);
  }
  return (NULL);
}

int inttype(int ptype) {
  int nonptr = (ptype & P_PTR_BITS) == 0;
  return (nonptr && (ptype >= P_CHAR && ptype <= P_LONG));
}

int ptrtype(int ptype) {
  return ((ptype & P_PTR_BITS) != 0);
}

static int int_fits_size(int intval, int bytesize, int unsignd) {
  int minsz;
  int maxsz;
  if (unsignd) {
    minsz = 0;
    maxsz = (1<<bytesize) - 1;
  } else {
    minsz = -(1<<bytesize - 1);
    maxsz = (1<<bytesize) - 1;
  }
  return (intval >= minsz && intval <= maxsz);
}

// Given an AST tree and a type which we want it to become,
// possibly modify the tree by widening or scaling so that
// it is compatible with this type. Return the original tree
// if no changes occurred, a modified tree, or NULL if the
// tree is not compatible with the given type.
// If this will be part of a binary operation, the AST op is not zero.
struct ASTnode *modify_type(struct ASTnode *rtree, int ltype,
    struct symtable *lctype, int op) {

  int rtype = rtree->type;
  int lsize, rsize;

  // any types will do here, ex: `if (ptr->name && !strcmp(ptr->name, "John"))`
  // has types `char* and char (bool)`
  if (op == A_LOGOR || op == A_LOGAND) {
    return (rtree);
  }

  // Compare scalar int types
  if (inttype(ltype) && inttype(rtype)) {

    // Both types same, nothing to do
    if (ltype == rtype) return (rtree);

    // Get the sizes for each type
    lsize = genprimsize(ltype);
    rsize = genprimsize(rtype);

    // ex: (char) = (int) is NOT safe, truncation can occur
    if (rsize > lsize) {
      // safe cast in this case
      if (rtree->op == A_INTLIT && int_fits_size(rtree->intvalue, lsize, 0)) {
        return (mkastunary(A_CAST, ltype, lctype, rtree, NULL, 0));
      }
      return (NULL);
    }

    // Widen to the right, ex: (int) = (char) -> (int) = (int) [char to int conversion]
    if (lsize > rsize) {
      return (mkastunary(A_WIDEN, ltype, lctype, rtree, NULL, 0));
    }
  }

  if (ptrtype(ltype) && ptrtype(rtype)) {
    // We can compare them
    if (op >= A_EQ && op <= A_GE)
      return (rtree);

    if (op == A_SUBTRACT) // ptr difference (p1-p2)
      return (rtree);

    if (op == A_ASSIGN && ltype == rtype) return (rtree);

    // A comparison of the same type for a non-binary operation is OK,
    // or when the left tree is of  `void *` type.
    // ex: char *str = (void*)0;
    //     char *mem = malloc(1024);
    if (!isbinastop(op) && (ltype == rtype || ltype == pointer_to(P_VOID) ||
          rtype == pointer_to(P_VOID))) {
      return (rtree);
    }
  }

  // We can scale only on A_ADD or A_SUBTRACT operation
  if (op == A_ADD || op == A_SUBTRACT || op == A_AS_ADD || op == A_AS_SUBTRACT) {

    // right is int type, left is pointer type and the size
    // of the left's element type is > 1 (char): scale the right
    if (inttype(rtype) && ptrtype(ltype)) {
      lsize = genprimsize(value_at(ltype));
      if (lsize > 1) {
        return (mkastunary(A_SCALE, rtype, lctype, rtree, NULL, lsize));
      } else {
        return (rtree); // Size 1, no need to scale
      }
    }
  }

  // so that char *ptr = 0 works
  if (op == A_ASSIGN && ptrtype(ltype) && inttype(rtype)) {
    if (rtree->intvalue == 0) {
      return (mkastunary(A_WIDEN, P_LONG, NULL, rtree, NULL, 0));
    }
  }

  // allow `if (ptr == 0) {...}` and `return NULL`
  if (op == A_EQ || op == A_NE || op == A_RETURN && (ptrtype(rtype) || ptrtype(ltype))) {
    if (inttype(rtype) || inttype(ltype)) {
      return (rtree);
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
  if (type == P_STRUCT || type == P_UNION) {
    ASSERT(ctype);
    return (ctype->size);
  }
  return (genprimsize(type));
}

// Is the given type a primitive type?
int primtype(int type) {
  if (ptrtype(type)) return (1);
  switch (type) {
    case P_CHAR:
    case P_INT:
    case P_LONG:
      return (1);
  }
  return (0);
}
