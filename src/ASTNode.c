/**
 * Filename    : ASTNode.c
 * Description : Abstract Syntax Tree (AST) for representing formula trees
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-05-02
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/common.h"
#include "sbml/ASTNode.h"


/**
 * Internal helper functions for ASTNode_canonicalize().
 */
int ASTNode_canonicalizeConstant   (ASTNode_t *node);
int ASTNode_canonicalizeFunction   (ASTNode_t *node);
int ASTNode_canonicalizeFunctionL1 (ASTNode_t *node);
int ASTNode_canonicalizeLogical    (ASTNode_t *node);
int ASTNode_canonicalizeRelational (ASTNode_t *node);


/**
 * ASTNodeType predicates
 */
#define ASTNodeType_isConstant(t) \
  ((t >= AST_CONSTANT_E) && (t <= AST_CONSTANT_TRUE))

#define ASTNodeType_isFunction(t) \
  ((t >= AST_FUNCTION) && (t <= AST_FUNCTION_TANH))

#define ASTNodeType_isLambda(t) \
  (t == AST_LAMBDA)

#define ASTNodeType_isLogical(t) \
  ((t >= AST_LOGICAL_AND) && (t <= AST_LOGICAL_XOR))

#define ASTNodeType_isName(t) \
  ((t >= AST_NAME) && (t <= AST_NAME_TIME))

#define ASTNodeType_isRelational(t) \
  ((t >= AST_RELATIONAL_EQ) && (t <= AST_RELATIONAL_NEQ))

#define ASTNodeType_isInteger(t) \
  (t == AST_INTEGER)

#define ASTNodeType_isRational(t) \
  (t == AST_RATIONAL)

#define ASTNodeType_isReal(t) \
  ((t >= AST_REAL) && (t <= AST_RATIONAL))

#define ASTNodeType_isNumber(t) \
  (ASTNodeType_isInteger(t) || ASTNodeType_isReal(t))

#define ASTNodeType_isOperator(t) \
  ( ( t == AST_PLUS   ) || \
    ( t == AST_MINUS  ) || \
    ( t == AST_TIMES  ) || \
    ( t == AST_DIVIDE ) || \
    ( t == AST_POWER  ) )

#define ASTNodeType_isUnknown(t) \
  (t == AST_UNKNOWN)


/**
 * String Constants
 */
static const char *AST_LAMBDA_STRING = "lambda";

static const char *AST_CONSTANT_STRINGS[] =
{
    "exponentiale"
  , "false"
  , "pi"
  , "true"
};

static const char *AST_FUNCTION_STRINGS[] =
{
    "abs"
  , "arccos"
  , "arccosh"
  , "arccot"
  , "arccoth"
  , "arccsc"
  , "arccsch"
  , "arcsec"
  , "arcsech"
  , "arcsin"
  , "arcsinh"
  , "arctan"
  , "arctanh"
  , "ceiling"
  , "cos"
  , "cosh"
  , "cot"
  , "coth"
  , "csc"
  , "csch"
  , "exp"
  , "factorial"
  , "floor"
  , "ln"
  , "log"
  , "piecewise"
  , "power"
  , "root"
  , "sec"
  , "sech"
  , "sin"
  , "sinh"
  , "tan"
  , "tanh"
};


static const char *AST_LOGICAL_STRINGS[] =
{
    "and"
  , "not"
  , "or"
  , "xor"
};


static const char *AST_RELATIONAL_STRINGS[] =
{
    "eq"
  , "geq"
  , "gt"
  , "leq"
  , "lt"
  , "neq"
};


/**
 * Creates a new ASTNode and returns a pointer to it.  The returned node
 * will have a type of AST_UNKNOWN and should be set to something else as
 * soon as possible.
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_create (void)
{
  ASTNode_t *node = (ASTNode_t *) safe_calloc(1, sizeof(ASTNode_t));


  node->type     = AST_UNKNOWN;
  node->children = List_create();

  return node;
}


/**
 * Creates a new ASTNode and sets its type to the given ASTNodeType.
 */
ASTNode_t *
ASTNode_createWithType (ASTNodeType_t type)
{
  ASTNode_t *node = ASTNode_create();


  ASTNode_setType(node, type);
  return node;
}


/**
 * Creates a new ASTNode from the given Token and returns a pointer to it.
 * The returned ASTNode will contain the same data as the Token.
 */
ASTNode_t *
ASTNode_createFromToken (Token_t *token)
{
  ASTNode_t *node = ASTNode_create();


  ASTNode_copyFromToken(node, token);
  return node;
}


/**
 * Copies the data from the given token to this ASTNode.
 */
void
ASTNode_copyFromToken (ASTNode_t *node, Token_t *token)
{
  if (token->type == TT_NAME)
  {
    ASTNode_setName(node, token->value.name);
  }
  else if (token->type == TT_INTEGER)
  {
    ASTNode_setInteger(node, token->value.integer);
  }
  else if (token->type == TT_REAL)
  {
    ASTNode_setReal(node, token->value.real);
  }
  else if (token->type == TT_REAL_E)
  {
    ASTNode_setRealWithExponent(node, token->value.real, token->exponent);
  }
  else
  {
    ASTNode_setCharacter(node, token->value.ch);
  }
}


/**
 * Frees the given ASTNode including any child nodes.
 */
LIBSBML_EXTERN
void
ASTNode_free (ASTNode_t *node)
{
  if (node == NULL) return;


  ASTNode_freeName(node);

  List_freeItems(node->children, ASTNode_free, ASTNode_t);
  List_free(node->children);

  safe_free(node);
}


/**
 * Frees the name of this ASTNode and sets it to NULL, if appropriate,
 * i.e. the node is not an operator, number or AST_UNKNOWN.
 */
void
ASTNode_freeName (ASTNode_t *node)
{
  ASTNodeType_t type = node->type;


  if (node == NULL) return;

  if ( !(ASTNodeType_isOperator(type) ||
         ASTNodeType_isNumber  (type) ||
         ASTNodeType_isUnknown (type)) )
  {
    if (node->value.name != NULL)
    {
      safe_free(node->value.name);
      node->value.name = NULL;
    }
  }
}


/**
 * Attempts to convert this ASTNode to a canonical form and returns true
 * (non-zero) if the conversion succeeded, false (0) otherwise.
 *
 * The rules determining the canonical form conversion are as follows:
 *
 *   1. If the node type is AST_NAME and the node name matches
 *   "ExponentialE", "Pi", "True" or "False" the node type is converted to
 *   the corresponding AST_CONSTANT type.
 *
 *   2. If the node type is an AST_FUNCTION and the node name matches an L1
 *   or L2 (MathML) function name, logical operator name, or relational
 *   operator name, the node is converted to the correspnding AST_FUNCTION,
 *   AST_LOGICAL or AST_CONSTANT type.
 *
 * L1 function names are searched first, so canonicalizing "log" will
 * result in a node type of AST_FUNCTION_LN (see L1 Specification,
 * Appendix C).
 *
 * Some canonicalizations result in a structural converion of the nodes (by
 * adding a child).  For example, a node with L1 function name "sqr" and a
 * single child node (the argument) will be transformed to a node of type
 * AST_FUNCTION_POWER with two children.  The first child will remain
 * unchanged, but the second child will be an ASTNode of type AST_INTEGER
 * and a value of 2.  The function names that result in structural changes
 * are: log10, sqr and sqrt.
 */
LIBSBML_EXTERN
int
ASTNode_canonicalize (ASTNode_t *node)
{
  int found = 0;


  if (node->type == AST_NAME)
  {
    found = ASTNode_canonicalizeConstant(node);
  }

  if (!found && node->type == AST_FUNCTION)
  {
    found = ASTNode_canonicalizeFunction(node);

    if (!found)
    {
      found = ASTNode_canonicalizeLogical(node);
    }

    if (!found)
    {
      found = ASTNode_canonicalizeRelational(node);
    }
  }

  return found;
}


/**
 * Internal helper function for ASTNode_canonicalize().
 */
int
ASTNode_canonicalizeConstant (ASTNode_t *node)
{
  const int  first = AST_CONSTANT_E;
  const int  last  = AST_CONSTANT_TRUE;
  const int  size  = last - first + 1;
  const char *name = node->value.name;

  int index;
  int found;


  index = util_bsearchStringsI(AST_CONSTANT_STRINGS, name, 0, size - 1);
  found = (index < size);

  if (found)
  {
    ASTNode_setType(node, first + index);
  }

  return found;
}


/**
 * Internal helper function for ASTNode_canonicalize().
 */
int
ASTNode_canonicalizeFunction (ASTNode_t *node)
{
  const int  first = AST_FUNCTION_ABS;
  const int  last  = AST_FUNCTION_TANH;
  const int  size  = last - first + 1;
  const char *name = node->value.name;

  int index;
  int found;


  /**
   * Search for SBML Level 1 function names first.
   */
  found = ASTNode_canonicalizeFunctionL1(node);

  /**
   * Now Lambda...
   */
  if (!found)
  {
    if ( (found = !strcmp_insensitive(name, AST_LAMBDA_STRING)) )
    {
      ASTNode_setType(node, AST_LAMBDA);
    }
  }

  /**
   * ... and finally the L2 (MathML) function names.
   */
  if (!found)
  {
    index = util_bsearchStringsI(AST_FUNCTION_STRINGS, name, 0, size - 1);
    found = (index < size);

    if (found)
    {
      ASTNode_setType(node, first + index);
    }
  }

  return found;
}


/**
 * Internal helper function for ASTNode_canonicalize().
 */
int
ASTNode_canonicalizeFunctionL1 (ASTNode_t *node)
{
  const char *name = node->value.name;
  ASTNode_t  *child;


  if ( !strcmp_insensitive(name, "acos") )
  {
    ASTNode_setType(node, AST_FUNCTION_ARCCOS);
  }
  else if ( !strcmp_insensitive(name, "asin") )
  {
    ASTNode_setType(node, AST_FUNCTION_ARCSIN);
  }
  else if ( !strcmp_insensitive(name, "atan") )
  {
    ASTNode_setType(node, AST_FUNCTION_ARCTAN);
  }
  else if ( !strcmp_insensitive(name, "ceil") )
  {
    ASTNode_setType(node, AST_FUNCTION_CEILING);
  }

  /**
   * "log(x)" in L1 is represented as "ln(x)" in L2.
   *
   * Notice, however, that the conversion is performed only ifthe number of
   * arguments is 1.  Thus "log(5, x)" will still be "log(5, x) when passed
   * through this filter.
   */
  else if ( !strcmp_insensitive(name, "log") &&
            (ASTNode_getNumChildren(node) == 1) )
  {
    ASTNode_setType(node, AST_FUNCTION_LN);
  }

  /**
   * "log10(x)" in L1 is represented as "log(10, x)" in L2.
   */
  else if ( !strcmp_insensitive(name, "log10") &&
            (ASTNode_getNumChildren(node) == 1) )
  {
    ASTNode_setType(node, AST_FUNCTION_LOG);

    child = ASTNode_create();
    ASTNode_setInteger(child, 10);

    ASTNode_prependChild(node, child);
  }

  /**
   * Here we set the type to AST_FUNCTION_POWER.  We could set it to
   * AST_POWER, but then we would loose the idea that it was a function
   * before it was canonicalized.
   */
  else if ( !strcmp_insensitive(name, "pow") )
  {
    ASTNode_setType(node, AST_FUNCTION_POWER);
  }

  /**
   * "sqr(x)" in L1 is represented as "power(x, 2)" in L2.
   */
  else if ( !strcmp_insensitive(name, "sqr") &&
            (ASTNode_getNumChildren(node) == 1) )
  {
    ASTNode_setType(node, AST_FUNCTION_POWER);

    child = ASTNode_create();
    ASTNode_setInteger(child, 2);

    ASTNode_addChild(node, child);
  }

  /**
   * "sqrt(x) in L1 is represented as "root(2, x)" in L1.
   */
  else if ( !strcmp_insensitive(name, "sqrt") &&
            (ASTNode_getNumChildren(node) == 1) )
  {
    ASTNode_setType(node, AST_FUNCTION_ROOT);

    child = ASTNode_create();
    ASTNode_setInteger(child, 2);

    ASTNode_prependChild(node, child);
  }

  /**
   * Was a conversion performed?
   */
  return (node->type != AST_FUNCTION);
}


/**
 * Internal helper function for ASTNode_canonicalize().
 */
int
ASTNode_canonicalizeLogical (ASTNode_t *node)
{
  const int  first = AST_LOGICAL_AND;
  const int  last  = AST_LOGICAL_XOR;
  const int  size  = last - first + 1;
  const char *name = node->value.name;

  int index;
  int found;


  index = util_bsearchStringsI(AST_LOGICAL_STRINGS, name, 0, size - 1);
  found = (index < size);

  if (found)
  {
    ASTNode_setType(node, first + index);
  }

  return found;
}


/**
 * Internal helper function for ASTNode_canonicalize().
 */
int
ASTNode_canonicalizeRelational (ASTNode_t *node)
{
  const int  first = AST_RELATIONAL_EQ;
  const int  last  = AST_RELATIONAL_NEQ;
  const int  size  = last - first + 1;
  const char *name = node->value.name;

  int index;
  int found;


  index = util_bsearchStringsI(AST_RELATIONAL_STRINGS, name, 0, size - 1);
  found = (index < size);

  if (found)
  {
    ASTNode_setType(node, first + index);
  }

  return found;
}


/**
 * Adds the given node as a child of this ASTNode.  Child nodes are added
 * in-order from "left-to-right".
 */
LIBSBML_EXTERN
void
ASTNode_addChild (ASTNode_t *node, ASTNode_t *child)
{
  List_add(node->children, child);
}

/**
 * Adds the given node as a child of this ASTNode.  This method adds child
 * nodes from "right-to-left".
 */
LIBSBML_EXTERN
void
ASTNode_prependChild (ASTNode_t *node, ASTNode_t *child)
{
  List_prepend(node->children, child);
}


/**
 * @return the nth child of this ASTNode or NULL if this node has no nth
 * child (n > ASTNode_getNumChildren() - 1).
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getChild (const ASTNode_t *node, unsigned int n)
{
  return List_get(node->children, n);
}


/**
 * @return the left child of this ASTNode.  This is equivalent to
 * ASTNode_getChild(node, 0);
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getLeftChild (const ASTNode_t *node)
{
  return List_get(node->children, 0);
}


/**
 * @return the right child of this ASTNode or NULL if this node has no
 * right child.  If ASTNode_getNumChildren(node) > 1, then this is
 * equivalent to:
 *
 *   ASTNode_getChild(node, ASTNode_getNumChildren(node) - 1);
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getRightChild (const ASTNode_t *node)
{
  unsigned int numChildren = List_size(node->children);


  return (numChildren > 1) ? List_get(node->children, numChildren - 1) : NULL;
}


/**
 * @return the number of children of this ASTNode or 0 is this node has no
 * children.
 */
LIBSBML_EXTERN
unsigned int
ASTNode_getNumChildren (const ASTNode_t *node)
{
  return List_size(node->children);
}


/**
 * Performs a depth-first search (DFS) of the tree rooted at node and
 * returns the List of nodes where predicate(node) returns true.
 *
 * The typedef for ASTNodePredicate is:
 *
 *   int (*ASTNodePredicate) (const ASTNode_t *node);
 *
 * where a return value of non-zero represents true and zero represents
 * false.
 *
 * The List returned is owned by the caller and should be freed with
 * List_free().  The ASTNodes in the list, however, are not owned by the
 * caller (as they still belong to the tree itself) and therefore should
 * not be freed.  That is, do not call List_freeItems().
 */
LIBSBML_EXTERN
List_t *
ASTNode_getListOfNodes (const ASTNode_t *node, ASTNodePredicate predicate)
{
  List_t *list = List_create();


  ASTNode_fillListOfNodes(node, predicate, list);

  return list;
}


/**
 * This method is identical in functionality to ASTNode_getListOfNodes(),
 * except the List is passed-in by the caller.
 */
LIBSBML_EXTERN
void
ASTNode_fillListOfNodes ( const ASTNode_t  *node,
                          ASTNodePredicate predicate,
                          List_t           *list )
{
  unsigned int numChildren = ASTNode_getNumChildren(node);
  unsigned int c;


  if (predicate(node) != 0)
  {
    List_add(list, (ASTNode_t *) node);
  }

  for (c = 0; c < numChildren; c++)
  {
    ASTNode_fillListOfNodes( ASTNode_getChild(node, c), predicate, list );
  }
}


/**
 * @return the value of this ASTNode as a single character.  This function
 * should be called only when ASTNode_getType() is one of AST_PLUS,
 * AST_MINUS, AST_TIMES, AST_DIVIDE or AST_POWER.
 */
LIBSBML_EXTERN
char
ASTNode_getCharacter (const ASTNode_t *node)
{
  return node->value.ch;
}


/**
 * @return the value of this ASTNode as a (long) integer.  This function
 * should be called only when ASTNode_getType() == AST_INTEGER.
 */
LIBSBML_EXTERN
long
ASTNode_getInteger (const ASTNode_t *node)
{
  return node->value.integer;
}


/**
 * @return the value of this ASTNode as a string.  This function may be
 * called on nodes that are not operators (ASTNode_isOperator(node) == 0)
 * or numbers (ASTNode_isNumber(node) == 0).
 */
LIBSBML_EXTERN
const char *
ASTNode_getName (const ASTNode_t *node)
{
  ASTNodeType_t type  = node->type;
  const char    *name = node->value.name;


  /**
   * If the node does not have a name and is not a user-defined function
   * (type == AST_FUNCTION), use the default name for the builtin node
   * types.
   */
  if (name == NULL && type != AST_FUNCTION)
  {
    if (ASTNodeType_isConstant(type))
    {
      name = AST_CONSTANT_STRINGS[ type - AST_CONSTANT_E ];
    }
    else if (ASTNodeType_isLambda(type))
    {
      name = AST_LAMBDA_STRING;
    }
    else if (ASTNodeType_isFunction(type))
    {
      name = AST_FUNCTION_STRINGS[ type - AST_FUNCTION_ABS ];
    }
    else if (ASTNodeType_isLogical(type))
    {
      name = AST_LOGICAL_STRINGS[ type - AST_LOGICAL_AND ];
    }
    else if (ASTNodeType_isRelational(type))
    {
      name = AST_RELATIONAL_STRINGS[ type - AST_RELATIONAL_EQ ];
    }
  }

  return name;
}


/**
 * @return the value of the numerator of this ASTNode.  This function
 * should be called only when ASTNode_getType() == AST_RATIONAL.
 */
LIBSBML_EXTERN
long
ASTNode_getNumerator (const ASTNode_t *node)
{
  return node->value.integer;
}


/**
 * @return the value of the denominator of this ASTNode.  This function
 * should be called only when ASTNode_getType() == AST_RATIONAL.
 */
LIBSBML_EXTERN
long
ASTNode_getDenominator (const ASTNode_t *node)
{
  return node->extra.denominator;
}


/**
 * @return the value of this ASTNode as a real (double).  This function
 * should be called only when ASTNode_isReal(node) != 0.
 *
 * This function performs the necessary arithmetic if the node type is
 * AST_REAL_E (mantissa * 10^exponent) or AST_RATIONAL
 * (numerator / denominator).
 */
LIBSBML_EXTERN
double
ASTNode_getReal (const ASTNode_t *node)
{
  double result = node->value.real;


  if (node->type == AST_REAL_E)
  {
    result *= pow(10,  node->extra.exponent);
  }
  else if (node->type == AST_RATIONAL)
  {
    result = (double) node->value.integer / node->extra.denominator;
  }

  return result;
}


/**
 * @return the value of the mantissa of this ASTNode.  This function should
 * be called only when ASTNode_getType() is AST_REAL_E or AST_REAL.  If
 * AST_REAL, this method is identical to ASTNode_getReal().
 */
LIBSBML_EXTERN
double
ASTNode_getMantissa (const ASTNode_t *node)
{
  return node->value.real;
}


/**
 * @return the value of the exponent of this ASTNode.  This function should
 * be called only when ASTNode_getType() is AST_REAL_E or AST_REAL.
 */
LIBSBML_EXTERN
long
ASTNode_getExponent (const ASTNode_t *node)
{
  return node->extra.exponent;
}


/**
 * @return the precedence of this ASTNode (as defined in the SBML L1
 * specification).
 */
LIBSBML_EXTERN
int
ASTNode_getPrecedence (const ASTNode_t *node)
{
  int precedence;


  if (ASTNode_isUMinus(node))
  {
    precedence = 5;
  }
  else
  {
    switch (node->type)
    {
      case AST_PLUS:
      case AST_MINUS:
        precedence = 2;
        break;

      case AST_DIVIDE:
      case AST_TIMES:
        precedence = 3;
        break;

      case AST_POWER:
        precedence = 4;
        break;

      default:
        precedence = 6;
        break;
    }
  }

  return precedence;
}


/**
 * @return the type of this ASTNode.
 */
LIBSBML_EXTERN
ASTNodeType_t
ASTNode_getType (const ASTNode_t *node)
{
  return node->type;
}


/**
 * @return true (non-zero) if this ASTNode is a MathML constant (true,
 * false, pi, exponentiale), false (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isConstant (const ASTNode_t *node)
{
  return ASTNodeType_isConstant(node->type);
}


/**
 * @return true (non-zero) if this ASTNode is a function in SBML L1, L2
 * (MathML) (everything from abs() to tanh()) or user-defined, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isFunction (const ASTNode_t *node)
{
  return ASTNodeType_isFunction(node->type);
}


/**
 * @return true (non-zero) if this ASTNode is of type AST_INTEGER, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isInteger (const ASTNode_t *node)
{
  return ASTNodeType_isInteger(node->type);
}


/**
 * @return true (non-zero) if this ASTNode is of type AST_LAMBDA, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isLambda (const ASTNode_t *node)
{
  return ASTNodeType_isLambda(node->type);
}

/**
 * @return true (non-zero) if the given ASTNode represents a log10()
 * function, false (0) otherwise.
 *
 * More precisley, the node type is AST_FUNCTION_LOG with two children the
 * first of which is an AST_INTEGER equal to 10.
 */
LIBSBML_EXTERN
int
ASTNode_isLog10 (const ASTNode_t *node)
{
  int        result = 0;
  ASTNode_t *c;


  if (node->type == AST_FUNCTION_LOG)
  {
    if (ASTNode_getNumChildren(node) == 2)
    {
      c = ASTNode_getLeftChild(node);

      if ((c->type == AST_INTEGER) && (c->value.integer == 10))
      {
        result = 1;
      }
    }
  }

  return result;
}


/**
 * @return true (non-zero) if this ASTNode is a MathML logical operator
 * (and, or, not, xor), false (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isLogical (const ASTNode_t *node)
{
  return ASTNodeType_isLogical(node->type);
}


/**
 * @return true (non-zero) if this ASTNode is a user-defined variable name
 * in SBML L1, L2 (MathML) or the special symbols delay or time, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isName (const ASTNode_t *node)
{
  return ASTNodeType_isName(node->type);
}


/**
 * @return true (non-zero) if this ASTNode is a number, false (0)
 * otherwise.
 *
 * This is functionally equivalent to:
 *
 *   ASTNode_isInteger(node) || ASTNode_isReal(node).
 */
LIBSBML_EXTERN
int
ASTNode_isNumber (const ASTNode_t *node)
{
  return ASTNodeType_isNumber(node->type);
}


/**
 * @return true (non-zero) if this ASTNode is an operator, false (0)
 * otherwise.  Operators are: +, -, *, / and ^ (power).
 */
LIBSBML_EXTERN
int
ASTNode_isOperator (const ASTNode_t *node)
{
  return ASTNodeType_isOperator(node->type);
}


/**
 * @return true (non-zero) if this ASTNode is of type AST_RATIONAL, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isRational (const ASTNode_t *node)
{
  return ASTNodeType_isRational(node->type);
}


/**
 * @return true (non-zero) if the value of this ASTNode can represented as
 * a real number, false (0) otherwise.
 *
 * To be a represented as a real number, this node must be of one of the
 * following types: AST_REAL, AST_REAL_E or AST_RATIONAL.
 */
LIBSBML_EXTERN
int
ASTNode_isReal (const ASTNode_t *node)
{
  return ASTNodeType_isReal(node->type);
}


/**
 * @return true (non-zero) if this ASTNode is a MathML relational operator
 * (==, >=, >, <=, < !=), false (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isRelational (const ASTNode_t *node)
{
  return ASTNodeType_isRelational(node->type);
}


/**
 * @return true (non-zero) if the given ASTNode represents a sqrt()
 * function, false (0) otherwise.
 *
 * More precisley, the node type is AST_FUNCTION_ROOT with two children the
 * first of which is an AST_INTEGER equal to 2.
 */
LIBSBML_EXTERN
int
ASTNode_isSqrt (const ASTNode_t *node)
{
  int        result = 0;
  ASTNode_t *c;


  if (node->type == AST_FUNCTION_ROOT)
  {
    if (ASTNode_getNumChildren(node) == 2)
    {
      c = ASTNode_getLeftChild(node);

      if ((c->type == AST_INTEGER) && (c->value.integer == 2))
      {
        result = 1;
      }
    }
  }

  return result;
}


/**
 * @return true (non-zero) if this ASTNode is a unary minus, false (0)
 * otherwise.
 *
 * For numbers, unary minus nodes can be "collapsed" by negating the
 * number.  In fact, SBML_parseFormula() does this during its parse.
 * However, unary minus nodes for symbols (AST_NAMES) cannot be
 * "collapsed", so this predicate function is necessary.
 *
 * A node is defined as a unary minus node if it is of type AST_MINUS and
 * has exactly one child.
 */
LIBSBML_EXTERN
int
ASTNode_isUMinus (const ASTNode_t *node)
{
  int uminus = 0;


  if (node->type == AST_MINUS)
  {
    if (ASTNode_getNumChildren(node) == 1)
    {
      uminus = 1;
    }
  }

  return uminus;
}


/**
 * @return true (non-zero) if this ASTNode is of type AST_UNKNOWN, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isUnknown (const ASTNode_t *node)
{
  return ASTNodeType_isUnknown(node->type);
}


/**
 * Sets the value of this ASTNode to the given character.  If character is
 * one of '+', '-', '*', '/' or '^', the node type will be set accordingly.
 * For all other characters, the node type will be set to AST_UNKNOWN.
 */
LIBSBML_EXTERN
void
ASTNode_setCharacter (ASTNode_t *node, char value)
{
  ASTNode_setType(node, value);
  node->value.ch = value;
}


/**
 * Sets the value of this ASTNode to the given name.
 *
 * The node type will be set (to AST_NAME) ONLY IF the ASTNode was
 * previously an operator (ASTNode_isOperator(node) != 0) or number
 * (ASTNode_isNumber(node) != 0).  This allows names to be set for
 * AST_FUNCTIONs and the like.
 */
LIBSBML_EXTERN
void
ASTNode_setName (ASTNode_t *node, const char *name)
{
  if (node->value.name == name) return;

  if ( ASTNode_isOperator(node) || ASTNode_isNumber(node) ||
       ASTNodeType_isUnknown(node->type) )
  {
    node->type = AST_NAME;
  }

  ASTNode_freeName(node);
  node->value.name = (name == NULL) ? NULL : safe_strdup(name);
}


/**
 * Sets the value of this ASTNode to the given (long) integer and sets the
 * node type to AST_INTEGER.
 */
LIBSBML_EXTERN
void
ASTNode_setInteger (ASTNode_t *node, long value)
{
  ASTNode_setType(node, AST_INTEGER);
  node->value.integer = value;
}


/**
 * Sets the value of this ASTNode to the given rational in two parts:
 * the numerator and denominator.  The node type is set to AST_RATIONAL.
 */
LIBSBML_EXTERN
void
ASTNode_setRational (ASTNode_t *node, long numerator, long denominator)
{
  ASTNode_setType(node, AST_RATIONAL);

  node->value.integer     = numerator;
  node->extra.denominator = denominator;
}


/**
 * Sets the value of this ASTNode to the given real (double) and sets the
 * node type to AST_REAL.
 *
 * This is functionally equivalent to:
 *
 *   ASTNode_setRealWithExponent(node, value, 0);
 */
LIBSBML_EXTERN
void
ASTNode_setReal (ASTNode_t *node, double value)
{
  ASTNode_setType(node, AST_REAL);

  node->value.real     = value;
  node->extra.exponent = 0;
}


/**
 * Sets the value of this ASTNode to the given real (double) in two parts:
 * the mantissa and the exponent.  The node type is set to AST_REAL_E.
 */
LIBSBML_EXTERN
void
ASTNode_setRealWithExponent (ASTNode_t *node, double mantissa, long exponent)
{
  ASTNode_setType(node, AST_REAL_E);

  node->value.real     = mantissa;
  node->extra.exponent = exponent;
}


/**
 * Sets the type of this ASTNode to the given ASTNodeType.
 */
LIBSBML_EXTERN
void
ASTNode_setType (ASTNode_t *node, ASTNodeType_t type)
{
  if (node->type == type) return;


  if (ASTNode_isOperator(node) || ASTNode_isNumber(node))
  {
    node->value.integer = 0;
  }

  /**
   * Free name only if the ASTNodeType is being set to something that
   * cannot contain a string.
   *
   * Note: ASTNode_freeName() will only free value.name if there is
   * something to be freed.
   */
  if (ASTNodeType_isOperator(type) || ASTNodeType_isNumber(type))
  {
    ASTNode_freeName(node);
  }

  if (ASTNodeType_isOperator(type))
  {
    node->type     = type;
    node->value.ch = (char) type;
  }
  else if ((type >= AST_INTEGER) && (type < AST_UNKNOWN))
  {
    node->type = type;
  }
  else
  {
    node->type = AST_UNKNOWN;
  }
}
