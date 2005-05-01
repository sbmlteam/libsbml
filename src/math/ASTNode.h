/**
 * \file    ASTNode.h
 * \brief   Abstract Syntax Tree (AST) for representing formula trees
 * \author  SBML Team <sbml-team@caltech.edu>
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef ASTNode_h
#define ASTNode_h


#include "common/extern.h"
#include "common/sbmlfwd.h"

#include "FormulaTokenizer.h"
#include "ASTNodeType.h"


/**
 * ASTNodePredicate
 *
 * This is a typedef for a pointer to a function that takes an ASTNode and
 * returns true (non-zero) or false (0).
 *
 * @see ASTNode_getListOfNodes(), ASTNode_fillListOfNodes()
 */
typedef int (*ASTNodePredicate) (const ASTNode_t *node);


#ifdef __cplusplus


class List;


class ASTNode
{
public:

  /**
   * Creates a new ASTNode.
   *
   * By default, node will have a type of AST_UNKNOWN and should be set to
   * something else as soon as possible.
   */
  LIBSBML_EXTERN
  ASTNode (ASTNodeType_t type = AST_UNKNOWN);

  /**
   * Creates a new ASTNode from the given Token.
   *
   * The ASTNode will contain the same data as the Token.
   */
  ASTNode (Token_t *token);

  /**
   * Destroys this ASTNode including any child nodes.
   */
  LIBSBML_EXTERN
  virtual ~ASTNode ();


  /**
   * Frees the name of this ASTNode and sets it to NULL, if appropriate,
   * i.e. the node is not an operator, number or AST_UNKNOWN.
   */
  void
  freeName ();

  /**
   * Attempts to convert this ASTNode to a canonical form and returns true
   * if the conversion succeeded, false otherwise.
   *
   * The rules determining the canonical form conversion are as follows:
   *
   *   1. If the node type is AST_NAME and the node name matches
   *   "ExponentialE", "Pi", "True" or "False" the node type is converted
   *   to the corresponding AST_CONSTANT type.
   *
   *   2. If the node type is an AST_FUNCTION and the node name matches an
   *   L1 or L2 (MathML) function name, logical operator name, or
   *   relational operator name, the node is converted to the correspnding
   *   AST_FUNCTION, AST_LOGICAL or AST_CONSTANT type.
   *
   * L1 function names are searched first, so canonicalizing "log" will
   * result in a node type of AST_FUNCTION_LN (see L1 Specification,
   * Appendix C).
   *
   * Some canonicalizations result in a structural converion of the nodes
   * (by adding a child).  For example, a node with L1 function name "sqr"
   * and a single child node (the argument) will be transformed to a node
   * of type AST_FUNCTION_POWER with two children.  The first child will
   * remain unchanged, but the second child will be an ASTNode of type
   * AST_INTEGER and a value of 2.  The function names that result in
   * structural changes are: log10, sqr and sqrt.
   */
  LIBSBML_EXTERN
  bool canonicalize ();

  /**
   * Adds the given node as a child of this ASTNode.  Child nodes are added
   * in-order from "left-to-right".
   */
  LIBSBML_EXTERN
  void addChild (ASTNode* child);

  /**
   * Adds the given node as a child of this ASTNode.  This method adds
   * child nodes from "right-to-left".
   */
  LIBSBML_EXTERN
  void prependChild (ASTNode* child);

  /**
   * @return the nth child of this ASTNode or NULL if this node has no nth
   * child (n > ASTNode_getNumChildren() - 1).
   */
  LIBSBML_EXTERN
  ASTNode* getChild (unsigned int n) const;

  /**
   * @return the left child of this ASTNode.  This is equivalent to
   * getChild(0);
   */
  LIBSBML_EXTERN
  ASTNode*
  getLeftChild () const;

  /**
   * @return the right child of this ASTNode or NULL if this node has no
   * right child.  If getNumChildren() > 1, then this is equivalent to:
   *
   *   getChild( getNumChildren() - 1);
   */
  LIBSBML_EXTERN
  ASTNode* getRightChild () const;

  /**
   * @return the number of children of this ASTNode or 0 is this node has
   * no children.
   */
  LIBSBML_EXTERN
  unsigned int getNumChildren () const;

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
   * The List returned is owned by the caller and should be deleted.  The
   * ASTNodes in the list, however, are not owned by the caller (as they
   * still belong to the tree itself) and therefore should not be deleted.
   */
  LIBSBML_EXTERN
  List* getListOfNodes (ASTNodePredicate predicate);

  /**
   * This method is identical in functionality to getListOfNodes(), except
   * the List is passed-in by the caller.
   */
  LIBSBML_EXTERN
  void fillListOfNodes (ASTNodePredicate predicate, List* lst);

  /**
   * @return the value of this ASTNode as a single character.  This
   * function should be called only when getType() is one of AST_PLUS,
   * AST_MINUS, AST_TIMES, AST_DIVIDE or AST_POWER.
   */
  LIBSBML_EXTERN
  char getCharacter () const;

  /**
   * @return the value of this ASTNode as a (long) integer.  This function
   * should be called only when getType() == AST_INTEGER.
   */
  LIBSBML_EXTERN
  long getInteger () const;

  /**
   * @return the value of this ASTNode as a string.  This function may be
   * called on nodes that are not operators (isOperator() == false) or
   * numbers (isNumber() == false).
   */
  LIBSBML_EXTERN
  const char* getName () const;

  /**
   * @return the value of the numerator of this ASTNode.  This function
   * should be called only when getType() == AST_RATIONAL.
   */
  LIBSBML_EXTERN
  long getNumerator () const;

  /**
   * @return the value of the denominator of this ASTNode.  This function
   * should be called only when getType() == AST_RATIONAL.
   */
  LIBSBML_EXTERN
  long getDenominator () const;

  /**
   * @return the value of this ASTNode as a real (double).  This function
   * should be called only when isReal() == true.
   *
   * This function performs the necessary arithmetic if the node type is
   * AST_REAL_E (mantissa * $10^exponent$) or AST_RATIONAL (numerator /
   * denominator).
   */
  LIBSBML_EXTERN
  double getReal () const;

  /**
   * @return the value of the mantissa of this ASTNode.  This function
   * should be called only when getType() is AST_REAL_E or AST_REAL.  If
   * AST_REAL, this method is identical to getReal().
   */
  LIBSBML_EXTERN
  double getMantissa () const;

  /**
   * @return the value of the exponent of this ASTNode.  This function
   * should be called only when getType() is AST_REAL_E or AST_REAL.
   */
  LIBSBML_EXTERN
  long getExponent () const;

  /**
   * @return the precedence of this ASTNode (as defined in the SBML L1
   * specification).
   */
  LIBSBML_EXTERN
  int getPrecedence () const;

  /**
   * @return the type of this ASTNode.
   */
  LIBSBML_EXTERN
  ASTNodeType_t getType () const;

  /**
   * @return true if this ASTNode is a boolean (a logical operator, a
   * relational operator, or the constants true or false), false otherwise.
   */
  LIBSBML_EXTERN
  bool isBoolean () const;

  /**
   * @return true if this ASTNode is a MathML constant (true, false, pi,
   * exponentiale), false otherwise.
   */
  LIBSBML_EXTERN
  bool isConstant () const;

  /**
   * @return true if this ASTNode is a function in SBML L1, L2 (MathML)
   * (everything from abs() to tanh()) or user-defined, false otherwise.
   */
  LIBSBML_EXTERN
  bool isFunction () const;

  /**
   * @return true if this ASTNode is of type AST_INTEGER, false otherwise.
   */
  LIBSBML_EXTERN
  bool isInteger () const;

  /**
   * @return true if this ASTNode is of type AST_LAMBDA, false otherwise.
   */
  LIBSBML_EXTERN
  bool isLambda () const;

  /**
   * @return true if the given ASTNode represents a log10() function, false
   * otherwise.
   *
   * More precisley, the node type is AST_FUNCTION_LOG with two children
   * the first of which is an AST_INTEGER equal to 10.
   */
  LIBSBML_EXTERN
  bool isLog10 () const;

  /**
   * @return true if this ASTNode is a MathML logical operator (and, or,
   * not, xor), false otherwise.
   */
  LIBSBML_EXTERN
  bool isLogical () const;

  /**
   * @return true if this ASTNode is a user-defined variable name in SBML
   * L1, L2 (MathML) or the special symbols delay or time, false otherwise.
   */
  LIBSBML_EXTERN
  bool isName () const;

  /**
   * @return true if this ASTNode is a number, false otherwise.
   *
   * This is functionally equivalent to:
   *
   *   isInteger() || isReal().
   */
  LIBSBML_EXTERN
  bool isNumber () const;

  /**
   * @return true if this ASTNode is an operator, false otherwise.
   * Operators are: +, -, *, / and \^ (power).
   */
  LIBSBML_EXTERN
  bool isOperator () const;

  /**
   * @return true if this ASTNode is of type AST_RATIONAL, false otherwise.
   */
  LIBSBML_EXTERN
  bool isRational () const;

  /**
   * @return true if the value of this ASTNode can represented as a real
   * number, false otherwise.
   *
   * To be a represented as a real number, this node must be of one of the
   * following types: AST_REAL, AST_REAL_E or AST_RATIONAL.
   */
  LIBSBML_EXTERN
  bool isReal () const;

  /**
   * @return true if this ASTNode is a MathML relational operator (==, >=,
   * >, <=, < !=), false otherwise.
   */
  LIBSBML_EXTERN
  bool isRelational () const;

  /**
   * @return true if the given ASTNode represents a sqrt() function, false
   * otherwise.
   *
   * More precisley, the node type is AST_FUNCTION_ROOT with two children
   * the first of which is an AST_INTEGER equal to 2.
   */
  LIBSBML_EXTERN
  bool isSqrt () const;

  /**
   * @return true if this ASTNode is a unary minus, false otherwise.
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
  bool isUMinus () const;

  /**
   * @return true if this ASTNode is of type AST_UNKNOWN, false otherwise.
   */
  LIBSBML_EXTERN
  bool isUnknown () const;

  /**
   * Sets the value of this ASTNode to the given character.  If character
   * is one of '+', '-', '*', '/' or '\^', the node type will be set
   * accordingly.  For all other characters, the node type will be set to
   * AST_UNKNOWN.
   */
  LIBSBML_EXTERN
  void setCharacter (char value);

  /**
   * Sets the value of this ASTNode to the given name.
   *
   * The node type will be set (to AST_NAME) ONLY IF the ASTNode was
   * previously an operator (isOperator(node) == true) or number
   * (isNumber(node) == true).  This allows names to be set for
   * AST_FUNCTIONs and the like.
   */
  LIBSBML_EXTERN
  void setName (const char *name);

  /**
   * Sets the value of this ASTNode to the given integer and sets the node
   * type to AST_INTEGER.
   */
  LIBSBML_EXTERN
  void
  setValue (int value);

  /**
   * Sets the value of this ASTNode to the given (long) integer and sets
   * the node type to AST_INTEGER.
   */
  LIBSBML_EXTERN
  void setValue (long value);

  /**
   * Sets the value of this ASTNode to the given rational in two parts: the
   * numerator and denominator.  The node type is set to AST_RATIONAL.
   */
  LIBSBML_EXTERN
  void setValue (long numerator, long denominator);

  /**
   * Sets the value of this ASTNode to the given real (double) and sets the
   * node type to AST_REAL.
   *
   * This is functionally equivalent to:
   *
   *   setValue(value, 0);
   */
  LIBSBML_EXTERN
  void setValue (double value);

  /**
   * Sets the value of this ASTNode to the given real (double) in two
   * parts: the mantissa and the exponent.  The node type is set to
   * AST_REAL_E.
   */
  LIBSBML_EXTERN
  void setValue (double mantissa, long exponent);

  /**
   * Sets the type of this ASTNode to the given ASTNodeType.
   */
  LIBSBML_EXTERN
  void setType (ASTNodeType_t type);

  /**
   * Swap the children of this ASTNode with the children of that ASTNode.
   */
  void swapChildren (ASTNode *that);


protected:

  /**
   * Internal helper function for canonicalize().
   */

  bool canonicalizeConstant   ();
  bool canonicalizeFunction   ();
  bool canonicalizeFunctionL1 ();
  bool canonicalizeLogical    ();
  bool canonicalizeRelational ();


  ASTNodeType_t mType;

  union
  {
    char   mChar;
    char*  mName;
    long   mInteger;
    double mReal;
  };    

  union
  {
    long mDenominator;
    long mExponent;
  };

  List *mChildren;


  friend class MathMLFormatter;
  friend class MathMLHandler;
};


#endif /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


/**
 * Creates a new ASTNode and returns a pointer to it.  The returned node
 * will have a type of AST_UNKNOWN and should be set to something else as
 * soon as possible.
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_create (void);

/**
 * Creates a new ASTNode and sets its type to the given ASTNodeType.
 */
ASTNode_t *
ASTNode_createWithType (ASTNodeType_t type);

/**
 * Creates a new ASTNode from the given Token and returns a pointer to it.
 * The returned ASTNode will contain the same data as the Token.
 */
ASTNode_t *
ASTNode_createFromToken (Token_t *token);


/**
 * Frees the given ASTNode including any child nodes.
 */
LIBSBML_EXTERN
void
ASTNode_free (ASTNode_t *node);

/**
 * Frees the name of this ASTNode and sets it to NULL, if appropriate,
 * i.e. the node is not an operator, number or AST_UNKNOWN.
 */
void
ASTNode_freeName (ASTNode_t *node);


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
ASTNode_canonicalize (ASTNode_t *node);


/**
 * Adds the given node as a child of this ASTNode.  Child nodes are added
 * in-order from "left-to-right".
 */
LIBSBML_EXTERN
void
ASTNode_addChild (ASTNode_t *node, ASTNode_t *child);

/**
 * Adds the given node as a child of this ASTNode.  This method adds child
 * nodes from "right-to-left".
 */
LIBSBML_EXTERN
void
ASTNode_prependChild (ASTNode_t *node, ASTNode_t *child);


/**
 * @return the nth child of this ASTNode or NULL if this node has no nth
 * child (n > ASTNode_getNumChildren() - 1).
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getChild (const ASTNode_t *node, unsigned int n);

/**
 * @return the left child of this ASTNode.  This is equivalent to
 * ASTNode_getChild(node, 0);
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getLeftChild (const ASTNode_t *node);

/**
 * @return the right child of this ASTNode or NULL if this node has no
 * right child.  If ASTNode_getNumChildren(node) > 1, then this is
 * equivalent to:
 *
 *   ASTNode_getChild(node, ASTNode_getNumChildren(node) - 1);
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getRightChild (const ASTNode_t *node);

/**
 * @return the number of children of this ASTNode or 0 is this node has no
 * children.
 */
LIBSBML_EXTERN
unsigned int
ASTNode_getNumChildren (const ASTNode_t *node);


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
ASTNode_getListOfNodes (ASTNode_t *node, ASTNodePredicate predicate);

/**
 * This method is identical in functionality to ASTNode_getListOfNodes(),
 * except the List is passed-in by the caller.
 */
LIBSBML_EXTERN
void
ASTNode_fillListOfNodes ( ASTNode_t        *node,
                          ASTNodePredicate predicate,
                          List_t           *lst );


/**
 * @return the value of this ASTNode as a single character.  This function
 * should be called only when ASTNode_getType() is one of AST_PLUS,
 * AST_MINUS, AST_TIMES, AST_DIVIDE or AST_POWER.
 */
LIBSBML_EXTERN
char
ASTNode_getCharacter (const ASTNode_t *node);

/**
 * @return the value of this ASTNode as a (long) integer.  This function
 * should be called only when ASTNode_getType() == AST_INTEGER.
 */
LIBSBML_EXTERN
long
ASTNode_getInteger (const ASTNode_t *node);

/**
 * @return the value of this ASTNode as a string.  This function may be
 * called on nodes that are not operators (ASTNode_isOperator(node) == 0)
 * or numbers (ASTNode_isNumber(node) == 0).
 */
LIBSBML_EXTERN
const char *
ASTNode_getName (const ASTNode_t *node);

/**
 * @return the value of the numerator of this ASTNode.  This function
 * should be called only when ASTNode_getType() == AST_RATIONAL.
 */
LIBSBML_EXTERN
long
ASTNode_getNumerator (const ASTNode_t *node);

/**
 * @return the value of the denominator of this ASTNode.  This function
 * should be called only when ASTNode_getType() == AST_RATIONAL.
 */
LIBSBML_EXTERN
long
ASTNode_getDenominator (const ASTNode_t *node);

/**
 * @return the value of this ASTNode as a real (double).  This function
 * should be called only when ASTNode_isReal(node) != 0.
 *
 * This function performs the necessary arithmetic if the node type is
 * AST_REAL_E (mantissa * $10^exponent$) or AST_RATIONAL
 * (numerator / denominator).
 */
LIBSBML_EXTERN
double
ASTNode_getReal (const ASTNode_t *node);

/**
 * @return the value of the mantissa of this ASTNode.  This function should
 * be called only when ASTNode_getType() is AST_REAL_E or AST_REAL.  If
 * AST_REAL, this method is identical to ASTNode_getReal().
 */
LIBSBML_EXTERN
double
ASTNode_getMantissa (const ASTNode_t *node);

/**
 * @return the value of the exponent of this ASTNode.  This function should
 * be called only when ASTNode_getType() is AST_REAL_E or AST_REAL.
 */
LIBSBML_EXTERN
long
ASTNode_getExponent (const ASTNode_t *node);

/**
 * @return the precedence of this ASTNode (as defined in the SBML L1
 * specification).
 */
LIBSBML_EXTERN
int
ASTNode_getPrecedence (const ASTNode_t *node);

/**
 * @return the type of this ASTNode.
 */
LIBSBML_EXTERN
ASTNodeType_t
ASTNode_getType (const ASTNode_t *node);

/**
 * @return true (non-zero) if this ASTNode is a boolean (a logical
 * operator, a relational operator, or the constants true or false), false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isBoolean (const ASTNode_t *node);

/**
 * @return true (non-zero) if this ASTNode is a MathML constant (true,
 * false, pi, exponentiale), false (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isConstant (const ASTNode_t *node);

/**
 * @return true (non-zero) if this ASTNode is a function in SBML L1, L2
 * (MathML) (everything from abs() to tanh()) or user-defined, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isFunction (const ASTNode_t *node);

/**
 * @return true (non-zero) if this ASTNode is of type AST_INTEGER, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isInteger (const ASTNode_t *node);

/**
 * @return true (non-zero) if this ASTNode is of type AST_LAMBDA, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isLambda (const ASTNode_t *node);

/**
 * @return true (non-zero) if the given ASTNode represents a log10()
 * function, false (0) otherwise.
 *
 * More precisley, the node type is AST_FUNCTION_LOG with two children the
 * first of which is an AST_INTEGER equal to 10.
 */
LIBSBML_EXTERN
int
ASTNode_isLog10 (const ASTNode_t *node);

/**
 * @return true (non-zero) if this ASTNode is a MathML logical operator
 * (and, or, not, xor), false (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isLogical (const ASTNode_t *node);

/**
 * @return true (non-zero) if this ASTNode is a user-defined variable name
 * in SBML L1, L2 (MathML) or the special symbols delay or time, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isName (const ASTNode_t *node);

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
ASTNode_isNumber (const ASTNode_t *node);

/**
 * @return true (non-zero) if this ASTNode is an operator, false (0)
 * otherwise.  Operators are: +, -, *, / and \^ (power).
 */
LIBSBML_EXTERN
int
ASTNode_isOperator (const ASTNode_t *node);

/**
 * @return true (non-zero) if this ASTNode is of type AST_RATIONAL, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isRational (const ASTNode_t *node);

/**
 * @return true (non-zero) if the value of this ASTNode can represented as
 * a real number, false (0) otherwise.
 *
 * To be a represented as a real number, this node must be of one of the
 * following types: AST_REAL, AST_REAL_E or AST_RATIONAL.
 */
LIBSBML_EXTERN
int
ASTNode_isReal (const ASTNode_t *node);

/**
 * @return true (non-zero) if this ASTNode is a MathML relational operator
 * (==, >=, >, <=, < !=), false (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isRelational (const ASTNode_t *node);

/**
 * @return true (non-zero) if the given ASTNode represents a sqrt()
 * function, false (0) otherwise.
 *
 * More precisley, the node type is AST_FUNCTION_ROOT with two children the
 * first of which is an AST_INTEGER equal to 2.
 */
LIBSBML_EXTERN
int
ASTNode_isSqrt (const ASTNode_t *node);

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
ASTNode_isUMinus (const ASTNode_t *node);

/**
 * @return true (non-zero) if this ASTNode is of type AST_UNKNOWN, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isUnknown (const ASTNode_t *node);


/**
 * Sets the value of this ASTNode to the given character.  If character is
 * one of '+', '-', '*', '/' or '\^', the node type will be set accordingly.
 * For all other characters, the node type will be set to AST_UNKNOWN.
 */
LIBSBML_EXTERN
void
ASTNode_setCharacter (ASTNode_t *node, char value);

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
ASTNode_setName (ASTNode_t *node, const char *name);

/**
 * Sets the value of this ASTNode to the given (long) integer and sets the
 * node type to AST_INTEGER.
 */
LIBSBML_EXTERN
void
ASTNode_setInteger (ASTNode_t *node, long value);

/**
 * Sets the value of this ASTNode to the given rational in two parts:
 * the numerator and denominator.  The node type is set to AST_RATIONAL.
 */
LIBSBML_EXTERN
void
ASTNode_setRational (ASTNode_t *node, long numerator, long denominator);

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
ASTNode_setReal (ASTNode_t *node, double value);

/**
 * Sets the value of this ASTNode to the given real (double) in two parts:
 * the mantissa and the exponent.  The node type is set to AST_REAL_E.
 */
LIBSBML_EXTERN
void
ASTNode_setRealWithExponent (ASTNode_t *node, double mantissa, long exponent);

/**
 * Sets the type of this ASTNode to the given ASTNodeType.
 */
LIBSBML_EXTERN
void
ASTNode_setType (ASTNode_t *node, ASTNodeType_t type);


/**
 * Swap the children of this ASTNode with the children of that ASTNode.
 */
void
ASTNode_swapChildren (ASTNode_t *node, ASTNode_t *that);


END_C_DECLS

#endif  /* !SWIG */
#endif  /* ASTNode_h */
