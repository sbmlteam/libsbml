/**
 * Filename    : ASTNode.h
 * Description : Abstract Syntax Tree (AST) for representing formula trees
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef ASTNode_h
#define ASTNode_h


#include "common.h"
#include "FormulaTokenizer.h"
#include "List.h"


BEGIN_C_DECLS


/**
 * Nodes of type AST_UNKNOWN are used internally as the AST is being
 * constructed.  Trees returned by SBML_parseFormula() will not contain
 * unknown nodes.
 */
typedef enum
{
    AST_PLUS    = '+'
  , AST_MINUS   = '-'
  , AST_TIMES   = '*'
  , AST_DIVIDE  = '/'
  , AST_POWER   = '^'

  , AST_INTEGER = 256
  , AST_REAL
  , AST_REAL_E
  , AST_RATIONAL

  , AST_NAME
  , AST_NAME_DELAY
  , AST_NAME_TIME

  , AST_CONSTANT_E
  , AST_CONSTANT_FALSE
  , AST_CONSTANT_PI
  , AST_CONSTANT_TRUE

  , AST_LAMBDA

  , AST_FUNCTION
  , AST_FUNCTION_ABS
  , AST_FUNCTION_ARCCOS
  , AST_FUNCTION_ARCCOSH
  , AST_FUNCTION_ARCCOT
  , AST_FUNCTION_ARCCOTH
  , AST_FUNCTION_ARCCSC
  , AST_FUNCTION_ARCCSCH
  , AST_FUNCTION_ARCSEC
  , AST_FUNCTION_ARCSECH
  , AST_FUNCTION_ARCSIN
  , AST_FUNCTION_ARCSINH
  , AST_FUNCTION_ARCTAN
  , AST_FUNCTION_ARCTANH
  , AST_FUNCTION_CEILING
  , AST_FUNCTION_COS
  , AST_FUNCTION_COSH
  , AST_FUNCTION_COT
  , AST_FUNCTION_COTH
  , AST_FUNCTION_CSC
  , AST_FUNCTION_CSCH
  , AST_FUNCTION_EXP
  , AST_FUNCTION_FACTORIAL
  , AST_FUNCTION_FLOOR
  , AST_FUNCTION_LN
  , AST_FUNCTION_LOG
  , AST_FUNCTION_PIECEWISE
  , AST_FUNCTION_POWER
  , AST_FUNCTION_ROOT
  , AST_FUNCTION_SEC
  , AST_FUNCTION_SECH
  , AST_FUNCTION_SIN
  , AST_FUNCTION_SINH
  , AST_FUNCTION_TAN
  , AST_FUNCTION_TANH

  , AST_LOGICAL_AND
  , AST_LOGICAL_NOT
  , AST_LOGICAL_OR
  , AST_LOGICAL_XOR

  , AST_RELATIONAL_EQ
  , AST_RELATIONAL_GEQ
  , AST_RELATIONAL_GT
  , AST_RELATIONAL_LEQ
  , AST_RELATIONAL_LT
  , AST_RELATIONAL_NEQ

  , AST_UNKNOWN
} ASTNodeType_t;


/**
 * An Abstract Syntax Tree (AST) node has a type (above), a value, and a
 * list of child nodes.  The node value is either a(n):
 *
 *   - character (an operator)
 *   - string    (for function and variable names)
 *   - integer, or
 *   - real number.
 *
 * There are two special cases on value to accomodate MathML:
 *
 *   - A node type of AST_REAL_E means the real number is expressed in two
 *     parts: mantissa (value.real) and exponent (extra.exponent).
 *
 *   - Similarly AST_RATIONALs are representated as a numerator
 *     (value.integer) and denominator (extra.denominator).
 */
typedef struct
{
  ASTNodeType_t type;

  union
  {
    char   ch;
    char   *name;
    long   integer;
    double real;
  } value;    

  union
  {
    long denominator;
    long exponent;
  } extra;

  List_t *children;
} ASTNode_t;


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
 * Copies the data from the given token to this ASTNode.
 */
void
ASTNode_copyFromToken (ASTNode_t *node, Token_t *token);


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
 * AST_REAL_E (mantissa * 10^exponent) or AST_RATIONAL
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
 * otherwise.  Operators are: +, -, *, / and ^ (power).
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
 * one of '+', '-', '*', '/' or '^', the node type will be set accordingly.
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


END_C_DECLS


#endif  /** ASTNode_h **/
