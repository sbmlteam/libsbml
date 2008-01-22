/**
 * @file    ASTNode.cpp
 * @brief   Abstract Syntax Tree (AST) for representing formula trees.
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2006-2007 California Institute of Technology.
 * Copyright 2005      California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <new>
#include <cmath>
#include <stdlib.h>

#include <sbml/common/common.h>
#include <sbml/util/List.h>

#include <sbml/math/ASTNode.h>
#include <sbml/xml/XMLAttributes.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


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
  , "delay"
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


/*
 * Creates a new ASTNode.
 *
 * By default, node will have a type of AST_UNKNOWN and should be set to
 * something else as soon as possible.
 */
LIBSBML_EXTERN
ASTNode::ASTNode (ASTNodeType_t type)
{
  unsetSemanticsFlag();
  mDefinitionURL = new XMLAttributes();
  mReal          = 0;
  mExponent      = 0;
  mType          = AST_UNKNOWN;

  setType(type);

  mChildren             = new List;
  mSemanticsAnnotations = new List;
}


/*
 * Creates a new ASTNode from the given Token.
 *
 * The ASTNode will contain the same data as the Token.
 */
ASTNode::ASTNode (Token_t* token)
{
  unsetSemanticsFlag();
  mDefinitionURL = new XMLAttributes();
  mReal          = 0;
  mExponent      = 0;
  mType          = AST_UNKNOWN;

  mChildren             = new List;
  mSemanticsAnnotations = new List;

  if (token->type == TT_NAME)
  {
    setName(token->value.name);
  }
  else if (token->type == TT_INTEGER)
  {
    setValue(token->value.integer);
  }
  else if (token->type == TT_REAL)
  {
    setValue(token->value.real);
  }
  else if (token->type == TT_REAL_E)
  {
    setValue(token->value.real, token->exponent);
  }
  else
  {
    setCharacter(token->value.ch);
  }
}


/*
 * Destroys this ASTNode including any child nodes.
 */
LIBSBML_EXTERN
ASTNode::~ASTNode ()
{
  unsigned int size = getNumChildren();


  while (size--) delete static_cast<ASTNode*>( mChildren->remove(0) );
  delete mChildren;

  size = mSemanticsAnnotations->getSize();
  while (size--)  mSemanticsAnnotations->remove(0) ;
  delete mSemanticsAnnotations;

  delete mDefinitionURL;
  
  freeName();
}


/*
 * Frees the name of this ASTNode and sets it to NULL.
 * 
 * This operation is only applicable to ASTNodes corresponding to
 * operators, numbers, or AST_UNKNOWN.  This method will have no
 * effect on other types of nodes.
 */
void
ASTNode::freeName ()
{
  if ( !(isOperator() || isNumber() || isUnknown()) )
  {
    if (mName != NULL)
    {
      safe_free(mName);
      mName = NULL;
    }
  }
}


/*
 * Attempts to convert this ASTNode to a canonical form and returns true if
 * the conversion succeeded, false otherwise.
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
bool
ASTNode::canonicalize ()
{
  bool found = false;


  if (mType == AST_NAME)
  {
    found = canonicalizeConstant();
  }

  if (!found && mType == AST_FUNCTION)
  {
    found = canonicalizeFunction();

    if (!found)
    {
      found = canonicalizeLogical();
    }

    if (!found)
    {
      found = canonicalizeRelational();
    }
  }

  return found;
}


/*
 * Internal helper function for canonicalize().
 */
bool
ASTNode::canonicalizeConstant ()
{
  const int first = AST_CONSTANT_E;
  const int last  = AST_CONSTANT_TRUE;
  const int size  = last - first + 1;

  int  index;
  bool found;


  index = util_bsearchStringsI(AST_CONSTANT_STRINGS, mName, 0, size - 1);
  found = (index < size);

  if (found)
  {
    setType( static_cast<ASTNodeType_t>(first + index) );
  }

  return found;
}


/*
 * Internal helper function for canonicalize().
 */
bool
ASTNode::canonicalizeFunction ()
{
  const int first = AST_FUNCTION_ABS;
  const int last  = AST_FUNCTION_TANH;
  const int size  = last - first + 1;

  int  index;
  bool found;


  /**
   * Search for SBML Level 1 function names first.
   */
  found = canonicalizeFunctionL1();

  /**
   * Now Lambda...
   */
  if (!found)
  {
    if ( (found = !strcmp_insensitive(mName, AST_LAMBDA_STRING)) )
    {
      setType(AST_LAMBDA);
    }
  }

  /**
   * ... and finally the L2 (MathML) function names.
   */
  if (!found)
  {
    index = util_bsearchStringsI(AST_FUNCTION_STRINGS, mName, 0, size - 1);
    found = (index < size);

    if (found)
    {
      setType( static_cast<ASTNodeType_t>(first + index) );
    }
  }

  return found;
}


/*
 * Internal helper function for canonicalize().
 */
bool
ASTNode::canonicalizeFunctionL1 ()
{
  ASTNode* child;


  if ( !strcmp_insensitive(mName, "acos") )
  {
    setType(AST_FUNCTION_ARCCOS);
  }
  else if ( !strcmp_insensitive(mName, "asin") )
  {
    setType(AST_FUNCTION_ARCSIN);
  }
  else if ( !strcmp_insensitive(mName, "atan") )
  {
    setType(AST_FUNCTION_ARCTAN);
  }
  else if ( !strcmp_insensitive(mName, "ceil") )
  {
    setType(AST_FUNCTION_CEILING);
  }

  /**
   * "log(x)" in L1 is represented as "ln(x)" in L2.
   *
   * Notice, however, that the conversion is performed only ifthe number of
   * arguments is 1.  Thus "log(5, x)" will still be "log(5, x) when passed
   * through this filter.
   */
  else if ( !strcmp_insensitive(mName, "log") && (getNumChildren() == 1) )
  {
    setType(AST_FUNCTION_LN);
  }

  /**
   * "log10(x)" in L1 is represented as "log(10, x)" in L2.
   */
  else if ( !strcmp_insensitive(mName, "log10") && (getNumChildren() == 1) )
  {
    setType(AST_FUNCTION_LOG);

    child = new ASTNode;
    child->setValue(10);

    prependChild(child);
  }

  /**
   * Here we set the type to AST_FUNCTION_POWER.  We could set it to
   * AST_POWER, but then we would loose the idea that it was a function
   * before it was canonicalized.
   */
  else if ( !strcmp_insensitive(mName, "pow") )
  {
    setType(AST_FUNCTION_POWER);
  }

  /**
   * "sqr(x)" in L1 is represented as "power(x, 2)" in L2.
   */
  else if ( !strcmp_insensitive(mName, "sqr") && (getNumChildren() == 1) )
  {
    setType(AST_FUNCTION_POWER);

    child = new ASTNode;
    child->setValue(2);

    addChild(child);
  }

  /**
   * "sqrt(x) in L1 is represented as "root(2, x)" in L1.
   */
  else if ( !strcmp_insensitive(mName, "sqrt") && (getNumChildren() == 1) )
  {
    setType(AST_FUNCTION_ROOT);

    child = new ASTNode;
    child->setValue(2);

    prependChild(child);
  }

  /**
   * Was a conversion performed?
   */
  return (mType != AST_FUNCTION);
}


/*
 * Internal helper function for canonicalize().
 */
bool
ASTNode::canonicalizeLogical ()
{
  const int first = AST_LOGICAL_AND;
  const int last  = AST_LOGICAL_XOR;
  const int size  = last - first + 1;

  int  index;
  bool found;


  index = util_bsearchStringsI(AST_LOGICAL_STRINGS, mName, 0, size - 1);
  found = (index < size);

  if (found)
  {
    setType( static_cast<ASTNodeType_t>(first + index) );
  }

  return found;
}


/*
 * Internal helper function for canonicalize().
 */
bool
ASTNode::canonicalizeRelational ()
{
  const int first = AST_RELATIONAL_EQ;
  const int last  = AST_RELATIONAL_NEQ;
  const int size  = last - first + 1;

  int  index;
  bool found;


  index = util_bsearchStringsI(AST_RELATIONAL_STRINGS, mName, 0, size - 1);
  found = (index < size);

  if (found)
  {
    setType( static_cast<ASTNodeType_t>(first + index) );
  }

  return found;
}


/*
 * Adds the given node as a child of this ASTNode.  Child nodes are added
 * in-order from "left-to-right".
 */
LIBSBML_EXTERN
void
ASTNode::addChild (ASTNode* child)
{
  mChildren->add(child);
}

/*
 * Adds the given node as a child of this ASTNode.  This method adds child
 * nodes from "right-to-left".
 */
LIBSBML_EXTERN
void
ASTNode::prependChild (ASTNode* child)
{
  mChildren->prepend(child);
}


/*
 * @return a copy of this ASTNode and all its children.  The caller owns
 * the returned ASTNode and is reponsible for deleting it.
 */
LIBSBML_EXTERN
ASTNode*
ASTNode::deepCopy () const
{
  ASTNode* copy = new ASTNode;


  copy->mType     = mType;
  copy->mExponent = mExponent;

  if ( !(isOperator() || isNumber() || isUnknown()) && mName)
  {
    copy->mName = safe_strdup(mName);
  }
  else
  {
    copy->mReal = mReal;
  }

  for (unsigned int c = 0; c < getNumChildren(); ++c)
  {
    copy->addChild( getChild(c)->deepCopy() );
  }

  return copy;
}


/*
 * @return the nth child of this ASTNode or NULL if this node has no nth
 * child (n > getNumChildren() - 1).
 */
LIBSBML_EXTERN
ASTNode*
ASTNode::getChild (unsigned int n) const
{
  return static_cast<ASTNode*>( mChildren->get(n) );
}


/*
 * @return the left child of this ASTNode.  This is equivalent to
 * getChild(0);
 */
LIBSBML_EXTERN
ASTNode*
ASTNode::getLeftChild () const
{
  return static_cast<ASTNode*>( mChildren->get(0) );
}


/*
 * @return the right child of this ASTNode or NULL if this node has no
 * right child.  If getNumChildren() > 1, then this is equivalent to:
 *
 *   getChild( getNumChildren() - 1);
 */
LIBSBML_EXTERN
ASTNode*
ASTNode::getRightChild () const
{
  unsigned int nc = getNumChildren();


  return (nc > 1) ? static_cast<ASTNode*>( mChildren->get(nc - 1) ): NULL;
}


/*
 * @return the number of children of this ASTNode or 0 is this node has no
 * children.
 */
LIBSBML_EXTERN
unsigned int
ASTNode::getNumChildren () const
{
  return mChildren->getSize();
}


/*
 * Adds the given xmlnode as an annotation of this ASTNode.  
 */
LIBSBML_EXTERN
void 
ASTNode::addSemanticsAnnotation (XMLNode* annotation)
{
  mSemanticsAnnotations->add(annotation);
}


/*
 * @return the number of annotations of this ASTNode.  
 */
LIBSBML_EXTERN
unsigned int 
ASTNode::getNumSemanticsAnnotations () const
{
  return mSemanticsAnnotations->getSize();
}


/*
 * @return the nth annotation of this ASTNode or NULL if this node has no nth
 * annotation.
 */
LIBSBML_EXTERN
XMLNode* 
ASTNode::getSemanticsAnnotation (unsigned int n) const
{
  return static_cast<XMLNode*>( mSemanticsAnnotations->get(n) );
}

/*
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
List*
ASTNode::getListOfNodes (ASTNodePredicate predicate) const
{
  List* lst = new List;


  fillListOfNodes(predicate, lst);

  return lst;
}


/*
 * This method is identical in functionality to getListOfNodes(), except
 * the List is passed-in by the caller.
 */
LIBSBML_EXTERN
void
ASTNode::fillListOfNodes (ASTNodePredicate predicate, List* lst) const
{
  ASTNode*     child;
  unsigned int c;
  unsigned int numChildren = getNumChildren();



  if (predicate(this) != 0)
  {
    lst->add( const_cast<ASTNode*>(this) );
  }

  for (c = 0; c < numChildren; c++)
  {
    child = getChild(c);
    child->fillListOfNodes(predicate, lst);
  }
}


/*
 * @return the value of this ASTNode as a single character.  This function
 * should be called only when getType() is one of AST_PLUS, AST_MINUS,
 * AST_TIMES, AST_DIVIDE or AST_POWER.
 */
LIBSBML_EXTERN
char
ASTNode::getCharacter () const
{
  return mChar;
}


/*
 * @return the value of this ASTNode as a (long) integer.  This function
 * should be called only when getType() == AST_INTEGER.
 */
LIBSBML_EXTERN
long
ASTNode::getInteger () const
{
  return mInteger;
}


/*
 * @return the value of this ASTNode as a string.  This function may be
 * called on nodes that are not operators (isOperator() == false)
 * or numbers (isNumber() == false).
 */
LIBSBML_EXTERN
const char*
ASTNode::getName () const
{
  const char* result = mName;


  /**
   * If the node does not have a name and is not a user-defined function
   * (type == AST_FUNCTION), use the default name for the builtin node
   * types.
   */
  if (mName == NULL && mType != AST_FUNCTION)
  {
    if ( isConstant() )
    {
      result = AST_CONSTANT_STRINGS[ mType - AST_CONSTANT_E ];
    }
    else if ( isLambda() )
    {
      result = AST_LAMBDA_STRING;
    }
    else if ( isFunction() )
    {
      result = AST_FUNCTION_STRINGS[ mType - AST_FUNCTION_ABS ];
    }
    else if ( isLogical() )
    {
      result = AST_LOGICAL_STRINGS[ mType - AST_LOGICAL_AND ];
    }
    else if ( isRelational() )
    {
      result = AST_RELATIONAL_STRINGS[ mType - AST_RELATIONAL_EQ ];
    }
  }

  return result;
}


/*
 * @return the value of the numerator of this ASTNode.  This function
 * should be called only when getType() == AST_RATIONAL.
 */
LIBSBML_EXTERN
long
ASTNode::getNumerator () const
{
  return mInteger;
}


/*
 * @return the value of the denominator of this ASTNode.  This function
 * should be called only when getType() == AST_RATIONAL.
 */
LIBSBML_EXTERN
long
ASTNode::getDenominator () const
{
  return mDenominator;
}


/*
 * @return the value of this ASTNode as a real (double).  This function
 * should be called only when isReal() == true.
 *
 * This function performs the necessary arithmetic if the node type is
 * AST_REAL_E (mantissa * $10^exponent$) or AST_RATIONAL (numerator /
 * denominator).
 */
LIBSBML_EXTERN
double
ASTNode::getReal () const
{
  double result = mReal;
  

  if (mType == AST_REAL_E)
  {
    result *= pow(10.0,  static_cast<double>(mExponent) );
  }
  else if (mType == AST_RATIONAL)
  {
    result = static_cast<double>(mInteger) / mDenominator;
  }

  return result;
}


/*
 * @return the value of the mantissa of this ASTNode.  This function should
 * be called only when getType() is AST_REAL_E or AST_REAL.  If AST_REAL,
 * this method is identical to getReal().
 */
LIBSBML_EXTERN
double
ASTNode::getMantissa () const
{
  return mReal;
}


/*
 * @return the value of the exponent of this ASTNode.  This function should
 * be called only when getType() is AST_REAL_E or AST_REAL.
 */
LIBSBML_EXTERN
long
ASTNode::getExponent () const
{
  return mExponent;
}


/*
 * @return the precedence of this ASTNode (as defined in the SBML L1
 * specification).
 */
LIBSBML_EXTERN
int
ASTNode::getPrecedence () const
{
  int precedence;


  if ( isUMinus() )
  {
    precedence = 5;
  }
  else
  {
    switch (mType)
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


/*
 * @return the type of this ASTNode.
 */
LIBSBML_EXTERN
ASTNodeType_t
ASTNode::getType () const
{
  return mType;
}


/*
 * @return true if this ASTNode is a boolean (a logical operator, a
 * relational operator, or the constants true or false), false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isBoolean () const
{
  return
    isLogical   () ||
    isRelational() ||
    mType == AST_CONSTANT_TRUE ||
    mType == AST_CONSTANT_FALSE;
}


/*
 * @return true if this ASTNode is a MathML constant (true,
 * false, pi, exponentiale), false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isConstant () const
{
  return ASTNodeType_isConstant(mType);
}


/*
 * @return true if this ASTNode is a function in SBML L1, L2 (MathML)
 * (everything from abs() to tanh()) or user-defined, false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isFunction () const
{
  return ASTNodeType_isFunction(mType);
}


/*
 * @return true if this ASTNode is the special IEEE 754 value infinity,
 * false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isInfinity () const
{
  return isReal() ? util_isInf( getReal() ) > 0 : false;
}


/*
 * @return true if this ASTNode is of type AST_INTEGER, false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isInteger () const
{
  return ASTNodeType_isInteger(mType);
}


/*
 * @return true if this ASTNode is of type AST_LAMBDA, false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isLambda () const
{
  return ASTNodeType_isLambda(mType);
}

/*
 * @return true if the given ASTNode represents a log10() function, false
 * otherwise.
 *
 * More precisley, the node type is AST_FUNCTION_LOG with two children the
 * first of which is an AST_INTEGER equal to 10.
 */
LIBSBML_EXTERN
bool
ASTNode::isLog10 () const
{
  bool     result = false;
  ASTNode* c;


  if (mType == AST_FUNCTION_LOG)
  {
    if (getNumChildren() == 2)
    {
      c = getLeftChild();

      if ((c->mType == AST_INTEGER) && (c->mInteger == 10))
      {
        result = true;
      }
    }
  }

  return result;
}


/*
 * @return true if this ASTNode is a MathML logical operator (and, or, not,
 * xor), false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isLogical () const
{
  return ASTNodeType_isLogical(mType);
}


/*
 * @return true if this ASTNode is a user-defined variable name in SBML L1,
 * L2 (MathML) or the special symbols delay or time, false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isName () const
{
  return ASTNodeType_isName(mType);
}


/*
 * @return true if this ASTNode is the special IEEE 754 value not a
 * number, false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isNaN () const
{
  if ( isReal() )
  {
    double value = getReal();
    return (value != value);
  }

  return false;
}


/*
 * @return true if this ASTNode is the special IEEE 754 value negative
 * infinity, false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isNegInfinity () const
{
  return isReal() ? util_isInf( getReal() ) < 0 : false;
}


/*
 * @return true if this ASTNode is a number, false otherwise.
 *
 * This is functionally equivalent to:
 *
 *   isInteger() || isReal().
 */
LIBSBML_EXTERN
bool
ASTNode::isNumber () const
{
  return ASTNodeType_isNumber(mType);
}


/*
 * @return true if this ASTNode is an operator, false otherwise.  Operators
 * are: +, -, *, / and \^ (power).
 */
LIBSBML_EXTERN
bool
ASTNode::isOperator () const
{
  return ASTNodeType_isOperator(mType);
}


/*
 * @return true if this ASTNode is a piecewise function, false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isPiecewise () const
{
  return (mType == AST_FUNCTION_PIECEWISE);
}


/*
 * @return true if this ASTNode is of type AST_RATIONAL, false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isRational () const
{
  return ASTNodeType_isRational(mType);
}


/*
 * @return true if the value of this ASTNode can represented as a real
 * number, false otherwise.
 *
 * To be a represented as a real number, this node must be of one of the
 * following types: AST_REAL, AST_REAL_E or AST_RATIONAL.
 */
LIBSBML_EXTERN
bool
ASTNode::isReal () const
{
  return ASTNodeType_isReal(mType);
}


/*
 * @return true if this ASTNode is a MathML relational operator (==, >=, >,
 * <=, < !=), false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isRelational () const
{
  return ASTNodeType_isRelational(mType);
}


/*
 * @return true if the given ASTNode represents a sqrt() function, false
 * otherwise.
 *
 * More precisley, the node type is AST_FUNCTION_ROOT with two children the
 * first of which is an AST_INTEGER equal to 2.
 */
LIBSBML_EXTERN
bool
ASTNode::isSqrt () const
{
  int      result = false;
  ASTNode* c;


  if (mType == AST_FUNCTION_ROOT)
  {
    if (getNumChildren() == 2)
    {
      c = getLeftChild();

      if ((c->mType == AST_INTEGER) && (c->mInteger == 2))
      {
        result = true;
      }
    }
  }

  return result;
}


/*
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
bool
ASTNode::isUMinus () const
{
  bool uminus = false;


  if (mType == AST_MINUS)
  {
    if (getNumChildren() == 1)
    {
      uminus = true;
    }
  }

  return uminus;
}


/*
 * @return true if this ASTNode is of type AST_UNKNOWN, false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isUnknown () const
{
  return ASTNodeType_isUnknown(mType);
}


/*
 * Sets the value of this ASTNode to the given character.  If character is
 * one of '+', '-', '*', '/' or '\^', the node type will be set
 * accordingly.  For all other characters, the node type will be set to
 * AST_UNKNOWN.
 */
LIBSBML_EXTERN
void
ASTNode::setCharacter (char value)
{
  setType( static_cast<ASTNodeType_t>(value) );
  mChar = value;
}


/*
 * Sets the value of this ASTNode to the given name.
 *
 * The node type will be set (to AST_NAME) ONLY IF the ASTNode was
 * previously an operator (isOperator(node) == true) or number
 * (isNumber(node) == true).  This allows names to be set for AST_FUNCTIONs
 * and the like.
 */
LIBSBML_EXTERN
void
ASTNode::setName (const char *name)
{
  if (mName == name) return;


  if ( isOperator() || isNumber() || isUnknown() )
  {
    mType = AST_NAME;
  }

  freeName();
  mName = (name == NULL) ? NULL : safe_strdup(name);
}


/*
 * Sets the value of this ASTNode to the given integer and sets the node
 * type to AST_INTEGER.
 */
LIBSBML_EXTERN
void
ASTNode::setValue (int value)
{
  setType(AST_INTEGER);
  mInteger = value;
}


/*
 * Sets the value of this ASTNode to the given (long) integer and sets the
 * node type to AST_INTEGER.
 */
LIBSBML_EXTERN
void
ASTNode::setValue (long value)
{
  setType(AST_INTEGER);
  mInteger = value;
}


/*
 * Sets the value of this ASTNode to the given rational in two parts:
 * the numerator and denominator.  The node type is set to AST_RATIONAL.
 */
LIBSBML_EXTERN
void
ASTNode::setValue (long numerator, long denominator)
{
  setType(AST_RATIONAL);

  mInteger     = numerator;
  mDenominator = denominator;
}


/*
 * Sets the value of this ASTNode to the given real (double) and sets the
 * node type to AST_REAL.
 *
 * This is functionally equivalent to:
 *
 *   setValue(value, 0);
 */
LIBSBML_EXTERN
void
ASTNode::setValue (double value)
{
  setType(AST_REAL);

  mReal     = value;
  mExponent = 0;
}


/*
 * Sets the value of this ASTNode to the given real (double) in two parts:
 * the mantissa and the exponent.  The node type is set to AST_REAL_E.
 */
LIBSBML_EXTERN
void
ASTNode::setValue (double mantissa, long exponent)
{
  setType(AST_REAL_E);

  mReal     = mantissa;
  mExponent = exponent;
}


/*
 * Sets the type of this ASTNode to the given ASTNodeType.
 */
LIBSBML_EXTERN
void
ASTNode::setType (ASTNodeType_t type)
{
  if (mType == type) return;


  if (isOperator() || isNumber())
  {
    mReal     = 0;
    mExponent = 0;
  }

  /**
   * Free name only if the ASTNodeType is being set to something that
   * cannot contain a string.
   *
   * Note: freeName() will only free value.name if there is something to be
   * freed.
   */
  if ( ASTNodeType_isOperator(type) || ASTNodeType_isNumber(type) )
  {
    freeName();
  }

  if ( ASTNodeType_isOperator(type) )
  {
    mType = type;
    mChar = (char) type;
  }
  else if ((type >= AST_INTEGER) && (type < AST_UNKNOWN))
  {
    mType = type;
  }
  else
  {
    mType = AST_UNKNOWN;
  }
}


/*
 * Swap the children of this ASTNode with the children of that ASTNode.
 */
LIBSBML_EXTERN
void
ASTNode::swapChildren (ASTNode *that)
{
  List* temp;


  temp            = this->mChildren;
  this->mChildren = that->mChildren;
  that->mChildren = temp;
}


/** @cond doxygen-libsbml-internal */
/**
  * Sets the flag indicating that this ASTNode has semantics attached
  */
void 
ASTNode::setSemanticsFlag() 
{ 
  hasSemantics = true; 
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
  * Unsets the flag indicating that this ASTNode has semantics attached
  */
void 
ASTNode::unsetSemanticsFlag()
{ 
  hasSemantics = false; 
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
  * gets the flag indicating that this ASTNode has semantics attached
  */
bool 
ASTNode::getSemanticsFlag() const
{
  return hasSemantics;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
  * sets the definitionURL attributes
  */
void 
ASTNode::setDefinitionURL(XMLAttributes url)
{
  mDefinitionURL = static_cast<XMLAttributes *>(url.clone());
}

/** @endcond doxygen-libsbml-internal */


LIBSBML_EXTERN
void
ASTNode::ReplaceArgument(const std::string bvar, ASTNode * arg)
{
  if (arg == NULL)
    return;

  for (unsigned int i = 0; i < getNumChildren(); i++)
  {
    if (getChild(i)->isName())
    {
      if (getChild(i)->getName() == bvar)
      {
        if (arg->isName())
        {
          getChild(i)->setName(arg->getName());
        }
        else if (arg->isReal())
        {
          getChild(i)->setValue(arg->getReal());
        }
        else if (arg->isInteger())
        {
          getChild(i)->setValue(arg->getInteger());
        }
        else if (arg->isConstant())
        {
          getChild(i)->setType(arg->getType());
        }
        else
        {
          getChild(i)->setType(arg->getType());
          for (unsigned int c = 0; c < arg->getNumChildren(); c++)
          {
            getChild(i)->addChild(arg->getChild(c)->deepCopy());
          }
        }
      }
    }
    else
    {
      getChild(i)->ReplaceArgument(bvar, arg);
    }
  }
}



/**
  * gets the definitionURL attributes
  */
LIBSBML_EXTERN
XMLAttributes*
ASTNode::getDefinitionURL() const
{
  return mDefinitionURL;
}





/**
 * Creates a new ASTNode and returns a pointer to it.  The returned node
 * will have a type of AST_UNKNOWN and should be set to something else as
 * soon as possible.
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_create (void)
{
  return new(nothrow) ASTNode;
}


/**
 * Creates a new ASTNode and sets its type to the given ASTNodeType.
 */
ASTNode_t *
ASTNode_createWithType (ASTNodeType_t type)
{
  return new(nothrow) ASTNode(type);
}


/**
 * Creates a new ASTNode from the given Token and returns a pointer to it.
 * The returned ASTNode will contain the same data as the Token.
 */
ASTNode_t *
ASTNode_createFromToken (Token_t *token)
{
  return new(nothrow) ASTNode(token);
}


/**
 * Frees the given ASTNode including any child nodes.
 */
LIBSBML_EXTERN
void
ASTNode_free (ASTNode_t *node)
{
  delete static_cast<ASTNode*>(node);
}


/**
 * Frees the name of this ASTNode and sets it to NULL, if appropriate,
 * i.e. the node is not an operator, number or AST_UNKNOWN.
 */
void
ASTNode_freeName (ASTNode_t *node)
{
  static_cast<ASTNode*>(node)->freeName();
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
  return (int) static_cast<ASTNode*>(node)->canonicalize();
}


/**
 * Adds the given node as a child of this ASTNode.  Child nodes are added
 * in-order from "left-to-right".
 */
LIBSBML_EXTERN
void
ASTNode_addChild (ASTNode_t *node, ASTNode_t *child)
{
  static_cast<ASTNode*>(node)->addChild( static_cast<ASTNode*>(child) );
}


/**
 * Adds the given node as a child of this ASTNode.  This method adds child
 * nodes from "right-to-left".
 */
LIBSBML_EXTERN
void
ASTNode_prependChild (ASTNode_t *node, ASTNode_t *child)
{
  static_cast<ASTNode*>(node)->prependChild( static_cast<ASTNode*>(child) );
}


/**
 * @return a copy of this ASTNode and all its children.  The caller owns
 * the returned ASTNode and is reponsible for freeing it.
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_deepCopy (const ASTNode_t *node)
{
  return
    static_cast<ASTNode_t *>( static_cast<const ASTNode*>(node)->deepCopy() );
}


/**
 * @return the nth child of this ASTNode or NULL if this node has no nth
 * child (n > ASTNode_getNumChildren() - 1).
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getChild (const ASTNode_t *node, unsigned int n)
{
  return static_cast<const ASTNode*>(node)->getChild(n);
}


/**
 * @return the left child of this ASTNode.  This is equivalent to
 * ASTNode_getChild(node, 0);
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getLeftChild (const ASTNode_t *node)
{
  return static_cast<const ASTNode*>(node)->getLeftChild();
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
  return static_cast<const ASTNode*>(node)->getRightChild();
}


/**
 * @return the number of children of this ASTNode or 0 is this node has no
 * children.
 */
LIBSBML_EXTERN
unsigned int
ASTNode_getNumChildren (const ASTNode_t *node)
{
  return static_cast<const ASTNode*>(node)->getNumChildren();
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
  return static_cast<const ASTNode*>(node)->getListOfNodes(predicate);
}


/**
 * This method is identical in functionality to ASTNode_getListOfNodes(),
 * except the List is passed-in by the caller.
 */
LIBSBML_EXTERN
void
ASTNode_fillListOfNodes ( const ASTNode_t  *node,
                          ASTNodePredicate predicate,
                          List_t           *lst )
{
  List* x = static_cast<List*>(lst);

  static_cast<const ASTNode*>(node)->fillListOfNodes(predicate, x);
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
  return static_cast<const ASTNode*>(node)->getCharacter();
}


/**
 * @return the value of this ASTNode as a (long) integer.  This function
 * should be called only when ASTNode_getType() == AST_INTEGER.
 */
LIBSBML_EXTERN
long
ASTNode_getInteger (const ASTNode_t *node)
{
  return static_cast<const ASTNode*>(node)->getInteger();
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
  return static_cast<const ASTNode*>(node)->getName();
}


/**
 * @return the value of the numerator of this ASTNode.  This function
 * should be called only when ASTNode_getType() == AST_RATIONAL.
 */
LIBSBML_EXTERN
long
ASTNode_getNumerator (const ASTNode_t *node)
{
  return static_cast<const ASTNode*>(node)->getNumerator();
}


/**
 * @return the value of the denominator of this ASTNode.  This function
 * should be called only when ASTNode_getType() == AST_RATIONAL.
 */
LIBSBML_EXTERN
long
ASTNode_getDenominator (const ASTNode_t *node)
{
  return static_cast<const ASTNode*>(node)->getDenominator();
}


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
ASTNode_getReal (const ASTNode_t *node)
{
  return static_cast<const ASTNode*>(node)->getReal();
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
  return static_cast<const ASTNode*>(node)->getMantissa();
}


/**
 * @return the value of the exponent of this ASTNode.  This function should
 * be called only when ASTNode_getType() is AST_REAL_E or AST_REAL.
 */
LIBSBML_EXTERN
long
ASTNode_getExponent (const ASTNode_t *node)
{
  return static_cast<const ASTNode*>(node)->getExponent();
}


/**
 * @return the precedence of this ASTNode (as defined in the SBML L1
 * specification).
 */
LIBSBML_EXTERN
int
ASTNode_getPrecedence (const ASTNode_t *node)
{
  return static_cast<const ASTNode*>(node)->getPrecedence();
}


/**
 * @return the type of this ASTNode.
 */
LIBSBML_EXTERN
ASTNodeType_t
ASTNode_getType (const ASTNode_t *node)
{
  return static_cast<const ASTNode*>(node)->getType();
}


/**
 * @return true (non-zero) if this ASTNode is a boolean (a logical
 * operator, a relational operator, or the constants true or false), false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isBoolean (const ASTNode_t *node)
{
  return (int) static_cast<const ASTNode*>(node)->isBoolean();
}


/**
 * @return true (non-zero) if this ASTNode is a MathML constant (true,
 * false, pi, exponentiale), false (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isConstant (const ASTNode_t *node)
{
  return (int) static_cast<const ASTNode*>(node)->isConstant();
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
  return (int) static_cast<const ASTNode*>(node)->isFunction();
}


/**
 * @return true if this ASTNode is the special IEEE 754 value infinity,
 * false otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isInfinity (const ASTNode_t *node)
{
  return static_cast<int>( node->isInfinity() );
}


/**
 * @return true (non-zero) if this ASTNode is of type AST_INTEGER, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isInteger (const ASTNode_t *node)
{
  return (int) static_cast<const ASTNode*>(node)->isInteger();
}


/**
 * @return true (non-zero) if this ASTNode is of type AST_LAMBDA, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isLambda (const ASTNode_t *node)
{
  return (int) static_cast<const ASTNode*>(node)->isLambda();
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
  return (int) static_cast<const ASTNode*>(node)->isLog10();
}


/**
 * @return true (non-zero) if this ASTNode is a MathML logical operator
 * (and, or, not, xor), false (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isLogical (const ASTNode_t *node)
{
  return (int) static_cast<const ASTNode*>(node)->isLogical();
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
  return (int) static_cast<const ASTNode*>(node)->isName();
}


/**
 * @return true (non-zero) if this ASTNode is the special IEEE 754 value
 * not a number, false (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isNaN (const ASTNode_t *node)
{
  return static_cast<int>( node->isNaN() );
}


/**
 * @return true if this ASTNode is the special IEEE 754 value negative
 * infinity, false otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isNegInfinity (const ASTNode_t *node)
{
  return static_cast<int>( node->isNegInfinity() );
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
  return (int) static_cast<const ASTNode*>(node)->isNumber();
}


/**
 * @return true (non-zero) if this ASTNode is an operator, false (0)
 * otherwise.  Operators are: +, -, *, / and \^ (power).
 */
LIBSBML_EXTERN
int
ASTNode_isOperator (const ASTNode_t *node)
{
  return (int) static_cast<const ASTNode*>(node)->isOperator();
}


/**
 * @return true (non-zero) if this ASTNode is a piecewise function, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isPiecewise (const ASTNode_t *node)
{
  return static_cast<int>( node->isPiecewise() );
}


/**
 * @return true (non-zero) if this ASTNode is of type AST_RATIONAL, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isRational (const ASTNode_t *node)
{
  return (int) static_cast<const ASTNode*>(node)->isRational();
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
  return (int) static_cast<const ASTNode*>(node)->isReal();
}


/**
 * @return true (non-zero) if this ASTNode is a MathML relational operator
 * (==, >=, >, <=, < !=), false (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isRelational (const ASTNode_t *node)
{
  return (int) static_cast<const ASTNode*>(node)->isRelational();
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
  return (int) static_cast<const ASTNode*>(node)->isSqrt();
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
  return (int) static_cast<const ASTNode*>(node)->isUMinus();
}


/**
 * @return true (non-zero) if this ASTNode is of type AST_UNKNOWN, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
ASTNode_isUnknown (const ASTNode_t *node)
{
  return (int) static_cast<const ASTNode*>(node)->isUnknown();
}


/**
 * Sets the value of this ASTNode to the given character.  If character is
 * one of '+', '-', '*', '/' or '\^', the node type will be set accordingly.
 * For all other characters, the node type will be set to AST_UNKNOWN.
 */
LIBSBML_EXTERN
void
ASTNode_setCharacter (ASTNode_t *node, char value)
{
  static_cast<ASTNode*>(node)->setCharacter(value);
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
  static_cast<ASTNode*>(node)->setName(name);
}


/**
 * Sets the value of this ASTNode to the given (long) integer and sets the
 * node type to AST_INTEGER.
 */
LIBSBML_EXTERN
void
ASTNode_setInteger (ASTNode_t *node, long value)
{
  static_cast<ASTNode*>(node)->setValue(value);
}


/**
 * Sets the value of this ASTNode to the given rational in two parts:
 * the numerator and denominator.  The node type is set to AST_RATIONAL.
 */
LIBSBML_EXTERN
void
ASTNode_setRational (ASTNode_t *node, long numerator, long denominator)
{
  static_cast<ASTNode*>(node)->setValue(numerator, denominator);
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
  static_cast<ASTNode*>(node)->setValue(value);
}


/**
 * Sets the value of this ASTNode to the given real (double) in two parts:
 * the mantissa and the exponent.  The node type is set to AST_REAL_E.
 */
LIBSBML_EXTERN
void
ASTNode_setRealWithExponent (ASTNode_t *node, double mantissa, long exponent)
{
  static_cast<ASTNode*>(node)->setValue(mantissa, exponent);
}


/**
 * Sets the type of this ASTNode to the given ASTNodeType_t.
 */
LIBSBML_EXTERN
void
ASTNode_setType (ASTNode_t *node, ASTNodeType_t type)
{
  static_cast<ASTNode*>(node)->setType(type);
}


/**
 * Swap the children of this ASTNode with the children of that ASTNode.
 */
LIBSBML_EXTERN
void
ASTNode_swapChildren (ASTNode_t *node, ASTNode_t *that)
{
  static_cast<ASTNode*>(node)->swapChildren( static_cast<ASTNode*>(that) );
}

/**
  * replaces occurences of a name within the given ASTNode_t structure
  * with the name/value/formula represented by the second argument ASTNode_t
  * e.g. if the formula in this ASTNode is x + y; bvar is x and arg is an 
  * ASTNode representing the real value 3 ReplaceArgument substitutes 3 for
  * x within this ASTNode
  *
  * @param node the ASTNode_t structure to replace argument
  * @param bvar a string representing the variable name to be substituted
  * @param arg an ASTNode representing the name/value/formula to substitute
  */
LIBSBML_EXTERN
void
ASTNode_replaceArgument(ASTNode_t* node, const char * bvar, ASTNode_t* arg)
{
  static_cast<ASTNode*>(node)->ReplaceArgument(bvar, 
                                                  static_cast<ASTNode*>(arg));
}
