/**
 * @file    ASTNode.cpp
 * @brief   Abstract Syntax Tree (AST) for representing formula trees.
 * @author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <new>
#include <cmath>
#include <stdlib.h>
#include <limits.h>

#include <sbml/common/common.h>
#include <sbml/util/List.h>

#include <sbml/math/ASTNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/Model.h>
#include <sbml/util/IdList.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/math/L3FormulaFormatter.h>

/** @cond doxygenIgnored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * ASTNodeType predicate
 */
#define ASTNodeType_isAvogadro(t) \
  (t == AST_NAME_AVOGADRO)

/**
 * ASTNodeType predicate
 */
#define ASTNodeType_isConstant(t) \
  (((t >= AST_CONSTANT_E) && (t <= AST_CONSTANT_TRUE)) || t == AST_NAME_AVOGADRO)

/**
 * ASTNodeType predicate
 */
#define ASTNodeType_isFunction(t) \
  (((t >= AST_FUNCTION) && (t <= AST_FUNCTION_TANH)) || t == AST_CSYMBOL_FUNCTION)

/**
 * ASTNodeType predicate
 */
#define ASTNodeType_isLambda(t) \
  (t == AST_LAMBDA)

/**
 * ASTNodeType predicate
 */
#define ASTNodeType_isLogical(t) \
  (((t >= AST_LOGICAL_AND) && (t <= AST_LOGICAL_XOR)))

/**
 * ASTNodeType predicate
 */
#define ASTNodeType_isName(t) \
  ((t >= AST_NAME) && (t <= AST_NAME_TIME))

/**
 * ASTNodeType predicate
 */
#define ASTNodeType_isRelational(t) \
  ((t >= AST_RELATIONAL_EQ) && (t <= AST_RELATIONAL_NEQ))

/**
 * ASTNodeType predicate
 */
#define ASTNodeType_isInteger(t) \
  (t == AST_INTEGER)

/**
 * ASTNodeType predicate
 */
#define ASTNodeType_isRational(t) \
  (t == AST_RATIONAL)

/**
 * ASTNodeType predicate
 */
#define ASTNodeType_isReal(t) \
  ((t >= AST_REAL) && (t <= AST_RATIONAL))

/**
 * ASTNodeType predicate
 */
#define ASTNodeType_isNumber(t) \
  (ASTNodeType_isInteger(t) || ASTNodeType_isReal(t))

 /**
 * ASTNodeType predicate
 */
#define ASTNodeType_isConstantNumber(t) \
  (t == AST_CONSTANT_E || t == AST_CONSTANT_PI || t == AST_NAME_AVOGADRO)
 /**
 * ASTNodeType predicate
 */
#define ASTNodeType_isOperator(t) \
  ( ( t == AST_PLUS   ) || \
    ( t == AST_MINUS  ) || \
    ( t == AST_TIMES  ) || \
    ( t == AST_DIVIDE ) || \
    ( t == AST_POWER  ) )

/**
 * ASTNodeType predicate
 */
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
  , "avogadro"
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


static const char *AST_OPERATOR_STRINGS[] =
{
    "divide"
  , "minus"
  , "plus"
  , "times"
  , "power"
};


#ifdef __cplusplus

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
  mChar          = 0;
  mName          = NULL;
  mInteger       = 0;
  mDenominator   = 1;
  mParentSBMLObject = NULL;
  mUnits         = "";
  mId			 = "";
  mClass		 = "";
  mStyle		 = "";
  mIsBvar = false;
  mUserData      = NULL;
  mNamespaces = NULL;

  // move to after we have loaded plugins
  //setType(type);

  mChildren             = new List;
  mSemanticsAnnotations = new List;
  // only load plugins when we need to
  //if (type > AST_END_OF_CORE && type < AST_UNKNOWN)
  //{
  //  loadASTPlugins(NULL);
  //  for (unsigned int i = 0; i < getNumPlugins(); i++)
  //  {
  //    getPlugin(i)->connectToParent(this);
  //  }
  //}
  
  setType(type);
}


/*
 * Creates a new ASTNode from the given Token_t structure.                   
 *                                                                           
 * The resulting ASTNode will contain the same data as the Token_t           
 * object.  Please refer to the documentation for Token_t to learn           
 * about the possible contents.                                              
 */
LIBSBML_EXTERN
ASTNode::ASTNode (Token_t* token)
{
  unsetSemanticsFlag();
  mDefinitionURL = new XMLAttributes();
  mReal          = 0;
  mExponent      = 0;
  mType          = AST_UNKNOWN;
  mChar          = 0;
  mName          = NULL;
  mInteger       = 0;
  mDenominator   = 1;
  mParentSBMLObject = NULL;
  mUnits         = "";
  mId			 = "";
  mClass		 = "";
  mStyle		 = "";
  mIsBvar = false;
  mUserData      = NULL;
  mNamespaces = NULL;

  mChildren             = new List;
  mSemanticsAnnotations = new List;

  if (token != NULL)
  {
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
  //loadASTPlugins(NULL);
  //for (unsigned int i = 0; i < getNumPlugins(); i++)
  //{
  //  getPlugin(i)->connectToParent(this);
  //}
}

/*
* Used by the Copy Constructor to clone each item in mPlugins.
*/
struct CloneASTPluginEntity 
{
  ASTBasePlugin* operator() (ASTBasePlugin* ast) {
    if (!ast) return 0;
    return ast->clone();
  }
};


/*
 * 
 * Copy constructor; Creates a deep copy of the given ASTNode
 *
 */
LIBSBML_EXTERN
ASTNode::ASTNode (const ASTNode& orig) :
  mType                 ( orig.mType )
 ,mChar                 ( orig.mChar )
 ,mName                 ( NULL )
 ,mInteger              ( orig.mInteger )
 ,mReal                 ( orig.mReal )
 ,mDenominator          ( orig.mDenominator )
 ,mExponent             ( orig.mExponent )
 ,mDefinitionURL        ( orig.mDefinitionURL->clone() )	
 ,hasSemantics          ( orig.hasSemantics )
 ,mChildren             ( new List() )
 ,mSemanticsAnnotations ( new List() )
 ,mParentSBMLObject     ( orig.mParentSBMLObject )
 ,mUnits                ( orig.mUnits)
 ,mId                   ( orig.mId)
 ,mClass                ( orig.mClass)
 ,mStyle                ( orig.mStyle)
 ,mIsBvar               ( orig.mIsBvar)
 ,mUserData             ( orig.mUserData )
 , mNamespaces          (NULL)
{
  if (orig.mName)
  {
    mName = safe_strdup(orig.mName);
  }

  for (unsigned int c = 0; c < orig.getNumChildren(); ++c)
  {
    addChild( orig.getChild(c)->deepCopy() );
  }

  for (unsigned int c = 0; c < orig.getNumSemanticsAnnotations(); ++c)
  {
    addSemanticsAnnotation( orig.getSemanticsAnnotation(c)->clone() );
  }
  if (orig.mNamespaces != NULL)
    this->mNamespaces =
    new XMLNamespaces(*const_cast<XMLNamespaces*>(orig.mNamespaces));
  mPlugins.resize(orig.mPlugins.size());
  transform(orig.mPlugins.begin(), orig.mPlugins.end(),
    mPlugins.begin(), CloneASTPluginEntity());
  for (size_t i = 0; i < mPlugins.size(); i++)
  {
    getPlugin((unsigned int)i)->connectToParent(this);
  }
}

/*
 * 
 * assignment operator
 *
 */
LIBSBML_EXTERN
ASTNode& ASTNode::operator=(const ASTNode& rhs)
{
  if(&rhs!=this)
  {
    mType                 = rhs.mType;
    mChar                 = rhs.mChar;
    mInteger              = rhs.mInteger;
    mReal                 = rhs.mReal;
    mDenominator          = rhs.mDenominator;
    mExponent             = rhs.mExponent;
    hasSemantics          = rhs.hasSemantics;
    mParentSBMLObject     = rhs.mParentSBMLObject;
    mUnits                = rhs.mUnits;
    mId                   = rhs.mId;
    mClass                = rhs.mClass;
    mStyle                = rhs.mStyle;
    mIsBvar               = rhs.mIsBvar;
    mUserData             = rhs.mUserData;

    freeName();
    if (rhs.mName)
    {
      mName = safe_strdup(rhs.mName);
    }
    else
    {
      mName = NULL;
    }

    unsigned int size = mChildren->getSize();
    while (size--) delete static_cast<ASTNode*>( mChildren->remove(0) );
    delete mChildren;
    mChildren = new List();

    for (unsigned int c = 0; c < rhs.getNumChildren(); ++c)
    {
      addChild( rhs.getChild(c)->deepCopy() );
    }

    size = mSemanticsAnnotations->getSize();
    while (size--)  delete static_cast<XMLNode*>(mSemanticsAnnotations->remove(0) );
    delete mSemanticsAnnotations;
    mSemanticsAnnotations = new List();

    for (unsigned int c = 0; c < rhs.getNumSemanticsAnnotations(); ++c)
    {
      addSemanticsAnnotation( rhs.getSemanticsAnnotation(c)->clone() );
    }
    
    delete mDefinitionURL;
    mDefinitionURL        = rhs.mDefinitionURL->clone();	
    unsetDeclaredNamespaces();
    if (rhs.mNamespaces != NULL)
      this->mNamespaces =
      new XMLNamespaces(*const_cast<XMLNamespaces*>(rhs.mNamespaces));

    clearPlugins();
    mPlugins.resize(rhs.mPlugins.size());
    transform(rhs.mPlugins.begin(), rhs.mPlugins.end(),
      mPlugins.begin(), CloneASTPluginEntity());
  }
  return *this;
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
  while (size--)  delete static_cast<XMLNode*>(mSemanticsAnnotations->remove(0) );
  delete mSemanticsAnnotations;

  delete mDefinitionURL;

  unsetDeclaredNamespaces();
  
  freeName();
  clearPlugins();

}


/*
 * Frees the name of this ASTNode and sets it to NULL.
 * 
 * This operation is only applicable to ASTNodes corresponding to
 * operators, numbers, or AST_UNKNOWN.  This method will have no
 * effect on other types of nodes.
 */
int
ASTNode::freeName ()
{
  if (mName != NULL)
  {
    safe_free(mName);
    mName = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
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

/** @cond doxygenLibsbmlInternal */
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
/** @endcond */


/** @cond doxygenLibsbmlInternal */
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


  /*
   * Search for SBML Level 1 function names first.
   */
  found = canonicalizeFunctionL1();

  /*
   * Now Lambda...
   */
  if (!found)
  {
    if ( (found = !strcmp_insensitive(mName, AST_LAMBDA_STRING)) )
    {
      setType(AST_LAMBDA);
    }
  }

  /*
   * ... the L2 (MathML) function names.
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

  // and then the ones defined in packages/l3v2.
  //if (!found)
  //{
  //  ASTNodeType_t type = mExtendedMathList->getASTNodeTypeForFunction(mName);
  //  if (type != AST_UNKNOWN) {
  //    found = true;
  //    setType(type);
  //  }
  //}


  return found;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
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

  /*
   * "log(x)" in L1 is represented as "ln(x)" in L2.
   *
   * Notice, however, that the conversion is performed only if the number of
   * arguments is 1.  Thus "log(5, x)" will still be "log(5, x) when passed
   * through this filter.
   */
  else if ( !strcmp_insensitive(mName, "log") && (getNumChildren() == 1) )
  {
    setType(AST_FUNCTION_LN);
  }

  /*
   * "log10(x)" in L1 is represented as "log(10, x)" in L2.
   */
  else if ( !strcmp_insensitive(mName, "log10") && (getNumChildren() == 1) )
  {
    setType(AST_FUNCTION_LOG);

    child = new ASTNode;
    child->setValue(10);

    prependChild(child);
  }

  /*
   * Here we set the type to AST_FUNCTION_POWER.  We could set it to
   * AST_POWER, but then we would loose the idea that it was a function
   * before it was canonicalized.
   */
  else if ( !strcmp_insensitive(mName, "pow") )
  {
    setType(AST_FUNCTION_POWER);
  }

  /*
   * "sqr(x)" in L1 is represented as "power(x, 2)" in L2.
   */
  else if ( !strcmp_insensitive(mName, "sqr") && (getNumChildren() == 1) )
  {
    setType(AST_FUNCTION_POWER);

    child = new ASTNode;
    child->setValue(2);

    addChild(child);
  }

  /*
   * "sqrt(x) in L1 is represented as "root(2, x)" in L1.
   */
  else if ( !strcmp_insensitive(mName, "sqrt") && (getNumChildren() == 1) )
  {
    setType(AST_FUNCTION_ROOT);

    child = new ASTNode;
    child->setValue(2);

    prependChild(child);
  }

  /*
   * Was a conversion performed?
   */
  return (mType != AST_FUNCTION);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Internal helper function for canonicalize().
 */
bool
ASTNode::canonicalizeLogical ()
{
  const int first = AST_LOGICAL_AND;
  const int last  = AST_LOGICAL_XOR;
  const int size  = last - first + 1;

  int  index = util_bsearchStringsI(AST_LOGICAL_STRINGS, mName, 0, size - 1);
  bool found = (index < size);

  if (found)
  {
    setType( static_cast<ASTNodeType_t>(first + index) );
  }

  //if (!found)
  //{
  //  ASTNodeType_t type = mExtendedMathList->getASTNodeTypeForFunction(mName);
  //  if (type != AST_UNKNOWN) {
  //    found = true;
  //    setType( static_cast<ASTNodeType_t>(new_first + index) );
  //  }
  //}

  return found;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
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
/** @endcond */


/*
 * Adds the given node as a child of this ASTNode.  Child nodes are added
 * in-order from "left-to-right".
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
ASTNode::addChild (ASTNode* child, bool inRead)
{

  unsigned int numBefore = getNumChildren();
  mChildren->add(child);

  /* HACK to allow representsBVar function to be correct */
  if (inRead == false && this->getType() == AST_LAMBDA
    && numBefore > 0)
  {
    getChild(numBefore-1)->setBvar();
  }

  if (getNumChildren() == numBefore + 1)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Adds the given node as a child of this ASTNode.  This method adds child
 * nodes from "right-to-left".
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
ASTNode::prependChild (ASTNode* child)
{
  if (child == NULL) return LIBSBML_INVALID_OBJECT;

  unsigned int numBefore = getNumChildren();
  mChildren->prepend(child);

  if (getNumChildren() == numBefore + 1)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


LIBSBML_EXTERN
int  
ASTNode::removeChild(unsigned int n, bool delremoved)
{
  int removed = LIBSBML_INDEX_EXCEEDS_SIZE;
  unsigned int size = getNumChildren();
  if (n < size)
  {
    ASTNode* child = (ASTNode*)(mChildren->remove(n));
    if (getNumChildren() == size-1)
    {
      removed = LIBSBML_OPERATION_SUCCESS;
    }
    if (delremoved)
    {
      delete child;
    }
  }

  return removed;
}

LIBSBML_EXTERN
int 
ASTNode::replaceChild(unsigned int n, ASTNode *newChild, bool delreplaced)
{
  if (newChild == NULL) return LIBSBML_INVALID_OBJECT;

  int replaced = LIBSBML_INDEX_EXCEEDS_SIZE;

  unsigned int size = getNumChildren();
  if (n < size)
  {
    ASTNode* rep = static_cast<ASTNode*>(mChildren->remove(n));
    if (delreplaced) 
    {
      delete rep;
    }
    if (insertChild(n, newChild) == LIBSBML_OPERATION_SUCCESS)
      replaced = LIBSBML_OPERATION_SUCCESS;    
  }
    
  return replaced;
}

LIBSBML_EXTERN
int 
ASTNode::insertChild(unsigned int n, ASTNode *newChild)
{
  if (newChild == NULL) return LIBSBML_INVALID_OBJECT;

  int inserted = LIBSBML_INDEX_EXCEEDS_SIZE;

  unsigned int i, size = getNumChildren();
  if (n == 0)
  {
    prependChild(newChild);
    inserted = LIBSBML_OPERATION_SUCCESS;
  }
  else if (n <= size) 
  {
    /* starting at the end take each child in the list and prepend it
    * then remove it from the end
    * at the insertion point prepend the newChild
    * eg list: a, b, c 
    * inserting d at position 2
    * list goes: c, a, b :  d, c, a, b : b, d, c, a : a, b, d, c
    */
    for (i = size-1; i >= n; i--)
    {
      prependChild(getChild(size-1));
      mChildren->remove(size);
    }

    prependChild(newChild);

    for (i = 0; i < n; i++)
    {
      prependChild(getChild(size));
      mChildren->remove(size+1);
    }

    if (getNumChildren() == size + 1)
      inserted = LIBSBML_OPERATION_SUCCESS;
  }

  /* HACK TO Make representBvar work */
  // mark all but last child as bvar
  // unless we have only inserted one
  if (size > 1)
  {
    for (unsigned int c = 0; c < getNumChildren() - 1; c++)
    {
      getChild(c)->setBvar();
    }
  }

  return inserted;
}

/*
 * @return a copy of this ASTNode and all its children.  The caller owns
 * the returned ASTNode and is reponsible for deleting it.
 */
LIBSBML_EXTERN
ASTNode*
ASTNode::deepCopy () const
{
  return new ASTNode(*this);
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
int 
ASTNode::addSemanticsAnnotation (XMLNode* sAnnotation)
{
  if (sAnnotation == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  mSemanticsAnnotations->add(sAnnotation);
  return LIBSBML_OPERATION_SUCCESS;
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
 * where a return value of nonzero represents true and zero represents
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
  if (predicate == NULL) return NULL;

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
  if (lst == NULL || predicate == NULL) return;

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


  /*
   * If the node does not have a name and is not a user-defined function
   * (type == AST_FUNCTION), use the default name for the builtin node
   * types.
   */
  if (mName == NULL && mType != AST_FUNCTION)
  {
    if ( isConstant() )
    {
      if (mType == AST_NAME_AVOGADRO)
      {
        result = AST_CONSTANT_STRINGS[4];
      }
      else
      {
        result = AST_CONSTANT_STRINGS[ mType - AST_CONSTANT_E ];
      }
    }
    else if ( isLambda() )
    {
      result = AST_LAMBDA_STRING;
    }
    else if ( isFunction() )
    {
      if (mType <= AST_FUNCTION_TANH && mType >= AST_FUNCTION_ABS)
      {
        result = AST_FUNCTION_STRINGS[ mType - AST_FUNCTION_ABS ];
      }
    }
    else if ( isLogical() )
    {
      if (mType <= AST_RELATIONAL_NEQ)
      {
        result = AST_LOGICAL_STRINGS[ mType - AST_LOGICAL_AND ];
      }
    }
    else if ( isRelational() )
    {
      result = AST_RELATIONAL_STRINGS[ mType - AST_RELATIONAL_EQ ];
    }
  }
  if (result == NULL && mType > AST_END_OF_CORE)
  {
    const ASTBasePlugin * baseplugin = getASTPlugin(mType);
    if (baseplugin != NULL)
    {
      result = baseplugin->getConstCharFor(mType);
    }
    //if (getNumPlugins() == 0)
    //{
    //  const_cast<ASTNode*>(this)->loadASTPlugins(NULL);
    //}

    //unsigned int i = 0;
    //while (result == NULL && i < getNumPlugins())
    //{
    //  result = getPlugin(i)->getConstCharFor(mType);
    //  i++;
    //}
  }
  //if (result == NULL && mExtendedMathList->defines(mType))
  //{
  //  result = mExtendedMathList->getConstCharFor(mType);
  //}

  return result;
}


LIBSBML_EXTERN
const char*
ASTNode::getOperatorName () const
{
  switch(mType) {
  case AST_DIVIDE:
    return AST_OPERATOR_STRINGS[0];
  case AST_MINUS:
    return AST_OPERATOR_STRINGS[1];
  case AST_PLUS:
    return AST_OPERATOR_STRINGS[2];
  case AST_TIMES:
    return AST_OPERATOR_STRINGS[3];
  case AST_POWER:
    return AST_OPERATOR_STRINGS[4];
  default:
    return NULL;
  }
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


LIBSBML_EXTERN
double
ASTNode::getValue () const
{
  double value = util_NaN();

  switch(mType)
  {
  case AST_CONSTANT_E:
    value = 2.71828182;
    break;
  case AST_CONSTANT_FALSE:
    value = 0;
    break;
  case AST_CONSTANT_PI:
    value = 3.14159292;
    break;
  case AST_CONSTANT_TRUE:
    value = 1;
    break;
  case AST_INTEGER:
    value = (double)(getInteger());
    break;
  case AST_NAME_AVOGADRO:
  case AST_RATIONAL:
  case AST_REAL:
  case AST_REAL_E:
    value = getReal();
    break;
  default:
    break;
  }

  return value;

}

/*
 * @return the precedence of this ASTNode (as defined in the SBML L1
 * specification).
 */
LIBSBML_EXTERN
int
ASTNode::getPrecedence () const
{
  int precedence = 6;


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
        if (mType > AST_END_OF_CORE)
        {
          const ASTBasePlugin * baseplugin = getASTPlugin(mType);
          if (baseplugin != NULL)
          {
            precedence = baseplugin->getL3PackageInfixPrecedence();
          }
        }
        else
        {
          precedence = 6;
        }
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

LIBSBML_EXTERN
std::string
ASTNode::getId() const
{
  return mId;
}

LIBSBML_EXTERN
std::string
ASTNode::getClass() const
{
  return mClass;
}

LIBSBML_EXTERN
std::string
ASTNode::getStyle() const
{
  return mStyle;
}

LIBSBML_EXTERN
std::string
ASTNode::getUnits() const
{
  return mUnits;
}

/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
bool
ASTNode::usesL3V2MathConstructs() const
{
  bool mathConstructs = false;

  ASTNodeType_t type = getType();

  if (type > AST_END_OF_CORE)
  {
    if (getASTPlugin(type) != NULL)
      mathConstructs = true;
  }
  unsigned int i = 0;
  while (!mathConstructs && i < getNumChildren())
  {
    mathConstructs = getChild(i)->usesL3V2MathConstructs();
    i++;
  }

  return mathConstructs;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
bool
ASTNode::usesRateOf() const
{
  bool mathConstructs = false;

  if (getType() == AST_FUNCTION_RATE_OF)
      mathConstructs = true;
  unsigned int i = 0;
  while (!mathConstructs && i < getNumChildren())
  {
    mathConstructs = getChild(i)->usesRateOf();
    i++;
  }

  return mathConstructs;
}
/** @endcond */

/*
 * @return true if this ASTNode is a boolean (a logical operator, a
 * relational operator, or the constants true or false), false otherwise.
 */
LIBSBML_EXTERN
bool
ASTNode::isAvogadro () const
{
  return ASTNodeType_isAvogadro(mType);
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


LIBSBML_EXTERN
bool
ASTNode::returnsBoolean (const Model* givenModel /*=NULL*/) const
{   

  if (isBoolean() == true)
  {
    return true;
  }

  const Model* model = givenModel;
  if (givenModel == NULL && getParentSBMLObject() != NULL)
  {
    model = getParentSBMLObject()->getModel();
  }

  if (getType() == AST_FUNCTION)
  {
    if (model == NULL)
    {
      return false;
    }
    else
    {
      const FunctionDefinition* fd = model->getFunctionDefinition( getName() );

      if (fd != NULL && fd->isSetMath())
      {
        return (fd->getBody() != NULL ? 
          fd->getBody()->returnsBoolean() : false);
      }
      else
      {
        return false;
      }
    }
  }

  else if (getType() == AST_FUNCTION_PIECEWISE)
  {
    for (unsigned int c = 0; c < getNumChildren(); c += 2)
    {
      if ( getChild(c)->returnsBoolean() == false ) 
        return false;
    }

    return true;
  }

  // add explicit return value in case we overlooked something
  return false;
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
* @return true if this ASTNode is a MathML constant number
*(pi, exponentiale), false otherwise.
*/
LIBSBML_EXTERN
bool
ASTNode::isCiNumber() const
{
  return (mType == AST_NAME);
}




/*
 * @return true if this ASTNode is a MathML constant number 
 *(pi, exponentiale), false otherwise.
 */
LIBSBML_EXTERN
bool 
ASTNode::isConstantNumber() const
{
  return ASTNodeType_isConstantNumber(mType);
}


/*
* @return true if this ASTNode is a MathML constant number
*(pi, exponentiale), false otherwise.
*/
LIBSBML_EXTERN
bool
ASTNode::isCSymbolFunction() const
{
  if (mType == AST_FUNCTION_DELAY)
  {
    return true;
  }
  const ASTBasePlugin* baseplugin = getASTPlugin(mType);
  if (baseplugin != NULL)
  {
    const char* csymbol = baseplugin->getConstCharCsymbolURLFor(mType);
    return (csymbol != NULL && !((string)csymbol).empty() && baseplugin->isFunction(mType));
  }
  return false;
}



bool
ASTNode::isFunction () const
{
  if (ASTNodeType_isFunction(mType))
  {
    return true;
  }
  const ASTBasePlugin* baseplugin = getASTPlugin(mType);
  if (baseplugin != NULL)
  {
    if (baseplugin->isFunction(mType))
    {
      return true;
    }
  }
  return false;
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
  if (ASTNodeType_isLogical(mType))
  {
    return true;
  }
  const ASTBasePlugin* baseplugin = getASTPlugin(mType);
  if (baseplugin != NULL)
  {
    if (baseplugin->isLogical(mType))
    {
      return true;
    }
  }
  return false;
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

LIBSBML_EXTERN
bool
ASTNode::isUPlus () const
{
  bool uplus = false;


  if (mType == AST_PLUS)
  {
    if (getNumChildren() == 1)
    {
      uplus = true;
    }
  }

  return uplus;
}

LIBSBML_EXTERN
bool
ASTNode::isUserFunction() const
{
  return (mType == AST_FUNCTION);
}

LIBSBML_EXTERN
int
ASTNode::hasTypeAndNumChildren(ASTNodeType_t type, unsigned int numchildren) const
{
  return (mType == type && getNumChildren() == numchildren);
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


LIBSBML_EXTERN
bool 
ASTNode::isSetId() const
{
  return (mId.empty() == false);
}
  
LIBSBML_EXTERN
bool 
ASTNode::isSetClass() const
{
  return (mClass.empty() == false);
}
  
LIBSBML_EXTERN
bool 
ASTNode::isSetStyle() const
{
  return (mStyle.empty() == false);
}
  
LIBSBML_EXTERN
bool 
ASTNode::isSetUnits() const
{
  return (mUnits.empty() == false);
}
  

LIBSBML_EXTERN
bool 
ASTNode::hasUnits() const
{
  bool hasUnits = isSetUnits();

  unsigned int n = 0;
  while(!hasUnits && n < getNumChildren())
  {
    hasUnits = getChild(n)->hasUnits();
    n++;
  }

  return hasUnits;
}

  
/*
 * Sets the value of this ASTNode to the given character.  If character is
 * one of '+', '-', '*', '/' or '\^', the node type will be set
 * accordingly.  For all other characters, the node type will be set to
 * AST_UNKNOWN.
 */
LIBSBML_EXTERN
int
ASTNode::setCharacter (char value)
{
  setType( static_cast<ASTNodeType_t>(value) );
  mChar = value;
  return LIBSBML_OPERATION_SUCCESS;
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
int
ASTNode::setName (const char *name)
{
  if (getName() == name) 
    return LIBSBML_OPERATION_SUCCESS;

  unsetUnits();

  if ( isOperator() || isNumber() || isUnknown() )
  {
    mType = AST_NAME;
  }

  freeName();
  mName = (name == NULL) ? NULL : safe_strdup(name);
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of this ASTNode to the given integer and sets the node
 * type to AST_INTEGER.
 */
LIBSBML_EXTERN
int
ASTNode::setValue (int value)
{
  setType(AST_INTEGER);
  mInteger = value;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of this ASTNode to the given (long) integer and sets the
 * node type to AST_INTEGER.
 */
LIBSBML_EXTERN
int
ASTNode::setValue (long value)
{
  setType(AST_INTEGER);
  mInteger = value;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of this ASTNode to the given rational in two parts:
 * the numerator and denominator.  The node type is set to AST_RATIONAL.
 */
LIBSBML_EXTERN
int
ASTNode::setValue (long numerator, long denominator)
{
  setType(AST_RATIONAL);

  mInteger     = numerator;
  mDenominator = denominator;
  return LIBSBML_OPERATION_SUCCESS;
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
int
ASTNode::setValue (double value)
{
  setType(AST_REAL);

  mReal     = value;
  mExponent = 0;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of this ASTNode to the given real (double) in two parts:
 * the mantissa and the exponent.  The node type is set to AST_REAL_E.
 */
LIBSBML_EXTERN
int
ASTNode::setValue (double mantissa, long exponent)
{
  setType(AST_REAL_E);

  mReal     = mantissa;
  mExponent = exponent;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the type of this ASTNode to the given ASTNodeType.
 */
LIBSBML_EXTERN
int
ASTNode::setType (ASTNodeType_t type)
{
  if (mType == type) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }

  if (isOperator() || isNumber())
  {
    mReal     = 0;
    mExponent = 0;
    mDenominator = 1;
    mInteger = 0;
  }

  /* if avogadro set value */
  if (type == AST_NAME_AVOGADRO)
  {
    /* this will free the name - which people may not want */
    /* this solution does not in fact work for all language bindings */

    //if (mName != NULL)
    //{
    //  char * name = mName;
    //  setValue(6.02214179e23);
    //  setName(name);
    //}
    //else
    //{
      mReal = 6.02214179e23;
    //}
    mDefinitionURL->clear();
    mDefinitionURL->add("definitionURL", 
                        "http://www.sbml.org/sbml/symbols/avogadro");
  }
  else if (type == AST_NAME_TIME)
  {
    mDefinitionURL->clear();
    mDefinitionURL->add("definitionURL", 
                        "http://www.sbml.org/sbml/symbols/time");
  }
  else if (type == AST_FUNCTION_DELAY)
  {
    mDefinitionURL->clear();
    mDefinitionURL->add("definitionURL", 
                        "http://www.sbml.org/sbml/symbols/delay");
  }

  /*
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

  // if the new type is not a number unset the units
  if (ASTNodeType_isNumber(type) == 0)
  {
    unsetUnits();
  }

  bool clearDefinitionURL = true;
  if (ASTNodeType_isOperator(type))
  {
    mType = type;
    mChar = (char)type;
  }
  else if ((type >= AST_INTEGER) && (type < AST_END_OF_CORE))
  {
    mType = type;
    mChar = 0;

    // clear the definitionURL unless new type is csymbol
    // or a number or is a semantics
    switch (type)
    {
    case AST_NAME:
    case AST_NAME_TIME:
    case AST_NAME_AVOGADRO:
    case AST_FUNCTION_DELAY:
    case AST_FUNCTION:
      clearDefinitionURL = false;
      break;
    default:
      break;
    }
  }
  else if (type > AST_END_OF_CORE && type < AST_UNKNOWN)
  {
    mType = type;
    mChar = 0;

    const ASTBasePlugin* baseplugin = getASTPlugin(mType);
    if (baseplugin != NULL)
    {
      if (baseplugin->getConstCharCsymbolURLFor(type) != NULL)
      {
        clearDefinitionURL = false;
      }
    }
  }
  else
  {
    mType = AST_UNKNOWN;
    mChar = 0;
    mDefinitionURL->clear();
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  if (clearDefinitionURL == true && getSemanticsFlag() == false)
  {
    mDefinitionURL->clear();
  }

    
  return LIBSBML_OPERATION_SUCCESS;

}

LIBSBML_EXTERN
int
ASTNode::setId (const std::string& id)
{
  mId     = id;
  return LIBSBML_OPERATION_SUCCESS;
}

LIBSBML_EXTERN
int
ASTNode::setClass (const std::string& className)
{
  mClass   = className;
  return LIBSBML_OPERATION_SUCCESS;
}

LIBSBML_EXTERN
int
ASTNode::setStyle (const std::string& style)
{
  mStyle     = style;
  return LIBSBML_OPERATION_SUCCESS;
}



LIBSBML_EXTERN
int
ASTNode::setUnits (const std::string& units)
{
  if (!isNumber())
    return LIBSBML_UNEXPECTED_ATTRIBUTE;

  if (!SyntaxChecker::isValidInternalUnitSId(units))
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;

  mUnits     = units;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Swap the children of this ASTNode with the children of that ASTNode.
 */
LIBSBML_EXTERN
int
ASTNode::swapChildren (ASTNode *that)
{
  if (that == NULL)
    return LIBSBML_OPERATION_FAILED;

  List *temp      = this->mChildren;
  this->mChildren = that->mChildren;
  that->mChildren = temp;
  return LIBSBML_OPERATION_SUCCESS;
}

LIBSBML_EXTERN
void 
ASTNode::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (getType() == AST_NAME ||
      getType() == AST_FUNCTION ||
      getType() == AST_UNKNOWN) {
    if (getName() == oldid) {
      setName(newid.c_str());
    }
  }
  for (unsigned int child=0; child<getNumChildren(); child++) {
    getChild(child)->renameSIdRefs(oldid, newid);
  }
}

LIBSBML_EXTERN
void 
ASTNode::renameUnitSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetUnits()) {
    if (getUnits() == oldid) {
      setUnits(newid);
    }
  }
  for (unsigned int child=0; child<getNumChildren(); child++) {
    getChild(child)->renameUnitSIdRefs(oldid, newid);
  }
}


/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
void 
ASTNode::replaceIDWithFunction(const std::string& id, const ASTNode* function)
{
  for (unsigned int i=0; i<getNumChildren(); i++) {
    ASTNode* child = getChild(i);
    if (child->getType() == AST_NAME &&
        child->getName() == id) {
      replaceChild(i, function->deepCopy(), true);
    }
    else {
      child->replaceIDWithFunction(id, function);
    }
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
void ASTNode::multiplyTimeBy(const ASTNode* function)
{
  for (unsigned int i=0; i<getNumChildren(); i++) {
    getChild(i)->multiplyTimeBy(function);
  }
  if (getType() == AST_NAME_TIME) {
    setType(AST_TIMES);
    addChild(function->deepCopy());
    ASTNode* time = new ASTNode(AST_NAME_TIME);
    addChild(time);
  }
}
/** @endcond */

LIBSBML_EXTERN
int
ASTNode::unsetId ()
{
  mId.erase();

  if (mId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


LIBSBML_EXTERN
int
ASTNode::unsetClass ()
{
  mClass.erase();

  if (mClass.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

LIBSBML_EXTERN
int
ASTNode::unsetStyle ()
{
  mStyle.erase();

  if (mStyle.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

LIBSBML_EXTERN
int
ASTNode::unsetUnits ()
{
  if (!isNumber())
    return LIBSBML_UNEXPECTED_ATTRIBUTE;

  mUnits.erase();

  if (mUnits.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the flag indicating that this ASTNode has semantics attached
 */
int 
ASTNode::setSemanticsFlag() 
{ 
  hasSemantics = true; 
  return LIBSBML_OPERATION_SUCCESS;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
  * Unsets the flag indicating that this ASTNode has semantics attached
  */
int 
ASTNode::unsetSemanticsFlag()
{ 
  hasSemantics = false; 

  if (hasSemantics)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
  * gets the flag indicating that this ASTNode has semantics attached
  */
bool 
ASTNode::getSemanticsFlag() const
{
  return hasSemantics;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
  * sets the definitionURL attributes
  */
int 
ASTNode::setDefinitionURL(XMLAttributes url)
{
  delete mDefinitionURL;
  mDefinitionURL = static_cast<XMLAttributes *>(url.clone());
  return LIBSBML_OPERATION_SUCCESS;
}


/*
  * sets the definitionURL attributes
  */
int 
ASTNode::setDefinitionURL(const std::string& url)
{
  mDefinitionURL->clear();
  mDefinitionURL->add("definitionURL", url);
  return LIBSBML_OPERATION_SUCCESS;
}

LIBSBML_EXTERN
bool 
ASTNode::isBvar() const 
{ 
  return mIsBvar; 
}


LIBSBML_EXTERN
bool 
ASTNode::representsBvar() const 
{ 
  return mIsBvar; 
}


LIBSBML_EXTERN
void 
ASTNode::setBvar() 
{ 
  mIsBvar = true; 
}


LIBSBML_EXTERN
bool 
ASTNode::isQualifier() const
{
  return false;
}

LIBSBML_EXTERN
bool
ASTNode::isSemantics() const
{
  return getSemanticsFlag();
}

LIBSBML_EXTERN
unsigned int 
ASTNode::getNumBvars() const
{
  unsigned int num = getNumChildren();
  if (num == 0)
    return num;
  if (getChild(num - 1)->isBvar())
    return num;
  else
    return num - 1;
}


bool
ASTNode::hasCorrectNumberArguments() const
{
  bool correctNum = true;
  ASTNodeType_t type = getType();
  unsigned int numChildren = getNumChildren();

  switch (type) 
  {
  case AST_INTEGER:
  case AST_REAL:
  case AST_REAL_E:
  case AST_RATIONAL:
  case AST_NAME:
  case AST_NAME_AVOGADRO:
  case AST_NAME_TIME:
  case AST_CONSTANT_E:
  case AST_CONSTANT_FALSE:
  case AST_CONSTANT_PI:
  case AST_CONSTANT_TRUE:

    if (numChildren != 0) {
      correctNum = false;
    }
    break;

  case AST_FUNCTION_ABS:
  case AST_FUNCTION_ARCCOS:
  case AST_FUNCTION_ARCCOSH:
  case AST_FUNCTION_ARCCOT:
  case AST_FUNCTION_ARCCOTH:
  case AST_FUNCTION_ARCCSC:
  case AST_FUNCTION_ARCCSCH:
  case AST_FUNCTION_ARCSEC:
  case AST_FUNCTION_ARCSECH:
  case AST_FUNCTION_ARCSIN:
  case AST_FUNCTION_ARCSINH:
  case AST_FUNCTION_ARCTAN:
  case AST_FUNCTION_ARCTANH:
  case AST_FUNCTION_CEILING:
  case AST_FUNCTION_COS:
  case AST_FUNCTION_COSH:
  case AST_FUNCTION_COT:
  case AST_FUNCTION_COTH:
  case AST_FUNCTION_CSC:
  case AST_FUNCTION_CSCH:
  case AST_FUNCTION_EXP:
  case AST_FUNCTION_FACTORIAL:
  case AST_FUNCTION_FLOOR:
  case AST_FUNCTION_LN:
  case AST_FUNCTION_SEC:
  case AST_FUNCTION_SECH:
  case AST_FUNCTION_SIN:
  case AST_FUNCTION_SINH:
  case AST_FUNCTION_TAN:
  case AST_FUNCTION_TANH:
  case AST_LOGICAL_NOT:

    if (numChildren != 1) {
      correctNum = false;
    }
    break;

  case AST_DIVIDE:
  case AST_POWER:
  case AST_RELATIONAL_NEQ:
  case AST_FUNCTION_DELAY:
  case AST_FUNCTION_POWER:
  case AST_FUNCTION_LOG:       // a log ASTNode has a child for base

    if (numChildren != 2) {
      correctNum = false;
    }
    break;

  case AST_TIMES:
  case AST_PLUS:
  case AST_LOGICAL_AND:
  case AST_LOGICAL_OR:
  case AST_LOGICAL_XOR:
    correctNum = true;
    break;

  case AST_RELATIONAL_EQ:
  case AST_RELATIONAL_GEQ:
  case AST_RELATIONAL_GT:
  case AST_RELATIONAL_LEQ:
  case AST_RELATIONAL_LT:

    if (numChildren < 2) {
      correctNum = false;
    }
    break;

  case AST_FUNCTION_ROOT:
  case AST_MINUS:

    if (numChildren < 1 || numChildren > 2) {
      correctNum = false;
    }
    break;

  case AST_FUNCTION_PIECEWISE:
  case AST_LAMBDA:

    if (numChildren < 1) {
      correctNum = false;
    }
    break;

  case AST_FUNCTION:
    break;

  default:
    if (mType > AST_END_OF_CORE)
    {
      const ASTBasePlugin * baseplugin = getASTPlugin(mType);
      if (baseplugin != NULL)
      {
        correctNum = baseplugin->hasCorrectNumArguments(this);
      }
    }
    break;

  }

  return correctNum;
}
bool 
ASTNode::isWellFormedASTNode() const
{
  bool valid = hasCorrectNumberArguments();
  unsigned int numChildren = getNumChildren();
  unsigned int i = 0;

  // check number of arguments
  while (valid && i < numChildren)
  {
    valid = getChild(i)->isWellFormedASTNode();
    i++;
  }
  return valid;
}


/** @endcond */

static void copyNode(const ASTNode * source, ASTNode * dest)
{
    if (source == NULL)
    {
        return;
    }
    if (source->isName())
    {
        dest->setType(source->getType());
        dest->setName(source->getName());
    }
    else if (source->isReal())
    {
        dest->setValue(source->getReal());
        if (source->isSetUnits())
        {
            dest->setUnits(source->getUnits());
        }
    }
    else if (source->isInteger())
    {
        dest->setValue(source->getInteger());
        if (source->isSetUnits())
        {
            dest->setUnits(source->getUnits());
        }
    }
    else if (source->isConstant())
    {
        dest->setType(source->getType());
    }
    else
    {
        dest->setType(source->getType());
        dest->setName(source->getName());
        for (unsigned int c = 0; c < source->getNumChildren(); c++)
        {
            dest->addChild(source->getChild(c)->deepCopy());
        }
    }
}

LIBSBML_EXTERN
void
ASTNode::replaceArgument(const std::string& bvar, ASTNode * arg)
{
  if (getNumChildren() == 0 && this->isName() && this->getName() == bvar)
  {
      copyNode(arg, this);
      return;
  }
  for (unsigned int i = 0; i < getNumChildren(); i++)
  {
    if (getChild(i)->isName() && getChild(i)->getName() == bvar)
    {
      copyNode(arg, getChild(i));
    }
    else
    {
      getChild(i)->replaceArgument(bvar, arg);
    }
  }
}

LIBSBML_EXTERN
void
ASTNode::replaceArguments(const std::vector<std::string>& bvars, std::vector<ASTNode *>& args)
{
    std::size_t n = bvars.size();
    if (getNumChildren() == 0)
    {
        for(std::size_t j=0; j<n; ++j)
        {
            if (this->isName() && this->getName() == bvars[j])
            {
                copyNode(args[j], this);
                return;
            }
        }
    }
    for (unsigned int i = 0; i < getNumChildren(); i++)
    {
        bool child_replaced = false;
        for(std::size_t j=0; j<n; ++j)
        {
            if (getChild(i)->isName() && getChild(i)->getName() == bvars[j])
            {
                copyNode(args[j], getChild(i));
                child_replaced = true;
                break;
            }
        }
        if (!child_replaced)
        {
            getChild(i)->replaceArguments(bvars, args);
        }
    }
}

LIBSBML_EXTERN
void
ASTNode::reduceToBinary()
{
  unsigned int numChildren = getNumChildren();
  /* number of children should be greater than 2 */
  if (numChildren < 3)
    return;

  ASTNode* op = new ASTNode( getType() );
  ASTNode* op2 = new ASTNode( getType() );

  // add the first two children to the first node
  op->addChild(getChild(0));
  op->addChild(getChild(1));

  op2->addChild(op);
  for (unsigned int n = 2; n < numChildren; n++)
    op2->addChild(getChild(n));

  swapChildren(op2);

  // need to clean up memory but the children of op2
  // are now the children of this ASTNode
  // so remove them from op2 before deleting it
  // this is why removeChild does not delete the child
  unsigned int num = op2->getNumChildren();
  unsigned int i = 0;
  while(i < num)
  {
    op2->removeChild(0);
    i++;
  }
  delete op2;

  reduceToBinary();
}


/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
void 
ASTNode::setParentSBMLObject(SBase * sb)
{
  mParentSBMLObject = sb;
}

LIBSBML_EXTERN
int 
ASTNode::unsetParentSBMLObject()
{
  mParentSBMLObject = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


LIBSBML_EXTERN
bool 
ASTNode::isSetParentSBMLObject() const
{
  return (mParentSBMLObject != NULL);
}
/** @endcond */


LIBSBML_EXTERN
SBase * 
ASTNode::getParentSBMLObject() const
{
  return mParentSBMLObject;
}


/*
  * gets the definitionURL attributes
  */
LIBSBML_EXTERN
XMLAttributes*
ASTNode::getDefinitionURL() const
{
  return mDefinitionURL;
}



LIBSBML_EXTERN
std::string
ASTNode::getDefinitionURLString() const
{
  if (mDefinitionURL == NULL)
  {
    return "";
  }
  else
  {
    return mDefinitionURL->getValue("definitionURL");
  }
}

LIBSBML_EXTERN
void *
ASTNode::getUserData() const
{
  return this->mUserData;
}


LIBSBML_EXTERN
int
ASTNode::setUserData(void *userData)
{
  this->mUserData = userData;
 
  // allow userData to be set to NULL
  if (userData == NULL)
  {
    if (mUserData != NULL)
    {
      return LIBSBML_OPERATION_FAILED;
    }
    else
    {
      return LIBSBML_OPERATION_SUCCESS;
    }
  }

  if (mUserData != NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


LIBSBML_EXTERN
int
ASTNode::unsetUserData()
{
  mUserData = NULL;
 
  if (mUserData == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


LIBSBML_EXTERN
bool
ASTNode::isSetUserData() const
{
  return (mUserData != NULL);
}


/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
bool ASTNode::containsVariable(const std::string& id) const
{
  bool found = false;

  List * nodes = this->getListOfNodes( ASTNode_isName );
  if (nodes == NULL) return false;
  
  unsigned int i = 0;
  while (found == false && i < nodes->getSize())
  {
    ASTNode* node = static_cast<ASTNode*>( nodes->get(i) );
    string   name = node->getName() ? node->getName() : "";
    if (name == id)
    {
      found = true;
    }
    i++;
  }

  delete nodes;
  
  return found;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
unsigned int ASTNode::getNumVariablesWithUndeclaredUnits(Model * m) const
{
  unsigned int number = 0;

  if (m == NULL)
  {
    if (this->getParentSBMLObject() != NULL)
    {
      m = static_cast <Model *>(this->getParentSBMLObject()
                                     ->getAncestorOfType(SBML_MODEL));
    }
  }

  // we are possibly in a kineticLaw where parameters might
  // have local ids
  KineticLaw* kl = NULL;

  if (this->getParentSBMLObject() != NULL && 
    this->getParentSBMLObject()->getTypeCode() == SBML_KINETIC_LAW)
  {
    kl = static_cast<KineticLaw*>(this->getParentSBMLObject());
  }

  // create a list of variables in the math
  List * nodes = this->getListOfNodes( ASTNode_isName );
  IdList * variables = new IdList();
  if (nodes != NULL)
  {
    for (unsigned int i = 0; i < nodes->getSize(); i++)
    {
      ASTNode* node = static_cast<ASTNode*>( nodes->get(i) );
      string   name = node->getName() ? node->getName() : "";
      if (name.empty() == false)
      {
        if (variables->contains(name) == false)
        {
          variables->append(name);
        }
      }
    }
    delete nodes;
  }

  if ( m == NULL)
  {
    // there is no model so we have no units
    number = variables->size();
  }
  else
  {    
    // should we look for reactions or speciesreferences in the math
    bool allowReactionId = true;
    //bool allowSpeciesRef = false;

    if ( (m->getLevel() < 2) 
     || ((m->getLevel() == 2) && (m->getVersion() == 1)) )
    {
      allowReactionId = false;
    }

    //if (m->getLevel() > 2)
    //{
    //  allowSpeciesRef = true;
    //}

    // loop through the list and check the unit status of each variable
    for (unsigned int v = 0; v < variables->size(); v++)
    {
      string id = variables->at(v);
      

      if (m->getParameter(id) != NULL)
      {
        if (m->getParameter(id)->isSetUnits() == false)
        {
          number++;
        }
      }
      else if (m->getSpecies(id) != NULL)
      {
        if (m->getSpecies(id)->getDerivedUnitDefinition()->getNumUnits() == 0)
        {
          number++;
        }
      }
      else if (m->getCompartment(id) != NULL)
      {
         if (m->getCompartment(id)->getDerivedUnitDefinition()
                                                         ->getNumUnits() == 0)
        {
          number++;
        }
      }
      else if (kl != NULL && kl->getParameter(id) != NULL)
      {
        if (kl->getParameter(id)->getDerivedUnitDefinition() == NULL ||
          kl->getParameter(id)->getDerivedUnitDefinition()->getNumUnits() == 0)
        {
          number++;
        }
      }
      else if (allowReactionId == true 
         && m->getReaction(id) != NULL 
         && m->getReaction(id)->getKineticLaw() != NULL)
      {
         if (m->getReaction(id)->getKineticLaw()->getDerivedUnitDefinition()
                                                         ->getNumUnits() == 0)
        {
          number++;
        }
      }
      /* actually these always are considered to be dimensionless */
      //else if (allowSpeciesRef == true && m->getSpeciesReference(id) != NULL)
      //{
      //}
    }
  }


  variables->clear();
  delete variables;

  return number;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */

LIBSBML_EXTERN
ASTBasePlugin * 
ASTNode::getASTPlugin(const SBMLNamespaces * sbmlns)
{
  if (sbmlns)
  {
    const XMLNamespaces *xmlns = sbmlns->getNamespaces();

    if (xmlns)
    {
      int numxmlns = xmlns->getLength();
      for (int i = 0; i < numxmlns; i++)
      {
        const std::string& uri = xmlns->getURI(i);
        const SBMLExtension* sbmlext = SBMLExtensionRegistry::getInstance().getExtensionInternal(uri);

        if (sbmlext && sbmlext->isEnabled())
        {
          const ASTBasePlugin* astPlugin = sbmlext->getASTBasePlugin();
          if (astPlugin != NULL)
          {
            return const_cast<ASTBasePlugin*>(astPlugin);
          }
        }
      }
    }
  }
  return NULL;
}

LIBSBML_EXTERN
ASTBasePlugin * 
ASTNode::getASTPlugin(ASTNodeType_t type)
{
  unsigned int numPkgs = SBMLExtensionRegistry::getInstance().getNumASTPlugins();

  for (unsigned int i = 0; i < numPkgs; i++)
  {
    const ASTBasePlugin* baseplugin = SBMLExtensionRegistry::getInstance().getASTPlugin(i);
    if (baseplugin->defines(type))
    {
      return const_cast<ASTBasePlugin*>(baseplugin);
    }
  }
  return NULL;
}

LIBSBML_EXTERN
ASTBasePlugin * 
ASTNode::getASTPlugin(const std::string& name, bool isCsymbol, bool strCmpIsCaseSensitive)
{
  unsigned int numPkgs = SBMLExtensionRegistry::getInstance().getNumASTPlugins();

  for (unsigned int i = 0; i < numPkgs; i++)
  {
    const ASTBasePlugin* baseplugin = SBMLExtensionRegistry::getInstance().getASTPlugin(i);
    if (isCsymbol)
    {
      if (baseplugin->getASTNodeTypeForCSymbolURL(name) != AST_UNKNOWN)
      {
        return const_cast<ASTBasePlugin*>(baseplugin);
      }
    }
    else
    {
      if (baseplugin->defines(name, strCmpIsCaseSensitive))
      {
        return const_cast<ASTBasePlugin*>(baseplugin);
      }
    }
  }
  return NULL;
}


LIBSBML_EXTERN
const ASTBasePlugin * 
ASTNode::getASTPlugin(const SBMLNamespaces * sbmlns) const
{
  if (sbmlns)
  {
    const XMLNamespaces *xmlns = sbmlns->getNamespaces();

    if (xmlns)
    {
      int numxmlns = xmlns->getLength();
      for (int i = 0; i < numxmlns; i++)
      {
        const std::string& uri = xmlns->getURI(i);
        const SBMLExtension* sbmlext = SBMLExtensionRegistry::getInstance().getExtensionInternal(uri);

        if (sbmlext && sbmlext->isEnabled())
        {
          const ASTBasePlugin* astPlugin = sbmlext->getASTBasePlugin();
          if (astPlugin != NULL)
          {
            return astPlugin;
          }
        }
      }
    }
  }
  return NULL;
}

LIBSBML_EXTERN
const ASTBasePlugin * 
ASTNode::getASTPlugin(ASTNodeType_t type) const
{
  unsigned int numPkgs = SBMLExtensionRegistry::getInstance().getNumASTPlugins();

  for (unsigned int i = 0; i < numPkgs; i++)
  {
    const ASTBasePlugin* baseplugin = SBMLExtensionRegistry::getInstance().getASTPlugin(i);
    if (baseplugin->defines(type))
    {
      return baseplugin;
    }
  }
  return NULL;
}

LIBSBML_EXTERN
const ASTBasePlugin * 
ASTNode::getASTPlugin(const std::string& name, bool isCsymbol, bool strCmpIsCaseSensitive) const
{
  unsigned int numPkgs = SBMLExtensionRegistry::getInstance().getNumASTPlugins();

  for (unsigned int i = 0; i < numPkgs; i++)
  {
    const ASTBasePlugin* baseplugin = SBMLExtensionRegistry::getInstance().getASTPlugin(i);
    if (isCsymbol)
    {
      if (baseplugin->getASTNodeTypeForCSymbolURL(name) != AST_UNKNOWN)
      {
        return baseplugin;
      }
    }
    else
    {
      if (baseplugin->defines(name, strCmpIsCaseSensitive))
      {
        return const_cast<ASTBasePlugin*>(baseplugin);
      }
    }
  }
  return NULL;
}



LIBSBML_EXTERN
void
ASTNode::loadASTPlugin(const std::string& pkgName)
{
  unsigned int numPkgs = SBMLExtensionRegistry::getInstance().getNumASTPlugins();

  for (unsigned int i = 0; i < numPkgs; i++)
  {
    const ASTBasePlugin* baseplugin = SBMLExtensionRegistry::getInstance().getASTPlugin(i);
    if (baseplugin->getPackageName() == pkgName)
    {
      ASTBasePlugin* myastPlugin = baseplugin->clone();
      myastPlugin->setPrefix(pkgName);
      myastPlugin->connectToParent(this);
      mPlugins.push_back(myastPlugin);
    }
  }
}


LIBSBML_EXTERN
void
ASTNode::loadASTPlugins(const SBMLNamespaces * sbmlns)
{
  if (sbmlns == NULL)
  {
    const std::vector<std::string>& names = SBMLExtensionRegistry::getAllRegisteredPackageNames();
    unsigned int numPkgs = (unsigned int)names.size();

    for (unsigned int i = 0; i < numPkgs; i++)
    {
      const std::string& uri = names[i];
      const SBMLExtension* sbmlext = SBMLExtensionRegistry::getInstance().getExtensionInternal(uri);

      if (sbmlext && sbmlext->isEnabled())
      {

        //const std::string &prefix = xmlns->getPrefix(i);
        const ASTBasePlugin* astPlugin = sbmlext->getASTBasePlugin();
        if (astPlugin != NULL)
        {
          ASTBasePlugin* myastPlugin = astPlugin->clone();
          myastPlugin->setSBMLExtension(sbmlext);
          myastPlugin->setPrefix(uri);
          myastPlugin->connectToParent(this);
          mPlugins.push_back(myastPlugin);
        }

      }
    }
  }
  else
  {
    const XMLNamespaces *xmlns = sbmlns->getNamespaces();

    if (xmlns)
    {
      int numxmlns = xmlns->getLength();
      for (int i = 0; i < numxmlns; i++)
      {
        const std::string& uri = xmlns->getURI(i);
        const SBMLExtension* sbmlext = SBMLExtensionRegistry::getInstance().getExtensionInternal(uri);

        if (sbmlext && sbmlext->isEnabled())
        {
          const ASTBasePlugin* astPlugin = sbmlext->getASTBasePlugin();
          if (astPlugin != NULL)
          {
            ASTBasePlugin* myastPlugin = astPlugin->clone();
            myastPlugin->setSBMLExtension(sbmlext);
            myastPlugin->setPrefix(xmlns->getPrefix(i));
            myastPlugin->connectToParent(this);
            mPlugins.push_back(myastPlugin);
          }
        }
      }
    }
  }
}

LIBSBML_EXTERN
void 
ASTNode::addPlugin(ASTBasePlugin* plugin)
{
  mPlugins.push_back(plugin);
}


/** @endcond */

/** @cond doxygenLibsbmlInternal */

LIBSBML_EXTERN
ASTBasePlugin*
ASTNode::getPlugin(const std::string& package)
{
  ASTBasePlugin * astPlugin = NULL;
  if (getNumPlugins() == 0)
  {
    loadASTPlugin(package);
  }
  for (size_t i = 0; i < mPlugins.size(); i++)
  {
    std::string uri = mPlugins[i]->getURI();
    const SBMLExtension* sbmlext = SBMLExtensionRegistry::getInstance().getExtensionInternal(uri);
    if (uri == package)
    {
      astPlugin = mPlugins[i];
      break;
    }
    else if (sbmlext && (sbmlext->getName() == package))
    {
      astPlugin = mPlugins[i];
      break;
    }
  }

  return astPlugin;
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */

LIBSBML_EXTERN
const ASTBasePlugin*
ASTNode::getPlugin(const std::string& package) const
{
  return const_cast<ASTNode*>(this)->getPlugin(package);
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */


LIBSBML_EXTERN
ASTBasePlugin*
ASTNode::getPlugin(unsigned int n)
{
  if (n >= getNumPlugins())
    return NULL;
  return mPlugins[n];
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */

LIBSBML_EXTERN
const ASTBasePlugin*
ASTNode::getPlugin(unsigned int n) const
{
  return const_cast<ASTNode*>(this)->getPlugin(n);
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */

LIBSBML_EXTERN
unsigned int
ASTNode::getNumPlugins() const
{
  return (unsigned int)mPlugins.size();
}


/** @endcond */

struct DeleteASTPluginEntity
{
  void operator() (ASTBasePlugin* ast) { delete ast; }
};

void ASTNode::clearPlugins()
{
  for_each(mPlugins.begin(), mPlugins.end(), DeleteASTPluginEntity());
  mPlugins.clear();
}

/* FUNCTIONS FOR INFER REACTIONS*/

/* returns true if astnodes are exactly the same
*  so 'a+4' == 'a+4'
* but 'a+4' != '4+a'
*/
LIBSBML_EXTERN
bool 
ASTNode::exactlyEqual(const ASTNode& rhs)
{
  bool equal = true;
  ASTNodeType_t type = getType();
  if (type != rhs.getType())
  {
    return false;
  }

  if (type == AST_NAME)
  {
    const char* n1 = getName();
    const char* n2 = rhs.getName();
    if (n1 == NULL || n2 == NULL)
    {
      return false;
    }
    else if (strcmp(n1, n2) != 0)
    {
      return false;
    }
  }
  else if (type == AST_INTEGER)
  {
    if (getInteger() != rhs.getInteger())
    {
      return false;
    }
  }
  else if (type == AST_RATIONAL || type == AST_REAL || type == AST_REAL_E)
  {
    if (!util_isEqual(getReal(), rhs.getReal()))
    {
      return false;
    }
  }

  unsigned int n = 0;
  while (equal && n < getNumChildren())
  {
    equal = getChild(n)->exactlyEqual(*(rhs.getChild(n)));
    n++;
  }

  return equal;
}

/* change all numbers to real*/
void
ASTNode::refactorNumbers()
{
  // change any number into a real
  if (mType == AST_INTEGER)
  {
    int value = getInteger();
    setType(AST_REAL);
    setValue(double(value));
  }
  else if (mType == AST_RATIONAL || mType == AST_REAL_E)
  {
    double value = getReal();
    setType(AST_REAL);
    setValue(value);
  }

  for (unsigned int n = 0; n < getNumChildren(); ++n)
  {
    getChild(n)->refactorNumbers();
  }

}


/*
* produce three vectors of the child index of the ASTNodes
* numbers : any nodes representing just a number
* names : any nodes representing a variable i.e.a node of type AST_NAME (in alphabetical order)
* others : any nodes that are not numbers/names - usually functions 
*/
void
ASTNode::createVectorOfChildTypes(std::vector<unsigned int>& numbers,
  std::vector<unsigned int>& names,
  std::vector<unsigned int>& others)
{
  for (unsigned int i = 0; i < getNumChildren(); ++i)
  {
    const ASTNode* child = getChild(i);
    if (child->isNumber())
    {
      // put in numbers vector in order
      if (numbers.size() == 0)
      {
        numbers.push_back(i);
      }
      else
      {
        double value = child->getReal();
        unsigned int sizeNumbers = numbers.size();
        unsigned int count = 0;
        bool added = false;
        while (!added && count < sizeNumbers)
        {
          if (value < getChild(numbers.at(count))->getReal())
          {
            std::vector<unsigned int>::iterator it = numbers.begin();
            numbers.insert(it + count, i);
            added = true;
          }
          count++;
        }
        if (!added)
        {
          numbers.push_back(i);
        }
      }
    }
    else if (child->isName())
    {
      // put in names vector in alphabetical order
      if (names.size() == 0)
      {
        names.push_back(i);
      }
      else
      {
        std::string name = string(child->getName());
        unsigned int sizeNames = names.size();
        unsigned int count = 0;
        bool added = false;
        while (!added && count < sizeNames)
        {
          if (name < string(getChild(names.at(count))->getName()))
          {
            std::vector<unsigned int>::iterator it = names.begin();
            names.insert(it + count, i);
            added = true;
          }
          count++;
        }
        if (!added)
        {
          names.push_back(i);
        }
      }
    }
    else
    {
      others.push_back(i);
    }
  }

}


void 
ASTNode::printMath(unsigned int level)
{
  if (this == NULL)
  {
    std::cout << "Level " << level << ": NULL" << std::endl;
    return;
  }

  const char * str = SBML_formulaToL3String(this);
  std::cout << "Level " << level << ": " << str << std::endl;
  safe_free((char *)(str));

  for (unsigned int i = 0; i < getNumChildren(); i++)
  {
    getChild(i)->printMath(level + 1);
  }
}

/* combine numbers:
* returns AST that is number 
*/
ASTNode*
ASTNode::combineNumbers(std::vector<unsigned int>& numbers)
{
  unsigned int num = numbers.size();
  if (num == 0)
  {
    return NULL;
  }

  std::vector<unsigned int>::iterator it = numbers.end() - 1;
  ASTNode* numberNode = getChild(*it)->deepCopy();
  if (num == 1)
  {
    return numberNode;
  }
  double number = numberNode->getValue();
  if (num == 2 && (mType == AST_MINUS || mType == AST_DIVIDE ||
    mType == AST_POWER || mType == AST_FUNCTION_POWER))
  {
    it = numbers.begin();
    switch (mType)
    {
    case AST_MINUS:
      number = number - (getChild(*it)->getValue());
      break;
    case AST_DIVIDE:
      number = number / (getChild(*it)->getValue());
      break;
    case AST_POWER:
    case AST_FUNCTION_POWER:
      number = pow(number, getChild(*it)->getValue());
      break;
    }

  }
  else
  {
    for (it = numbers.begin(); it != numbers.end() - 1; ++it)
    {
      switch (mType)
      {
      case AST_TIMES:
        number = number * (getChild(*it)->getValue());
        break;
      case AST_PLUS:
        number = number + (getChild(*it)->getValue());
        break;
      }
    }
  }
  numberNode->setValue(number);
  return numberNode;
}

/* create AST the is non binary
* Binary each node has 2 children
* Level 0: a + b + (c + s)
* Level 1: a + b
* Level 2: a
* Level 2: b
* Level 1: c + s
* Level 2: c
* Level 2: s
*
* Non binary Node at Level 0 has 4 children
* Level 0: a + b + c + s
* Level 1: a
* Level 1: b
* Level 1: c
* Level 1: s
*/
void 
ASTNode::createNonBinaryTree()
{
  unsigned int origNumChildren = getNumChildren();
  if (mType != AST_TIMES && mType != AST_PLUS)
  {
    // only applies to + *
    return;
  }
 
  for (unsigned int j = 0; j < origNumChildren; ++j)
  {
    if (getChild(j)->getType() == mType)
    {
      ASTNode* nbtree = getChild(j)->deepCopy();
      nbtree->createNonBinaryTree();
      for (unsigned int i = 0; i < nbtree->getNumChildren(); i++)
      {
        addChild(nbtree->getChild(i)->deepCopy());
      }
      delete nbtree;
    }
    else
    {
      addChild(getChild(j)->deepCopy());
    }
  }
  for (unsigned int i = origNumChildren; i > 0; --i)
  {
    ASTNode* rep = static_cast<ASTNode*>(mChildren->remove(i - 1));
    delete rep;
  }
}

/*
* simplify the node based on math i.e 1 * x becomes x
* see inline 
*/
void
ASTNode::simplify()
{
  unsigned int numChildren = getNumChildren();
  ASTNode *zero = new ASTNode(AST_REAL);
  zero->setValue(0.0);
  ASTNode *one = new ASTNode(AST_REAL);
  one->setValue(1.0);
  ASTNode *two = new ASTNode(AST_REAL);
  two->setValue(2.0);

  // if we now have only one child for plus or times we dont need the operator
  if (numChildren == 1 && (mType == AST_PLUS || mType == AST_TIMES))
  {
    ASTNode* child = getChild(0)->deepCopy();
    (*this) = *(child);
    delete child;
  }

  // if we have 1 * x * ... dont need 1
  if (mType == AST_TIMES && util_isEqual(getChild(0)->getValue(), 1.0))
  {
    ASTNode * newNode = new ASTNode(AST_TIMES);
    for (unsigned int i = 1; i < numChildren; ++i)
    {
      newNode->addChild(getChild(i)->deepCopy());
    }
    (*this) = *(newNode);
    delete newNode;
    simplify();
  }

  // if we have A - A should be 0
  if (mType == AST_MINUS && getChild(0)->exactlyEqual(*getChild(1)))
  {
      ASTNode* child = zero->deepCopy();
      (*this) = *(child);
      delete child;
  }

  // if we have A + A should get 2 * A
  // or n + a + a should get n + 2*a
  // or n + a + b + b should get n + a + 2*b

  if (mType == AST_PLUS)
  {
    bool match = false;
    unsigned int i;
    // do we have two children same
    for (i = 1; i <= getNumChildren()-1; ++i)
    {
      if (getChild(i - 1)->exactlyEqual(*getChild(i)))
      {
        match = true;
        break;
      }
    }
    if (match)
    {
        ASTNode * newNode = new ASTNode(AST_TIMES);
        newNode->addChild(two->deepCopy());
        newNode->addChild(getChild(i-1)->deepCopy());
        this->replaceChild(i - 1, newNode, true);
        this->removeChild(i, true);
        simplify();
    }
  }
  // if we have A/A should be 1
  if (mType == AST_DIVIDE && getChild(0)->exactlyEqual(*getChild(1)))
  {
    ASTNode* child = one->deepCopy();
    (*this) = *(child);
    delete child;
  }
  // if we have A^1 just have A
  if ((mType == AST_POWER || mType == AST_FUNCTION_POWER) && getChild(1)->exactlyEqual(*one))
  {
    ASTNode* child = getChild(0)->deepCopy();
    (*this) = *(child);
    delete child;
  }
  // if we have A^0 just have 1
  if ((mType == AST_POWER || mType == AST_FUNCTION_POWER) && getChild(1)->exactlyEqual(*zero))
  {
    ASTNode* child = one->deepCopy();
    (*this) = *(child);
    delete child;
  }

  delete zero;
  delete one;
  delete two;
}

/*
* change a root node to power ie root(2, x) becomes x^0.5
*/
void
ASTNode::convertRootToPower()
{
  //root has two child 1st degree 2nd subject
  ASTNode *c1 = getChild(0);
  double root = c1->getValue();
  c1->setValue(1.0 / root);
  ASTNode *newNode = new ASTNode(AST_POWER);
  newNode->addChild(getChild(1)->deepCopy());
  newNode->addChild(c1->deepCopy());
  (*this) = (*newNode);
  delete newNode;
}
/* for plus or times order arguments so we have number + names + functions
* 3.1 +b + 2 becomes 5.1 + b
* 2 * 5 becomes 10
* sin(2+3) + 3.1 + b becomes 3.1 +b + sin(5) 
*/
bool
ASTNode::reorderArguments(unsigned int level)
{
  if (mType == AST_FUNCTION_ROOT)
  {
    convertRootToPower();
  }
  bool mayNeedRefactor = false;
  
  if (mType == AST_TIMES || mType == AST_PLUS || mType == AST_MINUS || mType == AST_DIVIDE
    || mType == AST_POWER || mType == AST_FUNCTION_POWER)
  {
    unsigned int origNumChildren = getNumChildren();

    std::vector<unsigned int> numbers;
    std::vector<unsigned int> names;
    std::vector<unsigned int> others;

    createVectorOfChildTypes(numbers, names, others);

    // after this the numberNode will contain combined number
    ASTNode * numberNode = combineNumbers(numbers);
    if (mType == AST_TIMES || mType == AST_PLUS)
    {
      if (numberNode != NULL)
      {
        addChild(numberNode->deepCopy());
      }
      for (std::vector<unsigned int>::iterator it = names.begin(); it != names.end(); ++it)
      {
        addChild(getChild(*it)->deepCopy());
      }
      for (std::vector<unsigned int>::iterator it = others.begin(); it != others.end(); ++it)
      {
        addChild(getChild(*it)->deepCopy());
      }
    }
    else if (mType == AST_MINUS || mType == AST_DIVIDE || mType == AST_POWER || mType == AST_FUNCTION_POWER)
    {
      // then the result is numerical if there were no other types
      if ((names.size() == 0) && (others.size() == 0))
      {
        if (numberNode != NULL)
        {
          ASTNode* child = numberNode->deepCopy();
          (*this) = *(child);
          delete child;
        }
      }
      // otherwise the expression is unchanged
      // so either a single number or unchanged
      // so we dont delete original
      origNumChildren = 0;
    }
    for (unsigned int i = origNumChildren; i > 0; --i)
    {
      ASTNode* rep = static_cast<ASTNode*>(mChildren->remove(i - 1));
      delete rep;
    }

    simplify();

    if (names.size() == 0 && others.size() == 0 && level == 1)
    {
      mayNeedRefactor = true;
    }

    delete numberNode;
  }
  
  for (unsigned int i = 0; i < getNumChildren(); i++)
  {
    if(getChild(i)->reorderArguments(level + 1))
      mayNeedRefactor = true;
  }
  
  return mayNeedRefactor;
}

/* remove any instances of unary minus
*/
void
ASTNode::encompassUnaryMinus()
{ 
  ASTNode* minusOne = new ASTNode(AST_REAL);
  minusOne->setValue((double)(-1.0));

  if (isUMinus())
  {
    ASTNode* child = getChild(0)->deepCopy();
    if (child->isNumber())
    {
      // replace -(2) with -2
      double value = child->getValue();
      child->setValue((double)(-1.0 * value));
      (*this) = (*child);
      delete child;
    }
    else if ((child->getType() == AST_TIMES || child->getType() == AST_DIVIDE)
              && child->getNumChildren() > 0)
    {
      ASTNode *firstChild = child->getChild(0);
      // replace -(2*a) with -2*a or -(2/a) with -2/a
      if (firstChild->isNumber())
      {
        double value = firstChild->getReal();
        firstChild->setValue((double)(-1.0 * value));
        (*this) = (*child);
        delete child;
      }
      // replace -(a*b) with -1*a*b
      else if (child->getType() == AST_TIMES)
      {
        child->prependChild(minusOne->deepCopy());
        (*this) = (*child);
        delete child;

      }
      // replace -(a/b) with -1*a/b
      else if (child->getType() == AST_DIVIDE)
      {
        ASTNode * timesOne = new ASTNode(AST_TIMES);
        timesOne->addChild(minusOne->deepCopy());
        timesOne->addChild(firstChild->deepCopy());
        child->replaceChild(0, timesOne->deepCopy(), true);
        (*this) = (*child);
        delete child;
        delete timesOne;
      }
    }
    else 
    {
      ASTNode * timesOne = new ASTNode(AST_TIMES);
      timesOne->addChild(minusOne->deepCopy());
      timesOne->addChild(child->deepCopy());
      (*this) = (*timesOne);
      delete timesOne;
      delete child;
    }

  }

  delete minusOne;
  for (unsigned int i = 0; i < getNumChildren(); ++i)
  {
    getChild(i)->encompassUnaryMinus();
  }

}

/* create an ast of appropriate form*/
LIBSBML_EXTERN
void 
ASTNode::refactor()
{
  refactorNumbers();
  encompassUnaryMinus();
  createNonBinaryTree();
  if (reorderArguments())
  {
    refactor();
  }
}

/*
 * a decomposed ast is one where if the top level func is *or /
 * the arguments are not sums
 * (a + B) * c becomes ac + Bc
 * (5 + 3)/(a-4) becomes 8/(a-4)
 * (a + 4)/4 becomes 1 + a/4
 */
LIBSBML_EXTERN
void
ASTNode::decompose()
{
  refactor();
  bool decompose = false;
  unsigned int childNo = 0;
  ASTNodeType_t type, origType;

  if (getType() == AST_TIMES)
  {
    for (unsigned int i = 0; i < getNumChildren(); ++i)
    {
      type = getChild(i)->getType();
      if (type == AST_PLUS || type == AST_MINUS)
      {
        decompose = true;
        childNo = i;
        origType = AST_TIMES;
        break;
      }
    }
  }
  else if (getType() == AST_DIVIDE)
  {
    type = getChild(0)->getType();
    if (type == AST_PLUS || type == AST_MINUS)
    {
      decompose = true;
      childNo = 0;
      origType = AST_DIVIDE;
    }
  }
  else
  {
    for (unsigned int n = 0; n < getNumChildren(); ++n)
    {
      getChild(n)->decompose();
    }
  }

  if (decompose)
  {
    ASTNode* minusOne = new ASTNode(AST_REAL);
    minusOne->setValue((double)(-1.0));
    
    ASTNode* distrib = (ASTNode*)(mChildren->remove(childNo));
    std::vector<ASTNode*> otherChildren;
    for (unsigned int i = getNumChildren(); i > 0; --i)
    {
      otherChildren.push_back((ASTNode*)(mChildren->remove(i-1)));
    }
    std::vector<ASTNode*>::iterator it = otherChildren.begin();
    this->setType(AST_PLUS);
    // if the numchildren not 2 for minus - get out before we hit problems
    if (type == AST_MINUS && distrib->getNumChildren() != 2)
    {
      delete minusOne;
      return;
    }

    for (unsigned int j = 0; j < distrib->getNumChildren(); ++j)
    {
      ASTNode* newNode = new ASTNode(origType);
      if (type == AST_MINUS && j == 1)
      {
        ASTNode* first = otherChildren.at(0);
        if (first->isNumber())
        {
          double newVal = first->getValue() * -1.0;
          first->setValue(newVal);
        }
        else
        {
          newNode->addChild(minusOne->deepCopy());
        }
      }
      newNode->addChild(distrib->getChild(j)->deepCopy());
      for (it = otherChildren.begin(); it != otherChildren.end(); it++)
      {
        newNode->addChild((*it)->deepCopy());
      }
      newNode->refactor();
      this->addChild(newNode->deepCopy());
      delete newNode;
    }
    delete minusOne;
    delete distrib;
    for (it = otherChildren.begin(); it != otherChildren.end(); it++)
    {
      delete *it;
    }
    otherChildren.clear();
  }
  refactor();
}

/*
* Returns an ASTNode representing the derivative w.r.t variable
* e.g. Node represents 2*x^2
*      variable = "x"
* returns Node representing 4 * x
* since d(2*x^2)/dx = 4*x
*/
LIBSBML_EXTERN
ASTNode* 
ASTNode::derivative(const std::string& variable)
{
  ASTNode *copy = deepCopy();
  copy->decompose();
  ASTNode *zero = new ASTNode(AST_REAL);
  zero->setValue(0.0);
  ASTNode * derivative = NULL;

  //dy/dx = 0
  if (!(copy->containsVariable(variable)))
  {
    derivative = zero->deepCopy();
  }
  else
  {
    ASTNodeType_t type = copy->getType();

    switch (type)
    {
    case AST_NAME:
      // dx/dx == 1
      if (copy->getName() == variable)
      {
        derivative = new ASTNode(AST_REAL);
        derivative->setValue((double)(1.0));
      }
      break;
    case AST_CONSTANT_E:
    case AST_NAME_AVOGADRO:
    case AST_CONSTANT_PI:
    case AST_INTEGER:
    case AST_REAL:
    case AST_REAL_E:
    case AST_RATIONAL:
      // d(constant)/dx = 0 - will be caught above
      derivative = zero->deepCopy();
      break;
    case AST_PLUS:
      derivative = derivativePlus(variable);
      break;
    case AST_MINUS:
      derivative = derivativeMinus(variable);
      break;
    case AST_TIMES:
      derivative = derivativeTimes(variable);
      break;
    case AST_DIVIDE:
      derivative = derivativeDivide(variable);
      break;
    case AST_POWER:
    case AST_FUNCTION_POWER:
      derivative = derivativePower(variable);
      break;
    case AST_FUNCTION_LOG:
      derivative = derivativeLog(variable);
      break;
    case AST_FUNCTION_LN:
      derivative = derivativeLn(variable);
      break;

    case AST_FUNCTION_EXP:
      derivative = derivativeExp(variable);
      break;



    default:
      break;
    }
  }

  delete zero;
  delete copy;
  return derivative;

}

ASTNode*
ASTNode::derivativePlus(const std::string& variable)
{
  ASTNode *copy = deepCopy();
  copy->decompose();
  ASTNode *zero = new ASTNode(AST_REAL);
  zero->setValue(0.0);

  // d (A + B)/dx = dA/dx + dB/dx
  ASTNode * derivative = new ASTNode(AST_PLUS);
  for (unsigned int n = 0; n < copy->getNumChildren(); ++n)
  {
    ASTNode* child_der = copy->getChild(n)->derivative(variable);
    if (!(child_der->exactlyEqual(*zero)))
    {
      derivative->addChild(child_der->deepCopy());
    }
    delete child_der;
  }
  derivative->decompose();

  delete zero;
  delete copy;
  return derivative;
}


ASTNode*
ASTNode::derivativeMinus(const std::string& variable)
{
  ASTNode *copy = deepCopy();
  copy->decompose();
  ASTNode *zero = new ASTNode(AST_REAL);
  zero->setValue(0.0);
  ASTNode * derivative = NULL;

  // d (A - B)/dx = dA/dx - dB/dx
  ASTNode* child_derA = copy->getChild(0)->derivative(variable);
  ASTNode* child_derB = copy->getChild(1)->derivative(variable);
  if (child_derB->exactlyEqual(*zero))
  {
    derivative = child_derA->deepCopy();
  }
  else if (child_derA->exactlyEqual(*zero))
  {
    derivative = new ASTNode(AST_MINUS);
    derivative->addChild(child_derB->deepCopy());
  }
  else
  {
    derivative = new ASTNode(AST_MINUS);
    derivative->addChild(child_derA->deepCopy());
    derivative->addChild(child_derB->deepCopy());
  }

  derivative->decompose();

  delete child_derA;
  delete child_derB;
  delete zero;
  delete copy;
  return derivative;
}

ASTNode*
ASTNode::derivativeTimes(const std::string& variable)
{
  ASTNode *copy = this->deepCopy();
  copy->decompose();
  copy->reduceToBinary();
  ASTNode *zero = new ASTNode(AST_REAL);
  zero->setValue(0.0);
  ASTNode * derivative = NULL;
  ASTNode * term1 = NULL;
  ASTNode * term2 = NULL;

  // d(A*B)/dx = B * dA/dx + A * dB/dx
  ASTNode* child_derA = copy->getChild(0)->derivative(variable);
  ASTNode* child_derB = copy->getChild(1)->derivative(variable);
  if (child_derB->exactlyEqual(*zero))
  {
    // shouldnt get here as refactorng will put fns of x last
    derivative = new ASTNode(AST_TIMES);
    derivative->addChild(copy->getChild(1)->deepCopy());
    derivative->addChild(child_derA->deepCopy());
  }
  else if (child_derA->exactlyEqual(*zero))
  {
    derivative = new ASTNode(AST_TIMES);
    derivative->addChild(copy->getChild(0)->deepCopy());
    derivative->addChild(child_derB->deepCopy());
  }
  else
  {
    term1 = new ASTNode(AST_TIMES);
    term1->addChild(copy->getChild(1)->deepCopy());
    term1->addChild(child_derA->deepCopy());

    term2 = new ASTNode(AST_TIMES);
    term2->addChild(copy->getChild(0)->deepCopy());
    term2->addChild(child_derB->deepCopy());
    
    derivative = new ASTNode(AST_PLUS);
    derivative->addChild(term1->deepCopy());
    derivative->addChild(term2->deepCopy());

  }

  derivative->decompose();

  delete child_derA;
  delete child_derB;
  delete term1;
  delete term2;
  delete zero;
  delete copy;
  return derivative;
}

ASTNode*
ASTNode::derivativeDivide(const std::string& variable)
{
  ASTNode *copy = deepCopy();
  copy->decompose();
  ASTNode *zero = new ASTNode(AST_REAL);
  zero->setValue(0.0);
  ASTNode *two = new ASTNode(AST_REAL);
  two->setValue(2.0);
  ASTNode * derivative = NULL;
  ASTNode * nominator = NULL;
  ASTNode * term1 = NULL;
  ASTNode * term2 = NULL;

  // d(A/B)/dx = (B * dA/dx - A * dB/dx)/B^2
  ASTNode *denominator = new ASTNode(AST_POWER);
  denominator->addChild(copy->getChild(1)->deepCopy());
  denominator->addChild(two->deepCopy());

  ASTNode* child_derA = copy->getChild(0)->derivative(variable);
  ASTNode* child_derB = copy->getChild(1)->derivative(variable);
  if (child_derB->exactlyEqual(*zero))
  {
    nominator = new ASTNode(AST_TIMES);
    nominator->addChild(copy->getChild(1)->deepCopy());
    nominator->addChild(child_derA->deepCopy());
  }
  else if (child_derA->exactlyEqual(*zero))
  {
    term1 = new ASTNode(AST_TIMES);
    term1->addChild(copy->getChild(0)->deepCopy());
    term1->addChild(child_derB->deepCopy());
    
    nominator = new ASTNode(AST_MINUS);
    nominator->addChild(term1->deepCopy());
  }
  else
  {
    term1 = new ASTNode(AST_TIMES);
    term1->addChild(copy->getChild(1)->deepCopy());
    term1->addChild(child_derA->deepCopy());

    term2 = new ASTNode(AST_TIMES);
    term2->addChild(copy->getChild(0)->deepCopy());
    term2->addChild(child_derB->deepCopy());

    nominator = new ASTNode(AST_MINUS);
    nominator->addChild(term1->deepCopy());
    nominator->addChild(term2->deepCopy());
  }
  derivative = new ASTNode(AST_DIVIDE);
  derivative->addChild(nominator->deepCopy());
  derivative->addChild(denominator->deepCopy());

  derivative->decompose();

  delete child_derA;
  delete child_derB;
  delete term1;
  delete term2;
  delete nominator;
  delete denominator;
  delete two;
  delete zero;
  delete copy;
  return derivative;
}

ASTNode*
ASTNode::derivativePower(const std::string& variable)
{
  ASTNode *copy = deepCopy();
  copy->decompose();
  ASTNode *exp = new ASTNode(AST_REAL);
  ASTNode *mult = new ASTNode(AST_REAL);
  ASTNode *power = new ASTNode(AST_POWER);

  ASTNode * derivative = NULL;

  // dA^n/dx where n is number = nA^(n-1)
  if (copy->getChild(1)->isNumber())
  { 
    ASTNode* A = copy->getChild(0);
    double n = copy->getChild(1)->getValue();
    exp->setValue(n - 1);
    power->addChild(A->deepCopy());
    power->addChild(exp->deepCopy());
    mult->setValue(n);

    derivative = new ASTNode(AST_TIMES);
    derivative->addChild(mult->deepCopy());
    derivative->addChild(power->deepCopy());
  }

  derivative->decompose();

  delete copy;
  delete exp;
  delete mult;
  delete power;
  return derivative;
}

ASTNode*
ASTNode::derivativeLog(const std::string& variable)
{
  ASTNode *copy = deepCopy();
  copy->decompose();
  ASTNode * derivative = NULL;

  //d(log(base, x)/dx = 1/(ln(base) * x)
  //d(log(base,A)/dx = dA/dx / (ln(base) *A)

  // in log function child0 is base
  ASTNode *ln = new ASTNode(AST_FUNCTION_LN);
  ASTNode *number = new ASTNode(AST_REAL);
  number->setValue((double)(copy->getChild(0)->getValue()));
  ln->addChild(number->deepCopy());

  ASTNode *times = new ASTNode(AST_TIMES);
  times->addChild(ln->deepCopy());
  times->addChild(copy->getChild(1)->deepCopy());

  derivative = new ASTNode(AST_DIVIDE);
  derivative->addChild(getChild(1)->derivative(variable));
  derivative->addChild(times->deepCopy());


  derivative->decompose();

  delete number;
  delete ln;
  delete times;
  delete copy;
  return derivative;
}

ASTNode*
ASTNode::derivativeLn(const std::string& variable)
{
  ASTNode *copy = deepCopy();
  copy->decompose();
  ASTNode * derivative = NULL;

  //d(ln(x)/dx = 1/(x)
  //d(ln(A)/dx = dA/dx / (A)


  derivative = new ASTNode(AST_DIVIDE);
  derivative->addChild(getChild(0)->derivative(variable));
  derivative->addChild(getChild(0)->deepCopy());


  derivative->decompose();

  delete copy;
  return derivative;
}

ASTNode*
ASTNode::derivativeExp(const std::string& variable)
{
  ASTNode *copy = deepCopy();
  copy->decompose();
  ASTNode * derivative = NULL;

  //d(exp(x)/dx = exp(x)
  //d(exp(A)/dx = dA/dx * exp(A)


  derivative = new ASTNode(AST_TIMES);
  derivative->addChild(getChild(0)->derivative(variable));
  derivative->addChild(copy->deepCopy());


  derivative->decompose();

  delete copy;
  return derivative;
}

XMLNamespaces* 
ASTNode::getDeclaredNamespaces() const
{
  return mNamespaces;
}


void 
ASTNode::setDeclaredNamespaces(const XMLNamespaces* xmlns)
{
  mNamespaces = xmlns->clone();
}


void
ASTNode::unsetDeclaredNamespaces()
{
  if (mNamespaces != NULL)
  {
    delete mNamespaces;
    mNamespaces = NULL;
  }
}

#endif /* __cplusplus */


/** @cond doxygenIgnored */

LIBSBML_EXTERN
ASTNode_t *
ASTNode_create (void)
{
  return new(nothrow) ASTNode;
}


LIBSBML_EXTERN
ASTNode_t *
ASTNode_createWithType (ASTNodeType_t type)
{
  return new(nothrow) ASTNode(type);
}


LIBSBML_EXTERN
ASTNode_t *
ASTNode_createFromToken (Token_t *token)
{
  if (token == NULL) return NULL;
  return new(nothrow) ASTNode(token);
}


LIBSBML_EXTERN
void
ASTNode_free (ASTNode_t *node)
{
  if (node == NULL) return;

  delete static_cast<ASTNode*>(node);
}


LIBSBML_EXTERN
int
ASTNode_freeName (ASTNode_t *node)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)->freeName();
}


LIBSBML_EXTERN
int
ASTNode_canonicalize (ASTNode_t *node)
{
  if (node == NULL) return (int)false;
  return (int) static_cast<ASTNode*>(node)->canonicalize();
}


LIBSBML_EXTERN
int
ASTNode_addChild (ASTNode_t *node, ASTNode_t *child)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)->addChild
                                    ( static_cast<ASTNode*>(child) );
}


LIBSBML_EXTERN
int
ASTNode_prependChild (ASTNode_t *node, ASTNode_t *child)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)->prependChild
                                    ( static_cast<ASTNode*>(child) );
}


LIBSBML_EXTERN
ASTNode_t *
ASTNode_deepCopy (const ASTNode_t *node)
{
  if ( node == NULL ) return NULL;
  return
    static_cast<ASTNode_t *>( static_cast<const ASTNode*>(node)->deepCopy() );
}


LIBSBML_EXTERN
ASTNode_t *
ASTNode_getChild (const ASTNode_t *node, unsigned int n)
{
  if (node == NULL) return NULL;
  return static_cast<const ASTNode*>(node)->getChild(n);
}


LIBSBML_EXTERN
ASTNode_t *
ASTNode_getLeftChild (const ASTNode_t *node)
{
  if (node == NULL) return NULL;
  return static_cast<const ASTNode*>(node)->getLeftChild();
}


LIBSBML_EXTERN
ASTNode_t *
ASTNode_getRightChild (const ASTNode_t *node)
{
  if (node == NULL) return NULL;
  return static_cast<const ASTNode*>(node)->getRightChild();
}


LIBSBML_EXTERN
unsigned int
ASTNode_getNumChildren (const ASTNode_t *node)
{
  if (node == NULL) return 0;
  return static_cast<const ASTNode*>(node)->getNumChildren();
}


LIBSBML_EXTERN
List_t *
ASTNode_getListOfNodes (const ASTNode_t *node, ASTNodePredicate predicate)
{
  if (node == NULL) return NULL;
  return static_cast<const ASTNode*>(node)->getListOfNodes(predicate);
}


LIBSBML_EXTERN
void
ASTNode_fillListOfNodes ( const ASTNode_t  *node,
                          ASTNodePredicate predicate,
                          List_t           *lst )
{
  if (node == NULL) return;

  List* x = static_cast<List*>(lst);

  static_cast<const ASTNode*>(node)->fillListOfNodes(predicate, x);
}


LIBSBML_EXTERN
char
ASTNode_getCharacter (const ASTNode_t *node)
{
  if (node == NULL) return CHAR_MAX;
  return static_cast<const ASTNode*>(node)->getCharacter();
}


LIBSBML_EXTERN
long
ASTNode_getInteger (const ASTNode_t *node)
{
  if (node == NULL) return LONG_MAX;
  return static_cast<const ASTNode*>(node)->getInteger();
}


LIBSBML_EXTERN
const char *
ASTNode_getName (const ASTNode_t *node)
{
  if (node == NULL) return NULL;
  return static_cast<const ASTNode*>(node)->getName();
}


LIBSBML_EXTERN
long
ASTNode_getNumerator (const ASTNode_t *node)
{
  if (node == NULL) return LONG_MAX;
  return static_cast<const ASTNode*>(node)->getNumerator();
}


LIBSBML_EXTERN
long
ASTNode_getDenominator (const ASTNode_t *node)
{
  if (node == NULL) return LONG_MAX;
  return static_cast<const ASTNode*>(node)->getDenominator();
}


LIBSBML_EXTERN
double
ASTNode_getReal (const ASTNode_t *node)
{
  if (node == NULL) return util_NaN();
  return static_cast<const ASTNode*>(node)->getReal();
}


LIBSBML_EXTERN
double
ASTNode_getMantissa (const ASTNode_t *node)
{
  if (node == NULL) return numeric_limits<double>::quiet_NaN();
  return static_cast<const ASTNode*>(node)->getMantissa();
}


LIBSBML_EXTERN
long
ASTNode_getExponent (const ASTNode_t *node)
{
  if (node == NULL) return LONG_MAX;
  return static_cast<const ASTNode*>(node)->getExponent();
}


LIBSBML_EXTERN
double
ASTNode_getValue (const ASTNode_t *node)
{
  if (node == NULL) return util_NaN();
  return static_cast<const ASTNode*>(node)->getValue();
}


LIBSBML_EXTERN
int
ASTNode_getPrecedence (const ASTNode_t *node)
{
  if (node == NULL) return 6; // default precedence
  return static_cast<const ASTNode*>(node)->getPrecedence();
}


LIBSBML_EXTERN
ASTNodeType_t
ASTNode_getType (const ASTNode_t *node)
{
  if (node == NULL) return AST_UNKNOWN;
  return static_cast<const ASTNode*>(node)->getType();
}

LIBSBML_EXTERN
char *
ASTNode_getId(const ASTNode_t * node)
{
  if (node == NULL)
    return NULL;

  return safe_strdup(node->getId().c_str());
}

LIBSBML_EXTERN
char *
ASTNode_getClass(const ASTNode_t * node)
{
  if (node == NULL)
    return NULL;

  return safe_strdup(node->getClass().c_str());
}

LIBSBML_EXTERN
char *
ASTNode_getStyle(const ASTNode_t * node)
{
  if (node == NULL)
    return NULL;

  return safe_strdup(node->getStyle().c_str());
}


LIBSBML_EXTERN
char *
ASTNode_getUnits(const ASTNode_t * node)
{
  if (node == NULL) return NULL;
  return safe_strdup(node->getUnits().c_str());
}


LIBSBML_EXTERN
int
ASTNode_isAvogadro (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isAvogadro();
}


LIBSBML_EXTERN
int
ASTNode_isBoolean (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isBoolean();
}


LIBSBML_EXTERN
int
ASTNode_returnsBoolean (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->returnsBoolean();
}


LIBSBML_EXTERN
int
ASTNode_returnsBooleanForModel (const ASTNode_t *node, const Model_t* model)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->returnsBoolean(model);
}


LIBSBML_EXTERN
int
ASTNode_isConstant (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isConstant();
}


LIBSBML_EXTERN
int
ASTNode_isConstantNumber(const ASTNode_t *node)
{
  if (node == NULL) return (int)false;
  return (int) static_cast<const ASTNode*>(node)->isConstantNumber();
}


LIBSBML_EXTERN
int
ASTNode_isFunction (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isFunction();
}


LIBSBML_EXTERN
int
ASTNode_isInfinity (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return static_cast<int>( node->isInfinity() );
}


LIBSBML_EXTERN
int
ASTNode_isInteger (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isInteger();
}


LIBSBML_EXTERN
int
ASTNode_isLambda (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isLambda();
}


LIBSBML_EXTERN
int
ASTNode_isLog10 (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isLog10();
}


LIBSBML_EXTERN
int
ASTNode_isLogical (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isLogical();
}


LIBSBML_EXTERN
int
ASTNode_isName (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isName();
}


LIBSBML_EXTERN
int
ASTNode_isNaN (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return static_cast<int>( node->isNaN() );
}


LIBSBML_EXTERN
int
ASTNode_isNegInfinity (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return static_cast<int>( node->isNegInfinity() );
}


LIBSBML_EXTERN
int
ASTNode_isNumber (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isNumber();
}


LIBSBML_EXTERN
int
ASTNode_isOperator (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isOperator();
}


LIBSBML_EXTERN
int
ASTNode_isPiecewise (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return static_cast<int>( node->isPiecewise() );
}


LIBSBML_EXTERN
int
ASTNode_isRational (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isRational();
}


LIBSBML_EXTERN
int
ASTNode_isReal (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isReal();
}


LIBSBML_EXTERN
int
ASTNode_isRelational (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isRelational();
}


LIBSBML_EXTERN
int
ASTNode_isSqrt (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isSqrt();
}


LIBSBML_EXTERN
int
ASTNode_isUMinus (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isUMinus();
}

LIBSBML_EXTERN
int
ASTNode_isUPlus (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isUPlus();
}

LIBSBML_EXTERN
int
ASTNode_hasTypeAndNumChildren(const ASTNode_t *node, ASTNodeType_t type, unsigned int numchildren)
{
  if (node==NULL) return (int) false;
  return node->hasTypeAndNumChildren(type, numchildren);
}


LIBSBML_EXTERN
int
ASTNode_isUnknown (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return (int) static_cast<const ASTNode*>(node)->isUnknown();
}


LIBSBML_EXTERN
int
ASTNode_isSetId (const ASTNode_t *node)
{
  return static_cast<int>(node->isSetId());
}


LIBSBML_EXTERN
int
ASTNode_isSetClass (const ASTNode_t *node)
{
  return static_cast<int>(node->isSetClass());
}


LIBSBML_EXTERN
int
ASTNode_isSetStyle (const ASTNode_t *node)
{
  return static_cast<int>(node->isSetStyle());
}


LIBSBML_EXTERN
int
ASTNode_isSetUnits (const ASTNode_t *node)
{
  if (node == NULL) return (int) false;
  return static_cast<int>(node->isSetUnits());
}


LIBSBML_EXTERN
int
ASTNode_hasUnits (const ASTNode_t *node)
{
  if (node == NULL) return (int)false;
  return static_cast<int>(node->hasUnits());
}


LIBSBML_EXTERN
int
ASTNode_setCharacter (ASTNode_t *node, char value)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)->setCharacter(value);
}


LIBSBML_EXTERN
int
ASTNode_setName (ASTNode_t *node, const char *name)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)->setName(name);
}


LIBSBML_EXTERN
int
ASTNode_setInteger (ASTNode_t *node, long value)
{
  if(node == NULL) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)->setValue(value);
}


LIBSBML_EXTERN
int
ASTNode_setRational (ASTNode_t *node, long numerator, long denominator)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)->setValue(numerator, denominator);
}


LIBSBML_EXTERN
int
ASTNode_setReal (ASTNode_t *node, double value)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)->setValue(value);
}


LIBSBML_EXTERN
int
ASTNode_setRealWithExponent (ASTNode_t *node, double mantissa, long exponent)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)->setValue(mantissa, exponent);
}


LIBSBML_EXTERN
int
ASTNode_setType (ASTNode_t *node, ASTNodeType_t type)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)->setType(type);
}


LIBSBML_EXTERN
int
ASTNode_setId (ASTNode_t *node, const char *id)
{
  return static_cast<ASTNode*>(node)->setId(id);
}


LIBSBML_EXTERN
int
ASTNode_setClass (ASTNode_t *node, const char *className)
{
  return static_cast<ASTNode*>(node)->setClass(className);
}


LIBSBML_EXTERN
int
ASTNode_setStyle (ASTNode_t *node, const char *style)
{
  return static_cast<ASTNode*>(node)->setStyle(style);
}


LIBSBML_EXTERN
int
ASTNode_setUnits (ASTNode_t *node, const char *units)
{
  if (node == NULL ) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)->setUnits(units);
}


LIBSBML_EXTERN
int
ASTNode_swapChildren (ASTNode_t *node, ASTNode_t *that)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)
                         ->swapChildren( static_cast<ASTNode*>(that) );
}


LIBSBML_EXTERN
int
ASTNode_unsetId (ASTNode_t *node)
{
  return static_cast<ASTNode*>(node)->unsetId();
}


LIBSBML_EXTERN
int
ASTNode_unsetClass (ASTNode_t *node)
{
  return static_cast<ASTNode*>(node)->unsetClass();
}


LIBSBML_EXTERN
int
ASTNode_unsetStyle (ASTNode_t *node)
{
  return static_cast<ASTNode*>(node)->unsetStyle();
}


LIBSBML_EXTERN
int
ASTNode_unsetUnits (ASTNode_t *node)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return static_cast<ASTNode*>(node)->unsetUnits();
}


LIBSBML_EXTERN
void
ASTNode_replaceArgument(ASTNode_t* node, const char * bvar, ASTNode_t* arg)
{
  if (node == NULL) return ;
  static_cast<ASTNode*>(node)->replaceArgument(bvar, 
                                                  static_cast<ASTNode*>(arg));
}


LIBSBML_EXTERN
void
ASTNode_reduceToBinary(ASTNode_t* node)
{
  if (node == NULL) return;
  static_cast<ASTNode*>(node)->reduceToBinary();
}


LIBSBML_EXTERN
SBase_t * 
ASTNode_getParentSBMLObject(ASTNode_t* node)
{
  if (node == NULL) return NULL;
  return node->getParentSBMLObject();
}



LIBSBML_EXTERN
void
ASTNode_setParentSBMLObject(ASTNode_t* node, SBase_t * sb)
{
  if (node == NULL) return;
  node->setParentSBMLObject(sb);
}


LIBSBML_EXTERN
int 
ASTNode_unsetParentSBMLObject(ASTNode_t* node)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return node->unsetParentSBMLObject();
}


LIBSBML_EXTERN
int 
ASTNode_isSetParentSBMLObject(ASTNode_t* node)
{
  if (node == NULL) return (int) false;
  return static_cast<int>(node->isSetParentSBMLObject());
}



LIBSBML_EXTERN
int
ASTNode_removeChild(ASTNode_t* node, unsigned int n)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return node->removeChild(n);
}


LIBSBML_EXTERN
int
ASTNode_replaceChild(ASTNode_t* node, unsigned int n, ASTNode_t * newChild)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return node->replaceChild(n, newChild, false);
}


LIBSBML_EXTERN
int
ASTNode_replaceAndDeleteChild(ASTNode_t* node, unsigned int n, ASTNode_t * newChild)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return node->replaceChild(n, newChild, true);
}


LIBSBML_EXTERN
int
ASTNode_insertChild(ASTNode_t* node, unsigned int n, ASTNode_t * newChild)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return node->insertChild(n, newChild);
}


LIBSBML_EXTERN
int
ASTNode_addSemanticsAnnotation(ASTNode_t* node, XMLNode_t * annotation)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return node->addSemanticsAnnotation(annotation);
}


LIBSBML_EXTERN
unsigned int
ASTNode_getNumSemanticsAnnotations(ASTNode_t* node)
{
  if (node == NULL) return 0;
  return node->getNumSemanticsAnnotations();
}


LIBSBML_EXTERN
XMLNode_t *
ASTNode_getSemanticsAnnotation(ASTNode_t* node, unsigned int n)
{
  if (node == NULL) return NULL;
  return node->getSemanticsAnnotation(n);
}


LIBSBML_EXTERN
int
ASTNode_setUserData(ASTNode_t* node, void *userData)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return node->setUserData(userData);
}


LIBSBML_EXTERN
void *
ASTNode_getUserData(const ASTNode_t* node)
{
  if (node == NULL) return NULL;
  return node->getUserData();
}


LIBSBML_EXTERN
int
ASTNode_unsetUserData(ASTNode_t* node)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return node->unsetUserData();
}


LIBSBML_EXTERN
int
ASTNode_isSetUserData(const ASTNode_t* node)
{
  if (node == NULL) return (int) false;
  return static_cast<int>(node->isSetUserData());
}


LIBSBML_EXTERN
int
ASTNode_hasCorrectNumberArguments(ASTNode_t* node)
{
  if (node == NULL) return (int)false;
  return static_cast <int> (node->hasCorrectNumberArguments());
}

LIBSBML_EXTERN
int
ASTNode_isWellFormedASTNode(ASTNode_t* node)
{
  if (node == NULL) return (int) false;
  return static_cast <int> (node->isWellFormedASTNode());
}


LIBSBML_EXTERN
XMLAttributes_t * 
ASTNode_getDefinitionURL(ASTNode_t* node)
{
  if (node == NULL) return NULL;
  return node->getDefinitionURL();
}


LIBSBML_EXTERN
int 
ASTNode_setDefinitionURL(ASTNode_t* node, XMLAttributes_t defnURL)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  return node->setDefinitionURL(defnURL);
}


LIBSBML_EXTERN
char * 
ASTNode_getDefinitionURLString(ASTNode_t* node)
{
  if (node == NULL) return safe_strdup("");
  return safe_strdup(node->getDefinitionURLString().c_str());
}



LIBSBML_EXTERN
int 
ASTNode_setDefinitionURLString(ASTNode_t* node, const char * defnURL)
{
  if (node == NULL) return LIBSBML_INVALID_OBJECT;
  XMLAttributes_t *att = XMLAttributes_create();
  XMLAttributes_add(att, "definitionURL", defnURL);
  int ret = node->setDefinitionURL(*(att));
  XMLAttributes_free(att);
  return ret;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Internal utility function used in some language binding code.
 */
LIBSBML_EXTERN
int
ASTNode_true(const ASTNode *node)
{
  return 1;
}
/** @endcond */


/** @endcond */

LIBSBML_CPP_NAMESPACE_END
