/**
 * @file    ArraysASTPlugin.cpp
 * @brief   Implementation of ArraysASTPlugin, the plugin class of
 *          arrays package for the AST element.
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
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
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */

#include <sbml/packages/arrays/extension/ArraysASTPlugin.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>
#include <sbml/math/L3FormulaFormatter.h>

#include <iostream>
using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

static unsigned int
determineNumChildren(XMLInputStream & stream, const std::string element = "")
{
  unsigned int n = 0;

  n = stream.determineNumberChildren(element);

  return n;
}

#if (0)
static const char* ARRAYS_MATHML_ELEMENTS[] =
{
    "determinant"
  , "matrix"
  , "matrixrow"
  , "outerproduct"
  , "scalarproduct"
  , "selector"
  , "transpose"
  , "vector"
  , "vectorproduct"
};
#endif

static const char* ARRAYS_MATHML_ELEMENTS[] =
{
    "condition"
  , "exists"
  , "forall"
  , "lowlimit"
  , "mean"
  , "median"
  , "mode"
  , "moment"
  , "momentabout"
  , "product"
  , "sdev"
  , "selector"
  , "sum"
  , "uplimit"
  , "variance"
  , "vector"
};

#if (0)
static const int ARRAYS_MATHML_TYPES[] =
{
    AST_LINEAR_ALGEBRA_DETERMINANT
  , AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR
  , AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR
  , AST_LINEAR_ALGEBRA_OUTER_PRODUCT
  , AST_LINEAR_ALGEBRA_SCALAR_PRODUCT
  , AST_LINEAR_ALGEBRA_SELECTOR
  , AST_LINEAR_ALGEBRA_TRANSPOSE
  , AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR
  , AST_LINEAR_ALGEBRA_VECTOR_PRODUCT
  , AST_ARRAYS_UNKNOWN
};
#endif

static const int ARRAYS_MATHML_TYPES[] =
{
    AST_QUALIFIER_CONDITION
  , AST_LOGICAL_EXISTS
  , AST_LOGICAL_FORALL
  , AST_QUALIFIER_LOWLIMIT
  , AST_STATISTICS_MEAN
  , AST_STATISTICS_MEDIAN
  , AST_STATISTICS_MODE
  , AST_STATISTICS_MOMENT
  , AST_QUALIFIER_MOMENTABOUT
  , AST_SERIES_PRODUCT
  , AST_STATISTICS_SDEV
  , AST_LINEAR_ALGEBRA_SELECTOR
  , AST_SERIES_SUM
  , AST_QUALIFIER_UPLIMIT
  , AST_STATISTICS_VARIANCE
  , AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR
  , AST_ARRAYS_UNKNOWN
};

/*
 * Constructor
 */
ArraysASTPlugin::ArraysASTPlugin (const std::string &uri)
  : ASTBasePlugin(uri)
  , mVector (NULL)
#if (0)
  , mMatrix (NULL)
#endif
{
}



/*
 * Copy constructor. Creates a copy of this SBase object.
 */
ArraysASTPlugin::ArraysASTPlugin(const ArraysASTPlugin& orig)
  : ASTBasePlugin(orig)
  , mVector (NULL)
#if (0)
  , mMatrix (NULL)
#endif
{
  if ( orig.mVector  != NULL)
  {
    mVector = orig.mVector->deepCopy();
  }
  
#if (0)
  if ( orig.mMatrix  != NULL)
  {
    mMatrix = orig.mMatrix->deepCopy();
  }
#endif
}


/*
 * Destroy this object.
 */
ArraysASTPlugin::~ArraysASTPlugin () 
{
  delete mVector;
}

/*
 * Assignment operator for ArraysASTPlugin.
 */
ArraysASTPlugin& 
ArraysASTPlugin::operator=(const ArraysASTPlugin& orig)
{
  if(&orig!=this)
  {
    this->ASTBasePlugin::operator =(orig);

    delete mVector;
    if ( orig.mVector  != NULL)
    {
      mVector = static_cast<ASTArraysVectorFunctionNode*>
                                  ( orig.mVector->deepCopy() );
    }
    else
    {
      mVector = NULL;
    }

#if (0)
    delete mMatrix;
    if ( orig.mMatrix  != NULL)
    {
      mMatrix = static_cast<ASTArraysMatrixFunctionNode*>
                                  ( orig.mMatrix->deepCopy() );
    }
    else
    {
      mMatrix = NULL;
    }
#endif

  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this ArraysASTPlugin object.
 * 
 * @return a (deep) copy of this SBase object
 */
ArraysASTPlugin* 
ArraysASTPlugin::clone () const
{
  return new ArraysASTPlugin(*this);  
}


const std::string& 
ArraysASTPlugin::getPackageName() const
{
  static string arrays("arrays"); 
  return (mSBMLExt != NULL) ? mSBMLExt->getName() : arrays;
}


bool
ArraysASTPlugin::read(XMLInputStream& stream, const std::string& reqd_prefix, 
                                             const XMLToken& currentElement)
{
  bool read = false;
  
  stream.skipText();
  
  const string&  currentName = currentElement.getName();

  //ASTBase::checkPrefix(stream, reqd_prefix, currentElement);
  
  // create appropriate sub class
  if (currentName == "vector")
  {
    read = readVector(stream, reqd_prefix, currentElement);
  }
#if (0)
  else if (currentName == "matrix")
  {
    read = readMatrix(stream, reqd_prefix, currentElement);
  }
  else if (currentName == "matrixrow")
  {
    read = readMatrixRow(stream, reqd_prefix, currentElement);
  }
#endif
 
  return read;
}


bool 
ArraysASTPlugin::readVector(XMLInputStream& stream, const std::string& reqd_prefix,
                        const XMLToken& currentElement)
{
  bool read = false;
  
  stream.skipText();
  const XMLToken nextElement = stream.peek();
  //const string&  nextName = nextElement.getName();
  
  unsigned int numChildren = determineNumChildren(stream, "vector");
    
  mVector = new ASTArraysVectorFunctionNode();
  
  mVector->setExpectedNumChildren(numChildren);
  
  // read attributes on this element here since we have already consumed
  // the element
  ExpectedAttributes expectedAttributes;
  mVector->addExpectedAttributes(expectedAttributes, stream);
  read = mVector->ASTBase::readAttributes(currentElement.getAttributes(), 
                                expectedAttributes, stream, currentElement);
  if (read == false)
  {
    mVector = NULL;
  }
  else
  {  
    read = mVector->read(stream, reqd_prefix);
  }

  return read;
}

#if (0)

bool 
ArraysASTPlugin::readMatrix(XMLInputStream& stream, const std::string& reqd_prefix,
                        const XMLToken& currentElement)
{
  bool read = false;
  
  stream.skipText();
  const XMLToken nextElement = stream.peek();
  const string&  nextName = nextElement.getName();
  
  unsigned int numChildren = determineNumChildren(stream, "matrix");
    
  mMatrix = new ASTArraysMatrixFunctionNode();
  
  mMatrix->setExpectedNumChildren(numChildren);
  
  // read attributes on this element here since we have already consumed
  // the element
  ExpectedAttributes expectedAttributes;
  mMatrix->addExpectedAttributes(expectedAttributes, stream);
  read = mMatrix->ASTBase::readAttributes(currentElement.getAttributes(), 
                                expectedAttributes, stream, currentElement);
  if (read == false)
  {
    mMatrix = NULL;
  }
  else
  {  
    read = mMatrix->read(stream, reqd_prefix);
  }

  return read;
}


bool 
ArraysASTPlugin::readMatrixRow(XMLInputStream& stream, const std::string& reqd_prefix,
                        const XMLToken& currentElement)
{
  bool read = false;
  
  stream.skipText();
  const XMLToken nextElement = stream.peek();
  const string&  nextName = nextElement.getName();
  
  unsigned int numChildren = determineNumChildren(stream, "matrixrow");
    
  mVector = new ASTArraysVectorFunctionNode(AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  
  mVector->setExpectedNumChildren(numChildren);
  
  // read attributes on this element here since we have already consumed
  // the element
  ExpectedAttributes expectedAttributes;
  mVector->addExpectedAttributes(expectedAttributes, stream);
  read = mVector->ASTBase::readAttributes(currentElement.getAttributes(), 
                                expectedAttributes, stream, currentElement);
  if (read == false)
  {
    mVector = NULL;
  }
  else
  {  
    read = mVector->read(stream, reqd_prefix);
  }

  return read;
}
#endif

void
ArraysASTPlugin::reset()
{
  mVector = NULL;
#if (0)
  mMatrix = NULL;
#endif
}


const ASTBase* 
ArraysASTPlugin::getMath() const
{
  if (mVector != NULL)
  {
    return mVector;
  }
#if (0)
  else if (mMatrix != NULL)
  {
    return mMatrix;
  }
#endif
  else
  {
    return NULL;
  }
}



bool
ArraysASTPlugin::isSetMath() const
{
  if (mVector != NULL)
  {
    return true;
  }
#if (0)
  else if (mMatrix != NULL)
  {
    return true;
  }
#endif
  else
  {
    return false;
  }
}


void
ArraysASTPlugin::createMath(int type)
{
  reset();
  switch (type)
  {
  case AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR:
    mVector = new ASTArraysVectorFunctionNode();
    break;

#if (0)
  case AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR:
    mMatrix = new ASTArraysMatrixFunctionNode();
    break;
  case AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR:
    mVector = new ASTArraysVectorFunctionNode(AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
    break;
#endif

  default:
    break;
  }
}


int 
ArraysASTPlugin::addChild(ASTBase * child)
{ 
  if (child == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  if (mVector != NULL)
  {
    return mVector->addChild(child);
  }
#if (0)
  else if (mMatrix != NULL)
  {
    return mMatrix->addChild(child);
  }
#endif
  else
  {
    return LIBSBML_INVALID_OBJECT;
  }
}


ASTBase* 
ArraysASTPlugin::getChild (unsigned int n) const
{ 
  if (mVector != NULL)
  {
    return mVector->getChild(n);
  }
#if (0)
  else if (mMatrix != NULL)
  {
    return mMatrix->getChild(n);
  }
#endif
  else
  {
    return NULL;
  }
}


unsigned int 
ArraysASTPlugin::getNumChildren() const
{ 
  if (mVector != NULL)
  {
    return mVector->getNumChildren();
  }
#if (0)
  else if (mMatrix != NULL)
  {
    return mMatrix->getNumChildren();
  }
#endif
  else
  {
    return 0;
  }
}


int 
ArraysASTPlugin::insertChild(unsigned int n, ASTBase* newChild)
{ 
  if (mVector != NULL)
  {
    return mVector->insertChild(n, newChild);
  }
#if (0)
  else if (mMatrix != NULL)
  {
    return mMatrix->insertChild(n, newChild);
  }
#endif
  else
  {
    return LIBSBML_INVALID_OBJECT;
  }
}


int 
ArraysASTPlugin::prependChild(ASTBase* newChild)
{ 
  if (mVector != NULL)
  {
    return mVector->prependChild(newChild);
  }
#if (0)
  else if (mMatrix != NULL)
  {
    return mMatrix->prependChild(newChild);
  }
#endif
  else
  {
    return LIBSBML_INVALID_OBJECT;
  }
}


int 
ArraysASTPlugin::removeChild(unsigned int n)
{ 
  if (mVector != NULL)
  {
    return mVector->removeChild(n);
  }
#if (0)
  else if (mMatrix != NULL)
  {
    return mMatrix->removeChild(n);
  }
#endif
  else
  {
    return LIBSBML_INVALID_OBJECT;
  }
}


int 
ArraysASTPlugin::replaceChild(unsigned int n, ASTBase* newChild, bool delreplaced)
{ 
  if (mVector != NULL)
  {
    return mVector->replaceChild(n, newChild, delreplaced);
  }
#if (0)
  else if (mMatrix != NULL)
  {
    return mMatrix->replaceChild(n, newChild, delreplaced);
  }
#endif
  else
  {
    return LIBSBML_INVALID_OBJECT;
  }
}


int 
ArraysASTPlugin::swapChildren(ASTFunction* that)
{ 
  ASTArraysVectorFunctionNode * vectorToSwap = NULL;

#if (0)
  ASTArraysMatrixFunctionNode * matrixToSwap = NULL;
#endif

  ArraysASTPlugin * plugin = 
    static_cast<ArraysASTPlugin *>(that->getPlugin("arrays"));
  if (plugin != NULL && plugin->isSetMath() == true)
  {
    vectorToSwap = plugin->getVector();

#if (0)
    if (vectorToSwap == NULL)
    {
      matrixToSwap = plugin->getMatrix();
    }
#endif
  }

  if (mVector != NULL)
  {
    if (vectorToSwap != NULL)
    {
      return mVector->ASTFunctionBase::swapChildren(vectorToSwap);
    }
#if (0)
    else if (matrixToSwap != NULL)
    {
      return mVector->ASTFunctionBase::swapChildren(matrixToSwap);
    }
#endif
    else
    {
      return mVector->swapChildren(that);
    }
  }
#if (0)
  else if (mMatrix != NULL)
  {
    if (vectorToSwap != NULL)
    {
      return mMatrix->ASTFunctionBase::swapChildren(vectorToSwap);
    }
    else if (matrixToSwap != NULL)
    {
      return mMatrix->ASTFunctionBase::swapChildren(matrixToSwap);
    }
    else
    {
      return mMatrix->swapChildren(that);
    }
  }
#endif
  else
  {
    return LIBSBML_INVALID_OBJECT;
  }
}

/* default for components that have no required elements */
//bool
//ArraysASTPlugin::hasRequiredElements() const
//{
//  bool allPresent = true;
//
//  if (mArraysitativeSpecies.size() < 1)
//  {
//    allPresent = false;    
//  }
//  if (mTransitions.size() < 1)
//  {
//    allPresent = false;    
//  }
//  return allPresent;
//}
//
//


/*
 * Sets the parent SBML object of this plugin object to
 * this object and child elements (if any).
 * (Creates a child-parent relationship by this plugin object)
 */
void
ArraysASTPlugin::connectToParent (ASTBase* astbase)
{
  ASTBasePlugin::connectToParent(astbase);

}


/*
 * Enables/Disables the given package with child elements in this plugin
 * object (if any).
 */
void
ArraysASTPlugin::enablePackageInternal(const std::string& pkgURI,
                                        const std::string& pkgPrefix, bool flag)
{
}

  

bool 
ArraysASTPlugin::isFunction(int type) const
{
  bool valid = false;

  switch (type)
  {
    case AST_LINEAR_ALGEBRA_SELECTOR:
#if (0)
    case AST_LINEAR_ALGEBRA_DETERMINANT:
    case AST_LINEAR_ALGEBRA_TRANSPOSE:
    case AST_LINEAR_ALGEBRA_VECTOR_PRODUCT:
    case AST_LINEAR_ALGEBRA_SCALAR_PRODUCT:
    case AST_LINEAR_ALGEBRA_OUTER_PRODUCT:
#endif
    case AST_LOGICAL_EXISTS:
    case AST_LOGICAL_FORALL:
    case AST_STATISTICS_MEAN:
    case AST_STATISTICS_MEDIAN:
    case AST_STATISTICS_MODE:
    case AST_STATISTICS_MOMENT:
    case AST_SERIES_PRODUCT:
    case AST_STATISTICS_SDEV:
    case AST_SERIES_SUM:
    case AST_STATISTICS_VARIANCE:
      valid = true;
      break;
    default:
      break;

  }
  return valid;
}


bool 
ArraysASTPlugin::isFunctionNode(int type) const
{
  bool valid = false;

  if (isFunction(type) == true)
  {
    valid = true;
  }
  else
  {
    switch (type)
    {
      case AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR:
#if (0)
      case AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR:
      case AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR:
#endif
        valid = true;
        break;
      default:
        break;

    }
  }

  return valid;
}



bool 
ArraysASTPlugin::representsUnaryFunction(int type) const
{
  bool valid = false;

#if (0)
  switch (type)
  {
    case AST_LINEAR_ALGEBRA_DETERMINANT:
    case AST_LINEAR_ALGEBRA_TRANSPOSE:
      valid = true;
      break;
    default:
      break;

  }
#endif
  return valid;
}


bool 
ArraysASTPlugin::representsBinaryFunction(int type) const
{
  bool valid = false;

#if (0)
  switch (type)
  {
    case AST_LINEAR_ALGEBRA_VECTOR_PRODUCT:
    case AST_LINEAR_ALGEBRA_SCALAR_PRODUCT:
    case AST_LINEAR_ALGEBRA_OUTER_PRODUCT:
      valid = true;
      break;
    default:
      break;

  }
#endif
  return valid;
}


bool 
ArraysASTPlugin::representsNaryFunction(int type) const
{
  bool valid = false;

  switch (type)
  {
    case AST_LINEAR_ALGEBRA_SELECTOR:
    case AST_LOGICAL_EXISTS:
    case AST_LOGICAL_FORALL:
    case AST_STATISTICS_MEAN:
    case AST_STATISTICS_MEDIAN:
    case AST_STATISTICS_MODE:
    case AST_STATISTICS_MOMENT:
    case AST_SERIES_PRODUCT:
    case AST_STATISTICS_SDEV:
    case AST_SERIES_SUM:
    case AST_STATISTICS_VARIANCE:
      valid = true;
      break;
    default:
      break;

  }
  return valid;
}


bool 
ArraysASTPlugin::representsQualifier(int type) const
{
  bool valid = false;

  switch (type)
  {
    case AST_QUALIFIER_CONDITION:
    case AST_QUALIFIER_LOWLIMIT:
    case AST_QUALIFIER_MOMENTABOUT:
    case AST_QUALIFIER_UPLIMIT:
      valid = true;
      break;
    default:
      break;

  }
  return valid;
}


bool 
ArraysASTPlugin::isTopLevelMathMLFunctionNodeTag(const std::string& name) const
{
  bool valid = false;

  if (name == "vector")
  {
    valid = true;
  }
#if (0)
  else if (name == "matrix")
  {
    valid = true;
  }
  else if (name == "matrixrow")
  {
    valid = true;
  }
#endif
  else if (representsQualifier(getTypeFromName(name)) == true)
  {
    return true;
  }

  return valid;
}


int 
ArraysASTPlugin::getTypeFromName(const std::string& name) const
{
  int type = AST_UNKNOWN;

  static const int size = sizeof(ARRAYS_MATHML_ELEMENTS) / sizeof(ARRAYS_MATHML_ELEMENTS[0]);

  int  index = util_bsearchStringsI(ARRAYS_MATHML_ELEMENTS, name.c_str(), 0, size - 1);
  bool found = (index < size);

  if (found) 
  {
    type = ARRAYS_MATHML_TYPES[index];
  }

  return type;
}

const char * 
ArraysASTPlugin::getNameFromType(int type) const
{

  static const int size = sizeof(ARRAYS_MATHML_ELEMENTS) / sizeof(ARRAYS_MATHML_ELEMENTS[0]);
  if (type >= AST_QUALIFIER_CONDITION && type < AST_ARRAYS_UNKNOWN)
  {
    bool found = false;
    unsigned int i;
    for (i = 0; i < size && found == false; i++)
    {
      if (type == ARRAYS_MATHML_TYPES[i])
        found = true;
    }
    if (found == true)
    {
      return ARRAYS_MATHML_ELEMENTS[i-1];
    }
  }

  return "";
}


ASTArraysVectorFunctionNode *
ArraysASTPlugin::getVector() const
{
  return mVector;
}


#if (0)
ASTArraysMatrixFunctionNode *
ArraysASTPlugin::getMatrix() const
{
  return mMatrix;
}
#endif


ArraysASTNodeType_t
ArraysASTPlugin::getASTType() const
{
  int type = AST_ARRAYS_UNKNOWN;
  if (isSetMath() == true)
  {
    type = getMath()->getExtendedType();
  }
  else if (getParentASTObject() != NULL)
  {
    type = getParentASTObject()->getExtendedType();
  }


  if (type >= AST_QUALIFIER_CONDITION &&
    type < AST_ARRAYS_UNKNOWN)
  {
    return (ArraysASTNodeType_t)(type);
  }
  else
  {
    return (ArraysASTNodeType_t)(AST_ARRAYS_UNKNOWN);
  }
}

#define GET_NUM_CHILDREN(result,node) \
{\
  ASTFunctionBase* tmp = dynamic_cast<ASTFunctionBase*>(node);\
  if (tmp != NULL) result= tmp->getNumChildren(); \
  else\
  {\
    ASTNode* tmp2 = dynamic_cast<ASTNode*>(node);\
    if (tmp2 != NULL)\
      result= tmp2->getNumChildren(); \
    else result = 0;\
  }\
}


bool
ArraysASTPlugin::hasCorrectNumberArguments(int type) const
{
  bool correctNumArgs = true;
  unsigned int numChildren;

  ASTBase* function = const_cast<ASTBase*>(getMath());

  // so getMath will only return teh node if it is a vector or matrix
  if (function == NULL)
  {
    function = const_cast<ASTBase*>(getParentASTObject());
    
    if (function == NULL) 
    {
      return false;
    }
  }

  if (function->getExtendedType() != type)
    return false;

  GET_NUM_CHILDREN(numChildren, function);

  switch (type)
  {
  case AST_LINEAR_ALGEBRA_SELECTOR:
    if (numChildren < 1 || numChildren > 3)
    {
      correctNumArgs = false;
    }
    break;
  default:
    break;

  }

  return correctNumArgs;
}


bool
ArraysASTPlugin::isWellFormedNode(int type) const
{
  bool valid = hasCorrectNumberArguments(type);
  ASTBase* function = const_cast<ASTBase*>(getMath());

  // so getMath will only return teh node if it is a vector or matrix
  if (function == NULL)
  {
    function = const_cast<ASTBase*>(getParentASTObject());
    
    if (function == NULL) 
    {
      return false;
    }
  }

      // cast the function to an ASTNode
  ASTNode * newAST = dynamic_cast<ASTNode*>(function);

  // double check we are working with the right thing
  if (newAST == NULL)
  {
    return false;
  }
  else if (newAST->getExtendedType() != type)
  {
    return false;
  }

  unsigned int numChildren = newAST->getNumChildren();
  unsigned int i = 0;

  // check number of arguments
  while (valid && i < numChildren)
  {
    valid = newAST->getChild(i)->isWellFormedNode();
    i++;
  }
  return valid;
}

bool
ArraysASTPlugin::isPackageInfixFunction() const
{
  ASTBase* function = const_cast<ASTBase*>(getParentASTObject());
  if (function == NULL) return false;
  if (function->getType() != AST_ORIGINATES_IN_PACKAGE) return false;
  if (function->getPackageName() != "arrays") return false;

  // cast the function to an ASTNode
  ASTNode* newAST = dynamic_cast<ASTNode*>(function);
  if (newAST == NULL)
  {
    return false;
  }

  //unsigned int numChildren = newAST->getNumChildren();
  //unsigned int child = 0;
  const ArraysASTPlugin* aap = static_cast<const ArraysASTPlugin*>(newAST->getPlugin("arrays"));
  switch(aap->getASTType()) {
  case AST_LINEAR_ALGEBRA_SELECTOR:
  case AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR:
    return false;
#if (0)
  case AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR:
    //An empty matrix or a matrix with only one row looks like a vector, so we have to use the functional form
    if (numChildren<=1) return true;
    //Also, none of the rows may be empty for the { ... ; ... } to be parseable:
    for (child=0; child<numChildren; child++) {
      if(newAST->getChild(child)->getNumChildren() == 0) {
        return true;
      }
    }
    return false;
  case AST_LINEAR_ALGEBRA_DETERMINANT:
  case AST_LINEAR_ALGEBRA_TRANSPOSE:
  case AST_LINEAR_ALGEBRA_VECTOR_PRODUCT:
  case AST_LINEAR_ALGEBRA_SCALAR_PRODUCT:
  case AST_LINEAR_ALGEBRA_OUTER_PRODUCT:
  case AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR:
    return true;
#endif
  case AST_LOGICAL_EXISTS:
  case AST_LOGICAL_FORALL:
  case AST_STATISTICS_MEAN:
  case AST_STATISTICS_MEDIAN:
  case AST_STATISTICS_MODE:
  case AST_STATISTICS_MOMENT:
  case AST_SERIES_PRODUCT:
  case AST_STATISTICS_SDEV:
  case AST_SERIES_SUM:
  case AST_STATISTICS_VARIANCE:
  case AST_ARRAYS_UNKNOWN:
    return true;
  case AST_QUALIFIER_CONDITION:
  case AST_QUALIFIER_LOWLIMIT:
  case AST_QUALIFIER_MOMENTABOUT:
  case AST_QUALIFIER_UPLIMIT:
    return false;
  }
  return false;
}

bool
ArraysASTPlugin::hasPackageOnlyInfixSyntax() const
{
  const ASTBase* function = getParentASTObject();
  if (function == NULL) return false;
  if (function->getType() != AST_ORIGINATES_IN_PACKAGE) return false;
  if (function->getPackageName() != "arrays") return false;
  const ArraysASTPlugin* aap = static_cast<const ArraysASTPlugin*>(function->getPlugin("arrays"));
  switch(aap->getASTType()) {
  case AST_LINEAR_ALGEBRA_SELECTOR:
  case AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR:
#if(0)
  case AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR:
#endif
    return true; //x[y] and {x, y} and {x, y; p, q}
#if (0)
  case AST_LINEAR_ALGEBRA_DETERMINANT:
  case AST_LINEAR_ALGEBRA_TRANSPOSE:
  case AST_LINEAR_ALGEBRA_VECTOR_PRODUCT:
  case AST_LINEAR_ALGEBRA_SCALAR_PRODUCT:
  case AST_LINEAR_ALGEBRA_OUTER_PRODUCT:
  case AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR:
#endif
  case AST_QUALIFIER_CONDITION:
  case AST_LOGICAL_EXISTS:
  case AST_LOGICAL_FORALL:
  case AST_QUALIFIER_LOWLIMIT:
  case AST_STATISTICS_MEAN:
  case AST_STATISTICS_MEDIAN:
  case AST_STATISTICS_MODE:
  case AST_STATISTICS_MOMENT:
  case AST_QUALIFIER_MOMENTABOUT:
  case AST_SERIES_PRODUCT:
  case AST_STATISTICS_SDEV:
  case AST_SERIES_SUM:
  case AST_QUALIFIER_UPLIMIT:
  case AST_STATISTICS_VARIANCE:
  case AST_ARRAYS_UNKNOWN:
    return false;
  }
  return false;
}

int
ArraysASTPlugin::getL3PackageInfixPrecedence() const
{
  const ASTBase* function = getParentASTObject();
  if (function == NULL) return -1;
  if (function->getType() != AST_ORIGINATES_IN_PACKAGE) return -1;
  if (function->getPackageName() != "arrays") return -1;
  const ArraysASTPlugin* aap = static_cast<const ArraysASTPlugin*>(function->getPlugin("arrays"));
  switch(aap->getASTType()) {
  case AST_LINEAR_ALGEBRA_SELECTOR:
  case AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR:
#if (0)
  case AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR:
  case AST_LINEAR_ALGEBRA_DETERMINANT:
  case AST_LINEAR_ALGEBRA_TRANSPOSE:
  case AST_LINEAR_ALGEBRA_VECTOR_PRODUCT:
  case AST_LINEAR_ALGEBRA_SCALAR_PRODUCT:
  case AST_LINEAR_ALGEBRA_OUTER_PRODUCT:
  case AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR:
#endif
  case AST_QUALIFIER_CONDITION:
  case AST_LOGICAL_EXISTS:
  case AST_LOGICAL_FORALL:
  case AST_QUALIFIER_LOWLIMIT:
  case AST_STATISTICS_MEAN:
  case AST_STATISTICS_MEDIAN:
  case AST_STATISTICS_MODE:
  case AST_STATISTICS_MOMENT:
  case AST_QUALIFIER_MOMENTABOUT:
  case AST_SERIES_PRODUCT:
  case AST_STATISTICS_SDEV:
  case AST_SERIES_SUM:
  case AST_QUALIFIER_UPLIMIT:
  case AST_STATISTICS_VARIANCE:
    return 8; //Everything is either a function or has unambiguous syntax.
  case AST_ARRAYS_UNKNOWN:
    return -1;
  }
  return -1;
}

bool ArraysASTPlugin::hasUnambiguousPackageInfixGrammar(const ASTNode *child) const
{
  ASTBase* function = const_cast<ASTBase*>(getParentASTObject());
  if (function == NULL) return false;
  if (function->getType() != AST_ORIGINATES_IN_PACKAGE) return false;
  if (function->getPackageName() != "arrays") return false;

  // cast the function to an ASTNode
  ASTNode* newAST = dynamic_cast<ASTNode*>(function);
  if (newAST == NULL)
  {
    return false;
  }

  const ArraysASTPlugin* aap = static_cast<const ArraysASTPlugin*>(function->getPlugin("arrays"));
  switch(aap->getASTType()) {
  case AST_LINEAR_ALGEBRA_SELECTOR:
    if (newAST->getNumChildren() == 0) return true;
    if (newAST->getChild(0) == child) return false; //The *first* child of the selector needs parentheses in some situations!
    return true; //All other children are separated by commas, and thus don't need parentheses.
  case AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR:
#if (0)
  case AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR:
  case AST_LINEAR_ALGEBRA_DETERMINANT:
  case AST_LINEAR_ALGEBRA_TRANSPOSE:
  case AST_LINEAR_ALGEBRA_VECTOR_PRODUCT:
  case AST_LINEAR_ALGEBRA_SCALAR_PRODUCT:
  case AST_LINEAR_ALGEBRA_OUTER_PRODUCT:
  case AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR:
#endif
  case AST_QUALIFIER_CONDITION:
  case AST_LOGICAL_EXISTS:
  case AST_LOGICAL_FORALL:
  case AST_QUALIFIER_LOWLIMIT:
  case AST_STATISTICS_MEAN:
  case AST_STATISTICS_MEDIAN:
  case AST_STATISTICS_MODE:
  case AST_STATISTICS_MOMENT:
  case AST_QUALIFIER_MOMENTABOUT:
  case AST_SERIES_PRODUCT:
  case AST_STATISTICS_SDEV:
  case AST_SERIES_SUM:
  case AST_QUALIFIER_UPLIMIT:
  case AST_STATISTICS_VARIANCE:
    return true; //Everything is either a function or has unambiguous syntax.
  case AST_ARRAYS_UNKNOWN:
    return false;
  }
  return false;
}

void ArraysASTPlugin::visitPackageInfixSyntax ( const ASTNode *parent,
                                           const ASTNode *node,
                                           StringBuffer_t  *sb,
                                           const L3ParserSettings* settings) const
{
  if (node == NULL) return;
  if (node->getType() != AST_ORIGINATES_IN_PACKAGE) return;
  if (node->getPackageName() != "arrays") return;
  const ArraysASTPlugin* aap = static_cast<const ArraysASTPlugin*>(node->getPlugin("arrays"));
  switch(aap->getASTType()) {
  case AST_LINEAR_ALGEBRA_SELECTOR:
    return visitSelector(parent, node, sb, settings);
  case AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR:
    return visitVector(parent, node, sb, settings);
#if (0)
  case AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR:
    return visitMatrix(parent, node, sb, settings);
  case AST_LINEAR_ALGEBRA_DETERMINANT:
  case AST_LINEAR_ALGEBRA_TRANSPOSE:
  case AST_LINEAR_ALGEBRA_VECTOR_PRODUCT:
  case AST_LINEAR_ALGEBRA_SCALAR_PRODUCT:
  case AST_LINEAR_ALGEBRA_OUTER_PRODUCT:
  case AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR:
#endif
  case AST_QUALIFIER_CONDITION:
  case AST_LOGICAL_EXISTS:
  case AST_LOGICAL_FORALL:
  case AST_QUALIFIER_LOWLIMIT:
  case AST_STATISTICS_MEAN:
  case AST_STATISTICS_MEDIAN:
  case AST_STATISTICS_MODE:
  case AST_STATISTICS_MOMENT:
  case AST_QUALIFIER_MOMENTABOUT:
  case AST_SERIES_PRODUCT:
  case AST_STATISTICS_SDEV:
  case AST_SERIES_SUM:
  case AST_QUALIFIER_UPLIMIT:
  case AST_STATISTICS_VARIANCE:
  case AST_ARRAYS_UNKNOWN:
    return;
  }
  return;
}

int ArraysASTPlugin::checkNumArguments(const ASTNode* function, std::stringstream& error) const
{
  if (function->getType() != AST_ORIGINATES_IN_PACKAGE) return 0;
  if (function->getPackageName() != "arrays") return 0;
  const ArraysASTPlugin* aap = static_cast<const ArraysASTPlugin*>(function->getPlugin("arrays"));
  string product = "";
  //unsigned int c=0;
  //unsigned int firstrow=0;

  switch(aap->getASTType()) {
  case AST_LINEAR_ALGEBRA_SELECTOR:
    switch(function->getNumChildren()) {
    case 0:
      error << "The 'selector' function must have at least one argument: the vector or matrix in question.";
      return -1;
    case 1:
    case 2:
    case 3:
      return 1; //correct number
    default:
      error << "The 'selector' function may not have more than three arguments ('selector(a, x, y)'):  the first for the vector or matrix from which to select, the second for the index of the vector or the matrixrow of the matrix, and an optional third, which only applies to matrices, for the index of the selected matrixrow of the matrix.  Similarly, the bracketed form may have no more than two ('a[x, y]'), for the same reason.";
      return -1;
    }
  case AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR:
#if (0)
  case AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR:
#endif
    return 1; //Vectors and matrix rows can have any number of arguments.
#if (0)
  case AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR:
    //Matrices can have any number of children, but all of those children have to be matrixrows, and have the same number of kids.
    if (function->getNumChildren()==0) return 1;
    firstrow = function->getChild(0)->getNumChildren();
    for(c=0; c<function->getNumChildren(); c++) {
      if (function->getChild(c)->getExtendedType() != AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR) {
        error << "All children of matrices must be matrix rows created with 'matrixrow()', or the entire matrix may be constructed with curly braces and semicolon-delimited lists: '{a, b; x, y}'.";
        return -1;
      }
      if (firstrow != function->getChild(c)->getNumChildren()) {
        error << "All rows of matrices must have the same number of arguments:  In this matrix, the first row has " << firstrow << " children, but row " << c+1 << " has " << function->getChild(c)->getNumChildren() << " child(ren).";
        return -1;
      }
    }
    return 1;
  case AST_LINEAR_ALGEBRA_DETERMINANT:
    switch(function->getNumChildren()) {
    case 1:
      return 1; //correct number
    default:
      error << "The 'determinant' function must have exactly one argument: the matrix in question.";
      return -1;
    }
  case AST_LINEAR_ALGEBRA_TRANSPOSE:
    switch(function->getNumChildren()) {
    case 1:
      return 1; //correct number
    default:
      error << "The 'transpose' function must have exactly one argument: the vector or matrix in question.";
      return -1;
    }
  case AST_LINEAR_ALGEBRA_VECTOR_PRODUCT:
    product = "vector product";
    //Fall through to:
  case AST_LINEAR_ALGEBRA_SCALAR_PRODUCT:
    if (product.empty()) {
      product = "scalar product";
    }
    //Fall through to:
  case AST_LINEAR_ALGEBRA_OUTER_PRODUCT:
    if (product.empty()) {
      product = "outer product";
    }
    switch(function->getNumChildren()) {
    case 2:
      return 1; //correct number
    default:
      error << "The " << product << " function must have exactly two vector arguments.";
      return -1;
    }
#endif
  case AST_LOGICAL_EXISTS:
  case AST_LOGICAL_FORALL:
  case AST_STATISTICS_MEAN:
  case AST_STATISTICS_MEDIAN:
  case AST_STATISTICS_MODE:
  case AST_STATISTICS_MOMENT:
  case AST_SERIES_PRODUCT:
  case AST_STATISTICS_SDEV:
  case AST_SERIES_SUM:
  case AST_STATISTICS_VARIANCE:
    /* can have any number of arguments */
    return 1;
  case AST_QUALIFIER_CONDITION:
  case AST_QUALIFIER_LOWLIMIT:
  case AST_QUALIFIER_MOMENTABOUT:
  case AST_QUALIFIER_UPLIMIT:
    switch(function->getNumChildren()) {
    case 1:
      return 1; //correct number
    default:
      error << "The " << getNameFromType(aap->getASTType()) 
        << " qualifier must have exactly one argument.";
      return -1;
    }
  case AST_ARRAYS_UNKNOWN:
#if (0)
    assert(false); //It was supposed to originate in the Arrays package!
#endif
    return 0;
  }
#if (0)
  assert(false);
#endif
  return 0;
}


ASTNode*
ArraysASTPlugin::parsePackageInfix(L3ParserGrammarLineType_t type, 
    vector<ASTNode*> *nodeList, vector<std::string*> *stringList,
    vector<double> *doubleList) const
{
  ASTNode *node = NULL;
  //None of the arrays grammar lines have strings or doubles:
  if(stringList != NULL) return NULL;
  if(doubleList != NULL) return NULL;

  switch (type)
  {
  case INFIX_SYNTAX_NAMED_SQUARE_BRACKETS:
    if (nodeList == NULL) return NULL;
    if (nodeList->size() == 2)
    {
      node = parseNamedSquareBrackets(nodeList->at(0), nodeList->at(1));
    }
    else if (nodeList->size() == 1)
    {
      node = parseNamedSquareBrackets(nodeList->at(0), NULL);
    }
    break;
  case INFIX_SYNTAX_CURLY_BRACES:
    if (nodeList != NULL && nodeList->size() == 1)
    {
      node = parseCurlyBracesList(nodeList->at(0));
    }
    else if (nodeList == NULL)
    {
      node = parseCurlyBracesList(NULL);
    }
    break;
  case INFIX_SYNTAX_CURLY_BRACES_SEMICOLON:
    if (nodeList != NULL && nodeList->size() == 1)
    {
      node = parseCurlyBracesSemicolonList(nodeList->at(0));
    }
    break;
  default:
    break;
  }

  return node;
}




ASTNode* ArraysASTPlugin::parseCurlyBracesList(ASTNode* nodelist) const
{
  if (nodelist == NULL) 
  {
    nodelist = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  }
  else {
    nodelist->setType(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  }
  return nodelist;
}

ASTNode* ArraysASTPlugin::parseCurlyBracesSemicolonList(ASTNode* nodelist) const
{
#if (0)
  nodelist->setType(AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR);
  for (unsigned int child=0; child<nodelist->getNumChildren(); child++) {
    nodelist->getChild(child)->setType(AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  }
#endif

  return NULL;
}

ASTNode* ArraysASTPlugin::parseNamedSquareBrackets(ASTNode* parent, ASTNode* nodelist) const
{
  if (nodelist == NULL) {
    nodelist = new ASTNode(AST_LINEAR_ALGEBRA_SELECTOR);
  }
  else {
    nodelist->setType(AST_LINEAR_ALGEBRA_SELECTOR);
  }
  nodelist->insertChild(0, parent);
  return nodelist;
  //The following code can be used to allow square brackets with 3+ children:
  /*
  ASTNode* ret = nodelist;
  while (ret->getNumChildren() > 2) {
    ASTNode* newtop = new ASTNode(AST_LINEAR_ALGEBRA_SELECTOR);
    while (ret->getNumChildren() > 2) {
      ASTNode* thirdchild = ret->getChild(2);
      ret->removeChild(2);
      newtop->addChild(thirdchild);
    }
    newtop->insertChild(0, ret);
    ret = newtop;
  }
  return ret;
  */
}

int ArraysASTPlugin::getPackageFunctionFor(const std::string& name) const
{
#if (0)
  if (!strcmp_insensitive(name.c_str(), "determinant"))   return AST_LINEAR_ALGEBRA_DETERMINANT;
  if (!strcmp_insensitive(name.c_str(), "det"))           return AST_LINEAR_ALGEBRA_DETERMINANT;
  if (!strcmp_insensitive(name.c_str(), "selector"))      return AST_LINEAR_ALGEBRA_SELECTOR;
  if (!strcmp_insensitive(name.c_str(), "transpose"))     return AST_LINEAR_ALGEBRA_TRANSPOSE;
  if (!strcmp_insensitive(name.c_str(), "trans"))         return AST_LINEAR_ALGEBRA_TRANSPOSE;
  if (!strcmp_insensitive(name.c_str(), "vectorproduct")) return AST_LINEAR_ALGEBRA_VECTOR_PRODUCT;
  if (!strcmp_insensitive(name.c_str(), "vectorprod"))    return AST_LINEAR_ALGEBRA_VECTOR_PRODUCT;
  if (!strcmp_insensitive(name.c_str(), "cross"))         return AST_LINEAR_ALGEBRA_VECTOR_PRODUCT;
  if (!strcmp_insensitive(name.c_str(), "scalarproduct")) return AST_LINEAR_ALGEBRA_SCALAR_PRODUCT;
  if (!strcmp_insensitive(name.c_str(), "scalarprod"))    return AST_LINEAR_ALGEBRA_SCALAR_PRODUCT;
  if (!strcmp_insensitive(name.c_str(), "dot"))           return AST_LINEAR_ALGEBRA_SCALAR_PRODUCT;
  if (!strcmp_insensitive(name.c_str(), "outerproduct"))  return AST_LINEAR_ALGEBRA_OUTER_PRODUCT;
  if (!strcmp_insensitive(name.c_str(), "outerprod"))     return AST_LINEAR_ALGEBRA_OUTER_PRODUCT;
  if (!strcmp_insensitive(name.c_str(), "outer"))         return AST_LINEAR_ALGEBRA_OUTER_PRODUCT;
  if (!strcmp_insensitive(name.c_str(), "vector"))        return AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR;
  if (!strcmp_insensitive(name.c_str(), "matrix"))        return AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR;
  if (!strcmp_insensitive(name.c_str(), "matrixrow"))     return AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR;
#endif
  if (!strcmp_insensitive(name.c_str(), "selector"))      
    return AST_LINEAR_ALGEBRA_SELECTOR;
  if (!strcmp_insensitive(name.c_str(), "vector"))        
    return AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR;
  if (!strcmp_insensitive(name.c_str(), "exists"))        
    return AST_LOGICAL_EXISTS;
  if (!strcmp_insensitive(name.c_str(), "forall"))        
    return AST_LOGICAL_FORALL;
  if (!strcmp_insensitive(name.c_str(), "mean"))          
    return AST_STATISTICS_MEAN;
  if (!strcmp_insensitive(name.c_str(), "median"))  
    return AST_STATISTICS_MEDIAN;
  if (!strcmp_insensitive(name.c_str(), "mode"))  
    return AST_STATISTICS_MODE;
  if (!strcmp_insensitive(name.c_str(), "moment"))  
    return AST_STATISTICS_MOMENT;
  if (!strcmp_insensitive(name.c_str(), "product"))  
    return AST_SERIES_PRODUCT;
  if (!strcmp_insensitive(name.c_str(), "sdev"))  
    return AST_STATISTICS_SDEV;
  if (!strcmp_insensitive(name.c_str(), "sum"))  
    return AST_SERIES_SUM;
  if (!strcmp_insensitive(name.c_str(), "variance"))  
    return AST_STATISTICS_VARIANCE;
  return AST_UNKNOWN;
}

void ArraysASTPlugin::visitSelector( const ASTNode *parent,
                                     const ASTNode *node,
                                     StringBuffer_t  *sb,
                                     const L3ParserSettings* settings) const
{
  unsigned int numChildren = node->getNumChildren();
  if (numChildren == 0) {
    //Invalid, but we still have to write something
    StringBuffer_append(sb, "selector()");
    return;
  }
  ASTNode* firstchild = node->getChild(0);
  L3FormulaFormatter_visit(node, firstchild, sb, settings);
  StringBuffer_appendChar(sb, '[');
  for (unsigned int child=1; child<numChildren; child++) {
    if (child>1) {
      StringBuffer_appendChar(sb, ',');
      StringBuffer_appendChar(sb, ' ');
    }
    L3FormulaFormatter_visit(node, node->getChild(child), sb, settings);
  }
  StringBuffer_appendChar(sb, ']');
}

void ArraysASTPlugin::visitVector( const ASTNode *parent,
                                   const ASTNode *node,
                                   StringBuffer_t  *sb,
                                   const L3ParserSettings* settings) const
{
  unsigned int numChildren = node->getNumChildren();
  StringBuffer_appendChar(sb, '{');
  for (unsigned int child=0; child<numChildren; child++) {
    if (child>0) {
      StringBuffer_appendChar(sb, ',');
      StringBuffer_appendChar(sb, ' ');
    }
    L3FormulaFormatter_visit(node, node->getChild(child), sb, settings);
  }
  StringBuffer_appendChar(sb, '}');
}

#if (0)
void ArraysASTPlugin::visitMatrix( const ASTNode *parent,
                                   const ASTNode *node,
                                   StringBuffer_t  *sb,
                                   const L3ParserSettings* settings) const
{
  //Note:  if there is one or zero rows, or if any of the rows have zero children, a matrix is declared as a function, and 'visitMatrix' won't be called.
  unsigned int numChildren = node->getNumChildren();
  StringBuffer_appendChar(sb, '{');
  for (unsigned int child=0; child<numChildren; child++) {
    ASTNode* row = node->getChild(child);
    if (child>0) {
      StringBuffer_appendChar(sb, ';');
      StringBuffer_appendChar(sb, ' ');
    }
    for (unsigned int c2=0; c2<row->getNumChildren(); c2++) {
      if (c2>0) {
        StringBuffer_appendChar(sb, ',');
        StringBuffer_appendChar(sb, ' ');
      }
      L3FormulaFormatter_visit(row, row->getChild(c2), sb, settings);
    }
  }
  StringBuffer_appendChar(sb, '}');
}
#endif

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
