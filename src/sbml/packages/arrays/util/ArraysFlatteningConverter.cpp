/**
 * @file    ArraysFlatteningConverter.cpp
 * @brief   Implementation of a first flattening converter.
 * @author  Sarah M Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2017 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->
 */


#include <sbml/packages/arrays/util/ArraysFlatteningConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/packages/arrays/common/ArraysExtensionTypes.h>

#ifdef __cplusplus

#include <algorithm>
#include <string>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN


/** @cond doxygenLibsbmlInternal */
/*
 * SBML Converter stuff below
 */

void ArraysFlatteningConverter::init()
{
  //'addConverter' adds a clone, not the original.
  ArraysFlatteningConverter cfc;
  SBMLConverterRegistry::getInstance().addConverter(&cfc);
}
/** @endcond */


ArraysFlatteningConverter::ArraysFlatteningConverter() 
  : SBMLConverter("SBML Arrays Flattening Converter")
{
}


ArraysFlatteningConverter::ArraysFlatteningConverter
                         (const ArraysFlatteningConverter& orig) :
SBMLConverter(orig)
{
}

ArraysFlatteningConverter* 
ArraysFlatteningConverter::clone() const
{
  return new ArraysFlatteningConverter(*this);
}


/*
 * Destroy this object.
 */
ArraysFlatteningConverter::~ArraysFlatteningConverter ()
{
}


ConversionProperties
ArraysFlatteningConverter::getDefaultProperties() const
{
  static ConversionProperties prop;
  prop.addOption("flatten arrays", true, "flatten arrays");
  prop.addOption("performValidation", true, 
    "perform validation before and after trying to flatten");
  return prop;
}


bool 
ArraysFlatteningConverter::matchesProperties
                        (const ConversionProperties &props) const
{
  if (!props.hasOption("flatten arrays"))
    return false;
  return true;
}

int 
ArraysFlatteningConverter::convert()
{  

  int result = performConversion();
  return result;

}

/** @cond doxygenLibsbmlInternal */
int 
ArraysFlatteningConverter::performConversion()
{  
  bool success = true;
  int result = LIBSBML_OPERATION_FAILED;

  // if no document or no model return
  if (mDocument == NULL || !(mDocument->isSetModel())) 
  {
    return LIBSBML_INVALID_OBJECT;
  }

  // check that we have a parameter because without one we have no dimensions
  if (mDocument->getModel()->getNumParameters() == 0)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  // go through the model and expand all variable type objects
  VariableFilter* filter = new VariableFilter();
  List * variables = mDocument->getAllElements(filter);
  for (ListIterator it = variables->begin(); it != variables->end(); ++it)
  {
    success = expandVariableElement((const SBase*)(*(it)));
    if (!success)
      break;
  }

  // go through the model and expand all math type objects
  ArraysMathFilter* m_filter = new ArraysMathFilter();
  List * mathchildren = mDocument->getAllElements(m_filter);
  if (mathchildren->getSize() > 0)
  {
    SBMLTransforms::getComponentValuesForModel(mDocument->getModel(), mValues);
  }
  for (ListIterator it = mathchildren->begin(); it != mathchildren->end(); ++it)
  {
    success = expandMathElement((const SBase*)(*(it)));
    if (!success)
      break;
  }

  // check we are done and remove arrays ns
  mDocument->disablePackage("http://www.sbml.org/sbml/level3/version1/arrays/version1", "arrays");
  delete filter;
  delete m_filter;

  if (success)
    return LIBSBML_OPERATION_SUCCESS;
  else
    return LIBSBML_OPERATION_FAILED;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
std::string
getNewId(std::vector<unsigned int> arrayEntry, const std::string& id)
{
  ostringstream oss;
  oss << id;
  for (std::vector<unsigned int>::iterator it = arrayEntry.begin(); 
                                           it != arrayEntry.end(); ++it)
  {
    oss << "_" << *it;
  }
  return oss.str();
}


void
ArraysFlatteningConverter::updateArrayEntry(unsigned int index)
{
  if (index == 0)
    return;
  unsigned int current = mArrayEntry.at(index - 1) + 1;
  unsigned int columnSize = mArraySize.at(index - 1);
  std::vector<unsigned int> returnArray;

  if (current < columnSize)
  {
    unsigned int final = mArraySize.size() - 1;
    unsigned int i = 0;
    for (i = 0; i < index - 1; i++)
    {
      returnArray.push_back(mArrayEntry.at(i));
    }
    returnArray.push_back(mArrayEntry.at(index - 1) + 1);

    for (i; i < final; i++)
      returnArray.push_back(0);

    mArrayEntry.swap(returnArray);
  }
  else
  {
    updateArrayEntry(index - 1);
  }
}


unsigned int
ArraysFlatteningConverter::getNumElements(const Dimension* dim)
{
  unsigned int num = 0;
  if (dim != NULL && dim->isSetSize())
  {
    Parameter* p = mDocument->getModel()->getParameter(dim->getSize());
    if (p != NULL && p->isSetValue())
    {
      num = (unsigned int)(p->getValue());
    }
  }

  return num;
}

bool
ArraysFlatteningConverter::adjustIdentifiers(SBase* newElement, 
                                             const std::string& attributeName)
{
  std::string id;
  newElement->getAttribute(attributeName, id);
  std::string metaid = newElement->getMetaId();
  int success = newElement->setAttribute(attributeName, getNewId(mArrayEntry, id));
  if (success == LIBSBML_OPERATION_SUCCESS && !metaid.empty())
  {
    success = newElement->setMetaId(getNewId(mArrayEntry, metaid));
  }

  if (success == LIBSBML_OPERATION_SUCCESS)
    return true;
  else
    return false;

}
bool
ArraysFlatteningConverter::expandVariableElement(const SBase* element)
{
  bool success = true;
  const ArraysSBasePlugin * plugin =
    static_cast<const ArraysSBasePlugin*>(element->getPlugin("arrays"));
  std::string elementName = element->getElementName();
  std::string id = element->getIdAttribute();

  // get number of elements that need to be created
  mArraySize.clear();
  mArraySize = plugin->getNumArrayElements();
  mNoDimensions = mArraySize.size();
  if (mArraySize.size() >= 1)
  {

      success = expandElement(element);
  }

  SBase *obj = mDocument->getModel()->removeChildObject(elementName, id);
  delete obj;
  

  return success;

}

bool
ArraysFlatteningConverter::expandElement(const SBase* element)
{
  std::string elementName = element->getElementName();
  std::string id = element->getIdAttribute();
  std::string metaid = element->getMetaId();

  mArrayEntry.clear();
  unsigned int numEntries = 1;
  for (unsigned int i = 0; i < mNoDimensions; i++)
  {
    mArrayEntry.push_back(0);
    numEntries *= mArraySize.at(i);
  }

  for (unsigned int i = 0; i < numEntries; i++)
  {
    SBase* newElement = element->clone();
    if (!adjustIdentifiers(newElement, "id"))
    {
      return false;
    }
    if (!mDocument->getModel()->addChildObject(elementName, newElement)
      == LIBSBML_OPERATION_SUCCESS)
    {
      return false;
    }
    updateArrayEntry(mNoDimensions);
  }

  return true;

}


bool
ArraysFlatteningConverter::expandMathElement(const SBase* element)
{
  bool success = true;
  const ArraysSBasePlugin * plugin =
    static_cast<const ArraysSBasePlugin*>(element->getPlugin("arrays"));

  std::string elementName = element->getElementName();
  std::string id = element->getIdAttribute();
  if (id.empty()) id = element->getId();

  // get number of elements that need to be created
  mArraySize.clear();
  mArraySize = plugin->getNumArrayElements();
  mNoDimensions = mArraySize.size();
  if (mArraySize.size() >= 1 || mArraySize.at(0) >= 1)
  {

    mArrayEntry.clear();
    unsigned int numEntries = 1;
    for (unsigned int i = 0; i < mNoDimensions; i++)
    {
      mArrayEntry.push_back(0);
      numEntries *= mArraySize.at(i);
      mDimensionIndex.append(plugin->getDimensionByArrayDimension(i)->getId());
    }

    unsigned int i = 0, j = 0;

    while (success && j < mNoDimensions)
    {
      success = expandDimension(element, j);
      j++;
    }
  }

  if (success)
  {
    SBase *obj = mDocument->getModel()->removeChildObject(elementName, id);
    if (obj != NULL)  delete obj;
  }

  return success;
}

bool
ArraysFlatteningConverter::adjustMath(SBase* newElement, unsigned int i, const Index* index, unsigned int arrayDim)
{
  bool adjusted = false;
  std::map<std::string, double> values;

  values.clear();
  values.insert(std::pair<const std::string, double>(mDimensionIndex.at(0), i));
  
  // if the dimension is used in the math of the element we just want to replace it
  // but if the math of the element is a vector we need to work it out
  
  ASTNode * math = const_cast<ASTNode*>(newElement->getMath());

  if (math->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR && math->getNumChildren() == 2)
  {
    ASTNode* child = math->getChild(0);
    if (child->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR)
    {
      const ASTArraysVectorFunctionNode *vector = dynamic_cast<const ASTArraysVectorFunctionNode *>(child->getPlugin("arrays")->getMath());
      unsigned int n = vector->getNumChildren();
      if (i < n)
      {
        ASTNode* value = (ASTNode*)(vector->getChild(i));
        double calc = SBMLTransforms::evaluateASTNode(value);
        ASTNode* newAST = new ASTNode(AST_REAL);
        newAST->setValue(calc);
        newElement->setMath(newAST);
      }
      adjusted = true;
    }
  }
  
  if (!adjusted && SBMLTransforms::nodeContainsId(math, mDimensionIndex))
  {
    double calc = SBMLTransforms::evaluateASTNode(index->getMath(), values);
    ASTNode * newAST = new ASTNode(AST_INTEGER);
    newAST->setValue((int)(calc));
    math->replaceArgument(mDimensionIndex.at(0), newAST);
    adjusted = true;
  }

  return adjusted;
}

bool
ArraysFlatteningConverter::expandDimension(const SBase* element, unsigned int arrayDim)
{
  std::string elementName = element->getElementName();
  const ArraysSBasePlugin * plugin =
    static_cast<const ArraysSBasePlugin*>(element->getPlugin("arrays"));
  const Index* index = plugin->getIndexByArrayDimension(arrayDim);
  std::string refAtt = index->getReferencedAttribute();

  unsigned int numEntries = mArraySize.at(arrayDim);

  for (unsigned int i = 0; i < numEntries; i++)
  {
    SBase* newElement = element->clone();
//    ASTNode * math = const_cast<ASTNode*>(newElement->getMath());
    if (!adjustMath(newElement, i, index))
    {
      return false;
    }
    if (!adjustIdentifiers(newElement, refAtt))
    {
      return false;
    }
    if (!mDocument->getModel()->addChildObject(elementName, newElement)
      == LIBSBML_OPERATION_SUCCESS)
    {
      return false;
    }
    updateArrayEntry(mNoDimensions);
  }

  return true;

}


/** @endcond */


/** @cond doxygenLibsbmlInternal */
int
ArraysFlatteningConverter::validateOriginalDocument()
{
  int result = LIBSBML_OPERATION_FAILED;

  return LIBSBML_OPERATION_SUCCESS;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
int
ArraysFlatteningConverter::validateFlatDocument(Model * flatmodel,
            unsigned int pkgVersion, unsigned int level, unsigned int version)
{
  int result = LIBSBML_OPERATION_FAILED;

  return LIBSBML_OPERATION_SUCCESS;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
bool
ArraysFlatteningConverter::getPerformValidation() const
{
  if (getProperties() == NULL)
  {
    return false;
  }
  else if (getProperties()->hasOption("performValidation") == false)
  {
    return true;
  }
  else
  {
    return getProperties()->getBoolValue("performValidation");
  }
}
/** @endcond */



/** @cond doxygenIgnored */
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


