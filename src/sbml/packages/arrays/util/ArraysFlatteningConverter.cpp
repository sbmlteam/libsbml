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
#include <sbml/SpeciesReference.h>

#ifdef __cplusplus

#include <algorithm>
#include <string>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */

SBase*
getParentObject(const SBase* element)
{
  SBase* parent = const_cast<SBase*>(element->getParentSBMLObject());
  if (parent->getTypeCode() == SBML_LIST_OF)
  {
    return parent->getParentSBMLObject();
  }
  else
  {
    return parent;
  }

}
/** @endcond */

/** @cond doxygenLibsbmlInternal */

VariableFilter::VariableFilter() : 
  mParentType(SBML_UNKNOWN) 
{ 
}


VariableFilter::VariableFilter(const SBase* parent) 
{ 
  mParentType = parent->getTypeCode(); 
}


VariableFilter::~VariableFilter() 
{
}

void 
VariableFilter::setParentType(const SBase* parent) 
{ 
  mParentType = parent->getTypeCode(); 
}


bool
VariableFilter::filter(const SBase* element)
{
  bool isVariable = false;

  // for the variable filter we want elements that have 
  // dimensions but no math
  if (element->getMath() != NULL)
  {
    return isVariable;
  }

  // we only want direct children
  if (mParentType != SBML_UNKNOWN)
  {
    if (getParentObject(element)->getTypeCode() != mParentType)
    {
      return isVariable;
    }
  }
  const ArraysSBasePlugin * plugin =
    static_cast<const ArraysSBasePlugin*>(element->getPlugin("arrays"));

  if (plugin != NULL)
  {
    if (plugin->getNumImpliedDimensions() > 0)
    {
      isVariable = true;
    }
  }

  return isVariable;
}


/** @endcond */

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

  populateValueMap();

  // go through the model and expand all variable type objects
  VariableFilter* filter = new VariableFilter(mDocument->getModel());
  List * variables = mDocument->getAllElements(filter);
  for (ListIterator it = variables->begin(); it != variables->end(); ++it)
  {
    const SBase* obj = (const SBase*)(*it);

    //cout << "Obj is " << obj->getElementName() << endl;
    success = expandVariableElement(obj);
    if (!success)
      break;
  }

  //SK could probably move these together
  // go through the model and expand all math type objects
  ArraysMathFilter* m_filter = new ArraysMathFilter();
  List * mathchildren = mDocument->getAllElements(m_filter);
  //if (mathchildren->getSize() > 0)
  //{
  //  populateValueMap();
  //}
  for (ListIterator it = mathchildren->begin(); it != mathchildren->end(); ++it)
  {
    const SBase* obj = (const SBase*)(*it);

 //   cout << "Obj is " << obj->getElementName() << endl;
    success = expandMathElement(obj);
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


// we use an array of integers to keep record of the index values
// for the arrays
// So an array with 0D size 2 and 1D size 3 will have
// a fixed array mArraySize = [3, 2]
// and an updating array
// mArrayEntry that will go from [0, 0] -> [0, 1] ->
// [1, 0] -> [1, 1] -> [2, 0] -> [2, 1]
// 
// slightly confusing as the indexing of the vectors does not correspond to arrayDimension

void
ArraysFlatteningConverter::updateArrayEntry(unsigned int index)
{
  if (index == 0)
    return;
 // viewArray(mArrayEntry);
  //viewArray(mArraySize);
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

  // SK if we have no dimensions we might have inherited some

  return num;
}

bool
ArraysFlatteningConverter::adjustMath(SBase* newElement, const Index* index)
{
  bool adjusted = false;
  addDimensionToModelValues();

  unsigned int count = mArrayEntry.at(0); 
  // SK this is used to index a vector but may not be teh correct value

  // if the dimension is used in the math of the element we just want to replace it
  // but if the math of the element is a vector we need to work it out

  ASTNode * math = const_cast<ASTNode*>(newElement->getMath());

  if (math != NULL && 
      math->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR && 
      math->getNumChildren() == 2)
  {
    // SK TODO check that the second child is the dimension id
    ASTNode* child = math->getChild(0);
    if (child->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR)
    {
      const ASTArraysVectorFunctionNode *vector = dynamic_cast<const ASTArraysVectorFunctionNode *>(child->getPlugin("arrays")->getMath());
      unsigned int n = vector->getNumChildren();
      if (count < n)
      {
        ASTNode* value = (ASTNode*)(vector->getChild(count));
        double calc = SBMLTransforms::evaluateASTNode(value, mValues);
        ASTNode* newAST = new ASTNode(AST_REAL);
        newAST->setValue(calc);
        newElement->setMath(newAST);
      }
      adjusted = true;
    }
    else if (child->getType() == AST_NAME)
    {
      std::string varName = child->getName();
      // SK what if index is null
      unsigned int calc = (unsigned int)(SBMLTransforms::evaluateASTNode(index->getMath(), mValues));
      std::vector<unsigned int> indexArray;
      indexArray.push_back(calc);
      ASTNode* newAST = new ASTNode(AST_NAME);
      newAST->setName(getNewId(indexArray, varName).c_str());;
      newElement->setMath(newAST);
      adjusted = true;
    }
  }

  if (!adjusted && SBMLTransforms::nodeContainsId(math, mDimensionIndex))
  {
    // SK TO DO expand for all dimensions
    // what if index is null
    double calc = SBMLTransforms::evaluateASTNode(index->getMath(), mValues);
    ASTNode * newAST = new ASTNode(AST_INTEGER);
    newAST->setValue((int)(calc));
    math->replaceArgument(mDimensionIndex.at(0), newAST);
    adjusted = true;
  }

  removeDimensionFromModelValues();

  return adjusted;
}

bool
ArraysFlatteningConverter::adjustReferencedAttribute(SBase* newElement, bool calcIndex)
{
  std::string refAtt = "";
  const ArraysSBasePlugin * plugin =
    static_cast<const ArraysSBasePlugin*>(newElement->getPlugin("arrays"));
  // SK current dimension is never updated; 
  // also may need to looking there being two reference attribs
  const Index* index = plugin->getIndexByArrayDimension(mCurrentDimension);
  if (index != NULL)
  {
    refAtt = index->getReferencedAttribute();
  }
  std::string id;
  newElement->getAttribute(refAtt, id);
  std::vector<unsigned int> arrayIndex;
  if (plugin != NULL)
  {
    for (int j = mNoDimensions - 1; j >= 0; j--)
    {
      unsigned int value = evaluateIndex(plugin->getIndexByArrayDimension(j));
      arrayIndex.push_back(value);
    }

  }
  int success = LIBSBML_OPERATION_SUCCESS;
  if (!refAtt.empty())
  {
    // for some things we need to use the index to calculate the new array
    // however if the attribute being changed relates to an element with math
    // this will be left to the resulting flat model and the referenced attribute
    // should follow the automatic indexing
    if (calcIndex)
      success = newElement->setAttribute(refAtt, getNewId(arrayIndex, id));
    else
      success = newElement->setAttribute(refAtt, getNewId(mArrayEntry, id));

  }

  if (success == LIBSBML_OPERATION_SUCCESS)
    return true;
  else
    return false;
}
// this takes the existing id and metaid and appends the values from the array entry
// SK streamline this for id/metaid and other sidrefs
bool
ArraysFlatteningConverter::adjustIdentifiers(SBase* newElement)
{
  std::string id;
  newElement->getAttribute("id", id);
  std::string metaid = newElement->getMetaId();
  int success = LIBSBML_OPERATION_SUCCESS;
  if (!id.empty())
  {
    success = newElement->setAttribute("id", getNewId(mArrayEntry, id));
  }
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

  if (getArraySize(element) && mArraySize.size() >= 1)
  {
    mArrayEntry.clear();
    mDimensionIndex.clear();
    mCurrentDimension = 0;
    unsigned int numEntries = 1;
    for (unsigned int i = 0; i < mNoDimensions; i++)
    {
      mArrayEntry.push_back(0);

      mDimensionIndex.append(plugin->getDimensionByArrayDimension(i)->getId());
      numEntries *= mArraySize.at(i);
    }

    unsigned int j = 0;
    while (success && j < numEntries)
    {
      success = expandVariable(element);
      j++;
    }
  }


  if (success)
  {
    SBase* parent = getParentObject(element);
    if (elementName == "speciesReference")
    {
      const ListOfSpeciesReferences *losr = static_cast<const ListOfSpeciesReferences*>(element->getParentSBMLObject());
      if (losr != NULL)
      {
        switch (losr->getType())
        {
        case 1:
          elementName = "reactant";
          break;
        }
      }
    }
    if (parent != NULL)
    {
      SBase *obj = parent->removeChildObject(elementName, id);
      if (obj != NULL)  delete obj;
    }
  }

  return success;

}

bool
ArraysFlatteningConverter::expandNonDimensionedVariable(SBase* element)
{
  if (element->getPackageName() == "arrays")
  {
    return true;
  }
//  cout << "processing " << element->getElementName() << endl;
  std::string refAtt = "";
  const ArraysSBasePlugin * plugin =
    static_cast<const ArraysSBasePlugin*>(element->getPlugin("arrays"));
  const Index* index = NULL;
  // SK current dimension is never updated; 
  // also may need to looking there being two reference attribs
  if (plugin != NULL)
  {
    if (plugin->getNumIndices() > 0)
    {
      index = plugin->getIndexByArrayDimension(mCurrentDimension);
      if (index != NULL)
      {
        refAtt = index->getReferencedAttribute();
      }
    }
  }

  if (!adjustIdentifiers(element))
  {
    return false;
  }
  if (!refAtt.empty() && !adjustReferencedAttribute(element))
  {
    return false;
  }

  return true;
}




bool
ArraysFlatteningConverter::expandVariable(const SBase* element)
{
  std::string elementName = element->getElementName();
  std::string refAtt = "";
  const ArraysSBasePlugin * plugin =
    static_cast<const ArraysSBasePlugin*>(element->getPlugin("arrays"));
  // SK current dimension is never updated; 
  // also may need to looking there being two reference attribs
  const Index* index = plugin->getIndexByArrayDimension(mCurrentDimension);
  if (index != NULL)
  {
    refAtt = index->getReferencedAttribute();
  }

  SBase* newElement = element->clone();
  if (!adjustIdentifiers(newElement))
  {
    return false;
  }
  if (!refAtt.empty() && !adjustReferencedAttribute(newElement))
  {
    return false;
  }
  SBase* parent = getParentObject(element);

  if (!dealWithChildObjects(parent, newElement))
  {
    return false;
  }
  //if (elementName == "reaction")
  //{
  //  if (!dealWithReaction((Reaction*)(newElement)))
  //    return false;
  //}
  //else if (elementName == "event")
  //{
  //  if (!dealWithEvent((Event*)(newElement)))
  //    return false;

  //}
  // SK nested elements where model is not parent

  // if the parent is a reaction we need to know what sort of sr we are adding
  if (elementName == "speciesReference")
  {
    const ListOfSpeciesReferences *losr = static_cast<const ListOfSpeciesReferences*>(element->getParentSBMLObject());
    if (losr != NULL)
    {
      switch (losr->getType())
      {
      case 1:
        elementName = "reactant";
        break;
      }
    }
  }
  if (parent == NULL || parent->addChildObject(elementName, newElement)
    != LIBSBML_OPERATION_SUCCESS)
  {
    return false;
  }
  updateArrayEntry(mNoDimensions);

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

  // SK here we might have a case were there are no dimensions on the element but it 
  // inherits dimensions (see event example)
  mArraySize = plugin->getNumArrayElements();
  mNoDimensions = mArraySize.size();
  if (mArraySize.size() >= 1 || mArraySize.at(0) >= 1)
  {

    mDimensionIndex.clear();
    mArrayEntry.clear();
    mCurrentDimension = 0;
    unsigned int numEntries = 1;
    for (unsigned int i = 0; i < mNoDimensions; i++)
    {
      mArrayEntry.push_back(0);
      numEntries *= mArraySize.at(i);
      mDimensionIndex.append(plugin->getDimensionByArrayDimension(i)->getId());
    }

    unsigned int i = 0, j = 0;

    while (success && j < numEntries)
    {
      success = expandMath(element);
      j++;
    }
  }

  if (success)
  {
    SBase* parent = getParentObject(element);
    if (parent != NULL)
    {
      SBase *obj = parent->removeChildObject(elementName, id);
      if (obj != NULL)  delete obj;
    }
  }

  return success;
}

bool
ArraysFlatteningConverter::expandMath(const SBase* element)
{
  std::string elementName = element->getElementName();
  const ArraysSBasePlugin * plugin =
    static_cast<const ArraysSBasePlugin*>(element->getPlugin("arrays"));
  std::string refAtt = "";
  // SK current dimension is never updated
  const Index* index = plugin->getIndexByArrayDimension(mCurrentDimension);
  if (index != NULL)
  {
    refAtt = index->getReferencedAttribute();
  }

  SBase* newElement = element->clone();

  if (!adjustMath(newElement, index))
  {
    return false;
  }
  if (!adjustIdentifiers(newElement))
  {
    return false;
  }

  if (!adjustReferencedAttribute(newElement, false))
  {
    return false;
  }

  SBase* parent = getParentObject(element);
  if (parent == NULL || parent->addChildObject(elementName, newElement)
    != LIBSBML_OPERATION_SUCCESS)
  {
    return false;
  }
  updateArrayEntry(mNoDimensions);

  return true;

}

bool
ArraysFlatteningConverter::dealWithChildObjects(SBase* parent, SBase* element)
{
  bool success = true;
//  VariableFilter* filter = new VariableFilter(element);
  List * variables = element->getAllElements();
  for (ListIterator it = variables->begin(); it != variables->end(); ++it)
  {
    SBase* obj = (SBase*)(*it);

//    cout << "Obj is " << obj->getElementName() << endl;
    
    success = expandNonDimensionedVariable(obj);
//    success = expandVariableElement(obj); 
    if (!success)
      break;
    if (obj->isSetMath())
      success = dealWithMathChild(obj);
    if (!success)
      break;

  }
  return success;
}



bool
ArraysFlatteningConverter::dealWithEvent(Event* event)
{
  bool success = true;
  unsigned int i = 0;
  while (success && i < event->getNumEventAssignments())
  {
    success = dealWithEventAssignment(event->getEventAssignment(i));
    i++;
  }
  if (event->isSetTrigger())
  {
    success = dealWithMathChild(event->getTrigger());
  }
  if (event->isSetDelay())
  {
    success = dealWithMathChild(event->getDelay());
  }
  if (event->isSetPriority())
  {
    success = dealWithMathChild(event->getPriority());
  }
  return success;
}

bool
ArraysFlatteningConverter::dealWithEventAssignment(EventAssignment* ea)
{
  std::vector<unsigned int> arrayIndex;
  const ArraysSBasePlugin* plugin = static_cast<const ArraysSBasePlugin*>(ea->getPlugin("arrays"));
  if (plugin != NULL)
  {
    // assume no dimensions for now

    for (unsigned int j = 0; j < plugin->getNumIndices(); j++)
    {
      unsigned int value = evaluateIndex(plugin->getIndex(j));
      arrayIndex.push_back(value);
      adjustMath(ea, plugin->getIndex(j));
    }
    ea->setVariable(getNewId(arrayIndex, ea->getVariable()));


  }
  return true;
}



unsigned int
ArraysFlatteningConverter::evaluateIndex(const Index* index)
{
  unsigned int value = 0;
  addDimensionToModelValues();

  value = (unsigned int)(SBMLTransforms::evaluateASTNode(index->getMath(), mValues));

  removeDimensionFromModelValues();

  return value;
}

bool
ArraysFlatteningConverter::dealWithSpeciesReference(SimpleSpeciesReference* sr)
{
  std::vector<unsigned int> arrayIndex;
  const ArraysSBasePlugin* plugin = static_cast<const ArraysSBasePlugin*>(sr->getPlugin("arrays"));
  if (plugin != NULL)
  {
    // assume no dimensions for now
    
    for (unsigned int j = 0; j < plugin->getNumIndices(); j++)
    {
      unsigned int value = evaluateIndex(plugin->getIndex(j));
      arrayIndex.push_back(value);
    }
    if (!adjustIdentifiers(sr))
    {
      return false;
    }
    sr->setSpecies(getNewId(arrayIndex, sr->getSpecies()));
  }
  return true;
}

bool
ArraysFlatteningConverter::dealWithMathChild(SBase* element)
{
  bool success = true;
  if (element->isSetMath() && SBMLTransforms::nodeContainsId(element->getMath(), mDimensionIndex))
  {
    addDimensionToModelValues();
    ASTNode* math = (ASTNode*)(element->getMath());
    success = replaceSelector(math);

    removeDimensionFromModelValues();
  }
  return success;
}



bool
ArraysFlatteningConverter::dealWithKineticLaw(KineticLaw* kl)
{
  bool success = true;
  if (kl->isSetMath() && SBMLTransforms::nodeContainsId(kl->getMath(), mDimensionIndex))
  {
    addDimensionToModelValues();
    ASTNode* math = (ASTNode*)(kl->getMath());
    success = replaceSelector(math);

    removeDimensionFromModelValues();
  }
  return success;
}


bool
ArraysFlatteningConverter::replaceSelector(ASTNode* math)
{
  bool success = true;

  if (math->getType() == AST_ORIGINATES_IN_PACKAGE && math->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR)
  {
    // sort this case
  }
  
  for (unsigned int i = 0; i < math->getNumChildren(); i++)
  {
    ASTNode *child = math->getChild(i);
    ASTNode * newAST = NULL;
    if (child->getType() == AST_ORIGINATES_IN_PACKAGE && child->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR)
    {
      // assume we have selector A d for now
      std::string var_name = child->getChild(0)->getName();
      unsigned int var_index = (unsigned int)(SBMLTransforms::evaluateASTNode(child->getChild(1), mValues));
      newAST = new ASTNode(AST_NAME);
      ostringstream out;
      out << var_name << "_" << var_index;
      newAST->setName(out.str().c_str());
    }
    if (newAST != NULL)
    {
      if (math->replaceChild(i, newAST) != LIBSBML_OPERATION_SUCCESS)
        success = false;
    }
    else
    {
      success = replaceSelector(child);
    }
  }


  return success;
}



bool
ArraysFlatteningConverter::dealWithReaction(Reaction* reaction)
{
  bool success = true;
  unsigned int i = 0;
  while (success && i < reaction->getNumReactants())
  {
    success =  dealWithSpeciesReference(reaction->getReactant(i));
    i++;
  }
  i = 0;
  while (success && i < reaction->getNumProducts())
  {
    success = dealWithSpeciesReference(reaction->getProduct(i));
    i++;
  }
  i = 0;
  while (success && i < reaction->getNumModifiers())
  {
    success = dealWithSpeciesReference(reaction->getModifier(i));
    i++;
  }
  if (reaction->isSetKineticLaw())
  {
    success = dealWithMathChild(reaction->getKineticLaw());
  }
  return success;
}

bool
ArraysFlatteningConverter::getArraySize(const SBase* element)
{
  const ArraysSBasePlugin * plugin =
    static_cast<const ArraysSBasePlugin*>(element->getPlugin("arrays"));

  mArraySize.clear();
  mArraySize = plugin->getNumArrayElements();
  mNoDimensions = mArraySize.size();
  
  // if element is a child clone may not access the parent model
  if (mNoDimensions == 0)
  {
    for (unsigned int i = plugin->getNumDimensions(); i > 0; i--)
    {
      unsigned int thisDim = 0;
      const Dimension* dim = plugin->getDimensionByArrayDimension(i-1);
      if (dim->isSetSize())
      {
        std::string p = dim->getSize();
        if (mValues.find(p) != mValues.end())
        {
          thisDim = (unsigned int)((mValues.find(p)->second).first);
        }
        else
        {
          return false;
        }
      }
      else
      {
        return false;
      }
      mArraySize.push_back(thisDim);
    }
  }

  mNoDimensions = mArraySize.size();

  if (mNoDimensions == 0)
  { 
    return false;
  }

  return true;
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */

void
ArraysFlatteningConverter::addDimensionToModelValues()
{
  if (!isPopulatedValueMap())
  {
    if (!populateValueMap())
      cout << "PROBLEM!";
  }

  // we need to add the current value for the dimension id to the global model
  // values that might be needed in calculating 
  for (unsigned int i = 0; i < mNoDimensions; i++)
  {
    unsigned int value = mArrayEntry.at(mNoDimensions - 1 - i);
    SBMLTransforms::ValueSet v = make_pair(value, true);
    mValues.insert(pair<const std::string, SBMLTransforms::ValueSet>(mDimensionIndex.at(i), v));
  }
}

void
ArraysFlatteningConverter::removeDimensionFromModelValues()
{
  // remove the dimension id values as these are changing for each instance
  for (unsigned int i = 0; i < mNoDimensions; i++)
  {
    SBMLTransforms::IdValueIter it = mValues.find(mDimensionIndex.at(i));
    mValues.erase(it);
  }
}

bool
ArraysFlatteningConverter::isPopulatedValueMap()
{
  return (getValueMap().size() != 0);
}

SBMLTransforms::IdValueMap 
ArraysFlatteningConverter::getValueMap()
{
  return mValues;
}

void 
ArraysFlatteningConverter::clearValueMap()
{
  mValues.clear();
}

bool 
ArraysFlatteningConverter::populateValueMap()
{
  clearValueMap();
  SBMLTransforms::getComponentValuesForModel(mDocument->getModel(), mValues);
  mValuesSize = mValues.size();
  
  return isPopulatedValueMap();
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


