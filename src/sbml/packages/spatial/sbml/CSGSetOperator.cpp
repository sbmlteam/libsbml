/**
 * @file CSGSetOperator.cpp
 * @brief Implementation of the CSGSetOperator class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/packages/spatial/sbml/CSGSetOperator.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>
#include <sbml/packages/spatial/sbml/CSGSetOperator.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new CSGSetOperator using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
CSGSetOperator::CSGSetOperator(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion)
  : CSGNode(level, version, pkgVersion)
  , mOperationType (SPATIAL_SETOPERATION_INVALID)
  , mComplementA ("")
  , mComplementB ("")
  , mCSGNodes (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new CSGSetOperator using the given SpatialPkgNamespaces object.
 */
CSGSetOperator::CSGSetOperator(SpatialPkgNamespaces *spatialns)
  : CSGNode(spatialns)
  , mOperationType (SPATIAL_SETOPERATION_INVALID)
  , mComplementA ("")
  , mComplementB ("")
  , mCSGNodes (spatialns)
{
  setElementNamespace(spatialns->getURI());
  connectToChild();
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGSetOperator.
 */
CSGSetOperator::CSGSetOperator(const CSGSetOperator& orig)
  : CSGNode( orig )
  , mOperationType ( orig.mOperationType )
  , mComplementA ( orig.mComplementA )
  , mComplementB ( orig.mComplementB )
  , mCSGNodes ( orig.mCSGNodes )
{
  connectToChild();
}


/*
 * Assignment operator for CSGSetOperator.
 */
CSGSetOperator&
CSGSetOperator::operator=(const CSGSetOperator& rhs)
{
  if (&rhs != this)
  {
    CSGNode::operator=(rhs);
    mOperationType = rhs.mOperationType;
    mComplementA = rhs.mComplementA;
    mComplementB = rhs.mComplementB;
    mCSGNodes = rhs.mCSGNodes;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this CSGSetOperator object.
 */
CSGSetOperator*
CSGSetOperator::clone() const
{
  return new CSGSetOperator(*this);
}


/*
 * Destructor for CSGSetOperator.
 */
CSGSetOperator::~CSGSetOperator()
{
}


/*
 * Returns the value of the "operationType" attribute of this CSGSetOperator.
 */
SetOperation_t
CSGSetOperator::getOperationType() const
{
  return mOperationType;
}


/*
 * Returns the value of the "operationType" attribute of this CSGSetOperator.
 */
std::string
CSGSetOperator::getOperationTypeAsString() const
{
  return SetOperation_toString(mOperationType);
}


/*
 * Returns the value of the "complementA" attribute of this CSGSetOperator.
 */
const std::string&
CSGSetOperator::getComplementA() const
{
  return mComplementA;
}


/*
 * Returns the value of the "complementB" attribute of this CSGSetOperator.
 */
const std::string&
CSGSetOperator::getComplementB() const
{
  return mComplementB;
}


/*
 * Predicate returning @c true if this CSGSetOperator's "operationType"
 * attribute is set.
 */
bool
CSGSetOperator::isSetOperationType() const
{
  return (mOperationType != SPATIAL_SETOPERATION_INVALID);
}


/*
 * Predicate returning @c true if this CSGSetOperator's "complementA" attribute
 * is set.
 */
bool
CSGSetOperator::isSetComplementA() const
{
  return (mComplementA.empty() == false);
}


/*
 * Predicate returning @c true if this CSGSetOperator's "complementB" attribute
 * is set.
 */
bool
CSGSetOperator::isSetComplementB() const
{
  return (mComplementB.empty() == false);
}


/*
 * Sets the value of the "operationType" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::setOperationType(const SetOperation_t operationType)
{
  if (SetOperation_isValid(operationType) == 0)
  {
    mOperationType = SPATIAL_SETOPERATION_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mOperationType = operationType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "operationType" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::setOperationType(const std::string& operationType)
{
  mOperationType = SetOperation_fromString(operationType.c_str());

  if (mOperationType == SPATIAL_SETOPERATION_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "complementA" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::setComplementA(const std::string& complementA)
{
  if (!(SyntaxChecker::isValidInternalSId(complementA)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mComplementA = complementA;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "complementB" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::setComplementB(const std::string& complementB)
{
  if (!(SyntaxChecker::isValidInternalSId(complementB)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mComplementB = complementB;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "operationType" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::unsetOperationType()
{
  mOperationType = SPATIAL_SETOPERATION_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "complementA" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::unsetComplementA()
{
  mComplementA.erase();

  if (mComplementA.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "complementB" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::unsetComplementB()
{
  mComplementB.erase();

  if (mComplementB.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the ListOfCSGNodes from this CSGSetOperator.
 */
const ListOfCSGNodes*
CSGSetOperator::getListOfCSGNodes() const
{
  return &mCSGNodes;
}


/*
 * Returns the ListOfCSGNodes from this CSGSetOperator.
 */
ListOfCSGNodes*
CSGSetOperator::getListOfCSGNodes()
{
  return &mCSGNodes;
}


/*
 * Get a CSGNode from the CSGSetOperator.
 */
CSGNode*
CSGSetOperator::getCSGNode(unsigned int n)
{
  return mCSGNodes.get(n);
}


/*
 * Get a CSGNode from the CSGSetOperator.
 */
const CSGNode*
CSGSetOperator::getCSGNode(unsigned int n) const
{
  return mCSGNodes.get(n);
}


/*
 * Get a CSGNode from the CSGSetOperator based on its identifier.
 */
CSGNode*
CSGSetOperator::getCSGNode(const std::string& sid)
{
  return mCSGNodes.get(sid);
}


/*
 * Get a CSGNode from the CSGSetOperator based on its identifier.
 */
const CSGNode*
CSGSetOperator::getCSGNode(const std::string& sid) const
{
  return mCSGNodes.get(sid);
}


/*
 * Adds a copy of the given CSGNode to this CSGSetOperator.
 */
int
CSGSetOperator::addCSGNode(const CSGNode* csgn)
{
  if (csgn == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (csgn->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != csgn->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != csgn->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(csgn)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (csgn->isSetId() && (mCSGNodes.get(csgn->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mCSGNodes.append(csgn);
  }
}


/*
 * Get the number of CSGNode objects in this CSGSetOperator.
 */
unsigned int
CSGSetOperator::getNumCSGNodes() const
{
  return mCSGNodes.size();
}


/*
 * Creates a new CSGPrimitive object, adds it to this CSGSetOperator object and
 * returns the CSGPrimitive object created.
 */
CSGPrimitive*
CSGSetOperator::createCSGPrimitive()
{
  CSGPrimitive* csgp = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgp = new CSGPrimitive(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (csgp != NULL)
  {
    mCSGNodes.appendAndOwn(csgp);
  }

  return csgp;
}


/*
 * Creates a new CSGTranslation object, adds it to this CSGSetOperator object
 * and returns the CSGTranslation object created.
 */
CSGTranslation*
CSGSetOperator::createCSGTranslation()
{
  CSGTranslation* csgt = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgt = new CSGTranslation(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (csgt != NULL)
  {
    mCSGNodes.appendAndOwn(csgt);
  }

  return csgt;
}


/*
 * Creates a new CSGRotation object, adds it to this CSGSetOperator object and
 * returns the CSGRotation object created.
 */
CSGRotation*
CSGSetOperator::createCSGRotation()
{
  CSGRotation* csgr = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgr = new CSGRotation(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (csgr != NULL)
  {
    mCSGNodes.appendAndOwn(csgr);
  }

  return csgr;
}


/*
 * Creates a new CSGScale object, adds it to this CSGSetOperator object and
 * returns the CSGScale object created.
 */
CSGScale*
CSGSetOperator::createCSGScale()
{
  CSGScale* csgs = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgs = new CSGScale(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (csgs != NULL)
  {
    mCSGNodes.appendAndOwn(csgs);
  }

  return csgs;
}


/*
 * Creates a new CSGHomogeneousTransformation object, adds it to this
 * CSGSetOperator object and returns the CSGHomogeneousTransformation object
 * created.
 */
CSGHomogeneousTransformation*
CSGSetOperator::createCSGHomogeneousTransformation()
{
  CSGHomogeneousTransformation* csght = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csght = new CSGHomogeneousTransformation(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (csght != NULL)
  {
    mCSGNodes.appendAndOwn(csght);
  }

  return csght;
}


/*
 * Creates a new CSGSetOperator object, adds it to this CSGSetOperator object
 * and returns the CSGSetOperator object created.
 */
CSGSetOperator*
CSGSetOperator::createCSGSetOperator()
{
  CSGSetOperator* csgso = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgso = new CSGSetOperator(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (csgso != NULL)
  {
    mCSGNodes.appendAndOwn(csgso);
  }

  return csgso;
}


/*
 * Removes the nth CSGNode from this CSGSetOperator and returns a pointer to
 * it.
 */
CSGNode*
CSGSetOperator::removeCSGNode(unsigned int n)
{
  return mCSGNodes.remove(n);
}


/*
 * Removes the CSGNode from this CSGSetOperator based on its identifier and
 * returns a pointer to it.
 */
CSGNode*
CSGSetOperator::removeCSGNode(const std::string& sid)
{
  return mCSGNodes.remove(sid);
}


/*
 * @copydoc doc_renamesidref_common
 */
void
CSGSetOperator::renameSIdRefs(const std::string& oldid,
                              const std::string& newid)
{
  if (isSetComplementA() && mComplementA == oldid)
  {
    setComplementA(newid);
  }

  if (isSetComplementB() && mComplementB == oldid)
  {
    setComplementB(newid);
  }
}


/*
 * Returns the XML element name of this CSGSetOperator object.
 */
const std::string&
CSGSetOperator::getElementName() const
{
  static const string name = "csgSetOperator";
  return name;
}


/*
 * Returns the libSBML type code for this CSGSetOperator object.
 */
int
CSGSetOperator::getTypeCode() const
{
  return SBML_SPATIAL_CSGSETOPERATOR;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * CSGSetOperator object have been set.
 */
bool
CSGSetOperator::hasRequiredAttributes() const
{
  bool allPresent = CSGNode::hasRequiredAttributes();

  if (isSetOperationType() == false)
  {
    allPresent = false;
  }

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * CSGSetOperator object have been set.
 */
bool
CSGSetOperator::hasRequiredElements() const
{
  bool allPresent = CSGNode::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
CSGSetOperator::writeElements(XMLOutputStream& stream) const
{
  CSGNode::writeElements(stream);

  if (getNumCSGNodes() > 0)
  {
    mCSGNodes.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
CSGSetOperator::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mCSGNodes.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
CSGSetOperator::setSBMLDocument(SBMLDocument* d)
{
  CSGNode::setSBMLDocument(d);

  mCSGNodes.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
CSGSetOperator::connectToChild()
{
  CSGNode::connectToChild();

  mCSGNodes.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
CSGSetOperator::enablePackageInternal(const std::string& pkgURI,
                                      const std::string& pkgPrefix,
                                      bool flag)
{
  CSGNode::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mCSGNodes.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
CSGSetOperator::updateSBMLNamespace(const std::string& package,
                                    unsigned int level,
                                    unsigned int version)
{
  CSGNode::updateSBMLNamespace(package, level, version);

  mCSGNodes.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::getAttribute(const std::string& attributeName,
                             bool& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::getAttribute(const std::string& attributeName,
                             int& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::getAttribute(const std::string& attributeName,
                             double& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::getAttribute(const std::string& attributeName,
                             unsigned int& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::getAttribute(const std::string& attributeName,
                             std::string& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "operationType")
  {
    value = getOperationTypeAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "complementA")
  {
    value = getComplementA();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "complementB")
  {
    value = getComplementB();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this CSGSetOperator's attribute
 * "attributeName" is set.
 */
bool
CSGSetOperator::isSetAttribute(const std::string& attributeName) const
{
  bool value = CSGNode::isSetAttribute(attributeName);

  if (attributeName == "operationType")
  {
    value = isSetOperationType();
  }
  else if (attributeName == "complementA")
  {
    value = isSetComplementA();
  }
  else if (attributeName == "complementB")
  {
    value = isSetComplementB();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::setAttribute(const std::string& attributeName, int value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::setAttribute(const std::string& attributeName, double value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::setAttribute(const std::string& attributeName,
                             unsigned int value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::setAttribute(const std::string& attributeName,
                             const std::string& value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  if (attributeName == "operationType")
  {
    return_value = setOperationType(value);
  }
  else if (attributeName == "complementA")
  {
    return_value = setComplementA(value);
  }
  else if (attributeName == "complementB")
  {
    return_value = setComplementB(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this CSGSetOperator.
 */
int
CSGSetOperator::unsetAttribute(const std::string& attributeName)
{
  int value = CSGNode::unsetAttribute(attributeName);

  if (attributeName == "operationType")
  {
    value = unsetOperationType();
  }
  else if (attributeName == "complementA")
  {
    value = unsetComplementA();
  }
  else if (attributeName == "complementB")
  {
    value = unsetComplementB();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this CSGSetOperator.
 */
SBase*
CSGSetOperator::createChildObject(const std::string& elementName)
{
  CSGNode* obj = NULL;

  if (elementName == "csgPrimitive")
  {
    return createCSGPrimitive();
  }
  else if (elementName == "csgTranslation")
  {
    return createCSGTranslation();
  }
  else if (elementName == "csgRotation")
  {
    return createCSGRotation();
  }
  else if (elementName == "csgScale")
  {
    return createCSGScale();
  }
  else if (elementName == "csgHomogeneousTransformation")
  {
    return createCSGHomogeneousTransformation();
  }
  else if (elementName == "csgSetOperator")
  {
    return createCSGSetOperator();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this CSGSetOperator.
 */
int
CSGSetOperator::addChildObject(const std::string& elementName,
                               const SBase* element)
{
  if (elementName == "csgPrimitive" && element->getTypeCode() ==
    SBML_SPATIAL_CSGPRIMITIVE)
  {
    return addCSGNode((const CSGNode*)(element));
  }
  else if (elementName == "csgTranslation" && element->getTypeCode() ==
    SBML_SPATIAL_CSGTRANSLATION)
  {
    return addCSGNode((const CSGNode*)(element));
  }
  else if (elementName == "csgRotation" && element->getTypeCode() ==
    SBML_SPATIAL_CSGROTATION)
  {
    return addCSGNode((const CSGNode*)(element));
  }
  else if (elementName == "csgScale" && element->getTypeCode() ==
    SBML_SPATIAL_CSGSCALE)
  {
    return addCSGNode((const CSGNode*)(element));
  }
  else if (elementName == "csgHomogeneousTransformation" &&
    element->getTypeCode() == SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION)
  {
    return addCSGNode((const CSGNode*)(element));
  }
  else if (elementName == "csgSetOperator" && element->getTypeCode() ==
    SBML_SPATIAL_CSGSETOPERATOR)
  {
    return addCSGNode((const CSGNode*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * CSGSetOperator.
 */
SBase*
CSGSetOperator::removeChildObject(const std::string& elementName,
                                  const std::string& id)
{
  if (elementName == "csgPrimitive")
  {
    return removeCSGNode(id);
  }
  else if (elementName == "csgTranslation")
  {
    return removeCSGNode(id);
  }
  else if (elementName == "csgRotation")
  {
    return removeCSGNode(id);
  }
  else if (elementName == "csgScale")
  {
    return removeCSGNode(id);
  }
  else if (elementName == "csgHomogeneousTransformation")
  {
    return removeCSGNode(id);
  }
  else if (elementName == "csgSetOperator")
  {
    return removeCSGNode(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this CSGSetOperator.
 */
unsigned int
CSGSetOperator::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "csgNode")
  {
    return getNumCSGNodes();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this CSGSetOperator.
 */
SBase*
CSGSetOperator::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "csgNode")
  {
    return getCSGNode(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
CSGSetOperator::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mCSGNodes.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
CSGSetOperator::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mCSGNodes.getMetaId() == metaid)
  {
    return &mCSGNodes;
  }

  obj = mCSGNodes.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
CSGSetOperator::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mCSGNodes, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
CSGSetOperator::createObject(XMLInputStream& stream)
{
  SBase* obj = CSGNode::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfCSGNodes")
  {
    if (mCSGNodes.size() != 0)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialCSGSetOperatorAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), "", getLine(), getColumn());
    }

    obj = &mCSGNodes;
  }

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
CSGSetOperator::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGNode::addExpectedAttributes(attributes);

  attributes.add("operationType");

  attributes.add("complementA");

  attributes.add("complementB");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CSGSetOperator::readAttributes(const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  CSGNode::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialCSGSetOperatorAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialCSGSetOperatorAllowedCoreAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
    }
  }

  // 
  // operationType enum (use = "required" )
  // 

  std::string operationType;
  assigned = attributes.readInto("operationType", operationType);

  if (assigned == true)
  {
    if (operationType.empty() == true)
    {
      logEmptyString(operationType, level, version, "<csgSetOperator>");
    }
    else
    {
      mOperationType = SetOperation_fromString(operationType.c_str());

      if (SetOperation_isValid(mOperationType) == 0)
      {
        std::string msg = "The operationType on the <csgSetOperator> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + operationType + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialCSGSetOperatorOperationTypeMustBeSetOperationEnum, pkgVersion,
            level, version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
    std::string message = "Spatial attribute 'operationType' is missing.";
    log->logPackageError("spatial", SpatialCSGSetOperatorAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }

  // 
  // complementA SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("complementA", mComplementA);

  if (assigned == true)
  {
    if (mComplementA.empty() == true)
    {
      logEmptyString(mComplementA, level, version, "<csgSetOperator>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mComplementA) == false)
    {
      std::string msg = "The complementA attribute on the <" + getElementName()
        + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mComplementA + "', which does not conform to the "
        "syntax.";
      log->logPackageError("spatial",
        SpatialCSGSetOperatorComplementsMustReferenceChildren, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }

  // 
  // complementB SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("complementB", mComplementB);

  if (assigned == true)
  {
    if (mComplementB.empty() == true)
    {
      logEmptyString(mComplementB, level, version, "<csgSetOperator>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mComplementB) == false)
    {
      std::string msg = "The complementB attribute on the <" + getElementName()
        + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mComplementB + "', which does not conform to the "
        "syntax.";
      log->logPackageError("spatial",
        SpatialCSGSetOperatorComplementsMustReferenceChildren, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
CSGSetOperator::writeAttributes(XMLOutputStream& stream) const
{
  CSGNode::writeAttributes(stream);

  if (isSetOperationType() == true)
  {
    stream.writeAttribute("operationType", getPrefix(),
      SetOperation_toString(mOperationType));
  }

  if (isSetComplementA() == true)
  {
    stream.writeAttribute("complementA", getPrefix(), mComplementA);
  }

  if (isSetComplementB() == true)
  {
    stream.writeAttribute("complementB", getPrefix(), mComplementB);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new CSGSetOperator_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGSetOperator_t *
CSGSetOperator_create(unsigned int level,
                      unsigned int version,
                      unsigned int pkgVersion)
{
  return new CSGSetOperator(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CSGSetOperator_t object.
 */
LIBSBML_EXTERN
CSGSetOperator_t*
CSGSetOperator_clone(const CSGSetOperator_t* csgso)
{
  if (csgso != NULL)
  {
    return static_cast<CSGSetOperator_t*>(csgso->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this CSGSetOperator_t object.
 */
LIBSBML_EXTERN
void
CSGSetOperator_free(CSGSetOperator_t* csgso)
{
  if (csgso != NULL)
  {
    delete csgso;
  }
}


/*
 * Returns the value of the "operationType" attribute of this CSGSetOperator_t.
 */
LIBSBML_EXTERN
SetOperation_t
CSGSetOperator_getOperationType(const CSGSetOperator_t * csgso)
{
  if (csgso == NULL)
  {
    return SPATIAL_SETOPERATION_INVALID;
  }

  return csgso->getOperationType();
}


/*
 * Returns the value of the "operationType" attribute of this CSGSetOperator_t.
 */
LIBSBML_EXTERN
char *
CSGSetOperator_getOperationTypeAsString(const CSGSetOperator_t * csgso)
{
  return (char*)(SetOperation_toString(csgso->getOperationType()));
}


/*
 * Returns the value of the "complementA" attribute of this CSGSetOperator_t.
 */
LIBSBML_EXTERN
char *
CSGSetOperator_getComplementA(const CSGSetOperator_t * csgso)
{
  if (csgso == NULL)
  {
    return NULL;
  }

  return csgso->getComplementA().empty() ? NULL :
    safe_strdup(csgso->getComplementA().c_str());
}


/*
 * Returns the value of the "complementB" attribute of this CSGSetOperator_t.
 */
LIBSBML_EXTERN
char *
CSGSetOperator_getComplementB(const CSGSetOperator_t * csgso)
{
  if (csgso == NULL)
  {
    return NULL;
  }

  return csgso->getComplementB().empty() ? NULL :
    safe_strdup(csgso->getComplementB().c_str());
}


/*
 * Predicate returning @c 1 (true) if this CSGSetOperator_t's "operationType"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CSGSetOperator_isSetOperationType(const CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? static_cast<int>(csgso->isSetOperationType()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CSGSetOperator_t's "complementA"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CSGSetOperator_isSetComplementA(const CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? static_cast<int>(csgso->isSetComplementA()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CSGSetOperator_t's "complementB"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CSGSetOperator_isSetComplementB(const CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? static_cast<int>(csgso->isSetComplementB()) : 0;
}


/*
 * Sets the value of the "operationType" attribute of this CSGSetOperator_t.
 */
LIBSBML_EXTERN
int
CSGSetOperator_setOperationType(CSGSetOperator_t * csgso,
                                SetOperation_t operationType)
{
  return (csgso != NULL) ? csgso->setOperationType(operationType) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "operationType" attribute of this CSGSetOperator_t.
 */
LIBSBML_EXTERN
int
CSGSetOperator_setOperationTypeAsString(CSGSetOperator_t * csgso,
                                        const char * operationType)
{
  return (csgso != NULL) ? csgso->setOperationType(operationType):
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "complementA" attribute of this CSGSetOperator_t.
 */
LIBSBML_EXTERN
int
CSGSetOperator_setComplementA(CSGSetOperator_t * csgso,
                              const char * complementA)
{
  return (csgso != NULL) ? csgso->setComplementA(complementA) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "complementB" attribute of this CSGSetOperator_t.
 */
LIBSBML_EXTERN
int
CSGSetOperator_setComplementB(CSGSetOperator_t * csgso,
                              const char * complementB)
{
  return (csgso != NULL) ? csgso->setComplementB(complementB) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "operationType" attribute of this CSGSetOperator_t.
 */
LIBSBML_EXTERN
int
CSGSetOperator_unsetOperationType(CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? csgso->unsetOperationType() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "complementA" attribute of this CSGSetOperator_t.
 */
LIBSBML_EXTERN
int
CSGSetOperator_unsetComplementA(CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? csgso->unsetComplementA() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "complementB" attribute of this CSGSetOperator_t.
 */
LIBSBML_EXTERN
int
CSGSetOperator_unsetComplementB(CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? csgso->unsetComplementB() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing CSGNode_t objects from this
 * CSGSetOperator_t.
 */
LIBSBML_EXTERN
ListOf_t*
CSGSetOperator_getListOfCSGNodes(CSGSetOperator_t* csgso)
{
  return (csgso != NULL) ? csgso->getListOfCSGNodes() : NULL;
}


/*
 * Get a CSGNode_t from the CSGSetOperator_t.
 */
LIBSBML_EXTERN
CSGNode_t*
CSGSetOperator_getCSGNode(CSGSetOperator_t* csgso, unsigned int n)
{
  return (csgso != NULL) ? csgso->getCSGNode(n) : NULL;
}


/*
 * Get a CSGNode_t from the CSGSetOperator_t based on its identifier.
 */
LIBSBML_EXTERN
CSGNode_t*
CSGSetOperator_getCSGNodeById(CSGSetOperator_t* csgso, const char *sid)
{
  return (csgso != NULL && sid != NULL) ? csgso->getCSGNode(sid) : NULL;
}


/*
 * Adds a copy of the given CSGNode_t to this CSGSetOperator_t.
 */
LIBSBML_EXTERN
int
CSGSetOperator_addCSGNode(CSGSetOperator_t* csgso, const CSGNode_t* csgn)
{
  return (csgso != NULL) ? csgso->addCSGNode(csgn) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of CSGNode_t objects in this CSGSetOperator_t.
 */
LIBSBML_EXTERN
unsigned int
CSGSetOperator_getNumCSGNodes(CSGSetOperator_t* csgso)
{
  return (csgso != NULL) ? csgso->getNumCSGNodes() : SBML_INT_MAX;
}


/*
 * Creates a new CSGPrimitive_t object, adds it to this CSGSetOperator_t object
 * and returns the CSGPrimitive_t object created.
 */
LIBSBML_EXTERN
CSGPrimitive_t*
CSGSetOperator_createCSGPrimitive(CSGSetOperator_t* csgso)
{
  return (csgso != NULL) ? csgso->createCSGPrimitive() : NULL;
}


/*
 * Creates a new CSGTranslation_t object, adds it to this CSGSetOperator_t
 * object and returns the CSGTranslation_t object created.
 */
LIBSBML_EXTERN
CSGTranslation_t*
CSGSetOperator_createCSGTranslation(CSGSetOperator_t* csgso)
{
  return (csgso != NULL) ? csgso->createCSGTranslation() : NULL;
}


/*
 * Creates a new CSGRotation_t object, adds it to this CSGSetOperator_t object
 * and returns the CSGRotation_t object created.
 */
LIBSBML_EXTERN
CSGRotation_t*
CSGSetOperator_createCSGRotation(CSGSetOperator_t* csgso)
{
  return (csgso != NULL) ? csgso->createCSGRotation() : NULL;
}


/*
 * Creates a new CSGScale_t object, adds it to this CSGSetOperator_t object and
 * returns the CSGScale_t object created.
 */
LIBSBML_EXTERN
CSGScale_t*
CSGSetOperator_createCSGScale(CSGSetOperator_t* csgso)
{
  return (csgso != NULL) ? csgso->createCSGScale() : NULL;
}


/*
 * Creates a new CSGHomogeneousTransformation_t object, adds it to this
 * CSGSetOperator_t object and returns the CSGHomogeneousTransformation_t
 * object created.
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t*
CSGSetOperator_createCSGHomogeneousTransformation(CSGSetOperator_t* csgso)
{
  return (csgso != NULL) ? csgso->createCSGHomogeneousTransformation() : NULL;
}


/*
 * Creates a new CSGSetOperator_t object, adds it to this CSGSetOperator_t
 * object and returns the CSGSetOperator_t object created.
 */
LIBSBML_EXTERN
CSGSetOperator_t*
CSGSetOperator_createCSGSetOperator(CSGSetOperator_t* csgso)
{
  return (csgso != NULL) ? csgso->createCSGSetOperator() : NULL;
}


/*
 * Removes the nth CSGNode_t from this CSGSetOperator_t and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
CSGNode_t*
CSGSetOperator_removeCSGNode(CSGSetOperator_t* csgso, unsigned int n)
{
  return (csgso != NULL) ? csgso->removeCSGNode(n) : NULL;
}


/*
 * Removes the CSGNode_t from this CSGSetOperator_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
CSGNode_t*
CSGSetOperator_removeCSGNodeById(CSGSetOperator_t* csgso, const char* sid)
{
  return (csgso != NULL && sid != NULL) ? csgso->removeCSGNode(sid) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGSetOperator_t object have been set.
 */
LIBSBML_EXTERN
int
CSGSetOperator_hasRequiredAttributes(const CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? static_cast<int>(csgso->hasRequiredAttributes()) :
    0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * CSGSetOperator_t object have been set.
 */
LIBSBML_EXTERN
int
CSGSetOperator_hasRequiredElements(const CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? static_cast<int>(csgso->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


