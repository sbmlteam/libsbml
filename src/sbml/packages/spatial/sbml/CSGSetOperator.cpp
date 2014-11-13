/**
 * @file:   CSGSetOperator.cpp
 * @brief:  Implementation of the CSGSetOperator class
 * @author: SBMLTeam
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


#include <sbml/packages/spatial/sbml/CSGSetOperator.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>
#include <sbml/packages/spatial/sbml/CSGPseudoPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGSetOperator.h>



using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CSGSetOperator with the given level, version, and package version.
 */
CSGSetOperator::CSGSetOperator (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : CSGNode(level, version)
  , mOperationType (SETOPERATION_UNKNOWN)
  , mComplementA ("")
  , mComplementB ("")
  , mCsgNodes (level, version, pkgVersion)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new CSGSetOperator with the given SpatialPkgNamespaces object.
 */
CSGSetOperator::CSGSetOperator (SpatialPkgNamespaces* spatialns)
  : CSGNode(spatialns)
  , mOperationType (SETOPERATION_UNKNOWN)
  , mComplementA ("")
  , mComplementB ("")
  , mCsgNodes (spatialns)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGSetOperator.
 */
CSGSetOperator::CSGSetOperator (const CSGSetOperator& orig)
  : CSGNode(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mOperationType  = orig.mOperationType;
    mComplementA  = orig.mComplementA;
    mComplementB  = orig.mComplementB;
    mCsgNodes  = orig.mCsgNodes;

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for CSGSetOperator.
 */
CSGSetOperator&
CSGSetOperator::operator=(const CSGSetOperator& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    CSGNode::operator=(rhs);
    mOperationType  = rhs.mOperationType;
    mComplementA  = rhs.mComplementA;
    mComplementB  = rhs.mComplementB;
    mCsgNodes  = rhs.mCsgNodes;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for CSGSetOperator.
 */
CSGSetOperator*
CSGSetOperator::clone () const
{
  return new CSGSetOperator(*this);
}


/*
 * Destructor for CSGSetOperator.
 */
CSGSetOperator::~CSGSetOperator ()
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
 * Returns true/false if operationType is set.
 */
bool
CSGSetOperator::isSetOperationType() const
{
  return mOperationType != SETOPERATION_UNKNOWN;
}


/*
 * Returns true/false if complementA is set.
 */
bool
CSGSetOperator::isSetComplementA() const
{
  return (mComplementA.empty() == false);
}


/*
 * Returns true/false if complementB is set.
 */
bool
CSGSetOperator::isSetComplementB() const
{
  return (mComplementB.empty() == false);
}


/*
 * Sets operationType and returns value indicating success.
 */
int
CSGSetOperator::setOperationType(SetOperation_t operationType)
{
  mOperationType = operationType;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets operationType and returns value indicating success.
 */
int
CSGSetOperator::setOperationType(const std::string& operationType)
{
  SetOperation_t parsed = SetOperation_parse(operationType.c_str());
  if (parsed == SETOPERATION_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mOperationType = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets complementA and returns value indicating success.
 */
int
CSGSetOperator::setComplementA(const std::string& complementA)
{
  if (&(complementA) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(complementA)))
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
 * Sets complementB and returns value indicating success.
 */
int
CSGSetOperator::setComplementB(const std::string& complementB)
{
  if (&(complementB) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(complementB)))
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
 * Unsets operationType and returns value indicating success.
 */
int
CSGSetOperator::unsetOperationType()
{
  mOperationType = SETOPERATION_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets complementA and returns value indicating success.
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
 * Unsets complementB and returns value indicating success.
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
 * Returns the  "ListOfCSGNodes" in this CSGSetOperator object.
 */
const ListOfCSGNodes*
CSGSetOperator::getListOfCsgNodes() const
{
  return &mCsgNodes;
}


/*
 * Returns the  "ListOfCSGNodes" in this CSGSetOperator object.
 */
ListOfCSGNodes*
CSGSetOperator::getListOfCsgNodes()
{
  return &mCsgNodes;
}


/*
 * Removes the nth CsgNode from the ListOfCSGNodes.
 */
CSGNode*
CSGSetOperator::removeCsgNode(unsigned int n)
{
	return mCsgNodes.remove(n);
}


/*
 * Removes the a CsgNode with given id from the ListOfCSGNodes.
 */
CSGNode*
CSGSetOperator::removeCsgNode(const std::string& sid)
{
	return mCsgNodes.remove(sid);
}


/*
 * Return the nth CsgNode in the ListOfCSGNodes within this CSGSetOperator.
 */
CSGNode*
CSGSetOperator::getCsgNode(unsigned int n)
{
	return mCsgNodes.get(n);
}


/*
 * Return the nth CsgNode in the ListOfCSGNodes within this CSGSetOperator.
 */
const CSGNode*
CSGSetOperator::getCsgNode(unsigned int n) const
{
	return mCsgNodes.get(n);
}


/*
 * Return a CsgNode from the ListOfCSGNodes by id.
 */
CSGNode*
CSGSetOperator::getCsgNode(const std::string& sid)
{
	return mCsgNodes.get(sid);
}


/*
 * Return a CsgNode from the ListOfCSGNodes by id.
 */
const CSGNode*
CSGSetOperator::getCsgNode(const std::string& sid) const
{
	return mCsgNodes.get(sid);
}


/*
 * Adds a copy the given "CSGNode" to this CSGSetOperator.
 *
 * @param csgn; the CSGNode object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
CSGSetOperator::addCsgNode(const CSGNode* csgn)
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
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(csgn)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mCsgNodes.append(csgn);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of CSGNode objects in this CSGSetOperator.
 *
 * @return the number of CSGNode objects in this CSGSetOperator
 */
unsigned int
CSGSetOperator::getNumCsgNodes() const
{
  return mCsgNodes.size();
}


/**
 * Creates a new CSGPrimitive object, adds it to this CSGSetOperators
 * ListOfCSGNodes and returns the CSGPrimitive object created. 
 *
 * @return a new CSGPrimitive object instance
 *
 * @see addCSGNode(const CSGNode*)
 */
CSGPrimitive* 
CSGSetOperator::createCsgPrimitive()
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csgp != NULL)
  {
    mCsgNodes.appendAndOwn(csgp);
  }

  return csgp;
}


/**
 * Creates a new CSGTranslation object, adds it to this CSGSetOperators
 * ListOfCSGNodes and returns the CSGTranslation object created. 
 *
 * @return a new CSGTranslation object instance
 *
 * @see addCSGNode(const CSGNode*)
 */
CSGTranslation* 
CSGSetOperator::createCsgTranslation()
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csgt != NULL)
  {
    mCsgNodes.appendAndOwn(csgt);
  }

  return csgt;
}


/**
 * Creates a new CSGRotation object, adds it to this CSGSetOperators
 * ListOfCSGNodes and returns the CSGRotation object created. 
 *
 * @return a new CSGRotation object instance
 *
 * @see addCSGNode(const CSGNode*)
 */
CSGRotation* 
CSGSetOperator::createCsgRotation()
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csgr != NULL)
  {
    mCsgNodes.appendAndOwn(csgr);
  }

  return csgr;
}


/**
 * Creates a new CSGScale object, adds it to this CSGSetOperators
 * ListOfCSGNodes and returns the CSGScale object created. 
 *
 * @return a new CSGScale object instance
 *
 * @see addCSGNode(const CSGNode*)
 */
CSGScale* 
CSGSetOperator::createCsgScale()
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csgs != NULL)
  {
    mCsgNodes.appendAndOwn(csgs);
  }

  return csgs;
}


/**
 * Creates a new CSGHomogeneousTransformation object, adds it to this CSGSetOperators
 * ListOfCSGNodes and returns the CSGHomogeneousTransformation object created. 
 *
 * @return a new CSGHomogeneousTransformation object instance
 *
 * @see addCSGNode(const CSGNode*)
 */
CSGHomogeneousTransformation* 
CSGSetOperator::createCsgHomogeneousTransformation()
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csght != NULL)
  {
    mCsgNodes.appendAndOwn(csght);
  }

  return csght;
}


/**
 * Creates a new CSGPseudoPrimitive object, adds it to this CSGSetOperators
 * ListOfCSGNodes and returns the CSGPseudoPrimitive object created. 
 *
 * @return a new CSGPseudoPrimitive object instance
 *
 * @see addCSGNode(const CSGNode*)
 */
CSGPseudoPrimitive* 
CSGSetOperator::createCsgPseudoPrimitive()
{
  CSGPseudoPrimitive* csgpp = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgpp = new CSGPseudoPrimitive(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csgpp != NULL)
  {
    mCsgNodes.appendAndOwn(csgpp);
  }

  return csgpp;
}


/**
 * Creates a new CSGSetOperator object, adds it to this CSGSetOperators
 * ListOfCSGNodes and returns the CSGSetOperator object created. 
 *
 * @return a new CSGSetOperator object instance
 *
 * @see addCSGNode(const CSGNode*)
 */
CSGSetOperator* 
CSGSetOperator::createCsgSetOperator()
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csgso != NULL)
  {
    mCsgNodes.appendAndOwn(csgso);
  }

  return csgso;
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
CSGSetOperator::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetComplementA() == true && mComplementA == oldid)
  {
    setComplementA(newid);
  }

  if (isSetComplementB() == true && mComplementB == oldid)
  {
    setComplementB(newid);
  }

}


List*
CSGSetOperator::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
CSGSetOperator::getElementName () const
{
  static const string name = "csgSetOperator";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CSGSetOperator::getTypeCode () const
{
  return SBML_SPATIAL_CSGSETOPERATOR;
}


/*
 * check if all the required attributes are set
 */
bool
CSGSetOperator::hasRequiredAttributes () const
{
  bool allPresent = CSGNode::hasRequiredAttributes();

  if (isSetOperationType() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
CSGSetOperator::hasRequiredElements () const
{
  bool allPresent = CSGNode::hasRequiredElements();

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CSGSetOperator::writeElements (XMLOutputStream& stream) const
{
  CSGNode::writeElements(stream);
  if (getNumCsgNodes() > 0)
  {
    mCsgNodes.write(stream);
  }

  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGSetOperator::accept (SBMLVisitor& v) const
{
  v.visit(*this);

/* VISIT CHILDREN */

  v.leave(*this);

  return true;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
CSGSetOperator::setSBMLDocument (SBMLDocument* d)
{
  CSGNode::setSBMLDocument(d);
  mCsgNodes.setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
CSGSetOperator::connectToChild()
{
  CSGNode::connectToChild();

  mCsgNodes.connectToParent(this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CSGSetOperator::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  CSGNode::enablePackageInternal(pkgURI, pkgPrefix, flag);
  mCsgNodes.enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
CSGSetOperator::createObject(XMLInputStream& stream)
{
  SBase* object = CSGNode::createObject(stream);

  const string& name = stream.peek().getName();

  if (name == "listOfCsgNodes")
  {
    object = &mCsgNodes;
  }
  connectToChild();


  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
CSGSetOperator::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGNode::addExpectedAttributes(attributes);

  attributes.add("operationType");
  attributes.add("complementA");
  attributes.add("complementB");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CSGSetOperator::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  CSGNode::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // operationType enum  ( use = "required" )
  //
  mOperationType = SETOPERATION_UNKNOWN;
  std::string stringValue;
  assigned = attributes.readInto("operationType", stringValue);

  if (assigned == true)
  {
    // parse enum

    mOperationType = SetOperation_parse(stringValue.c_str());
    if(mOperationType == SETOPERATION_UNKNOWN) {
      std::string message = "Unknown value for spatial attribute 'operationType' in 'csgSetOperator' object: " + stringValue;
      getErrorLog()->logPackageError("spatial", SpatialUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  if(mOperationType == SETOPERATION_UNKNOWN)
  {
    std::string message = "Spatial attribute 'operationType' is missing from 'csgSetOperator' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // complementA SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("complementA", mComplementA);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mComplementA.empty() == true)
    {
      logEmptyString(mComplementA, getLevel(), getVersion(), "<CSGSetOperator>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mComplementA) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute complementA='" + mComplementA + "' does not conform.");
    }
  }

  //
  // complementB SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("complementB", mComplementB);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mComplementB.empty() == true)
    {
      logEmptyString(mComplementB, getLevel(), getVersion(), "<CSGSetOperator>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mComplementB) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute complementB='" + mComplementB + "' does not conform.");
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
CSGSetOperator::writeAttributes (XMLOutputStream& stream) const
{
  CSGNode::writeAttributes(stream);

  if (isSetOperationType() == true)
    stream.writeAttribute("operationType", getPrefix(), SetOperation_toString(mOperationType));

  if (isSetComplementA() == true)
    stream.writeAttribute("complementA", getPrefix(), mComplementA);

  if (isSetComplementB() == true)
    stream.writeAttribute("complementB", getPrefix(), mComplementB);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
CSGSetOperator_t *
CSGSetOperator_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion)
{
  return new CSGSetOperator(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CSGSetOperator_free(CSGSetOperator_t * csgso)
{
  if (csgso != NULL)
    delete csgso;
}


LIBSBML_EXTERN
CSGSetOperator_t *
CSGSetOperator_clone(CSGSetOperator_t * csgso)
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


LIBSBML_EXTERN
SetOperation_t
CSGSetOperator_getOperationType(const CSGSetOperator_t * csgso)
{
	return (csgso != NULL) ? csgso->getOperationType() : SETOPERATION_UNKNOWN;
}


LIBSBML_EXTERN
const char *
CSGSetOperator_getComplementA(const CSGSetOperator_t * csgso)
{
	return (csgso != NULL && csgso->isSetComplementA()) ? csgso->getComplementA().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
CSGSetOperator_getComplementB(const CSGSetOperator_t * csgso)
{
	return (csgso != NULL && csgso->isSetComplementB()) ? csgso->getComplementB().c_str() : NULL;
}


LIBSBML_EXTERN
int
CSGSetOperator_isSetOperationType(const CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? static_cast<int>(csgso->isSetOperationType()) : 0;
}


LIBSBML_EXTERN
int
CSGSetOperator_isSetComplementA(const CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? static_cast<int>(csgso->isSetComplementA()) : 0;
}


LIBSBML_EXTERN
int
CSGSetOperator_isSetComplementB(const CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? static_cast<int>(csgso->isSetComplementB()) : 0;
}


LIBSBML_EXTERN
int
CSGSetOperator_setOperationType(CSGSetOperator_t * csgso, SetOperation_t operationType)
{
  if (csgso != NULL)
    return csgso->setOperationType(operationType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGSetOperator_setComplementA(CSGSetOperator_t * csgso, const char * complementA)
{
  if (csgso != NULL)
    return (complementA == NULL) ? csgso->setComplementA("") : csgso->setComplementA(complementA);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGSetOperator_setComplementB(CSGSetOperator_t * csgso, const char * complementB)
{
  if (csgso != NULL)
    return (complementB == NULL) ? csgso->setComplementB("") : csgso->setComplementB(complementB);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGSetOperator_unsetOperationType(CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? csgso->unsetOperationType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGSetOperator_unsetComplementA(CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? csgso->unsetComplementA() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGSetOperator_unsetComplementB(CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? csgso->unsetComplementB() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGSetOperator_addCsgNode(CSGSetOperator_t * csgso, CSGNode_t * csgn)
{
	return  (csgso != NULL) ? csgso->addCsgNode(csgn) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
CSGPrimitive_t *
CSGSetOperator_createCsgPrimitive(CSGSetOperator_t * csgso)
{
	return  (csgso != NULL) ? csgso->createCsgPrimitive() : NULL;
}

LIBSBML_EXTERN
CSGTranslation_t *
CSGSetOperator_createCsgTranslation(CSGSetOperator_t * csgso)
{
	return  (csgso != NULL) ? csgso->createCsgTranslation() : NULL;
}

LIBSBML_EXTERN
CSGRotation_t *
CSGSetOperator_createCsgRotation(CSGSetOperator_t * csgso)
{
	return  (csgso != NULL) ? csgso->createCsgRotation() : NULL;
}

LIBSBML_EXTERN
CSGScale_t *
CSGSetOperator_createCsgScale(CSGSetOperator_t * csgso)
{
	return  (csgso != NULL) ? csgso->createCsgScale() : NULL;
}

LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGSetOperator_createCsgHomogeneousTransformation(CSGSetOperator_t * csgso)
{
	return  (csgso != NULL) ? csgso->createCsgHomogeneousTransformation() : NULL;
}

LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGSetOperator_createCsgPseudoPrimitive(CSGSetOperator_t * csgso)
{
	return  (csgso != NULL) ? csgso->createCsgPseudoPrimitive() : NULL;
}

LIBSBML_EXTERN
CSGSetOperator_t *
CSGSetOperator_createCsgSetOperator(CSGSetOperator_t * csgso)
{
	return  (csgso != NULL) ? csgso->createCsgSetOperator() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
CSGSetOperator_getListOfCSGNodes(CSGSetOperator_t * csgso)
{
	return  (csgso != NULL) ? (ListOf_t *)csgso->getListOfCsgNodes() : NULL;
}

LIBSBML_EXTERN
CSGNode_t *
CSGSetOperator_getCsgNode(CSGSetOperator_t * csgso, unsigned int n)
{
	return  (csgso != NULL) ? csgso->getCsgNode(n) : NULL;
}

LIBSBML_EXTERN
CSGNode_t *
CSGSetOperator_getCsgNodeById(CSGSetOperator_t * csgso, const char * sid)
{
	return  (csgso != NULL) ? csgso->getCsgNode(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
CSGSetOperator_getNumCsgNodes(CSGSetOperator_t * csgso)
{
	return  (csgso != NULL) ? csgso->getNumCsgNodes() : SBML_INT_MAX;
}

LIBSBML_EXTERN
CSGNode_t *
CSGSetOperator_removeCsgNode(CSGSetOperator_t * csgso, unsigned int n)
{
	return  (csgso != NULL) ? csgso->removeCsgNode(n) : NULL;
}

LIBSBML_EXTERN
CSGNode_t *
CSGSetOperator_removeCsgNodeById(CSGSetOperator_t * csgso, const char * sid)
{
	return  (csgso != NULL) ? csgso->removeCsgNode(sid) : NULL;
}

LIBSBML_EXTERN
int
CSGSetOperator_hasRequiredAttributes(const CSGSetOperator_t * csgso)
{
  return (csgso != NULL) ? static_cast<int>(csgso->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
CSGSetOperator_hasRequiredElements(const CSGSetOperator_t * csgso)
{
	return (csgso != NULL) ? static_cast<int>(csgso->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


