/**
 * @file:   CSGObject.cpp
 * @brief:  Implementation of the CSGObject class
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


#include <sbml/packages/spatial/sbml/CSGObject.h>
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
 * Creates a new CSGObject with the given level, version, and package version.
 */
CSGObject::CSGObject (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mDomainType ("")
  , mOrdinal (SBML_INT_MAX)
  , mIsSetOrdinal (false)
  , mCsgNode (NULL)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new CSGObject with the given SpatialPkgNamespaces object.
 */
CSGObject::CSGObject (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mDomainType ("")
  , mOrdinal (SBML_INT_MAX)
  , mIsSetOrdinal (false)
  , mCsgNode (NULL)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGObject.
 */
CSGObject::CSGObject (const CSGObject& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mDomainType  = orig.mDomainType;
    mOrdinal  = orig.mOrdinal;
    mIsSetOrdinal  = orig.mIsSetOrdinal;
    if (orig.mCsgNode != NULL)
    {
      mCsgNode = orig.mCsgNode->clone();
    }
    else
    {
      mCsgNode = NULL;
    }

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for CSGObject.
 */
CSGObject&
CSGObject::operator=(const CSGObject& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mDomainType  = rhs.mDomainType;
    mOrdinal  = rhs.mOrdinal;
    mIsSetOrdinal  = rhs.mIsSetOrdinal;
    if (rhs.mCsgNode != NULL)
    {
      mCsgNode = rhs.mCsgNode->clone();
    }
    else
    {
      mCsgNode = NULL;
    }

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for CSGObject.
 */
CSGObject*
CSGObject::clone () const
{
  return new CSGObject(*this);
}


/*
 * Destructor for CSGObject.
 */
CSGObject::~CSGObject ()
{
  delete mCsgNode;
  mCsgNode = NULL;
}


/*
 * Returns the value of the "id" attribute of this CSGObject.
 */
const std::string&
CSGObject::getId() const
{
  return mId;
}


/*
 * Returns the value of the "domainType" attribute of this CSGObject.
 */
const std::string&
CSGObject::getDomainType() const
{
  return mDomainType;
}


/*
 * Returns the value of the "ordinal" attribute of this CSGObject.
 */
int
CSGObject::getOrdinal() const
{
  return mOrdinal;
}


/*
 * Returns the value of the "csgNode" attribute of this CSGObject.
 */
const CSGNode*
CSGObject::getCsgNode() const
{
  return mCsgNode;
}


/*
 * Returns the value of the "csgNode" attribute of this CSGObject.
 */
CSGNode*
CSGObject::getCsgNode()
{
  return mCsgNode;
}


/*
 * Creates a new "csgNode" element of this CSGObject and returns it.
 */
CSGPrimitive*
CSGObject::createCsgPrimitive()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGPrimitive(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGPrimitive*>(mCsgNode);
}


/*
 * Creates a new "csgNode" element of this CSGObject and returns it.
 */
CSGTranslation*
CSGObject::createCsgTranslation()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGTranslation(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGTranslation*>(mCsgNode);
}


/*
 * Creates a new "csgNode" element of this CSGObject and returns it.
 */
CSGRotation*
CSGObject::createCsgRotation()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGRotation(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGRotation*>(mCsgNode);
}


/*
 * Creates a new "csgNode" element of this CSGObject and returns it.
 */
CSGScale*
CSGObject::createCsgScale()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGScale(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGScale*>(mCsgNode);
}


/*
 * Creates a new "csgNode" element of this CSGObject and returns it.
 */
CSGHomogeneousTransformation*
CSGObject::createCsgHomogeneousTransformation()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGHomogeneousTransformation(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGHomogeneousTransformation*>(mCsgNode);
}


/*
 * Creates a new "csgNode" element of this CSGObject and returns it.
 */
CSGPseudoPrimitive*
CSGObject::createCsgPseudoPrimitive()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGPseudoPrimitive(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGPseudoPrimitive*>(mCsgNode);
}


/*
 * Creates a new "csgNode" element of this CSGObject and returns it.
 */
CSGSetOperator*
CSGObject::createCsgSetOperator()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGSetOperator(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGSetOperator*>(mCsgNode);
}


/*
 * Returns true/false if id is set.
 */
bool
CSGObject::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if domainType is set.
 */
bool
CSGObject::isSetDomainType() const
{
  return (mDomainType.empty() == false);
}


/*
 * Returns true/false if ordinal is set.
 */
bool
CSGObject::isSetOrdinal() const
{
  return mIsSetOrdinal;
}


/*
 * Returns true/false if csgNode is set.
 */
bool
CSGObject::isSetCsgNode() const
{
  return (mCsgNode != NULL);
}


/*
 * Sets id and returns value indicating success.
 */
int
CSGObject::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets domainType and returns value indicating success.
 */
int
CSGObject::setDomainType(const std::string& domainType)
{
  if (&(domainType) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(domainType)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDomainType = domainType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets ordinal and returns value indicating success.
 */
int
CSGObject::setOrdinal(int ordinal)
{
  mOrdinal = ordinal;
  mIsSetOrdinal = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets csgNode and returns value indicating success.
 */
int
CSGObject::setCsgNode(CSGNode* csgNode)
{
  if (mCsgNode == csgNode)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (csgNode == NULL)
  {
    delete mCsgNode;
    mCsgNode = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mCsgNode;
    mCsgNode = (csgNode != NULL) ?
      static_cast<CSGNode*>(csgNode->clone()) : NULL;
    if (mCsgNode != NULL)
    {
      mCsgNode->connectToParent(this);
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
CSGObject::unsetId()
{
  mId.erase();

  if (mId.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets domainType and returns value indicating success.
 */
int
CSGObject::unsetDomainType()
{
  mDomainType.erase();

  if (mDomainType.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets ordinal and returns value indicating success.
 */
int
CSGObject::unsetOrdinal()
{
  mOrdinal = SBML_INT_MAX;
  mIsSetOrdinal = false;

  if (isSetOrdinal() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets csgNode and returns value indicating success.
 */
int
CSGObject::unsetCsgNode()
{
  delete mCsgNode;
  mCsgNode = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
CSGObject::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetDomainType() == true && mDomainType == oldid)
  {
    setDomainType(newid);
  }

}


List*
CSGObject::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mCsgNode, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
CSGObject::getElementName () const
{
  static const string name = "csgObject";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CSGObject::getTypeCode () const
{
  return SBML_SPATIAL_CSGOBJECT;
}


/*
 * check if all the required attributes are set
 */
bool
CSGObject::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetDomainType() == false)
    allPresent = false;

  if (isSetOrdinal() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
CSGObject::hasRequiredElements () const
{
  bool allPresent = true;

  if (isSetCsgNode() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CSGObject::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  if (isSetCsgNode() == true)
  {
    mCsgNode->write(stream);
  }
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGObject::accept (SBMLVisitor& v) const
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
CSGObject::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
  if ( mCsgNode != NULL)
    mCsgNode->setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
CSGObject::connectToChild()
{
  SBase::connectToChild();

  if (mCsgNode != NULL)
    mCsgNode->connectToParent(this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CSGObject::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
CSGObject::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  const string& name = stream.peek().getName();

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "csgPrimitive")
  {
    mCsgNode = new CSGPrimitive(spatialns);
    object = mCsgNode;
  }
  else if (name == "csgTranslation")
  {
    mCsgNode = new CSGTranslation(spatialns);
    object = mCsgNode;
  }
  else if (name == "csgRotation")
  {
    mCsgNode = new CSGRotation(spatialns);
    object = mCsgNode;
  }
  else if (name == "csgScale")
  {
    mCsgNode = new CSGScale(spatialns);
    object = mCsgNode;
  }
  else if (name == "csgHomogeneousTransformation")
  {
    mCsgNode = new CSGHomogeneousTransformation(spatialns);
    object = mCsgNode;
  }
  else if (name == "csgPseudoPrimitive")
  {
    mCsgNode = new CSGPseudoPrimitive(spatialns);
    object = mCsgNode;
  }
  else if (name == "csgSetOperator")
  {
    mCsgNode = new CSGSetOperator(spatialns);
    object = mCsgNode;
  }

  delete spatialns;

  connectToChild();


  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
CSGObject::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("domainType");
  attributes.add("ordinal");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CSGObject::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfCSGObjects - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfCSGObjects*>(getParentSBMLObject())->size() < 2)
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

  SBase::readAttributes(attributes, expectedAttributes);

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
  // id SId  ( use = "required" )
  //
  assigned = attributes.readInto("id", mId);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<CSGObject>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'csgObject' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // domainType SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("domainType", mDomainType);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mDomainType.empty() == true)
    {
      logEmptyString(mDomainType, getLevel(), getVersion(), "<CSGObject>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomainType) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute domainType='" + mDomainType + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domainType' is missing from 'csgObject' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // ordinal int   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetOrdinal = attributes.readInto("ordinal", mOrdinal);

  if (mIsSetOrdinal == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
      else
      {
        std::string message = "Spatial attribute 'ordinal' is missing from 'csgObject' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message);
      }
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
CSGObject::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetDomainType() == true)
    stream.writeAttribute("domainType", getPrefix(), mDomainType);

  if (isSetOrdinal() == true)
    stream.writeAttribute("ordinal", getPrefix(), mOrdinal);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfCSGObjects::ListOfCSGObjects(unsigned int level, 
                   unsigned int version, 
                   unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfCSGObjects::ListOfCSGObjects(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfCSGObjects 
 */
ListOfCSGObjects* 
ListOfCSGObjects::clone () const
 {
  return new ListOfCSGObjects(*this);
}


/*
 * Get a CsgObject from the ListOfCSGObjects by index.
*/
CSGObject*
ListOfCSGObjects::get(unsigned int n)
{
  return static_cast<CSGObject*>(ListOf::get(n));
}


/*
 * Get a CsgObject from the ListOfCSGObjects by index.
 */
const CSGObject*
ListOfCSGObjects::get(unsigned int n) const
{
  return static_cast<const CSGObject*>(ListOf::get(n));
}


/*
 * Get a CsgObject from the ListOfCSGObjects by id.
 */
CSGObject*
ListOfCSGObjects::get(const std::string& sid)
{
	return const_cast<CSGObject*>(
    static_cast<const ListOfCSGObjects&>(*this).get(sid));
}


/*
 * Get a CsgObject from the ListOfCSGObjects by id.
 */
const CSGObject*
ListOfCSGObjects::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CSGObject>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <CSGObject*> (*result);
}


/**
 * Adds a copy the given "CSGObject" to this ListOfCSGObjects.
 *
 * @param csgo; the CSGObject object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfCSGObjects::addCsgObject(const CSGObject* csgo)
{
  if (csgo == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (csgo->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != csgo->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != csgo->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(csgo)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(csgo);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of CSGObject objects in this ListOfCSGObjects.
 *
 * @return the number of CSGObject objects in this ListOfCSGObjects
 */
unsigned int 
ListOfCSGObjects::getNumCsgObjects() const
{
	return size();
}

/**
 * Creates a new CSGObject object, adds it to this ListOfCSGObjects
 * CSGObject and returns the CSGObject object created. 
 *
 * @return a new CSGObject object instance
 *
 * @see addCSGObject(const CSGObject* csgo)
 */
CSGObject* 
ListOfCSGObjects::createCsgObject()
{
  CSGObject* csgo = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgo = new CSGObject(spatialns);
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

  if(csgo != NULL)
  {
    appendAndOwn(csgo);
  }

  return csgo;
}

/*
 * Removes the nth CsgObject from this ListOfCSGObjects
 */
CSGObject*
ListOfCSGObjects::remove(unsigned int n)
{
  return static_cast<CSGObject*>(ListOf::remove(n));
}


/*
 * Removes the CsgObject from this ListOfCSGObjects with the given identifier
 */
CSGObject*
ListOfCSGObjects::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CSGObject>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <CSGObject*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfCSGObjects::getElementName () const
{
  static const string name = "listOfCSGObjects";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfCSGObjects::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfCSGObjects::getItemTypeCode () const
{
  return SBML_SPATIAL_CSGOBJECT;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new CSGObject in this ListOfCSGObjects
 */
SBase*
ListOfCSGObjects::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "csgObject")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new CSGObject(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Spatial package.
 */
void
ListOfCSGObjects::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(SpatialExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(SpatialExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
CSGObject_t *
CSGObject_create(unsigned int level, unsigned int version,
                 unsigned int pkgVersion)
{
  return new CSGObject(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CSGObject_free(CSGObject_t * csgo)
{
  if (csgo != NULL)
    delete csgo;
}


LIBSBML_EXTERN
CSGObject_t *
CSGObject_clone(CSGObject_t * csgo)
{
  if (csgo != NULL)
  {
    return static_cast<CSGObject_t*>(csgo->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
CSGObject_getId(const CSGObject_t * csgo)
{
	return (csgo != NULL && csgo->isSetId()) ? csgo->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
CSGObject_getDomainType(const CSGObject_t * csgo)
{
	return (csgo != NULL && csgo->isSetDomainType()) ? csgo->getDomainType().c_str() : NULL;
}


LIBSBML_EXTERN
int
CSGObject_getOrdinal(const CSGObject_t * csgo)
{
	return (csgo != NULL) ? csgo->getOrdinal() : SBML_INT_MAX;
}


LIBSBML_EXTERN
CSGNode_t*
CSGObject_getCsgNode(CSGObject_t * csgo)
{
	if (csgo == NULL)
		return NULL;

	return (CSGNode_t*)csgo->getCsgNode();
}


LIBSBML_EXTERN
CSGPrimitive_t *
CSGObject_createCsgPrimitive(CSGObject_t * csgo)
{
	return  (csgo != NULL) ? csgo->createCsgPrimitive() : NULL;
}

LIBSBML_EXTERN
CSGTranslation_t *
CSGObject_createCsgTranslation(CSGObject_t * csgo)
{
	return  (csgo != NULL) ? csgo->createCsgTranslation() : NULL;
}

LIBSBML_EXTERN
CSGRotation_t *
CSGObject_createCsgRotation(CSGObject_t * csgo)
{
	return  (csgo != NULL) ? csgo->createCsgRotation() : NULL;
}

LIBSBML_EXTERN
CSGScale_t *
CSGObject_createCsgScale(CSGObject_t * csgo)
{
	return  (csgo != NULL) ? csgo->createCsgScale() : NULL;
}

LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGObject_createCsgHomogeneousTransformation(CSGObject_t * csgo)
{
	return  (csgo != NULL) ? csgo->createCsgHomogeneousTransformation() : NULL;
}

LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGObject_createCsgPseudoPrimitive(CSGObject_t * csgo)
{
	return  (csgo != NULL) ? csgo->createCsgPseudoPrimitive() : NULL;
}

LIBSBML_EXTERN
CSGSetOperator_t *
CSGObject_createCsgSetOperator(CSGObject_t * csgo)
{
	return  (csgo != NULL) ? csgo->createCsgSetOperator() : NULL;
}

LIBSBML_EXTERN
int
CSGObject_isSetId(const CSGObject_t * csgo)
{
  return (csgo != NULL) ? static_cast<int>(csgo->isSetId()) : 0;
}


LIBSBML_EXTERN
int
CSGObject_isSetDomainType(const CSGObject_t * csgo)
{
  return (csgo != NULL) ? static_cast<int>(csgo->isSetDomainType()) : 0;
}


LIBSBML_EXTERN
int
CSGObject_isSetOrdinal(const CSGObject_t * csgo)
{
  return (csgo != NULL) ? static_cast<int>(csgo->isSetOrdinal()) : 0;
}


LIBSBML_EXTERN
int
CSGObject_isSetCsgNode(const CSGObject_t * csgo)
{
  return (csgo != NULL) ? static_cast<int>(csgo->isSetCsgNode()) : 0;
}


LIBSBML_EXTERN
int
CSGObject_setId(CSGObject_t * csgo, const char * id)
{
  if (csgo != NULL)
    return (id == NULL) ? csgo->setId("") : csgo->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGObject_setDomainType(CSGObject_t * csgo, const char * domainType)
{
  if (csgo != NULL)
    return (domainType == NULL) ? csgo->setDomainType("") : csgo->setDomainType(domainType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGObject_setOrdinal(CSGObject_t * csgo, int ordinal)
{
  if (csgo != NULL)
    return csgo->setOrdinal(ordinal);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGObject_setCsgNode(CSGObject_t * csgo, CSGNode_t* csgNode)
{
	return (csgo != NULL) ? csgo->setCsgNode(csgNode) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGObject_unsetId(CSGObject_t * csgo)
{
  return (csgo != NULL) ? csgo->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGObject_unsetDomainType(CSGObject_t * csgo)
{
  return (csgo != NULL) ? csgo->unsetDomainType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGObject_unsetOrdinal(CSGObject_t * csgo)
{
  return (csgo != NULL) ? csgo->unsetOrdinal() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGObject_hasRequiredAttributes(const CSGObject_t * csgo)
{
  return (csgo != NULL) ? static_cast<int>(csgo->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
CSGObject_hasRequiredElements(const CSGObject_t * csgo)
{
	return (csgo != NULL) ? static_cast<int>(csgo->hasRequiredElements()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
CSGObject_t *
ListOfCSGObjects_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfCSGObjects *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
CSGObject_t *
ListOfCSGObjects_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfCSGObjects *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


