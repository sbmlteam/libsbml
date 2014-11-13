/**
 * @file:   DomainType.cpp
 * @brief:  Implementation of the DomainType class
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


#include <sbml/packages/spatial/sbml/DomainType.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new DomainType with the given level, version, and package version.
 */
DomainType::DomainType (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mSpatialDimensions (SBML_INT_MAX)
  , mIsSetSpatialDimensions (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new DomainType with the given SpatialPkgNamespaces object.
 */
DomainType::DomainType (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mSpatialDimensions (SBML_INT_MAX)
  , mIsSetSpatialDimensions (false)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for DomainType.
 */
DomainType::DomainType (const DomainType& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mSpatialDimensions  = orig.mSpatialDimensions;
    mIsSetSpatialDimensions  = orig.mIsSetSpatialDimensions;
  }
}


/*
 * Assignment for DomainType.
 */
DomainType&
DomainType::operator=(const DomainType& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mSpatialDimensions  = rhs.mSpatialDimensions;
    mIsSetSpatialDimensions  = rhs.mIsSetSpatialDimensions;
  }
  return *this;
}


/*
 * Clone for DomainType.
 */
DomainType*
DomainType::clone () const
{
  return new DomainType(*this);
}


/*
 * Destructor for DomainType.
 */
DomainType::~DomainType ()
{
}


/*
 * Returns the value of the "id" attribute of this DomainType.
 */
const std::string&
DomainType::getId() const
{
  return mId;
}


/*
 * Returns the value of the "spatialDimensions" attribute of this DomainType.
 */
int
DomainType::getSpatialDimensions() const
{
  return mSpatialDimensions;
}


/*
 * Returns true/false if id is set.
 */
bool
DomainType::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if spatialDimensions is set.
 */
bool
DomainType::isSetSpatialDimensions() const
{
  return mIsSetSpatialDimensions;
}


/*
 * Sets id and returns value indicating success.
 */
int
DomainType::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets spatialDimensions and returns value indicating success.
 */
int
DomainType::setSpatialDimensions(int spatialDimensions)
{
  mSpatialDimensions = spatialDimensions;
  mIsSetSpatialDimensions = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
 */
int
DomainType::unsetId()
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
 * Unsets spatialDimensions and returns value indicating success.
 */
int
DomainType::unsetSpatialDimensions()
{
  mSpatialDimensions = SBML_INT_MAX;
  mIsSetSpatialDimensions = false;

  if (isSetSpatialDimensions() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the XML element name of this object
 */
const std::string&
DomainType::getElementName () const
{
  static const string name = "domainType";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
DomainType::getTypeCode () const
{
  return SBML_SPATIAL_DOMAINTYPE;
}


/*
 * check if all the required attributes are set
 */
bool
DomainType::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetSpatialDimensions() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
DomainType::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
DomainType::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
DomainType::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
DomainType::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
DomainType::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("spatialDimensions");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
DomainType::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfDomainTypes - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfDomainTypes*>(getParentSBMLObject())->size() < 2)
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
      logEmptyString(mId, getLevel(), getVersion(), "<DomainType>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'domainType' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // spatialDimensions int   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetSpatialDimensions = attributes.readInto("spatialDimensions", mSpatialDimensions);

  if (mIsSetSpatialDimensions == false)
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
        std::string message = "Spatial attribute 'spatialDimensions' is missing from 'domainType' object.";
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
DomainType::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetSpatialDimensions() == true)
    stream.writeAttribute("spatialDimensions", getPrefix(), mSpatialDimensions);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfDomainTypes::ListOfDomainTypes(unsigned int level, 
                    unsigned int version, 
                    unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfDomainTypes::ListOfDomainTypes(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfDomainTypes 
 */
ListOfDomainTypes* 
ListOfDomainTypes::clone () const
 {
  return new ListOfDomainTypes(*this);
}


/*
 * Get a DomainType from the ListOfDomainTypes by index.
*/
DomainType*
ListOfDomainTypes::get(unsigned int n)
{
  return static_cast<DomainType*>(ListOf::get(n));
}


/*
 * Get a DomainType from the ListOfDomainTypes by index.
 */
const DomainType*
ListOfDomainTypes::get(unsigned int n) const
{
  return static_cast<const DomainType*>(ListOf::get(n));
}


/*
 * Get a DomainType from the ListOfDomainTypes by id.
 */
DomainType*
ListOfDomainTypes::get(const std::string& sid)
{
	return const_cast<DomainType*>(
    static_cast<const ListOfDomainTypes&>(*this).get(sid));
}


/*
 * Get a DomainType from the ListOfDomainTypes by id.
 */
const DomainType*
ListOfDomainTypes::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<DomainType>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <DomainType*> (*result);
}


/**
 * Adds a copy the given "DomainType" to this ListOfDomainTypes.
 *
 * @param dt; the DomainType object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfDomainTypes::addDomainType(const DomainType* dt)
{
  if (dt == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (dt->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != dt->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != dt->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(dt)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(dt);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of DomainType objects in this ListOfDomainTypes.
 *
 * @return the number of DomainType objects in this ListOfDomainTypes
 */
unsigned int 
ListOfDomainTypes::getNumDomainTypes() const
{
	return size();
}

/**
 * Creates a new DomainType object, adds it to this ListOfDomainTypes
 * DomainType and returns the DomainType object created. 
 *
 * @return a new DomainType object instance
 *
 * @see addDomainType(const DomainType* dt)
 */
DomainType* 
ListOfDomainTypes::createDomainType()
{
  DomainType* dt = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    dt = new DomainType(spatialns);
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

  if(dt != NULL)
  {
    appendAndOwn(dt);
  }

  return dt;
}

/*
 * Removes the nth DomainType from this ListOfDomainTypes
 */
DomainType*
ListOfDomainTypes::remove(unsigned int n)
{
  return static_cast<DomainType*>(ListOf::remove(n));
}


/*
 * Removes the DomainType from this ListOfDomainTypes with the given identifier
 */
DomainType*
ListOfDomainTypes::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<DomainType>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <DomainType*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfDomainTypes::getElementName () const
{
  static const string name = "listOfDomainTypes";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfDomainTypes::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfDomainTypes::getItemTypeCode () const
{
  return SBML_SPATIAL_DOMAINTYPE;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new DomainType in this ListOfDomainTypes
 */
SBase*
ListOfDomainTypes::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "domainType")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new DomainType(spatialns);
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
ListOfDomainTypes::writeXMLNS(XMLOutputStream& stream) const
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
DomainType_t *
DomainType_create(unsigned int level, unsigned int version,
                  unsigned int pkgVersion)
{
  return new DomainType(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
DomainType_free(DomainType_t * dt)
{
  if (dt != NULL)
    delete dt;
}


LIBSBML_EXTERN
DomainType_t *
DomainType_clone(DomainType_t * dt)
{
  if (dt != NULL)
  {
    return static_cast<DomainType_t*>(dt->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
DomainType_getId(const DomainType_t * dt)
{
	return (dt != NULL && dt->isSetId()) ? dt->getId().c_str() : NULL;
}


LIBSBML_EXTERN
int
DomainType_getSpatialDimensions(const DomainType_t * dt)
{
	return (dt != NULL) ? dt->getSpatialDimensions() : SBML_INT_MAX;
}


LIBSBML_EXTERN
int
DomainType_isSetId(const DomainType_t * dt)
{
  return (dt != NULL) ? static_cast<int>(dt->isSetId()) : 0;
}


LIBSBML_EXTERN
int
DomainType_isSetSpatialDimensions(const DomainType_t * dt)
{
  return (dt != NULL) ? static_cast<int>(dt->isSetSpatialDimensions()) : 0;
}


LIBSBML_EXTERN
int
DomainType_setId(DomainType_t * dt, const char * id)
{
  if (dt != NULL)
    return (id == NULL) ? dt->setId("") : dt->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DomainType_setSpatialDimensions(DomainType_t * dt, int spatialDimensions)
{
  if (dt != NULL)
    return dt->setSpatialDimensions(spatialDimensions);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DomainType_unsetId(DomainType_t * dt)
{
  return (dt != NULL) ? dt->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DomainType_unsetSpatialDimensions(DomainType_t * dt)
{
  return (dt != NULL) ? dt->unsetSpatialDimensions() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DomainType_hasRequiredAttributes(const DomainType_t * dt)
{
  return (dt != NULL) ? static_cast<int>(dt->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
DomainType_t *
ListOfDomainTypes_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfDomainTypes *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
DomainType_t *
ListOfDomainTypes_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfDomainTypes *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


