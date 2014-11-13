/**
 * @file:   OrdinalMapping.cpp
 * @brief:  Implementation of the OrdinalMapping class
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


#include <sbml/packages/spatial/sbml/OrdinalMapping.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new OrdinalMapping with the given level, version, and package version.
 */
OrdinalMapping::OrdinalMapping (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mGeometryDefinition ("")
  , mOrdinal (SBML_INT_MAX)
  , mIsSetOrdinal (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new OrdinalMapping with the given SpatialPkgNamespaces object.
 */
OrdinalMapping::OrdinalMapping (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mGeometryDefinition ("")
  , mOrdinal (SBML_INT_MAX)
  , mIsSetOrdinal (false)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for OrdinalMapping.
 */
OrdinalMapping::OrdinalMapping (const OrdinalMapping& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mGeometryDefinition  = orig.mGeometryDefinition;
    mOrdinal  = orig.mOrdinal;
    mIsSetOrdinal  = orig.mIsSetOrdinal;
  }
}


/*
 * Assignment for OrdinalMapping.
 */
OrdinalMapping&
OrdinalMapping::operator=(const OrdinalMapping& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mGeometryDefinition  = rhs.mGeometryDefinition;
    mOrdinal  = rhs.mOrdinal;
    mIsSetOrdinal  = rhs.mIsSetOrdinal;
  }
  return *this;
}


/*
 * Clone for OrdinalMapping.
 */
OrdinalMapping*
OrdinalMapping::clone () const
{
  return new OrdinalMapping(*this);
}


/*
 * Destructor for OrdinalMapping.
 */
OrdinalMapping::~OrdinalMapping ()
{
}


/*
 * Returns the value of the "geometryDefinition" attribute of this OrdinalMapping.
 */
const std::string&
OrdinalMapping::getGeometryDefinition() const
{
  return mGeometryDefinition;
}


/*
 * Returns the value of the "ordinal" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::getOrdinal() const
{
  return mOrdinal;
}


/*
 * Returns true/false if geometryDefinition is set.
 */
bool
OrdinalMapping::isSetGeometryDefinition() const
{
  return (mGeometryDefinition.empty() == false);
}


/*
 * Returns true/false if ordinal is set.
 */
bool
OrdinalMapping::isSetOrdinal() const
{
  return mIsSetOrdinal;
}


/*
 * Sets geometryDefinition and returns value indicating success.
 */
int
OrdinalMapping::setGeometryDefinition(const std::string& geometryDefinition)
{
  if (&(geometryDefinition) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(geometryDefinition)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mGeometryDefinition = geometryDefinition;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets ordinal and returns value indicating success.
 */
int
OrdinalMapping::setOrdinal(int ordinal)
{
  mOrdinal = ordinal;
  mIsSetOrdinal = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets geometryDefinition and returns value indicating success.
 */
int
OrdinalMapping::unsetGeometryDefinition()
{
  mGeometryDefinition.erase();

  if (mGeometryDefinition.empty() == true)
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
OrdinalMapping::unsetOrdinal()
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
 * rename attributes that are SIdRefs or instances in math
 */
void
OrdinalMapping::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetGeometryDefinition() == true && mGeometryDefinition == oldid)
  {
    setGeometryDefinition(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
OrdinalMapping::getElementName () const
{
  static const string name = "ordinalMapping";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
OrdinalMapping::getTypeCode () const
{
  return SBML_SPATIAL_ORDINALMAPPING;
}


/*
 * check if all the required attributes are set
 */
bool
OrdinalMapping::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetGeometryDefinition() == false)
    allPresent = false;

  if (isSetOrdinal() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
OrdinalMapping::writeElements (XMLOutputStream& stream) const
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
OrdinalMapping::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
OrdinalMapping::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
OrdinalMapping::enablePackageInternal(const std::string& pkgURI,
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
OrdinalMapping::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("geometryDefinition");
  attributes.add("ordinal");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
OrdinalMapping::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfOrdinalMappings - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfOrdinalMappings*>(getParentSBMLObject())->size() < 2)
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
  // geometryDefinition SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("geometryDefinition", mGeometryDefinition);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mGeometryDefinition.empty() == true)
    {
      logEmptyString(mGeometryDefinition, getLevel(), getVersion(), "<OrdinalMapping>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mGeometryDefinition) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute geometryDefinition='" + mGeometryDefinition + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'geometryDefinition' is missing from 'ordinalMapping' object.";
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
        std::string message = "Spatial attribute 'ordinal' is missing from 'ordinalMapping' object.";
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
OrdinalMapping::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetGeometryDefinition() == true)
    stream.writeAttribute("geometryDefinition", getPrefix(), mGeometryDefinition);

  if (isSetOrdinal() == true)
    stream.writeAttribute("ordinal", getPrefix(), mOrdinal);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfOrdinalMappings::ListOfOrdinalMappings(unsigned int level, 
                        unsigned int version, 
                        unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfOrdinalMappings::ListOfOrdinalMappings(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfOrdinalMappings 
 */
ListOfOrdinalMappings* 
ListOfOrdinalMappings::clone () const
 {
  return new ListOfOrdinalMappings(*this);
}


/*
 * Get a OrdinalMapping from the ListOfOrdinalMappings by index.
*/
OrdinalMapping*
ListOfOrdinalMappings::get(unsigned int n)
{
  return static_cast<OrdinalMapping*>(ListOf::get(n));
}


/*
 * Get a OrdinalMapping from the ListOfOrdinalMappings by index.
 */
const OrdinalMapping*
ListOfOrdinalMappings::get(unsigned int n) const
{
  return static_cast<const OrdinalMapping*>(ListOf::get(n));
}


/*
 * Get a OrdinalMapping from the ListOfOrdinalMappings by id.
 */
OrdinalMapping*
ListOfOrdinalMappings::get(const std::string& sid)
{
	return const_cast<OrdinalMapping*>(
    static_cast<const ListOfOrdinalMappings&>(*this).get(sid));
}


/*
 * Get a OrdinalMapping from the ListOfOrdinalMappings by id.
 */
const OrdinalMapping*
ListOfOrdinalMappings::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<OrdinalMapping>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <OrdinalMapping*> (*result);
}


/**
 * Adds a copy the given "OrdinalMapping" to this ListOfOrdinalMappings.
 *
 * @param om; the OrdinalMapping object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfOrdinalMappings::addOrdinalMapping(const OrdinalMapping* om)
{
  if (om == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (om->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != om->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != om->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(om)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(om);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of OrdinalMapping objects in this ListOfOrdinalMappings.
 *
 * @return the number of OrdinalMapping objects in this ListOfOrdinalMappings
 */
unsigned int 
ListOfOrdinalMappings::getNumOrdinalMappings() const
{
	return size();
}

/**
 * Creates a new OrdinalMapping object, adds it to this ListOfOrdinalMappings
 * OrdinalMapping and returns the OrdinalMapping object created. 
 *
 * @return a new OrdinalMapping object instance
 *
 * @see addOrdinalMapping(const OrdinalMapping* om)
 */
OrdinalMapping* 
ListOfOrdinalMappings::createOrdinalMapping()
{
  OrdinalMapping* om = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    om = new OrdinalMapping(spatialns);
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

  if(om != NULL)
  {
    appendAndOwn(om);
  }

  return om;
}

/*
 * Removes the nth OrdinalMapping from this ListOfOrdinalMappings
 */
OrdinalMapping*
ListOfOrdinalMappings::remove(unsigned int n)
{
  return static_cast<OrdinalMapping*>(ListOf::remove(n));
}


/*
 * Removes the OrdinalMapping from this ListOfOrdinalMappings with the given identifier
 */
OrdinalMapping*
ListOfOrdinalMappings::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<OrdinalMapping>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <OrdinalMapping*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfOrdinalMappings::getElementName () const
{
  static const string name = "listOfOrdinalMappings";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfOrdinalMappings::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfOrdinalMappings::getItemTypeCode () const
{
  return SBML_SPATIAL_ORDINALMAPPING;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new OrdinalMapping in this ListOfOrdinalMappings
 */
SBase*
ListOfOrdinalMappings::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "ordinalMapping")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new OrdinalMapping(spatialns);
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
ListOfOrdinalMappings::writeXMLNS(XMLOutputStream& stream) const
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
OrdinalMapping_t *
OrdinalMapping_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion)
{
  return new OrdinalMapping(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
OrdinalMapping_free(OrdinalMapping_t * om)
{
  if (om != NULL)
    delete om;
}


LIBSBML_EXTERN
OrdinalMapping_t *
OrdinalMapping_clone(OrdinalMapping_t * om)
{
  if (om != NULL)
  {
    return static_cast<OrdinalMapping_t*>(om->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
OrdinalMapping_getGeometryDefinition(const OrdinalMapping_t * om)
{
	return (om != NULL && om->isSetGeometryDefinition()) ? om->getGeometryDefinition().c_str() : NULL;
}


LIBSBML_EXTERN
int
OrdinalMapping_getOrdinal(const OrdinalMapping_t * om)
{
	return (om != NULL) ? om->getOrdinal() : SBML_INT_MAX;
}


LIBSBML_EXTERN
int
OrdinalMapping_isSetGeometryDefinition(const OrdinalMapping_t * om)
{
  return (om != NULL) ? static_cast<int>(om->isSetGeometryDefinition()) : 0;
}


LIBSBML_EXTERN
int
OrdinalMapping_isSetOrdinal(const OrdinalMapping_t * om)
{
  return (om != NULL) ? static_cast<int>(om->isSetOrdinal()) : 0;
}


LIBSBML_EXTERN
int
OrdinalMapping_setGeometryDefinition(OrdinalMapping_t * om, const char * geometryDefinition)
{
  if (om != NULL)
    return (geometryDefinition == NULL) ? om->setGeometryDefinition("") : om->setGeometryDefinition(geometryDefinition);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OrdinalMapping_setOrdinal(OrdinalMapping_t * om, int ordinal)
{
  if (om != NULL)
    return om->setOrdinal(ordinal);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OrdinalMapping_unsetGeometryDefinition(OrdinalMapping_t * om)
{
  return (om != NULL) ? om->unsetGeometryDefinition() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OrdinalMapping_unsetOrdinal(OrdinalMapping_t * om)
{
  return (om != NULL) ? om->unsetOrdinal() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
OrdinalMapping_hasRequiredAttributes(const OrdinalMapping_t * om)
{
  return (om != NULL) ? static_cast<int>(om->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
OrdinalMapping_t *
ListOfOrdinalMappings_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfOrdinalMappings *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
OrdinalMapping_t *
ListOfOrdinalMappings_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfOrdinalMappings *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


