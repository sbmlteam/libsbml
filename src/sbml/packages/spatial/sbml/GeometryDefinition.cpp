/**
 * @file:   GeometryDefinition.cpp
 * @brief:  Implementation of the GeometryDefinition class
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


#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/spatial/sbml/AnalyticGeometry.h>
#include <sbml/packages/spatial/sbml/SampledFieldGeometry.h>
#include <sbml/packages/spatial/sbml/CSGeometry.h>
#include <sbml/packages/spatial/sbml/ParametricGeometry.h>
#include <sbml/packages/spatial/sbml/MixedGeometry.h>



using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new GeometryDefinition with the given level, version, and package version.
 */
GeometryDefinition::GeometryDefinition (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mIsActive (false)
  , mIsSetIsActive (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new GeometryDefinition with the given SpatialPkgNamespaces object.
 */
GeometryDefinition::GeometryDefinition (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mIsActive (false)
  , mIsSetIsActive (false)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for GeometryDefinition.
 */
GeometryDefinition::GeometryDefinition (const GeometryDefinition& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mIsActive  = orig.mIsActive;
    mIsSetIsActive  = orig.mIsSetIsActive;
  }
}


/*
 * Assignment for GeometryDefinition.
 */
GeometryDefinition&
GeometryDefinition::operator=(const GeometryDefinition& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mIsActive  = rhs.mIsActive;
    mIsSetIsActive  = rhs.mIsSetIsActive;
  }
  return *this;
}


/*
 * Clone for GeometryDefinition.
 */
GeometryDefinition*
GeometryDefinition::clone () const
{
  return new GeometryDefinition(*this);
}


/*
 * Destructor for GeometryDefinition.
 */
GeometryDefinition::~GeometryDefinition ()
{
}


/*
 * Returns the value of the "id" attribute of this GeometryDefinition.
 */
const std::string&
GeometryDefinition::getId() const
{
  return mId;
}


/*
 * Returns the value of the "isActive" attribute of this GeometryDefinition.
 */
bool
GeometryDefinition::getIsActive() const
{
  return mIsActive;
}


/*
 * Returns true/false if id is set.
 */
bool
GeometryDefinition::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if isActive is set.
 */
bool
GeometryDefinition::isSetIsActive() const
{
  return mIsSetIsActive;
}


/*
 * Sets id and returns value indicating success.
 */
int
GeometryDefinition::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets isActive and returns value indicating success.
 */
int
GeometryDefinition::setIsActive(bool isActive)
{
  mIsActive = isActive;
  mIsSetIsActive = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
 */
int
GeometryDefinition::unsetId()
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
 * Unsets isActive and returns value indicating success.
 */
int
GeometryDefinition::unsetIsActive()
{
  mIsActive = false;
  mIsSetIsActive = false;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Return @c true if of type AnalyticGeometry.
 */
bool
GeometryDefinition::isAnalyticGeometry() const
{
  return dynamic_cast<const AnalyticGeometry*>(this) != NULL;
}


/*
 * Return @c true if of type SampledFieldGeometry.
 */
bool
GeometryDefinition::isSampledFieldGeometry() const
{
  return dynamic_cast<const SampledFieldGeometry*>(this) != NULL;
}


/*
 * Return @c true if of type CSGeometry.
 */
bool
GeometryDefinition::isCSGeometry() const
{
  return dynamic_cast<const CSGeometry*>(this) != NULL;
}


/*
 * Return @c true if of type ParametricGeometry.
 */
bool
GeometryDefinition::isParametricGeometry() const
{
  return dynamic_cast<const ParametricGeometry*>(this) != NULL;
}


/*
 * Return @c true if of type MixedGeometry.
 */
bool
GeometryDefinition::isMixedGeometry() const
{
  return dynamic_cast<const MixedGeometry*>(this) != NULL;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
GeometryDefinition::getElementName () const
{
  static const string name = "geometryDefinition";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
GeometryDefinition::getTypeCode () const
{
  return SBML_SPATIAL_GEOMETRYDEFINITION;
}


/*
 * check if all the required attributes are set
 */
bool
GeometryDefinition::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetIsActive() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
GeometryDefinition::writeElements (XMLOutputStream& stream) const
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
GeometryDefinition::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
GeometryDefinition::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
GeometryDefinition::enablePackageInternal(const std::string& pkgURI,
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
GeometryDefinition::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("isActive");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
GeometryDefinition::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfGeometryDefinitions - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfGeometryDefinitions*>(getParentSBMLObject())->size() < 2)
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
      logEmptyString(mId, getLevel(), getVersion(), "<GeometryDefinition>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'geometryDefinition' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // isActive bool   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetIsActive = attributes.readInto("isActive", mIsActive);

  if (mIsSetIsActive == false)
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
        std::string message = "Spatial attribute 'isActive' is missing from 'geometryDefinition' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
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
GeometryDefinition::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetIsActive() == true)
    stream.writeAttribute("isActive", getPrefix(), mIsActive);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfGeometryDefinitions::ListOfGeometryDefinitions(unsigned int level, 
                            unsigned int version, 
                            unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfGeometryDefinitions::ListOfGeometryDefinitions(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfGeometryDefinitions 
 */
ListOfGeometryDefinitions* 
ListOfGeometryDefinitions::clone () const
 {
  return new ListOfGeometryDefinitions(*this);
}


/*
 * Get a GeometryDefinition from the ListOfGeometryDefinitions by index.
*/
GeometryDefinition*
ListOfGeometryDefinitions::get(unsigned int n)
{
  return static_cast<GeometryDefinition*>(ListOf::get(n));
}


/*
 * Get a GeometryDefinition from the ListOfGeometryDefinitions by index.
 */
const GeometryDefinition*
ListOfGeometryDefinitions::get(unsigned int n) const
{
  return static_cast<const GeometryDefinition*>(ListOf::get(n));
}


/*
 * Get a GeometryDefinition from the ListOfGeometryDefinitions by id.
 */
GeometryDefinition*
ListOfGeometryDefinitions::get(const std::string& sid)
{
	return const_cast<GeometryDefinition*>(
    static_cast<const ListOfGeometryDefinitions&>(*this).get(sid));
}


/*
 * Get a GeometryDefinition from the ListOfGeometryDefinitions by id.
 */
const GeometryDefinition*
ListOfGeometryDefinitions::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<GeometryDefinition>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <GeometryDefinition*> (*result);
}


/**
 * Adds a copy the given "GeometryDefinition" to this ListOfGeometryDefinitions.
 *
 * @param gd; the GeometryDefinition object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfGeometryDefinitions::addGeometryDefinition(const GeometryDefinition* gd)
{
  if (gd == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (gd->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != gd->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != gd->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(gd)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(gd);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of GeometryDefinition objects in this ListOfGeometryDefinitions.
 *
 * @return the number of GeometryDefinition objects in this ListOfGeometryDefinitions
 */
unsigned int 
ListOfGeometryDefinitions::getNumGeometryDefinitions() const
{
	return size();
}

/**
 * Creates a new AnalyticGeometry object, adds it to this ListOfGeometryDefinitions
 * analyticGeometry and returns the AnalyticGeometry object created. 
 *
 * @return a new AnalyticGeometry object instance
 *
 * @see addAnalyticGeometry(const GeometryDefinition* gd)
 */
AnalyticGeometry* 
ListOfGeometryDefinitions::createAnalyticGeometry()
{
  AnalyticGeometry* ag = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    ag = new AnalyticGeometry(spatialns);
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

  if(ag != NULL)
  {
    appendAndOwn(ag);
  }

  return ag;
}

/**
 * Creates a new SampledFieldGeometry object, adds it to this ListOfGeometryDefinitions
 * sampledFieldGeometry and returns the SampledFieldGeometry object created. 
 *
 * @return a new SampledFieldGeometry object instance
 *
 * @see addSampledFieldGeometry(const GeometryDefinition* gd)
 */
SampledFieldGeometry* 
ListOfGeometryDefinitions::createSampledFieldGeometry()
{
  SampledFieldGeometry* sfg = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    sfg = new SampledFieldGeometry(spatialns);
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

  if(sfg != NULL)
  {
    appendAndOwn(sfg);
  }

  return sfg;
}

/**
 * Creates a new CSGeometry object, adds it to this ListOfGeometryDefinitions
 * csGeometry and returns the CSGeometry object created. 
 *
 * @return a new CSGeometry object instance
 *
 * @see addCsGeometry(const GeometryDefinition* gd)
 */
CSGeometry* 
ListOfGeometryDefinitions::createCsGeometry()
{
  CSGeometry* csg = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csg = new CSGeometry(spatialns);
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

  if(csg != NULL)
  {
    appendAndOwn(csg);
  }

  return csg;
}

/**
 * Creates a new ParametricGeometry object, adds it to this ListOfGeometryDefinitions
 * parametricGeometry and returns the ParametricGeometry object created. 
 *
 * @return a new ParametricGeometry object instance
 *
 * @see addParametricGeometry(const GeometryDefinition* gd)
 */
ParametricGeometry* 
ListOfGeometryDefinitions::createParametricGeometry()
{
  ParametricGeometry* pg = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    pg = new ParametricGeometry(spatialns);
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

  if(pg != NULL)
  {
    appendAndOwn(pg);
  }

  return pg;
}

/**
 * Creates a new MixedGeometry object, adds it to this ListOfGeometryDefinitions
 * mixedGeometry and returns the MixedGeometry object created. 
 *
 * @return a new MixedGeometry object instance
 *
 * @see addMixedGeometry(const GeometryDefinition* gd)
 */
MixedGeometry* 
ListOfGeometryDefinitions::createMixedGeometry()
{
  MixedGeometry* mg = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    mg = new MixedGeometry(spatialns);
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

  if(mg != NULL)
  {
    appendAndOwn(mg);
  }

  return mg;
}

/*
 * Removes the nth GeometryDefinition from this ListOfGeometryDefinitions
 */
GeometryDefinition*
ListOfGeometryDefinitions::remove(unsigned int n)
{
  return static_cast<GeometryDefinition*>(ListOf::remove(n));
}


/*
 * Removes the GeometryDefinition from this ListOfGeometryDefinitions with the given identifier
 */
GeometryDefinition*
ListOfGeometryDefinitions::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<GeometryDefinition>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <GeometryDefinition*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfGeometryDefinitions::getElementName () const
{
  static const string name = "listOfGeometryDefinitions";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfGeometryDefinitions::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfGeometryDefinitions::getItemTypeCode () const
{
  return SBML_SPATIAL_GEOMETRYDEFINITION;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new GeometryDefinition in this ListOfGeometryDefinitions
 */
SBase*
ListOfGeometryDefinitions::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "geometryDefinition")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new GeometryDefinition(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  if (name == "analyticGeometry")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new AnalyticGeometry(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  if (name == "sampledFieldGeometry")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new SampledFieldGeometry(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  if (name == "csGeometry")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new CSGeometry(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  if (name == "parametricGeometry")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new ParametricGeometry(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  if (name == "mixedGeometry")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new MixedGeometry(spatialns);
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
ListOfGeometryDefinitions::writeXMLNS(XMLOutputStream& stream) const
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
GeometryDefinition_t *
GeometryDefinition_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion)
{
  return new GeometryDefinition(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
GeometryDefinition_free(GeometryDefinition_t * gd)
{
  if (gd != NULL)
    delete gd;
}


LIBSBML_EXTERN
GeometryDefinition_t *
GeometryDefinition_clone(GeometryDefinition_t * gd)
{
  if (gd != NULL)
  {
    return static_cast<GeometryDefinition_t*>(gd->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
GeometryDefinition_getId(const GeometryDefinition_t * gd)
{
	return (gd != NULL && gd->isSetId()) ? gd->getId().c_str() : NULL;
}


LIBSBML_EXTERN
int
GeometryDefinition_getIsActive(const GeometryDefinition_t * gd)
{
	return (gd != NULL) ? static_cast<int>(gd->getIsActive()) : 0;
}


LIBSBML_EXTERN
int
GeometryDefinition_isSetId(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->isSetId()) : 0;
}


LIBSBML_EXTERN
int
GeometryDefinition_isSetIsActive(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->isSetIsActive()) : 0;
}


LIBSBML_EXTERN
int
GeometryDefinition_setId(GeometryDefinition_t * gd, const char * id)
{
  if (gd != NULL)
    return (id == NULL) ? gd->setId("") : gd->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeometryDefinition_setIsActive(GeometryDefinition_t * gd, int isActive)
{
  if (gd != NULL)
    return gd->setIsActive(isActive);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeometryDefinition_unsetId(GeometryDefinition_t * gd)
{
  return (gd != NULL) ? gd->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeometryDefinition_unsetIsActive(GeometryDefinition_t * gd)
{
  return (gd != NULL) ? gd->unsetIsActive() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeometryDefinition_hasRequiredAttributes(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
GeometryDefinition_t *
ListOfGeometryDefinitions_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfGeometryDefinitions *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
GeometryDefinition_t *
ListOfGeometryDefinitions_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfGeometryDefinitions *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


