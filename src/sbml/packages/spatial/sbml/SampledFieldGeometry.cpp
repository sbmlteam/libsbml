/**
 * @file:   SampledFieldGeometry.cpp
 * @brief:  Implementation of the SampledFieldGeometry class
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


#include <sbml/packages/spatial/sbml/SampledFieldGeometry.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SampledFieldGeometry with the given level, version, and package version.
 */
SampledFieldGeometry::SampledFieldGeometry (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : GeometryDefinition(level, version)
  , mSampledVolumes (level, version, pkgVersion)
  , mSampledField (NULL)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new SampledFieldGeometry with the given SpatialPkgNamespaces object.
 */
SampledFieldGeometry::SampledFieldGeometry (SpatialPkgNamespaces* spatialns)
  : GeometryDefinition(spatialns)
  , mSampledVolumes (spatialns)
  , mSampledField (NULL)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for SampledFieldGeometry.
 */
SampledFieldGeometry::SampledFieldGeometry (const SampledFieldGeometry& orig)
  : GeometryDefinition(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mSampledVolumes  = orig.mSampledVolumes;
    if (orig.mSampledField != NULL)
    {
      mSampledField = orig.mSampledField->clone();
    }
    else
    {
      mSampledField = NULL;
    }

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for SampledFieldGeometry.
 */
SampledFieldGeometry&
SampledFieldGeometry::operator=(const SampledFieldGeometry& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    GeometryDefinition::operator=(rhs);
    mSampledVolumes  = rhs.mSampledVolumes;
    if (rhs.mSampledField != NULL)
    {
      mSampledField = rhs.mSampledField->clone();
    }
    else
    {
      mSampledField = NULL;
    }

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for SampledFieldGeometry.
 */
SampledFieldGeometry*
SampledFieldGeometry::clone () const
{
  return new SampledFieldGeometry(*this);
}


/*
 * Destructor for SampledFieldGeometry.
 */
SampledFieldGeometry::~SampledFieldGeometry ()
{
  delete mSampledField;
  mSampledField = NULL;
}


/*
 * Returns the value of the "sampledField" attribute of this SampledFieldGeometry.
 */
const SampledField*
SampledFieldGeometry::getSampledField() const
{
  return mSampledField;
}


/*
 * Returns the value of the "sampledField" attribute of this SampledFieldGeometry.
 */
SampledField*
SampledFieldGeometry::getSampledField()
{
  return mSampledField;
}


/*
 * Creates a new "sampledField" element of this SampledFieldGeometry and returns it.
 */
SampledField*
SampledFieldGeometry::createSampledField()
{
  if (mSampledField != NULL) delete mSampledField;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mSampledField = new SampledField(spatialns);
  delete spatialns;
  connectToChild();
  return mSampledField;
}


/*
 * Returns true/false if sampledField is set.
 */
bool
SampledFieldGeometry::isSetSampledField() const
{
  return (mSampledField != NULL);
}


/*
 * Sets sampledField and returns value indicating success.
 */
int
SampledFieldGeometry::setSampledField(SampledField* sampledField)
{
  if (mSampledField == sampledField)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (sampledField == NULL)
  {
    delete mSampledField;
    mSampledField = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mSampledField;
    mSampledField = (sampledField != NULL) ?
      static_cast<SampledField*>(sampledField->clone()) : NULL;
    if (mSampledField != NULL)
    {
      mSampledField->connectToParent(this);
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets sampledField and returns value indicating success.
 */
int
SampledFieldGeometry::unsetSampledField()
{
  delete mSampledField;
  mSampledField = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the  "ListOfSampledVolumes" in this SampledFieldGeometry object.
 */
const ListOfSampledVolumes*
SampledFieldGeometry::getListOfSampledVolumes() const
{
  return &mSampledVolumes;
}


/*
 * Returns the  "ListOfSampledVolumes" in this SampledFieldGeometry object.
 */
ListOfSampledVolumes*
SampledFieldGeometry::getListOfSampledVolumes()
{
  return &mSampledVolumes;
}


/*
 * Removes the nth SampledVolume from the ListOfSampledVolumes.
 */
SampledVolume*
SampledFieldGeometry::removeSampledVolume(unsigned int n)
{
	return mSampledVolumes.remove(n);
}


/*
 * Removes the a SampledVolume with given id from the ListOfSampledVolumes.
 */
SampledVolume*
SampledFieldGeometry::removeSampledVolume(const std::string& sid)
{
	return mSampledVolumes.remove(sid);
}


/*
 * Return the nth SampledVolume in the ListOfSampledVolumes within this SampledFieldGeometry.
 */
SampledVolume*
SampledFieldGeometry::getSampledVolume(unsigned int n)
{
	return mSampledVolumes.get(n);
}


/*
 * Return the nth SampledVolume in the ListOfSampledVolumes within this SampledFieldGeometry.
 */
const SampledVolume*
SampledFieldGeometry::getSampledVolume(unsigned int n) const
{
	return mSampledVolumes.get(n);
}


/*
 * Return a SampledVolume from the ListOfSampledVolumes by id.
 */
SampledVolume*
SampledFieldGeometry::getSampledVolume(const std::string& sid)
{
	return mSampledVolumes.get(sid);
}


/*
 * Return a SampledVolume from the ListOfSampledVolumes by id.
 */
const SampledVolume*
SampledFieldGeometry::getSampledVolume(const std::string& sid) const
{
	return mSampledVolumes.get(sid);
}


/*
 * Adds a copy the given "SampledVolume" to this SampledFieldGeometry.
 *
 * @param sv; the SampledVolume object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
SampledFieldGeometry::addSampledVolume(const SampledVolume* sv)
{
  if (sv == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (sv->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sv->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sv->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(sv)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mSampledVolumes.append(sv);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of SampledVolume objects in this SampledFieldGeometry.
 *
 * @return the number of SampledVolume objects in this SampledFieldGeometry
 */
unsigned int
SampledFieldGeometry::getNumSampledVolumes() const
{
  return mSampledVolumes.size();
}


/*
 * Creates a new SampledVolume object, adds it to this SampledFieldGeometrys
 * SampledFieldGeometry and returns the SampledVolume object created. 
 *
 * @return a new SampledVolume object instance
 *
 * @see addSampledVolume(const SampledVolume* sv)
 */
SampledVolume*
SampledFieldGeometry::createSampledVolume()
{
  SampledVolume* sv = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    sv = new SampledVolume(spatialns);
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

  if(sv != NULL)
  {
    mSampledVolumes.appendAndOwn(sv);
  }

  return sv;
}


List*
SampledFieldGeometry::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mSampledField, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
SampledFieldGeometry::getElementName () const
{
  static const string name = "sampledFieldGeometry";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SampledFieldGeometry::getTypeCode () const
{
  return SBML_SPATIAL_SAMPLEDFIELDGEOMETRY;
}


/*
 * check if all the required attributes are set
 */
bool
SampledFieldGeometry::hasRequiredAttributes () const
{
  bool allPresent = GeometryDefinition::hasRequiredAttributes();

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
SampledFieldGeometry::hasRequiredElements () const
{
  bool allPresent = GeometryDefinition::hasRequiredElements();

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
SampledFieldGeometry::writeElements (XMLOutputStream& stream) const
{
  GeometryDefinition::writeElements(stream);
  if (getNumSampledVolumes() > 0)
  {
    mSampledVolumes.write(stream);
  }

  if (isSetSampledField() == true)
  {
    mSampledField->write(stream);
  }
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
SampledFieldGeometry::accept (SBMLVisitor& v) const
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
SampledFieldGeometry::setSBMLDocument (SBMLDocument* d)
{
  GeometryDefinition::setSBMLDocument(d);
  mSampledVolumes.setSBMLDocument(d);
  if (mSampledField != NULL)
    mSampledField->setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
SampledFieldGeometry::connectToChild()
{
  GeometryDefinition::connectToChild();

  mSampledVolumes.connectToParent(this);
  if (mSampledField != NULL)
    mSampledField->connectToParent(this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
SampledFieldGeometry::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  GeometryDefinition::enablePackageInternal(pkgURI, pkgPrefix, flag);
  mSampledVolumes.enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
SampledFieldGeometry::createObject(XMLInputStream& stream)
{
  SBase* object = GeometryDefinition::createObject(stream);

  const string& name = stream.peek().getName();

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "listOfSampledVolumes")
  {
    object = &mSampledVolumes;
  }
  else if (name == "sampledField")
  {
    mSampledField = new SampledField(spatialns);
    object = mSampledField;
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
SampledFieldGeometry::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GeometryDefinition::addExpectedAttributes(attributes);

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SampledFieldGeometry::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  GeometryDefinition::readAttributes(attributes, expectedAttributes);

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
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
    }
  }

  bool assigned = false;

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
SampledFieldGeometry::writeAttributes (XMLOutputStream& stream) const
{
  GeometryDefinition::writeAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
SampledFieldGeometry_t *
SampledFieldGeometry_create(unsigned int level, unsigned int version,
                            unsigned int pkgVersion)
{
  return new SampledFieldGeometry(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
SampledFieldGeometry_free(SampledFieldGeometry_t * sfg)
{
  if (sfg != NULL)
    delete sfg;
}


LIBSBML_EXTERN
SampledFieldGeometry_t *
SampledFieldGeometry_clone(SampledFieldGeometry_t * sfg)
{
  if (sfg != NULL)
  {
    return static_cast<SampledFieldGeometry_t*>(sfg->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
SampledField_t*
SampledFieldGeometry_getSampledField(SampledFieldGeometry_t * sfg)
{
	if (sfg == NULL)
		return NULL;

	return (SampledField_t*)sfg->getSampledField();
}


LIBSBML_EXTERN
SampledField_t*
SampledFieldGeometry_createSampledField(SampledFieldGeometry_t * sfg)
{
	if (sfg == NULL)
		return NULL;

	return (SampledField_t*)sfg->createSampledField();
}


LIBSBML_EXTERN
int
SampledFieldGeometry_isSetSampledField(const SampledFieldGeometry_t * sfg)
{
  return (sfg != NULL) ? static_cast<int>(sfg->isSetSampledField()) : 0;
}


LIBSBML_EXTERN
int
SampledFieldGeometry_setSampledField(SampledFieldGeometry_t * sfg, SampledField_t* sampledField)
{
	return (sfg != NULL) ? sfg->setSampledField(sampledField) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledFieldGeometry_addSampledVolume(SampledFieldGeometry_t * sfg, SampledVolume_t * sv)
{
	return  (sfg != NULL) ? sfg->addSampledVolume(sv) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_createSampledVolume(SampledFieldGeometry_t * sfg)
{
	return  (sfg != NULL) ? sfg->createSampledVolume() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
SampledFieldGeometry_getListOfSampledVolumes(SampledFieldGeometry_t * sfg)
{
	return  (sfg != NULL) ? (ListOf_t *)sfg->getListOfSampledVolumes() : NULL;
}

LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_getSampledVolume(SampledFieldGeometry_t * sfg, unsigned int n)
{
	return  (sfg != NULL) ? sfg->getSampledVolume(n) : NULL;
}

LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_getSampledVolumeById(SampledFieldGeometry_t * sfg, const char * sid)
{
	return  (sfg != NULL) ? sfg->getSampledVolume(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
SampledFieldGeometry_getNumSampledVolumes(SampledFieldGeometry_t * sfg)
{
	return  (sfg != NULL) ? sfg->getNumSampledVolumes() : SBML_INT_MAX;
}

LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_removeSampledVolume(SampledFieldGeometry_t * sfg, unsigned int n)
{
	return  (sfg != NULL) ? sfg->removeSampledVolume(n) : NULL;
}

LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_removeSampledVolumeById(SampledFieldGeometry_t * sfg, const char * sid)
{
	return  (sfg != NULL) ? sfg->removeSampledVolume(sid) : NULL;
}

LIBSBML_EXTERN
int
SampledFieldGeometry_hasRequiredAttributes(const SampledFieldGeometry_t * sfg)
{
  return (sfg != NULL) ? static_cast<int>(sfg->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
SampledFieldGeometry_hasRequiredElements(const SampledFieldGeometry_t * sfg)
{
	return (sfg != NULL) ? static_cast<int>(sfg->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


