/**
 * @file SampledFieldGeometry.cpp
 * @brief Implementation of the SampledFieldGeometry class.
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
#include <sbml/packages/spatial/sbml/SampledFieldGeometry.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new SampledFieldGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
SampledFieldGeometry::SampledFieldGeometry(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion)
  : GeometryDefinition(level, version, pkgVersion)
  , mSampledVolumes (level, version, pkgVersion)
  , mSampledField ("")
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new SampledFieldGeometry using the given SpatialPkgNamespaces
 * object.
 */
SampledFieldGeometry::SampledFieldGeometry(SpatialPkgNamespaces *spatialns)
  : GeometryDefinition(spatialns)
  , mSampledVolumes (spatialns)
  , mSampledField ("")
{
  setElementNamespace(spatialns->getURI());
  connectToChild();
  loadPlugins(spatialns);
}


/*
 * Copy constructor for SampledFieldGeometry.
 */
SampledFieldGeometry::SampledFieldGeometry(const SampledFieldGeometry& orig)
  : GeometryDefinition( orig )
  , mSampledVolumes ( orig.mSampledVolumes )
  , mSampledField ( orig.mSampledField )
{
  connectToChild();
}


/*
 * Assignment operator for SampledFieldGeometry.
 */
SampledFieldGeometry&
SampledFieldGeometry::operator=(const SampledFieldGeometry& rhs)
{
  if (&rhs != this)
  {
    GeometryDefinition::operator=(rhs);
    mSampledVolumes = rhs.mSampledVolumes;
    mSampledField = rhs.mSampledField;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SampledFieldGeometry object.
 */
SampledFieldGeometry*
SampledFieldGeometry::clone() const
{
  return new SampledFieldGeometry(*this);
}


/*
 * Destructor for SampledFieldGeometry.
 */
SampledFieldGeometry::~SampledFieldGeometry()
{
}


/*
 * Returns the value of the "sampledField" attribute of this
 * SampledFieldGeometry.
 */
const std::string&
SampledFieldGeometry::getSampledField() const
{
  return mSampledField;
}


/*
 * Predicate returning @c true if this SampledFieldGeometry's "sampledField"
 * attribute is set.
 */
bool
SampledFieldGeometry::isSetSampledField() const
{
  return (mSampledField.empty() == false);
}


/*
 * Sets the value of the "sampledField" attribute of this SampledFieldGeometry.
 */
int
SampledFieldGeometry::setSampledField(const std::string& sampledField)
{
  if (!(SyntaxChecker::isValidInternalSId(sampledField)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSampledField = sampledField;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "sampledField" attribute of this
 * SampledFieldGeometry.
 */
int
SampledFieldGeometry::unsetSampledField()
{
  mSampledField.erase();

  if (mSampledField.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the ListOfSampledVolumes from this SampledFieldGeometry.
 */
const ListOfSampledVolumes*
SampledFieldGeometry::getListOfSampledVolumes() const
{
  return &mSampledVolumes;
}


/*
 * Returns the ListOfSampledVolumes from this SampledFieldGeometry.
 */
ListOfSampledVolumes*
SampledFieldGeometry::getListOfSampledVolumes()
{
  return &mSampledVolumes;
}


/*
 * Get a SampledVolume from the SampledFieldGeometry.
 */
SampledVolume*
SampledFieldGeometry::getSampledVolume(unsigned int n)
{
  return mSampledVolumes.get(n);
}


/*
 * Get a SampledVolume from the SampledFieldGeometry.
 */
const SampledVolume*
SampledFieldGeometry::getSampledVolume(unsigned int n) const
{
  return mSampledVolumes.get(n);
}


/*
 * Get a SampledVolume from the SampledFieldGeometry based on its identifier.
 */
SampledVolume*
SampledFieldGeometry::getSampledVolume(const std::string& sid)
{
  return mSampledVolumes.get(sid);
}


/*
 * Get a SampledVolume from the SampledFieldGeometry based on its identifier.
 */
const SampledVolume*
SampledFieldGeometry::getSampledVolume(const std::string& sid) const
{
  return mSampledVolumes.get(sid);
}


/*
 * Get a SampledVolume from the SampledFieldGeometry based on the DomainType to
 * which it refers.
 */
const SampledVolume*
SampledFieldGeometry::getSampledVolumeByDomainType(const std::string& sid)
  const
{
  return mSampledVolumes.getByDomainType(sid);
}


/*
 * Get a SampledVolume from the SampledFieldGeometry based on the DomainType to
 * which it refers.
 */
SampledVolume*
SampledFieldGeometry::getSampledVolumeByDomainType(const std::string& sid)
{
  return mSampledVolumes.getByDomainType(sid);
}


/*
 * Adds a copy of the given SampledVolume to this SampledFieldGeometry.
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
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(sv)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (sv->isSetId() && (mSampledVolumes.get(sv->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mSampledVolumes.append(sv);
  }
}


/*
 * Get the number of SampledVolume objects in this SampledFieldGeometry.
 */
unsigned int
SampledFieldGeometry::getNumSampledVolumes() const
{
  return mSampledVolumes.size();
}


/*
 * Creates a new SampledVolume object, adds it to this SampledFieldGeometry
 * object and returns the SampledVolume object created.
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
  }

  if (sv != NULL)
  {
    mSampledVolumes.appendAndOwn(sv);
  }

  return sv;
}


/*
 * Removes the nth SampledVolume from this SampledFieldGeometry and returns a
 * pointer to it.
 */
SampledVolume*
SampledFieldGeometry::removeSampledVolume(unsigned int n)
{
  return mSampledVolumes.remove(n);
}


/*
 * Removes the SampledVolume from this SampledFieldGeometry based on its
 * identifier and returns a pointer to it.
 */
SampledVolume*
SampledFieldGeometry::removeSampledVolume(const std::string& sid)
{
  return mSampledVolumes.remove(sid);
}


/*
 * @copydoc doc_renamesidref_common
 */
void
SampledFieldGeometry::renameSIdRefs(const std::string& oldid,
                                    const std::string& newid)
{
  if (isSetSampledField() && mSampledField == oldid)
  {
    setSampledField(newid);
  }
}


/*
 * Returns the XML element name of this SampledFieldGeometry object.
 */
const std::string&
SampledFieldGeometry::getElementName() const
{
  static const string name = "sampledFieldGeometry";
  return name;
}


/*
 * Returns the libSBML type code for this SampledFieldGeometry object.
 */
int
SampledFieldGeometry::getTypeCode() const
{
  return SBML_SPATIAL_SAMPLEDFIELDGEOMETRY;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * SampledFieldGeometry object have been set.
 */
bool
SampledFieldGeometry::hasRequiredAttributes() const
{
  bool allPresent = GeometryDefinition::hasRequiredAttributes();

  if (isSetSampledField() == false)
  {
    allPresent = false;
  }

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * SampledFieldGeometry object have been set.
 */
bool
SampledFieldGeometry::hasRequiredElements() const
{
  bool allPresent = GeometryDefinition::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
SampledFieldGeometry::writeElements(XMLOutputStream& stream) const
{
  GeometryDefinition::writeElements(stream);

  if (getNumSampledVolumes() > 0)
  {
    mSampledVolumes.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
SampledFieldGeometry::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mSampledVolumes.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
SampledFieldGeometry::setSBMLDocument(SBMLDocument* d)
{
  GeometryDefinition::setSBMLDocument(d);

  mSampledVolumes.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
SampledFieldGeometry::connectToChild()
{
  GeometryDefinition::connectToChild();

  mSampledVolumes.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
SampledFieldGeometry::enablePackageInternal(const std::string& pkgURI,
                                            const std::string& pkgPrefix,
                                            bool flag)
{
  GeometryDefinition::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mSampledVolumes.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
SampledFieldGeometry::updateSBMLNamespace(const std::string& package,
                                          unsigned int level,
                                          unsigned int version)
{
  GeometryDefinition::updateSBMLNamespace(package, level, version);

  mSampledVolumes.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SampledFieldGeometry.
 */
int
SampledFieldGeometry::getAttribute(const std::string& attributeName,
                                   bool& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SampledFieldGeometry.
 */
int
SampledFieldGeometry::getAttribute(const std::string& attributeName,
                                   int& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SampledFieldGeometry.
 */
int
SampledFieldGeometry::getAttribute(const std::string& attributeName,
                                   double& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SampledFieldGeometry.
 */
int
SampledFieldGeometry::getAttribute(const std::string& attributeName,
                                   unsigned int& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SampledFieldGeometry.
 */
int
SampledFieldGeometry::getAttribute(const std::string& attributeName,
                                   std::string& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "sampledField")
  {
    value = getSampledField();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this SampledFieldGeometry's attribute
 * "attributeName" is set.
 */
bool
SampledFieldGeometry::isSetAttribute(const std::string& attributeName) const
{
  bool value = GeometryDefinition::isSetAttribute(attributeName);

  if (attributeName == "sampledField")
  {
    value = isSetSampledField();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SampledFieldGeometry.
 */
int
SampledFieldGeometry::setAttribute(const std::string& attributeName,
                                   bool value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SampledFieldGeometry.
 */
int
SampledFieldGeometry::setAttribute(const std::string& attributeName,
                                   int value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SampledFieldGeometry.
 */
int
SampledFieldGeometry::setAttribute(const std::string& attributeName,
                                   double value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SampledFieldGeometry.
 */
int
SampledFieldGeometry::setAttribute(const std::string& attributeName,
                                   unsigned int value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SampledFieldGeometry.
 */
int
SampledFieldGeometry::setAttribute(const std::string& attributeName,
                                   const std::string& value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  if (attributeName == "sampledField")
  {
    return_value = setSampledField(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * SampledFieldGeometry.
 */
int
SampledFieldGeometry::unsetAttribute(const std::string& attributeName)
{
  int value = GeometryDefinition::unsetAttribute(attributeName);

  if (attributeName == "sampledField")
  {
    value = unsetSampledField();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * SampledFieldGeometry.
 */
SBase*
SampledFieldGeometry::createChildObject(const std::string& elementName)
{
  GeometryDefinition* obj = NULL;

  if (elementName == "sampledVolume")
  {
    return createSampledVolume();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this SampledFieldGeometry.
 */
int
SampledFieldGeometry::addChildObject(const std::string& elementName,
                                     const SBase* element)
{
  if (elementName == "sampledVolume" && element->getTypeCode() ==
    SBML_SPATIAL_SAMPLEDVOLUME)
  {
    return addSampledVolume((const SampledVolume*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * SampledFieldGeometry.
 */
SBase*
SampledFieldGeometry::removeChildObject(const std::string& elementName,
                                        const std::string& id)
{
  if (elementName == "sampledVolume")
  {
    return removeSampledVolume(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this SampledFieldGeometry.
 */
unsigned int
SampledFieldGeometry::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "sampledVolume")
  {
    return getNumSampledVolumes();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this SampledFieldGeometry.
 */
SBase*
SampledFieldGeometry::getObject(const std::string& elementName,
                                unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "sampledVolume")
  {
    return getSampledVolume(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
SampledFieldGeometry::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mSampledVolumes.getElementBySId(id);

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
SampledFieldGeometry::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mSampledVolumes.getMetaId() == metaid)
  {
    return &mSampledVolumes;
  }

  obj = mSampledVolumes.getElementByMetaId(metaid);

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
SampledFieldGeometry::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mSampledVolumes, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
SampledFieldGeometry::createObject(XMLInputStream& stream)
{
  SBase* obj = GeometryDefinition::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfSampledVolumes")
  {
    if (mSampledVolumes.size() != 0)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialSampledFieldGeometryAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    obj = &mSampledVolumes;
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
SampledFieldGeometry::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GeometryDefinition::addExpectedAttributes(attributes);

  attributes.add("sampledField");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
SampledFieldGeometry::readAttributes(const XMLAttributes& attributes,
                                     const ExpectedAttributes&
                                       expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  GeometryDefinition::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial",
          SpatialSampledFieldGeometryAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialSampledFieldGeometryAllowedCoreAttributes, pkgVersion, level,
            version, details);
      }
    }
  }

  // 
  // sampledField SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("sampledField", mSampledField);

  if (assigned == true)
  {
    if (mSampledField.empty() == true)
    {
      logEmptyString(mSampledField, level, version, "<SampledFieldGeometry>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mSampledField) == false)
    {
      std::string msg = "The sampledField attribute on the <" +
        getElementName() + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mSampledField + "', which does not conform to the "
        "syntax.";
      log->logPackageError("spatial",
        SpatialSampledFieldGeometrySampledFieldMustBeSampledField, pkgVersion,
          level, version, msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'sampledField' is missing from the "
      "<SampledFieldGeometry> element.";
    log->logPackageError("spatial",
      SpatialSampledFieldGeometryAllowedAttributes, pkgVersion, level, version,
        message);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
SampledFieldGeometry::writeAttributes(XMLOutputStream& stream) const
{
  GeometryDefinition::writeAttributes(stream);

  if (isSetSampledField() == true)
  {
    stream.writeAttribute("sampledField", getPrefix(), mSampledField);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new SampledFieldGeometry_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
SampledFieldGeometry_t *
SampledFieldGeometry_create(unsigned int level,
                            unsigned int version,
                            unsigned int pkgVersion)
{
  return new SampledFieldGeometry(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this SampledFieldGeometry_t object.
 */
LIBSBML_EXTERN
SampledFieldGeometry_t*
SampledFieldGeometry_clone(const SampledFieldGeometry_t* sfg)
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


/*
 * Frees this SampledFieldGeometry_t object.
 */
LIBSBML_EXTERN
void
SampledFieldGeometry_free(SampledFieldGeometry_t* sfg)
{
  if (sfg != NULL)
  {
    delete sfg;
  }
}


/*
 * Returns the value of the "sampledField" attribute of this
 * SampledFieldGeometry_t.
 */
LIBSBML_EXTERN
char *
SampledFieldGeometry_getSampledField(const SampledFieldGeometry_t * sfg)
{
  if (sfg == NULL)
  {
    return NULL;
  }

  return sfg->getSampledField().empty() ? NULL :
    safe_strdup(sfg->getSampledField().c_str());
}


/*
 * Predicate returning @c 1 (true) if this SampledFieldGeometry_t's
 * "sampledField" attribute is set.
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_isSetSampledField(const SampledFieldGeometry_t * sfg)
{
  return (sfg != NULL) ? static_cast<int>(sfg->isSetSampledField()) : 0;
}


/*
 * Sets the value of the "sampledField" attribute of this
 * SampledFieldGeometry_t.
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_setSampledField(SampledFieldGeometry_t * sfg,
                                     const char * sampledField)
{
  return (sfg != NULL) ? sfg->setSampledField(sampledField) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "sampledField" attribute of this
 * SampledFieldGeometry_t.
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_unsetSampledField(SampledFieldGeometry_t * sfg)
{
  return (sfg != NULL) ? sfg->unsetSampledField() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing SampledVolume_t objects from this
 * SampledFieldGeometry_t.
 */
LIBSBML_EXTERN
ListOf_t*
SampledFieldGeometry_getListOfSampledVolumes(SampledFieldGeometry_t* sfg)
{
  return (sfg != NULL) ? sfg->getListOfSampledVolumes() : NULL;
}


/*
 * Get a SampledVolume_t from the SampledFieldGeometry_t.
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_getSampledVolume(SampledFieldGeometry_t* sfg,
                                      unsigned int n)
{
  return (sfg != NULL) ? sfg->getSampledVolume(n) : NULL;
}


/*
 * Get a SampledVolume_t from the SampledFieldGeometry_t based on its
 * identifier.
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_getSampledVolumeById(SampledFieldGeometry_t* sfg,
                                          const char *sid)
{
  return (sfg != NULL && sid != NULL) ? sfg->getSampledVolume(sid) : NULL;
}


/*
 * Get a SampledVolume_t from the SampledFieldGeometry_t based on the
 * DomainType to which it refers.
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_getSampledVolumeByDomainType(SampledFieldGeometry_t* sfg,
                                                  const char *sid)
{
  return (sfg != NULL && sid != NULL) ? sfg->getSampledVolumeByDomainType(sid)
    : NULL;
}


/*
 * Adds a copy of the given SampledVolume_t to this SampledFieldGeometry_t.
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_addSampledVolume(SampledFieldGeometry_t* sfg,
                                      const SampledVolume_t* sv)
{
  return (sfg != NULL) ? sfg->addSampledVolume(sv) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of SampledVolume_t objects in this SampledFieldGeometry_t.
 */
LIBSBML_EXTERN
unsigned int
SampledFieldGeometry_getNumSampledVolumes(SampledFieldGeometry_t* sfg)
{
  return (sfg != NULL) ? sfg->getNumSampledVolumes() : SBML_INT_MAX;
}


/*
 * Creates a new SampledVolume_t object, adds it to this SampledFieldGeometry_t
 * object and returns the SampledVolume_t object created.
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_createSampledVolume(SampledFieldGeometry_t* sfg)
{
  return (sfg != NULL) ? sfg->createSampledVolume() : NULL;
}


/*
 * Removes the nth SampledVolume_t from this SampledFieldGeometry_t and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_removeSampledVolume(SampledFieldGeometry_t* sfg,
                                         unsigned int n)
{
  return (sfg != NULL) ? sfg->removeSampledVolume(n) : NULL;
}


/*
 * Removes the SampledVolume_t from this SampledFieldGeometry_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_removeSampledVolumeById(SampledFieldGeometry_t* sfg,
                                             const char* sid)
{
  return (sfg != NULL && sid != NULL) ? sfg->removeSampledVolume(sid) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SampledFieldGeometry_t object have been set.
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_hasRequiredAttributes(const SampledFieldGeometry_t * sfg)
{
  return (sfg != NULL) ? static_cast<int>(sfg->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * SampledFieldGeometry_t object have been set.
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_hasRequiredElements(const SampledFieldGeometry_t * sfg)
{
  return (sfg != NULL) ? static_cast<int>(sfg->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


