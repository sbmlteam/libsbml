/**
 * @file AnalyticGeometry.cpp
 * @brief Implementation of the AnalyticGeometry class.
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
#include <sbml/packages/spatial/sbml/AnalyticGeometry.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new AnalyticGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
AnalyticGeometry::AnalyticGeometry(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion)
  : GeometryDefinition(level, version, pkgVersion)
  , mAnalyticVolumes (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new AnalyticGeometry using the given SpatialPkgNamespaces object.
 */
AnalyticGeometry::AnalyticGeometry(SpatialPkgNamespaces *spatialns)
  : GeometryDefinition(spatialns)
  , mAnalyticVolumes (spatialns)
{
  setElementNamespace(spatialns->getURI());
  connectToChild();
  loadPlugins(spatialns);
}


/*
 * Copy constructor for AnalyticGeometry.
 */
AnalyticGeometry::AnalyticGeometry(const AnalyticGeometry& orig)
  : GeometryDefinition( orig )
  , mAnalyticVolumes ( orig.mAnalyticVolumes )
{
  connectToChild();
}


/*
 * Assignment operator for AnalyticGeometry.
 */
AnalyticGeometry&
AnalyticGeometry::operator=(const AnalyticGeometry& rhs)
{
  if (&rhs != this)
  {
    GeometryDefinition::operator=(rhs);
    mAnalyticVolumes = rhs.mAnalyticVolumes;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this AnalyticGeometry object.
 */
AnalyticGeometry*
AnalyticGeometry::clone() const
{
  return new AnalyticGeometry(*this);
}


/*
 * Destructor for AnalyticGeometry.
 */
AnalyticGeometry::~AnalyticGeometry()
{
}


/*
 * Returns the ListOfAnalyticVolumes from this AnalyticGeometry.
 */
const ListOfAnalyticVolumes*
AnalyticGeometry::getListOfAnalyticVolumes() const
{
  return &mAnalyticVolumes;
}


/*
 * Returns the ListOfAnalyticVolumes from this AnalyticGeometry.
 */
ListOfAnalyticVolumes*
AnalyticGeometry::getListOfAnalyticVolumes()
{
  return &mAnalyticVolumes;
}


/*
 * Get an AnalyticVolume from the AnalyticGeometry.
 */
AnalyticVolume*
AnalyticGeometry::getAnalyticVolume(unsigned int n)
{
  return mAnalyticVolumes.get(n);
}


/*
 * Get an AnalyticVolume from the AnalyticGeometry.
 */
const AnalyticVolume*
AnalyticGeometry::getAnalyticVolume(unsigned int n) const
{
  return mAnalyticVolumes.get(n);
}


/*
 * Get an AnalyticVolume from the AnalyticGeometry based on its identifier.
 */
AnalyticVolume*
AnalyticGeometry::getAnalyticVolume(const std::string& sid)
{
  return mAnalyticVolumes.get(sid);
}


/*
 * Get an AnalyticVolume from the AnalyticGeometry based on its identifier.
 */
const AnalyticVolume*
AnalyticGeometry::getAnalyticVolume(const std::string& sid) const
{
  return mAnalyticVolumes.get(sid);
}


/*
 * Get an AnalyticVolume from the AnalyticGeometry based on the DomainType to
 * which it refers.
 */
const AnalyticVolume*
AnalyticGeometry::getAnalyticVolumeByDomainType(const std::string& sid) const
{
  return mAnalyticVolumes.getByDomainType(sid);
}


/*
 * Get an AnalyticVolume from the AnalyticGeometry based on the DomainType to
 * which it refers.
 */
AnalyticVolume*
AnalyticGeometry::getAnalyticVolumeByDomainType(const std::string& sid)
{
  return mAnalyticVolumes.getByDomainType(sid);
}


/*
 * Adds a copy of the given AnalyticVolume to this AnalyticGeometry.
 */
int
AnalyticGeometry::addAnalyticVolume(const AnalyticVolume* av)
{
  if (av == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (av->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (av->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != av->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != av->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(av)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (av->isSetId() && (mAnalyticVolumes.get(av->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mAnalyticVolumes.append(av);
  }
}


/*
 * Get the number of AnalyticVolume objects in this AnalyticGeometry.
 */
unsigned int
AnalyticGeometry::getNumAnalyticVolumes() const
{
  return mAnalyticVolumes.size();
}


/*
 * Creates a new AnalyticVolume object, adds it to this AnalyticGeometry object
 * and returns the AnalyticVolume object created.
 */
AnalyticVolume*
AnalyticGeometry::createAnalyticVolume()
{
  AnalyticVolume* av = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    av = new AnalyticVolume(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (av != NULL)
  {
    mAnalyticVolumes.appendAndOwn(av);
  }

  return av;
}


/*
 * Removes the nth AnalyticVolume from this AnalyticGeometry and returns a
 * pointer to it.
 */
AnalyticVolume*
AnalyticGeometry::removeAnalyticVolume(unsigned int n)
{
  return mAnalyticVolumes.remove(n);
}


/*
 * Removes the AnalyticVolume from this AnalyticGeometry based on its
 * identifier and returns a pointer to it.
 */
AnalyticVolume*
AnalyticGeometry::removeAnalyticVolume(const std::string& sid)
{
  return mAnalyticVolumes.remove(sid);
}


/*
 * Returns the XML element name of this AnalyticGeometry object.
 */
const std::string&
AnalyticGeometry::getElementName() const
{
  static const string name = "analyticGeometry";
  return name;
}


/*
 * Returns the libSBML type code for this AnalyticGeometry object.
 */
int
AnalyticGeometry::getTypeCode() const
{
  return SBML_SPATIAL_ANALYTICGEOMETRY;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * AnalyticGeometry object have been set.
 */
bool
AnalyticGeometry::hasRequiredAttributes() const
{
  bool allPresent = GeometryDefinition::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * AnalyticGeometry object have been set.
 */
bool
AnalyticGeometry::hasRequiredElements() const
{
  bool allPresent = GeometryDefinition::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
AnalyticGeometry::writeElements(XMLOutputStream& stream) const
{
  GeometryDefinition::writeElements(stream);

  if (getNumAnalyticVolumes() > 0)
  {
    mAnalyticVolumes.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
AnalyticGeometry::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mAnalyticVolumes.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
AnalyticGeometry::setSBMLDocument(SBMLDocument* d)
{
  GeometryDefinition::setSBMLDocument(d);

  mAnalyticVolumes.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
AnalyticGeometry::connectToChild()
{
  GeometryDefinition::connectToChild();

  mAnalyticVolumes.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
AnalyticGeometry::enablePackageInternal(const std::string& pkgURI,
                                        const std::string& pkgPrefix,
                                        bool flag)
{
  GeometryDefinition::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mAnalyticVolumes.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
AnalyticGeometry::updateSBMLNamespace(const std::string& package,
                                      unsigned int level,
                                      unsigned int version)
{
  GeometryDefinition::updateSBMLNamespace(package, level, version);

  mAnalyticVolumes.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this AnalyticGeometry.
 */
int
AnalyticGeometry::getAttribute(const std::string& attributeName,
                               bool& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this AnalyticGeometry.
 */
int
AnalyticGeometry::getAttribute(const std::string& attributeName,
                               int& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this AnalyticGeometry.
 */
int
AnalyticGeometry::getAttribute(const std::string& attributeName,
                               double& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this AnalyticGeometry.
 */
int
AnalyticGeometry::getAttribute(const std::string& attributeName,
                               unsigned int& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this AnalyticGeometry.
 */
int
AnalyticGeometry::getAttribute(const std::string& attributeName,
                               std::string& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this AnalyticGeometry's attribute
 * "attributeName" is set.
 */
bool
AnalyticGeometry::isSetAttribute(const std::string& attributeName) const
{
  bool value = GeometryDefinition::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this AnalyticGeometry.
 */
int
AnalyticGeometry::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this AnalyticGeometry.
 */
int
AnalyticGeometry::setAttribute(const std::string& attributeName, int value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this AnalyticGeometry.
 */
int
AnalyticGeometry::setAttribute(const std::string& attributeName, double value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this AnalyticGeometry.
 */
int
AnalyticGeometry::setAttribute(const std::string& attributeName,
                               unsigned int value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this AnalyticGeometry.
 */
int
AnalyticGeometry::setAttribute(const std::string& attributeName,
                               const std::string& value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this AnalyticGeometry.
 */
int
AnalyticGeometry::unsetAttribute(const std::string& attributeName)
{
  int value = GeometryDefinition::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this AnalyticGeometry.
 */
SBase*
AnalyticGeometry::createChildObject(const std::string& elementName)
{
  GeometryDefinition* obj = NULL;

  if (elementName == "analyticVolume")
  {
    return createAnalyticVolume();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this AnalyticGeometry.
 */
int
AnalyticGeometry::addChildObject(const std::string& elementName,
                                 const SBase* element)
{
  if (elementName == "analyticVolume" && element->getTypeCode() ==
    SBML_SPATIAL_ANALYTICVOLUME)
  {
    return addAnalyticVolume((const AnalyticVolume*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * AnalyticGeometry.
 */
SBase*
AnalyticGeometry::removeChildObject(const std::string& elementName,
                                    const std::string& id)
{
  if (elementName == "analyticVolume")
  {
    return removeAnalyticVolume(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this AnalyticGeometry.
 */
unsigned int
AnalyticGeometry::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "analyticVolume")
  {
    return getNumAnalyticVolumes();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this AnalyticGeometry.
 */
SBase*
AnalyticGeometry::getObject(const std::string& elementName,
                            unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "analyticVolume")
  {
    return getAnalyticVolume(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
AnalyticGeometry::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mAnalyticVolumes.getElementBySId(id);

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
AnalyticGeometry::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mAnalyticVolumes.getMetaId() == metaid)
  {
    return &mAnalyticVolumes;
  }

  obj = mAnalyticVolumes.getElementByMetaId(metaid);

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
AnalyticGeometry::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mAnalyticVolumes, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
AnalyticGeometry::createObject(XMLInputStream& stream)
{
  SBase* obj = GeometryDefinition::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfAnalyticVolumes")
  {
    if (mAnalyticVolumes.size() != 0)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialAnalyticGeometryAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), "", getLine(), getColumn());
    }

    obj = &mAnalyticVolumes;
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
AnalyticGeometry::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GeometryDefinition::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
AnalyticGeometry::readAttributes(const XMLAttributes& attributes,
                                 const ExpectedAttributes& expectedAttributes)
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
        log->logPackageError("spatial", SpatialUnknown, pkgVersion, level,
          version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialAnalyticGeometryAllowedCoreAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
      }
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
AnalyticGeometry::writeAttributes(XMLOutputStream& stream) const
{
  GeometryDefinition::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new AnalyticGeometry_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
AnalyticGeometry_t *
AnalyticGeometry_create(unsigned int level,
                        unsigned int version,
                        unsigned int pkgVersion)
{
  return new AnalyticGeometry(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this AnalyticGeometry_t object.
 */
LIBSBML_EXTERN
AnalyticGeometry_t*
AnalyticGeometry_clone(const AnalyticGeometry_t* ag)
{
  if (ag != NULL)
  {
    return static_cast<AnalyticGeometry_t*>(ag->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this AnalyticGeometry_t object.
 */
LIBSBML_EXTERN
void
AnalyticGeometry_free(AnalyticGeometry_t* ag)
{
  if (ag != NULL)
  {
    delete ag;
  }
}


/*
 * Returns a ListOf_t * containing AnalyticVolume_t objects from this
 * AnalyticGeometry_t.
 */
LIBSBML_EXTERN
ListOf_t*
AnalyticGeometry_getListOfAnalyticVolumes(AnalyticGeometry_t* ag)
{
  return (ag != NULL) ? ag->getListOfAnalyticVolumes() : NULL;
}


/*
 * Get an AnalyticVolume_t from the AnalyticGeometry_t.
 */
LIBSBML_EXTERN
AnalyticVolume_t*
AnalyticGeometry_getAnalyticVolume(AnalyticGeometry_t* ag, unsigned int n)
{
  return (ag != NULL) ? ag->getAnalyticVolume(n) : NULL;
}


/*
 * Get an AnalyticVolume_t from the AnalyticGeometry_t based on its identifier.
 */
LIBSBML_EXTERN
AnalyticVolume_t*
AnalyticGeometry_getAnalyticVolumeById(AnalyticGeometry_t* ag,
                                       const char *sid)
{
  return (ag != NULL && sid != NULL) ? ag->getAnalyticVolume(sid) : NULL;
}


/*
 * Get an AnalyticVolume_t from the AnalyticGeometry_t based on the DomainType
 * to which it refers.
 */
LIBSBML_EXTERN
AnalyticVolume_t*
AnalyticGeometry_getAnalyticVolumeByDomainType(AnalyticGeometry_t* ag,
                                               const char *sid)
{
  return (ag != NULL && sid != NULL) ? ag->getAnalyticVolumeByDomainType(sid) :
    NULL;
}


/*
 * Adds a copy of the given AnalyticVolume_t to this AnalyticGeometry_t.
 */
LIBSBML_EXTERN
int
AnalyticGeometry_addAnalyticVolume(AnalyticGeometry_t* ag,
                                   const AnalyticVolume_t* av)
{
  return (ag != NULL) ? ag->addAnalyticVolume(av) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of AnalyticVolume_t objects in this AnalyticGeometry_t.
 */
LIBSBML_EXTERN
unsigned int
AnalyticGeometry_getNumAnalyticVolumes(AnalyticGeometry_t* ag)
{
  return (ag != NULL) ? ag->getNumAnalyticVolumes() : SBML_INT_MAX;
}


/*
 * Creates a new AnalyticVolume_t object, adds it to this AnalyticGeometry_t
 * object and returns the AnalyticVolume_t object created.
 */
LIBSBML_EXTERN
AnalyticVolume_t*
AnalyticGeometry_createAnalyticVolume(AnalyticGeometry_t* ag)
{
  return (ag != NULL) ? ag->createAnalyticVolume() : NULL;
}


/*
 * Removes the nth AnalyticVolume_t from this AnalyticGeometry_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
AnalyticVolume_t*
AnalyticGeometry_removeAnalyticVolume(AnalyticGeometry_t* ag, unsigned int n)
{
  return (ag != NULL) ? ag->removeAnalyticVolume(n) : NULL;
}


/*
 * Removes the AnalyticVolume_t from this AnalyticGeometry_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
AnalyticVolume_t*
AnalyticGeometry_removeAnalyticVolumeById(AnalyticGeometry_t* ag,
                                          const char* sid)
{
  return (ag != NULL && sid != NULL) ? ag->removeAnalyticVolume(sid) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * AnalyticGeometry_t object have been set.
 */
LIBSBML_EXTERN
int
AnalyticGeometry_hasRequiredAttributes(const AnalyticGeometry_t * ag)
{
  return (ag != NULL) ? static_cast<int>(ag->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * AnalyticGeometry_t object have been set.
 */
LIBSBML_EXTERN
int
AnalyticGeometry_hasRequiredElements(const AnalyticGeometry_t * ag)
{
  return (ag != NULL) ? static_cast<int>(ag->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


