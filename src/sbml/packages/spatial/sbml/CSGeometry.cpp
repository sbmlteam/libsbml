/**
 * @file CSGeometry.cpp
 * @brief Implementation of the CSGeometry class.
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
#include <sbml/packages/spatial/sbml/CSGeometry.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new CSGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
CSGeometry::CSGeometry(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion)
  : GeometryDefinition(level, version, pkgVersion)
  , mCSGObjects (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new CSGeometry using the given SpatialPkgNamespaces object.
 */
CSGeometry::CSGeometry(SpatialPkgNamespaces *spatialns)
  : GeometryDefinition(spatialns)
  , mCSGObjects (spatialns)
{
  setElementNamespace(spatialns->getURI());
  connectToChild();
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGeometry.
 */
CSGeometry::CSGeometry(const CSGeometry& orig)
  : GeometryDefinition( orig )
  , mCSGObjects ( orig.mCSGObjects )
{
  connectToChild();
}


/*
 * Assignment operator for CSGeometry.
 */
CSGeometry&
CSGeometry::operator=(const CSGeometry& rhs)
{
  if (&rhs != this)
  {
    GeometryDefinition::operator=(rhs);
    mCSGObjects = rhs.mCSGObjects;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this CSGeometry object.
 */
CSGeometry*
CSGeometry::clone() const
{
  return new CSGeometry(*this);
}


/*
 * Destructor for CSGeometry.
 */
CSGeometry::~CSGeometry()
{
}


/*
 * Returns the ListOfCSGObjects from this CSGeometry.
 */
const ListOfCSGObjects*
CSGeometry::getListOfCSGObjects() const
{
  return &mCSGObjects;
}


/*
 * Returns the ListOfCSGObjects from this CSGeometry.
 */
ListOfCSGObjects*
CSGeometry::getListOfCSGObjects()
{
  return &mCSGObjects;
}


/*
 * Get a CSGObject from the CSGeometry.
 */
CSGObject*
CSGeometry::getCSGObject(unsigned int n)
{
  return mCSGObjects.get(n);
}


/*
 * Get a CSGObject from the CSGeometry.
 */
const CSGObject*
CSGeometry::getCSGObject(unsigned int n) const
{
  return mCSGObjects.get(n);
}


/*
 * Get a CSGObject from the CSGeometry based on its identifier.
 */
CSGObject*
CSGeometry::getCSGObject(const std::string& sid)
{
  return mCSGObjects.get(sid);
}


/*
 * Get a CSGObject from the CSGeometry based on its identifier.
 */
const CSGObject*
CSGeometry::getCSGObject(const std::string& sid) const
{
  return mCSGObjects.get(sid);
}


/*
 * Get a CSGObject from the CSGeometry based on the DomainType to which it
 * refers.
 */
const CSGObject*
CSGeometry::getCSGObjectByDomainType(const std::string& sid) const
{
  return mCSGObjects.getByDomainType(sid);
}


/*
 * Get a CSGObject from the CSGeometry based on the DomainType to which it
 * refers.
 */
CSGObject*
CSGeometry::getCSGObjectByDomainType(const std::string& sid)
{
  return mCSGObjects.getByDomainType(sid);
}


/*
 * Adds a copy of the given CSGObject to this CSGeometry.
 */
int
CSGeometry::addCSGObject(const CSGObject* csgo)
{
  if (csgo == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (csgo->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (csgo->hasRequiredElements() == false)
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
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(csgo)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (csgo->isSetId() && (mCSGObjects.get(csgo->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mCSGObjects.append(csgo);
  }
}


/*
 * Get the number of CSGObject objects in this CSGeometry.
 */
unsigned int
CSGeometry::getNumCSGObjects() const
{
  return mCSGObjects.size();
}


/*
 * Creates a new CSGObject object, adds it to this CSGeometry object and
 * returns the CSGObject object created.
 */
CSGObject*
CSGeometry::createCSGObject()
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
  }

  if (csgo != NULL)
  {
    mCSGObjects.appendAndOwn(csgo);
  }

  return csgo;
}


/*
 * Removes the nth CSGObject from this CSGeometry and returns a pointer to it.
 */
CSGObject*
CSGeometry::removeCSGObject(unsigned int n)
{
  return mCSGObjects.remove(n);
}


/*
 * Removes the CSGObject from this CSGeometry based on its identifier and
 * returns a pointer to it.
 */
CSGObject*
CSGeometry::removeCSGObject(const std::string& sid)
{
  return mCSGObjects.remove(sid);
}


/*
 * Returns the XML element name of this CSGeometry object.
 */
const std::string&
CSGeometry::getElementName() const
{
  static const string name = "csGeometry";
  return name;
}


/*
 * Returns the libSBML type code for this CSGeometry object.
 */
int
CSGeometry::getTypeCode() const
{
  return SBML_SPATIAL_CSGEOMETRY;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * CSGeometry object have been set.
 */
bool
CSGeometry::hasRequiredAttributes() const
{
  bool allPresent = GeometryDefinition::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this CSGeometry
 * object have been set.
 */
bool
CSGeometry::hasRequiredElements() const
{
  bool allPresent = GeometryDefinition::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
CSGeometry::writeElements(XMLOutputStream& stream) const
{
  GeometryDefinition::writeElements(stream);

  if (getNumCSGObjects() > 0)
  {
    mCSGObjects.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
CSGeometry::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mCSGObjects.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
CSGeometry::setSBMLDocument(SBMLDocument* d)
{
  GeometryDefinition::setSBMLDocument(d);

  mCSGObjects.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
CSGeometry::connectToChild()
{
  GeometryDefinition::connectToChild();

  mCSGObjects.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
CSGeometry::enablePackageInternal(const std::string& pkgURI,
                                  const std::string& pkgPrefix,
                                  bool flag)
{
  GeometryDefinition::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mCSGObjects.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
CSGeometry::updateSBMLNamespace(const std::string& package,
                                unsigned int level,
                                unsigned int version)
{
  GeometryDefinition::updateSBMLNamespace(package, level, version);

  mCSGObjects.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGeometry.
 */
int
CSGeometry::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGeometry.
 */
int
CSGeometry::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGeometry.
 */
int
CSGeometry::getAttribute(const std::string& attributeName,
                         double& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGeometry.
 */
int
CSGeometry::getAttribute(const std::string& attributeName,
                         unsigned int& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGeometry.
 */
int
CSGeometry::getAttribute(const std::string& attributeName,
                         std::string& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this CSGeometry's attribute "attributeName"
 * is set.
 */
bool
CSGeometry::isSetAttribute(const std::string& attributeName) const
{
  bool value = GeometryDefinition::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGeometry.
 */
int
CSGeometry::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGeometry.
 */
int
CSGeometry::setAttribute(const std::string& attributeName, int value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGeometry.
 */
int
CSGeometry::setAttribute(const std::string& attributeName, double value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGeometry.
 */
int
CSGeometry::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGeometry.
 */
int
CSGeometry::setAttribute(const std::string& attributeName,
                         const std::string& value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this CSGeometry.
 */
int
CSGeometry::unsetAttribute(const std::string& attributeName)
{
  int value = GeometryDefinition::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this CSGeometry.
 */
SBase*
CSGeometry::createChildObject(const std::string& elementName)
{
  GeometryDefinition* obj = NULL;

  if (elementName == "csgObject")
  {
    return createCSGObject();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this CSGeometry.
 */
int
CSGeometry::addChildObject(const std::string& elementName,
                           const SBase* element)
{
  if (elementName == "csgObject" && element->getTypeCode() ==
    SBML_SPATIAL_CSGOBJECT)
  {
    return addCSGObject((const CSGObject*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * CSGeometry.
 */
SBase*
CSGeometry::removeChildObject(const std::string& elementName,
                              const std::string& id)
{
  if (elementName == "csgObject")
  {
    return removeCSGObject(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this CSGeometry.
 */
unsigned int
CSGeometry::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "csgObject")
  {
    return getNumCSGObjects();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this CSGeometry.
 */
SBase*
CSGeometry::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "csgObject")
  {
    return getCSGObject(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
CSGeometry::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mCSGObjects.getElementBySId(id);

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
CSGeometry::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mCSGObjects.getMetaId() == metaid)
  {
    return &mCSGObjects;
  }

  obj = mCSGObjects.getElementByMetaId(metaid);

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
CSGeometry::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mCSGObjects, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
CSGeometry::createObject(XMLInputStream& stream)
{
  SBase* obj = GeometryDefinition::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfCSGObjects")
  {
    if (mCSGObjects.size() != 0)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialCSGeometryAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), "", getLine(), getColumn());
    }

    obj = &mCSGObjects;
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
CSGeometry::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GeometryDefinition::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CSGeometry::readAttributes(const XMLAttributes& attributes,
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
        log->logPackageError("spatial", SpatialCSGeometryAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
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
CSGeometry::writeAttributes(XMLOutputStream& stream) const
{
  GeometryDefinition::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new CSGeometry_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGeometry_t *
CSGeometry_create(unsigned int level,
                  unsigned int version,
                  unsigned int pkgVersion)
{
  return new CSGeometry(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CSGeometry_t object.
 */
LIBSBML_EXTERN
CSGeometry_t*
CSGeometry_clone(const CSGeometry_t* csg)
{
  if (csg != NULL)
  {
    return static_cast<CSGeometry_t*>(csg->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this CSGeometry_t object.
 */
LIBSBML_EXTERN
void
CSGeometry_free(CSGeometry_t* csg)
{
  if (csg != NULL)
  {
    delete csg;
  }
}


/*
 * Returns a ListOf_t * containing CSGObject_t objects from this CSGeometry_t.
 */
LIBSBML_EXTERN
ListOf_t*
CSGeometry_getListOfCSGObjects(CSGeometry_t* csg)
{
  return (csg != NULL) ? csg->getListOfCSGObjects() : NULL;
}


/*
 * Get a CSGObject_t from the CSGeometry_t.
 */
LIBSBML_EXTERN
CSGObject_t*
CSGeometry_getCSGObject(CSGeometry_t* csg, unsigned int n)
{
  return (csg != NULL) ? csg->getCSGObject(n) : NULL;
}


/*
 * Get a CSGObject_t from the CSGeometry_t based on its identifier.
 */
LIBSBML_EXTERN
CSGObject_t*
CSGeometry_getCSGObjectById(CSGeometry_t* csg, const char *sid)
{
  return (csg != NULL && sid != NULL) ? csg->getCSGObject(sid) : NULL;
}


/*
 * Get a CSGObject_t from the CSGeometry_t based on the DomainType to which it
 * refers.
 */
LIBSBML_EXTERN
CSGObject_t*
CSGeometry_getCSGObjectByDomainType(CSGeometry_t* csg, const char *sid)
{
  return (csg != NULL && sid != NULL) ? csg->getCSGObjectByDomainType(sid) :
    NULL;
}


/*
 * Adds a copy of the given CSGObject_t to this CSGeometry_t.
 */
LIBSBML_EXTERN
int
CSGeometry_addCSGObject(CSGeometry_t* csg, const CSGObject_t* csgo)
{
  return (csg != NULL) ? csg->addCSGObject(csgo) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of CSGObject_t objects in this CSGeometry_t.
 */
LIBSBML_EXTERN
unsigned int
CSGeometry_getNumCSGObjects(CSGeometry_t* csg)
{
  return (csg != NULL) ? csg->getNumCSGObjects() : SBML_INT_MAX;
}


/*
 * Creates a new CSGObject_t object, adds it to this CSGeometry_t object and
 * returns the CSGObject_t object created.
 */
LIBSBML_EXTERN
CSGObject_t*
CSGeometry_createCSGObject(CSGeometry_t* csg)
{
  return (csg != NULL) ? csg->createCSGObject() : NULL;
}


/*
 * Removes the nth CSGObject_t from this CSGeometry_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
CSGObject_t*
CSGeometry_removeCSGObject(CSGeometry_t* csg, unsigned int n)
{
  return (csg != NULL) ? csg->removeCSGObject(n) : NULL;
}


/*
 * Removes the CSGObject_t from this CSGeometry_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
CSGObject_t*
CSGeometry_removeCSGObjectById(CSGeometry_t* csg, const char* sid)
{
  return (csg != NULL && sid != NULL) ? csg->removeCSGObject(sid) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGeometry_t object have been set.
 */
LIBSBML_EXTERN
int
CSGeometry_hasRequiredAttributes(const CSGeometry_t * csg)
{
  return (csg != NULL) ? static_cast<int>(csg->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * CSGeometry_t object have been set.
 */
LIBSBML_EXTERN
int
CSGeometry_hasRequiredElements(const CSGeometry_t * csg)
{
  return (csg != NULL) ? static_cast<int>(csg->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


