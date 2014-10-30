/**
 * @file:   AnalyticGeometry.cpp
 * @brief:  Implementation of the AnalyticGeometry class
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


#include <sbml/packages/spatial/sbml/AnalyticGeometry.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new AnalyticGeometry with the given level, version, and package version.
 */
AnalyticGeometry::AnalyticGeometry (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : GeometryDefinition(level, version)
  , mAnalyticVolumes (level, version, pkgVersion)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new AnalyticGeometry with the given SpatialPkgNamespaces object.
 */
AnalyticGeometry::AnalyticGeometry (SpatialPkgNamespaces* spatialns)
  : GeometryDefinition(spatialns)
  , mAnalyticVolumes (spatialns)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for AnalyticGeometry.
 */
AnalyticGeometry::AnalyticGeometry (const AnalyticGeometry& orig)
  : GeometryDefinition(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mAnalyticVolumes  = orig.mAnalyticVolumes;

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for AnalyticGeometry.
 */
AnalyticGeometry&
AnalyticGeometry::operator=(const AnalyticGeometry& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    GeometryDefinition::operator=(rhs);
    mAnalyticVolumes  = rhs.mAnalyticVolumes;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for AnalyticGeometry.
 */
AnalyticGeometry*
AnalyticGeometry::clone () const
{
  return new AnalyticGeometry(*this);
}


/*
 * Destructor for AnalyticGeometry.
 */
AnalyticGeometry::~AnalyticGeometry ()
{
}


/*
 * Returns the  "ListOfAnalyticVolumes" in this AnalyticGeometry object.
 */
const ListOfAnalyticVolumes*
AnalyticGeometry::getListOfAnalyticVolumes() const
{
  return &mAnalyticVolumes;
}


/*
 * Returns the  "ListOfAnalyticVolumes" in this AnalyticGeometry object.
 */
ListOfAnalyticVolumes*
AnalyticGeometry::getListOfAnalyticVolumes()
{
  return &mAnalyticVolumes;
}


/*
 * Removes the nth AnalyticVolume from the ListOfAnalyticVolumes.
 */
AnalyticVolume*
AnalyticGeometry::removeAnalyticVolume(unsigned int n)
{
	return mAnalyticVolumes.remove(n);
}


/*
 * Removes the a AnalyticVolume with given id from the ListOfAnalyticVolumes.
 */
AnalyticVolume*
AnalyticGeometry::removeAnalyticVolume(const std::string& sid)
{
	return mAnalyticVolumes.remove(sid);
}


/*
 * Return the nth AnalyticVolume in the ListOfAnalyticVolumes within this AnalyticGeometry.
 */
AnalyticVolume*
AnalyticGeometry::getAnalyticVolume(unsigned int n)
{
	return mAnalyticVolumes.get(n);
}


/*
 * Return the nth AnalyticVolume in the ListOfAnalyticVolumes within this AnalyticGeometry.
 */
const AnalyticVolume*
AnalyticGeometry::getAnalyticVolume(unsigned int n) const
{
	return mAnalyticVolumes.get(n);
}


/*
 * Return a AnalyticVolume from the ListOfAnalyticVolumes by id.
 */
AnalyticVolume*
AnalyticGeometry::getAnalyticVolume(const std::string& sid)
{
	return mAnalyticVolumes.get(sid);
}


/*
 * Return a AnalyticVolume from the ListOfAnalyticVolumes by id.
 */
const AnalyticVolume*
AnalyticGeometry::getAnalyticVolume(const std::string& sid) const
{
	return mAnalyticVolumes.get(sid);
}


/*
 * Adds a copy the given "AnalyticVolume" to this AnalyticGeometry.
 *
 * @param av; the AnalyticVolume object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
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
  else if (getLevel() != av->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != av->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(av)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mAnalyticVolumes.append(av);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of AnalyticVolume objects in this AnalyticGeometry.
 *
 * @return the number of AnalyticVolume objects in this AnalyticGeometry
 */
unsigned int
AnalyticGeometry::getNumAnalyticVolumes() const
{
  return mAnalyticVolumes.size();
}


/*
 * Creates a new AnalyticVolume object, adds it to this AnalyticGeometrys
 * AnalyticGeometry and returns the AnalyticVolume object created. 
 *
 * @return a new AnalyticVolume object instance
 *
 * @see addAnalyticVolume(const AnalyticVolume* av)
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(av != NULL)
  {
    mAnalyticVolumes.appendAndOwn(av);
  }

  return av;
}


List*
AnalyticGeometry::getAllElements(ElementFilter* filter)
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
AnalyticGeometry::getElementName () const
{
  static const string name = "analyticGeometry";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
AnalyticGeometry::getTypeCode () const
{
  return SBML_SPATIAL_ANALYTICGEOMETRY;
}


/*
 * check if all the required attributes are set
 */
bool
AnalyticGeometry::hasRequiredAttributes () const
{
  bool allPresent = GeometryDefinition::hasRequiredAttributes();

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
AnalyticGeometry::hasRequiredElements () const
{
  bool allPresent = GeometryDefinition::hasRequiredElements();

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
AnalyticGeometry::writeElements (XMLOutputStream& stream) const
{
  GeometryDefinition::writeElements(stream);
  if (getNumAnalyticVolumes() > 0)
  {
    mAnalyticVolumes.write(stream);
  }

  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
AnalyticGeometry::accept (SBMLVisitor& v) const
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
AnalyticGeometry::setSBMLDocument (SBMLDocument* d)
{
  GeometryDefinition::setSBMLDocument(d);
  mAnalyticVolumes.setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
AnalyticGeometry::connectToChild()
{
  GeometryDefinition::connectToChild();

  mAnalyticVolumes.connectToParent(this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
AnalyticGeometry::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  GeometryDefinition::enablePackageInternal(pkgURI, pkgPrefix, flag);
  mAnalyticVolumes.enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
AnalyticGeometry::createObject(XMLInputStream& stream)
{
  SBase* object = GeometryDefinition::createObject(stream);

  const string& name = stream.peek().getName();

  if (name == "listOfAnalyticVolumes")
  {
    object = &mAnalyticVolumes;
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
AnalyticGeometry::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GeometryDefinition::addExpectedAttributes(attributes);

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
AnalyticGeometry::readAttributes (const XMLAttributes& attributes,
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

  //bool assigned = false;

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
AnalyticGeometry::writeAttributes (XMLOutputStream& stream) const
{
  GeometryDefinition::writeAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
AnalyticGeometry_t *
AnalyticGeometry_create(unsigned int level, unsigned int version,
                        unsigned int pkgVersion)
{
  return new AnalyticGeometry(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
AnalyticGeometry_free(AnalyticGeometry_t * ag)
{
  if (ag != NULL)
    delete ag;
}


LIBSBML_EXTERN
AnalyticGeometry_t *
AnalyticGeometry_clone(AnalyticGeometry_t * ag)
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


LIBSBML_EXTERN
int
AnalyticGeometry_addAnalyticVolume(AnalyticGeometry_t * ag, AnalyticVolume_t * av)
{
	return  (ag != NULL) ? ag->addAnalyticVolume(av) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_createAnalyticVolume(AnalyticGeometry_t * ag)
{
	return  (ag != NULL) ? ag->createAnalyticVolume() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
AnalyticGeometry_getListOfAnalyticVolumes(AnalyticGeometry_t * ag)
{
	return  (ag != NULL) ? (ListOf_t *)ag->getListOfAnalyticVolumes() : NULL;
}

LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_getAnalyticVolume(AnalyticGeometry_t * ag, unsigned int n)
{
	return  (ag != NULL) ? ag->getAnalyticVolume(n) : NULL;
}

LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_getAnalyticVolumeById(AnalyticGeometry_t * ag, const char * sid)
{
	return  (ag != NULL) ? ag->getAnalyticVolume(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
AnalyticGeometry_getNumAnalyticVolumes(AnalyticGeometry_t * ag)
{
	return  (ag != NULL) ? ag->getNumAnalyticVolumes() : SBML_INT_MAX;
}

LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_removeAnalyticVolume(AnalyticGeometry_t * ag, unsigned int n)
{
	return  (ag != NULL) ? ag->removeAnalyticVolume(n) : NULL;
}

LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_removeAnalyticVolumeById(AnalyticGeometry_t * ag, const char * sid)
{
	return  (ag != NULL) ? ag->removeAnalyticVolume(sid) : NULL;
}

LIBSBML_EXTERN
int
AnalyticGeometry_hasRequiredAttributes(const AnalyticGeometry_t * ag)
{
  return (ag != NULL) ? static_cast<int>(ag->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
AnalyticGeometry_hasRequiredElements(const AnalyticGeometry_t * ag)
{
	return (ag != NULL) ? static_cast<int>(ag->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


