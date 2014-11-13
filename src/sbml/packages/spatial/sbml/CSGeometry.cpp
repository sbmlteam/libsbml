/**
 * @file:   CSGeometry.cpp
 * @brief:  Implementation of the CSGeometry class
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


#include <sbml/packages/spatial/sbml/CSGeometry.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CSGeometry with the given level, version, and package version.
 */
CSGeometry::CSGeometry (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : GeometryDefinition(level, version)
  , mCsgObjects (level, version, pkgVersion)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new CSGeometry with the given SpatialPkgNamespaces object.
 */
CSGeometry::CSGeometry (SpatialPkgNamespaces* spatialns)
  : GeometryDefinition(spatialns)
  , mCsgObjects (spatialns)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGeometry.
 */
CSGeometry::CSGeometry (const CSGeometry& orig)
  : GeometryDefinition(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mCsgObjects  = orig.mCsgObjects;

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for CSGeometry.
 */
CSGeometry&
CSGeometry::operator=(const CSGeometry& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    GeometryDefinition::operator=(rhs);
    mCsgObjects  = rhs.mCsgObjects;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for CSGeometry.
 */
CSGeometry*
CSGeometry::clone () const
{
  return new CSGeometry(*this);
}


/*
 * Destructor for CSGeometry.
 */
CSGeometry::~CSGeometry ()
{
}


/*
 * Returns the  "ListOfCSGObjects" in this CSGeometry object.
 */
const ListOfCSGObjects*
CSGeometry::getListOfCsgObjects() const
{
  return &mCsgObjects;
}


/*
 * Returns the  "ListOfCSGObjects" in this CSGeometry object.
 */
ListOfCSGObjects*
CSGeometry::getListOfCsgObjects()
{
  return &mCsgObjects;
}


/*
 * Removes the nth CsgObject from the ListOfCSGObjects.
 */
CSGObject*
CSGeometry::removeCsgObject(unsigned int n)
{
	return mCsgObjects.remove(n);
}


/*
 * Removes the a CsgObject with given id from the ListOfCSGObjects.
 */
CSGObject*
CSGeometry::removeCsgObject(const std::string& sid)
{
	return mCsgObjects.remove(sid);
}


/*
 * Return the nth CsgObject in the ListOfCSGObjects within this CSGeometry.
 */
CSGObject*
CSGeometry::getCsgObject(unsigned int n)
{
	return mCsgObjects.get(n);
}


/*
 * Return the nth CsgObject in the ListOfCSGObjects within this CSGeometry.
 */
const CSGObject*
CSGeometry::getCsgObject(unsigned int n) const
{
	return mCsgObjects.get(n);
}


/*
 * Return a CsgObject from the ListOfCSGObjects by id.
 */
CSGObject*
CSGeometry::getCsgObject(const std::string& sid)
{
	return mCsgObjects.get(sid);
}


/*
 * Return a CsgObject from the ListOfCSGObjects by id.
 */
const CSGObject*
CSGeometry::getCsgObject(const std::string& sid) const
{
	return mCsgObjects.get(sid);
}


/*
 * Adds a copy the given "CSGObject" to this CSGeometry.
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
CSGeometry::addCsgObject(const CSGObject* csgo)
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
    mCsgObjects.append(csgo);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of CSGObject objects in this CSGeometry.
 *
 * @return the number of CSGObject objects in this CSGeometry
 */
unsigned int
CSGeometry::getNumCsgObjects() const
{
  return mCsgObjects.size();
}


/*
 * Creates a new CSGObject object, adds it to this CSGeometrys
 * CSGeometry and returns the CSGObject object created. 
 *
 * @return a new CSGObject object instance
 *
 * @see addCSGObject(const CSGObject* csgo)
 */
CSGObject*
CSGeometry::createCsgObject()
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
    mCsgObjects.appendAndOwn(csgo);
  }

  return csgo;
}


List*
CSGeometry::getAllElements(ElementFilter* filter)
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
CSGeometry::getElementName () const
{
  static const string name = "csGeometry";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CSGeometry::getTypeCode () const
{
  return SBML_SPATIAL_CSGEOMETRY;
}


/*
 * check if all the required attributes are set
 */
bool
CSGeometry::hasRequiredAttributes () const
{
  bool allPresent = GeometryDefinition::hasRequiredAttributes();

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
CSGeometry::hasRequiredElements () const
{
  bool allPresent = GeometryDefinition::hasRequiredElements();

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CSGeometry::writeElements (XMLOutputStream& stream) const
{
  GeometryDefinition::writeElements(stream);
  if (getNumCsgObjects() > 0)
  {
    mCsgObjects.write(stream);
  }

  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGeometry::accept (SBMLVisitor& v) const
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
CSGeometry::setSBMLDocument (SBMLDocument* d)
{
  GeometryDefinition::setSBMLDocument(d);
  mCsgObjects.setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
CSGeometry::connectToChild()
{
  GeometryDefinition::connectToChild();

  mCsgObjects.connectToParent(this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CSGeometry::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  GeometryDefinition::enablePackageInternal(pkgURI, pkgPrefix, flag);
  mCsgObjects.enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
CSGeometry::createObject(XMLInputStream& stream)
{
  SBase* object = GeometryDefinition::createObject(stream);

  const string& name = stream.peek().getName();

  if (name == "listOfCSGObjects")
  {
    object = &mCsgObjects;
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
CSGeometry::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GeometryDefinition::addExpectedAttributes(attributes);

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CSGeometry::readAttributes (const XMLAttributes& attributes,
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
CSGeometry::writeAttributes (XMLOutputStream& stream) const
{
  GeometryDefinition::writeAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
CSGeometry_t *
CSGeometry_create(unsigned int level, unsigned int version,
                  unsigned int pkgVersion)
{
  return new CSGeometry(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CSGeometry_free(CSGeometry_t * csg)
{
  if (csg != NULL)
    delete csg;
}


LIBSBML_EXTERN
CSGeometry_t *
CSGeometry_clone(CSGeometry_t * csg)
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


LIBSBML_EXTERN
int
CSGeometry_addCsgObject(CSGeometry_t * csg, CSGObject_t * csgo)
{
	return  (csg != NULL) ? csg->addCsgObject(csgo) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
CSGObject_t *
CSGeometry_createCsgObject(CSGeometry_t * csg)
{
	return  (csg != NULL) ? csg->createCsgObject() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
CSGeometry_getListOfCSGObjects(CSGeometry_t * csg)
{
	return  (csg != NULL) ? (ListOf_t *)csg->getListOfCsgObjects() : NULL;
}

LIBSBML_EXTERN
CSGObject_t *
CSGeometry_getCsgObject(CSGeometry_t * csg, unsigned int n)
{
	return  (csg != NULL) ? csg->getCsgObject(n) : NULL;
}

LIBSBML_EXTERN
CSGObject_t *
CSGeometry_getCsgObjectById(CSGeometry_t * csg, const char * sid)
{
	return  (csg != NULL) ? csg->getCsgObject(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
CSGeometry_getNumCsgObjects(CSGeometry_t * csg)
{
	return  (csg != NULL) ? csg->getNumCsgObjects() : SBML_INT_MAX;
}

LIBSBML_EXTERN
CSGObject_t *
CSGeometry_removeCsgObject(CSGeometry_t * csg, unsigned int n)
{
	return  (csg != NULL) ? csg->removeCsgObject(n) : NULL;
}

LIBSBML_EXTERN
CSGObject_t *
CSGeometry_removeCsgObjectById(CSGeometry_t * csg, const char * sid)
{
	return  (csg != NULL) ? csg->removeCsgObject(sid) : NULL;
}

LIBSBML_EXTERN
int
CSGeometry_hasRequiredAttributes(const CSGeometry_t * csg)
{
  return (csg != NULL) ? static_cast<int>(csg->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
CSGeometry_hasRequiredElements(const CSGeometry_t * csg)
{
	return (csg != NULL) ? static_cast<int>(csg->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


