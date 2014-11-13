/**
 * @file:   CoordinateReference.cpp
 * @brief:  Implementation of the CoordinateReference class
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


#include <sbml/packages/spatial/sbml/CoordinateReference.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CoordinateReference with the given level, version, and package version.
 */
CoordinateReference::CoordinateReference (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mCoordinate (COORDINATEKIND_UNKNOWN)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new CoordinateReference with the given SpatialPkgNamespaces object.
 */
CoordinateReference::CoordinateReference (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mCoordinate (COORDINATEKIND_UNKNOWN)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CoordinateReference.
 */
CoordinateReference::CoordinateReference (const CoordinateReference& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mCoordinate  = orig.mCoordinate;
  }
}


/*
 * Assignment for CoordinateReference.
 */
CoordinateReference&
CoordinateReference::operator=(const CoordinateReference& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mCoordinate  = rhs.mCoordinate;
  }
  return *this;
}


/*
 * Clone for CoordinateReference.
 */
CoordinateReference*
CoordinateReference::clone () const
{
  return new CoordinateReference(*this);
}


/*
 * Destructor for CoordinateReference.
 */
CoordinateReference::~CoordinateReference ()
{
}


/*
 * Returns the value of the "coordinate" attribute of this CoordinateReference.
 */
CoordinateKind_t
CoordinateReference::getCoordinate() const
{
  return mCoordinate;
}


/*
 * Returns true/false if coordinate is set.
 */
bool
CoordinateReference::isSetCoordinate() const
{
  return mCoordinate != COORDINATEKIND_UNKNOWN;
}


/*
 * Sets coordinate and returns value indicating success.
 */
int
CoordinateReference::setCoordinate(CoordinateKind_t coordinate)
{
  mCoordinate = coordinate;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets coordinate and returns value indicating success.
 */
int
CoordinateReference::setCoordinate(const std::string& coordinate)
{
  CoordinateKind_t parsed = CoordinateKind_parse(coordinate.c_str());
  if (parsed == COORDINATEKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mCoordinate = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets coordinate and returns value indicating success.
 */
int
CoordinateReference::unsetCoordinate()
{
  mCoordinate = COORDINATEKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
CoordinateReference::getElementName () const
{
  static const string name = "coordinateReference";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CoordinateReference::getTypeCode () const
{
  return SBML_SPATIAL_COORDINATEREFERENCE;
}


/*
 * check if all the required attributes are set
 */
bool
CoordinateReference::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetCoordinate() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CoordinateReference::writeElements (XMLOutputStream& stream) const
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
CoordinateReference::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
CoordinateReference::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CoordinateReference::enablePackageInternal(const std::string& pkgURI,
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
CoordinateReference::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("coordinate");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CoordinateReference::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfCoordinateReferences - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfCoordinateReferences*>(getParentSBMLObject())->size() < 2)
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
  // coordinate enum  ( use = "required" )
  //
  mCoordinate = COORDINATEKIND_UNKNOWN;
  std::string stringValue;
  assigned = attributes.readInto("coordinate", stringValue);

  if (assigned == true)
  {
    // parse enum

    mCoordinate = CoordinateKind_parse(stringValue.c_str());
    if(mCoordinate == COORDINATEKIND_UNKNOWN) {
      std::string message = "Unknown value for spatial attribute 'coordinate' in 'coordinateReference' object: " + stringValue;
      getErrorLog()->logPackageError("spatial", SpatialUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  if(mCoordinate == COORDINATEKIND_UNKNOWN)
  {
    std::string message = "Spatial attribute 'coordinate' is missing from 'coordinateReference' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
CoordinateReference::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetCoordinate() == true)
    stream.writeAttribute("coordinate", getPrefix(), CoordinateKind_toString(mCoordinate));

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfCoordinateReferences::ListOfCoordinateReferences(unsigned int level, 
                             unsigned int version, 
                             unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfCoordinateReferences::ListOfCoordinateReferences(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfCoordinateReferences 
 */
ListOfCoordinateReferences* 
ListOfCoordinateReferences::clone () const
 {
  return new ListOfCoordinateReferences(*this);
}


/*
 * Get a CoordinateReference from the ListOfCoordinateReferences by index.
*/
CoordinateReference*
ListOfCoordinateReferences::get(unsigned int n)
{
  return static_cast<CoordinateReference*>(ListOf::get(n));
}


/*
 * Get a CoordinateReference from the ListOfCoordinateReferences by index.
 */
const CoordinateReference*
ListOfCoordinateReferences::get(unsigned int n) const
{
  return static_cast<const CoordinateReference*>(ListOf::get(n));
}


/*
 * Get a CoordinateReference from the ListOfCoordinateReferences by id.
 */
CoordinateReference*
ListOfCoordinateReferences::get(const std::string& sid)
{
	return const_cast<CoordinateReference*>(
    static_cast<const ListOfCoordinateReferences&>(*this).get(sid));
}


/*
 * Get a CoordinateReference from the ListOfCoordinateReferences by id.
 */
const CoordinateReference*
ListOfCoordinateReferences::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CoordinateReference>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <CoordinateReference*> (*result);
}


/**
 * Adds a copy the given "CoordinateReference" to this ListOfCoordinateReferences.
 *
 * @param cr; the CoordinateReference object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfCoordinateReferences::addCoordinateReference(const CoordinateReference* cr)
{
  if (cr == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (cr->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != cr->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != cr->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(cr)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(cr);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of CoordinateReference objects in this ListOfCoordinateReferences.
 *
 * @return the number of CoordinateReference objects in this ListOfCoordinateReferences
 */
unsigned int 
ListOfCoordinateReferences::getNumCoordinateReferences() const
{
	return size();
}

/**
 * Creates a new CoordinateReference object, adds it to this ListOfCoordinateReferences
 * CoordinateReference and returns the CoordinateReference object created. 
 *
 * @return a new CoordinateReference object instance
 *
 * @see addCoordinateReference(const CoordinateReference* cr)
 */
CoordinateReference* 
ListOfCoordinateReferences::createCoordinateReference()
{
  CoordinateReference* cr = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    cr = new CoordinateReference(spatialns);
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

  if(cr != NULL)
  {
    appendAndOwn(cr);
  }

  return cr;
}

/*
 * Removes the nth CoordinateReference from this ListOfCoordinateReferences
 */
CoordinateReference*
ListOfCoordinateReferences::remove(unsigned int n)
{
  return static_cast<CoordinateReference*>(ListOf::remove(n));
}


/*
 * Removes the CoordinateReference from this ListOfCoordinateReferences with the given identifier
 */
CoordinateReference*
ListOfCoordinateReferences::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CoordinateReference>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <CoordinateReference*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfCoordinateReferences::getElementName () const
{
  static const string name = "listOfCoordinateReferences";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfCoordinateReferences::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfCoordinateReferences::getItemTypeCode () const
{
  return SBML_SPATIAL_COORDINATEREFERENCE;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new CoordinateReference in this ListOfCoordinateReferences
 */
SBase*
ListOfCoordinateReferences::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "coordinateReference")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new CoordinateReference(spatialns);
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
ListOfCoordinateReferences::writeXMLNS(XMLOutputStream& stream) const
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
CoordinateReference_t *
CoordinateReference_create(unsigned int level, unsigned int version,
                           unsigned int pkgVersion)
{
  return new CoordinateReference(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CoordinateReference_free(CoordinateReference_t * cr)
{
  if (cr != NULL)
    delete cr;
}


LIBSBML_EXTERN
CoordinateReference_t *
CoordinateReference_clone(CoordinateReference_t * cr)
{
  if (cr != NULL)
  {
    return static_cast<CoordinateReference_t*>(cr->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
CoordinateKind_t
CoordinateReference_getCoordinate(const CoordinateReference_t * cr)
{
	return (cr != NULL) ? cr->getCoordinate() : COORDINATEKIND_UNKNOWN;
}


LIBSBML_EXTERN
int
CoordinateReference_isSetCoordinate(const CoordinateReference_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->isSetCoordinate()) : 0;
}


LIBSBML_EXTERN
int
CoordinateReference_setCoordinate(CoordinateReference_t * cr, CoordinateKind_t coordinate)
{
  if (cr != NULL)
    return cr->setCoordinate(coordinate);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CoordinateReference_unsetCoordinate(CoordinateReference_t * cr)
{
  return (cr != NULL) ? cr->unsetCoordinate() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CoordinateReference_hasRequiredAttributes(const CoordinateReference_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
CoordinateReference_t *
ListOfCoordinateReferences_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfCoordinateReferences *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
CoordinateReference_t *
ListOfCoordinateReferences_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfCoordinateReferences *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


