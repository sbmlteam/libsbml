/**
 * @file:   CoordinateComponent.cpp
 * @brief:  Implementation of the CoordinateComponent class
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


#include <sbml/packages/spatial/sbml/CoordinateComponent.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CoordinateComponent with the given level, version, and package version.
 */
CoordinateComponent::CoordinateComponent (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mType (COORDINATEKIND_UNKNOWN)
  , mUnit ("")
  , mBoundaryMin (NULL)
  , mBoundaryMax (NULL)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new CoordinateComponent with the given SpatialPkgNamespaces object.
 */
CoordinateComponent::CoordinateComponent (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mType (COORDINATEKIND_UNKNOWN)
  , mUnit ("")
  , mBoundaryMin (NULL)
  , mBoundaryMax (NULL)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CoordinateComponent.
 */
CoordinateComponent::CoordinateComponent (const CoordinateComponent& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mType  = orig.mType;
    mUnit  = orig.mUnit;
    if (orig.mBoundaryMin != NULL)
    {
      mBoundaryMin = orig.mBoundaryMin->clone();
    }
    else
    {
      mBoundaryMin = NULL;
    }
    if (orig.mBoundaryMax != NULL)
    {
      mBoundaryMax = orig.mBoundaryMax->clone();
    }
    else
    {
      mBoundaryMax = NULL;
    }

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for CoordinateComponent.
 */
CoordinateComponent&
CoordinateComponent::operator=(const CoordinateComponent& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mType  = rhs.mType;
    mUnit  = rhs.mUnit;
    if (rhs.mBoundaryMin != NULL)
    {
      mBoundaryMin = rhs.mBoundaryMin->clone();
    }
    else
    {
      mBoundaryMin = NULL;
    }
    if (rhs.mBoundaryMax != NULL)
    {
      mBoundaryMax = rhs.mBoundaryMax->clone();
    }
    else
    {
      mBoundaryMax = NULL;
    }

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for CoordinateComponent.
 */
CoordinateComponent*
CoordinateComponent::clone () const
{
  return new CoordinateComponent(*this);
}


/*
 * Destructor for CoordinateComponent.
 */
CoordinateComponent::~CoordinateComponent ()
{
  delete mBoundaryMin;
  mBoundaryMin = NULL;
  delete mBoundaryMax;
  mBoundaryMax = NULL;
}


/*
 * Returns the value of the "id" attribute of this CoordinateComponent.
 */
const std::string&
CoordinateComponent::getId() const
{
  return mId;
}


/*
 * Returns the value of the "type" attribute of this CoordinateComponent.
 */
CoordinateKind_t
CoordinateComponent::getType() const
{
  return mType;
}


/*
 * Returns the value of the "unit" attribute of this CoordinateComponent.
 */
const std::string&
CoordinateComponent::getUnit() const
{
  return mUnit;
}


/*
 * Returns the value of the "boundaryMin" attribute of this CoordinateComponent.
 */
const Boundary*
CoordinateComponent::getBoundaryMin() const
{
  return mBoundaryMin;
}


/*
 * Returns the value of the "boundaryMin" attribute of this CoordinateComponent.
 */
Boundary*
CoordinateComponent::getBoundaryMin()
{
  return mBoundaryMin;
}


/*
 * Creates a new "boundaryMin" element of this CoordinateComponent and returns it.
 */
Boundary*
CoordinateComponent::createBoundaryMin()
{
  if (mBoundaryMin != NULL) delete mBoundaryMin;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mBoundaryMin = new Boundary(spatialns);
  mBoundaryMin->setElementName("boundaryMin");
  delete spatialns;
  connectToChild();
  return mBoundaryMin;
}


/*
 * Returns the value of the "boundaryMax" attribute of this CoordinateComponent.
 */
const Boundary*
CoordinateComponent::getBoundaryMax() const
{
  return mBoundaryMax;
}


/*
 * Returns the value of the "boundaryMax" attribute of this CoordinateComponent.
 */
Boundary*
CoordinateComponent::getBoundaryMax()
{
  return mBoundaryMax;
}


/*
 * Creates a new "boundaryMax" element of this CoordinateComponent and returns it.
 */
Boundary*
CoordinateComponent::createBoundaryMax()
{
  if (mBoundaryMax != NULL) delete mBoundaryMax;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mBoundaryMax = new Boundary(spatialns);
  mBoundaryMax->setElementName("boundaryMax");
  delete spatialns;
  connectToChild();
  return mBoundaryMax;
}


/*
 * Returns true/false if id is set.
 */
bool
CoordinateComponent::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if type is set.
 */
bool
CoordinateComponent::isSetType() const
{
  return mType != COORDINATEKIND_UNKNOWN;
}


/*
 * Returns true/false if unit is set.
 */
bool
CoordinateComponent::isSetUnit() const
{
  return (mUnit.empty() == false);
}


/*
 * Returns true/false if boundaryMin is set.
 */
bool
CoordinateComponent::isSetBoundaryMin() const
{
  return (mBoundaryMin != NULL);
}


/*
 * Returns true/false if boundaryMax is set.
 */
bool
CoordinateComponent::isSetBoundaryMax() const
{
  return (mBoundaryMax != NULL);
}


/*
 * Sets id and returns value indicating success.
 */
int
CoordinateComponent::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets type and returns value indicating success.
 */
int
CoordinateComponent::setType(CoordinateKind_t type)
{
  mType = type;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets type and returns value indicating success.
 */
int
CoordinateComponent::setType(const std::string& type)
{
  CoordinateKind_t parsed = CoordinateKind_parse(type.c_str());
  if (parsed == COORDINATEKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mType = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets unit and returns value indicating success.
 */
int
CoordinateComponent::setUnit(const std::string& unit)
{
  if (&(unit) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(unit)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mUnit = unit;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets boundaryMin and returns value indicating success.
 */
int
CoordinateComponent::setBoundaryMin(Boundary* boundaryMin)
{
  if (mBoundaryMin == boundaryMin)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (boundaryMin == NULL)
  {
    delete mBoundaryMin;
    mBoundaryMin = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mBoundaryMin;
    mBoundaryMin = (boundaryMin != NULL) ?
      static_cast<Boundary*>(boundaryMin->clone()) : NULL;
    if (mBoundaryMin != NULL)
    {
      mBoundaryMin->setElementName("boundaryMin");
      mBoundaryMin->connectToParent(this);
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets boundaryMax and returns value indicating success.
 */
int
CoordinateComponent::setBoundaryMax(Boundary* boundaryMax)
{
  if (mBoundaryMax == boundaryMax)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (boundaryMax == NULL)
  {
    delete mBoundaryMax;
    mBoundaryMax = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mBoundaryMax;
    mBoundaryMax = (boundaryMax != NULL) ?
      static_cast<Boundary*>(boundaryMax->clone()) : NULL;
    if (mBoundaryMax != NULL)
    {
      mBoundaryMax->setElementName("boundaryMax");
      mBoundaryMax->connectToParent(this);
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
CoordinateComponent::unsetId()
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
 * Unsets type and returns value indicating success.
 */
int
CoordinateComponent::unsetType()
{
  mType = COORDINATEKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets unit and returns value indicating success.
 */
int
CoordinateComponent::unsetUnit()
{
  mUnit.erase();

  if (mUnit.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets boundaryMin and returns value indicating success.
 */
int
CoordinateComponent::unsetBoundaryMin()
{
  delete mBoundaryMin;
  mBoundaryMin = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets boundaryMax and returns value indicating success.
 */
int
CoordinateComponent::unsetBoundaryMax()
{
  delete mBoundaryMax;
  mBoundaryMax = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
CoordinateComponent::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetUnit() == true && mUnit == oldid)
  {
    setUnit(newid);
  }

}


List*
CoordinateComponent::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mBoundaryMin, filter);
  ADD_FILTERED_POINTER(ret, sublist, mBoundaryMax, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
CoordinateComponent::getElementName () const
{
  static const string name = "coordinateComponent";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CoordinateComponent::getTypeCode () const
{
  return SBML_SPATIAL_COORDINATECOMPONENT;
}


/*
 * check if all the required attributes are set
 */
bool
CoordinateComponent::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetType() == false)
    allPresent = false;

  if (isSetUnit() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
CoordinateComponent::hasRequiredElements () const
{
  bool allPresent = true;

  if (isSetBoundaryMin() == false)
    allPresent = false;

  if (isSetBoundaryMax() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CoordinateComponent::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  if (isSetBoundaryMin() == true)
  {
    mBoundaryMin->write(stream);
  }
  if (isSetBoundaryMax() == true)
  {
    mBoundaryMax->write(stream);
  }
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
CoordinateComponent::accept (SBMLVisitor& v) const
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
CoordinateComponent::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
  if ( mBoundaryMin != NULL)
    mBoundaryMin->setSBMLDocument(d);
  if ( mBoundaryMax != NULL)
    mBoundaryMax->setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
CoordinateComponent::connectToChild()
{
  SBase::connectToChild();

  if (mBoundaryMin != NULL)
    mBoundaryMin->connectToParent(this);
  if (mBoundaryMax != NULL)
    mBoundaryMax->connectToParent(this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CoordinateComponent::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
CoordinateComponent::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  const string& name = stream.peek().getName();

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "boundaryMin")
  {
    mBoundaryMin = new Boundary(spatialns);
    mBoundaryMin->setElementName(name);
    object = mBoundaryMin;
  }
  else if (name == "boundaryMax")
  {
    mBoundaryMax = new Boundary(spatialns);
    mBoundaryMax->setElementName(name);
    object = mBoundaryMax;
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
CoordinateComponent::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("type");
  attributes.add("unit");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CoordinateComponent::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfCoordinateComponents - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfCoordinateComponents*>(getParentSBMLObject())->size() < 2)
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
      logEmptyString(mId, getLevel(), getVersion(), "<CoordinateComponent>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'coordinateComponent' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // type enum  ( use = "required" )
  //
  mType = COORDINATEKIND_UNKNOWN;
  std::string stringValue;
  assigned = attributes.readInto("type", stringValue);

  if (assigned == true)
  {
    // parse enum

    mType = CoordinateKind_parse(stringValue.c_str());
    if(mType == COORDINATEKIND_UNKNOWN) {
      std::string message = "Unknown value for spatial attribute 'type' in 'coordinateComponent' object: " + stringValue;
      getErrorLog()->logPackageError("spatial", SpatialUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  if(mType == COORDINATEKIND_UNKNOWN)
  {
    std::string message = "Spatial attribute 'type' is missing from 'coordinateComponent' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
      getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // unit SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("unit", mUnit);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mUnit.empty() == true)
    {
      logEmptyString(mUnit, getLevel(), getVersion(), "<CoordinateComponent>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mUnit) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute unit='" + mUnit + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'unit' is missing from 'coordinateComponent' object.";
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
CoordinateComponent::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetType() == true)
    stream.writeAttribute("type", getPrefix(), CoordinateKind_toString(mType));

  if (isSetUnit() == true)
    stream.writeAttribute("unit", getPrefix(), mUnit);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfCoordinateComponents::ListOfCoordinateComponents(unsigned int level, 
                             unsigned int version, 
                             unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfCoordinateComponents::ListOfCoordinateComponents(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfCoordinateComponents 
 */
ListOfCoordinateComponents* 
ListOfCoordinateComponents::clone () const
 {
  return new ListOfCoordinateComponents(*this);
}


/*
 * Get a CoordinateComponent from the ListOfCoordinateComponents by index.
*/
CoordinateComponent*
ListOfCoordinateComponents::get(unsigned int n)
{
  return static_cast<CoordinateComponent*>(ListOf::get(n));
}


/*
 * Get a CoordinateComponent from the ListOfCoordinateComponents by index.
 */
const CoordinateComponent*
ListOfCoordinateComponents::get(unsigned int n) const
{
  return static_cast<const CoordinateComponent*>(ListOf::get(n));
}


/*
 * Get a CoordinateComponent from the ListOfCoordinateComponents by id.
 */
CoordinateComponent*
ListOfCoordinateComponents::get(const std::string& sid)
{
	return const_cast<CoordinateComponent*>(
    static_cast<const ListOfCoordinateComponents&>(*this).get(sid));
}


/*
 * Get a CoordinateComponent from the ListOfCoordinateComponents by id.
 */
const CoordinateComponent*
ListOfCoordinateComponents::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CoordinateComponent>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <CoordinateComponent*> (*result);
}


/**
 * Adds a copy the given "CoordinateComponent" to this ListOfCoordinateComponents.
 *
 * @param cc; the CoordinateComponent object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfCoordinateComponents::addCoordinateComponent(const CoordinateComponent* cc)
{
  if (cc == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (cc->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != cc->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != cc->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(cc)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(cc);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of CoordinateComponent objects in this ListOfCoordinateComponents.
 *
 * @return the number of CoordinateComponent objects in this ListOfCoordinateComponents
 */
unsigned int 
ListOfCoordinateComponents::getNumCoordinateComponents() const
{
	return size();
}

/**
 * Creates a new CoordinateComponent object, adds it to this ListOfCoordinateComponents
 * CoordinateComponent and returns the CoordinateComponent object created. 
 *
 * @return a new CoordinateComponent object instance
 *
 * @see addCoordinateComponent(const CoordinateComponent* cc)
 */
CoordinateComponent* 
ListOfCoordinateComponents::createCoordinateComponent()
{
  CoordinateComponent* cc = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    cc = new CoordinateComponent(spatialns);
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

  if(cc != NULL)
  {
    appendAndOwn(cc);
  }

  return cc;
}

/*
 * Removes the nth CoordinateComponent from this ListOfCoordinateComponents
 */
CoordinateComponent*
ListOfCoordinateComponents::remove(unsigned int n)
{
  return static_cast<CoordinateComponent*>(ListOf::remove(n));
}


/*
 * Removes the CoordinateComponent from this ListOfCoordinateComponents with the given identifier
 */
CoordinateComponent*
ListOfCoordinateComponents::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CoordinateComponent>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <CoordinateComponent*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfCoordinateComponents::getElementName () const
{
  static const string name = "listOfCoordinateComponents";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfCoordinateComponents::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfCoordinateComponents::getItemTypeCode () const
{
  return SBML_SPATIAL_COORDINATECOMPONENT;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new CoordinateComponent in this ListOfCoordinateComponents
 */
SBase*
ListOfCoordinateComponents::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "coordinateComponent")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new CoordinateComponent(spatialns);
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
ListOfCoordinateComponents::writeXMLNS(XMLOutputStream& stream) const
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
CoordinateComponent_t *
CoordinateComponent_create(unsigned int level, unsigned int version,
                           unsigned int pkgVersion)
{
  return new CoordinateComponent(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CoordinateComponent_free(CoordinateComponent_t * cc)
{
  if (cc != NULL)
    delete cc;
}


LIBSBML_EXTERN
CoordinateComponent_t *
CoordinateComponent_clone(CoordinateComponent_t * cc)
{
  if (cc != NULL)
  {
    return static_cast<CoordinateComponent_t*>(cc->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
CoordinateComponent_getId(const CoordinateComponent_t * cc)
{
	return (cc != NULL && cc->isSetId()) ? cc->getId().c_str() : NULL;
}


LIBSBML_EXTERN
CoordinateKind_t
CoordinateComponent_getType(const CoordinateComponent_t * cc)
{
	return (cc != NULL) ? cc->getType() : COORDINATEKIND_UNKNOWN;
}


LIBSBML_EXTERN
const char *
CoordinateComponent_getUnit(const CoordinateComponent_t * cc)
{
	return (cc != NULL && cc->isSetUnit()) ? cc->getUnit().c_str() : NULL;
}


LIBSBML_EXTERN
Boundary_t*
CoordinateComponent_getBoundaryMin(CoordinateComponent_t * cc)
{
	if (cc == NULL)
		return NULL;

	return (Boundary_t*)cc->getBoundaryMin();
}


LIBSBML_EXTERN
Boundary_t*
CoordinateComponent_createBoundaryMin(CoordinateComponent_t * cc)
{
	if (cc == NULL)
		return NULL;

	return (Boundary_t*)cc->createBoundaryMin();
}


LIBSBML_EXTERN
Boundary_t*
CoordinateComponent_getBoundaryMax(CoordinateComponent_t * cc)
{
	if (cc == NULL)
		return NULL;

	return (Boundary_t*)cc->getBoundaryMax();
}


LIBSBML_EXTERN
Boundary_t*
CoordinateComponent_createBoundaryMax(CoordinateComponent_t * cc)
{
	if (cc == NULL)
		return NULL;

	return (Boundary_t*)cc->createBoundaryMax();
}


LIBSBML_EXTERN
int
CoordinateComponent_isSetId(const CoordinateComponent_t * cc)
{
  return (cc != NULL) ? static_cast<int>(cc->isSetId()) : 0;
}


LIBSBML_EXTERN
int
CoordinateComponent_isSetType(const CoordinateComponent_t * cc)
{
  return (cc != NULL) ? static_cast<int>(cc->isSetType()) : 0;
}


LIBSBML_EXTERN
int
CoordinateComponent_isSetUnit(const CoordinateComponent_t * cc)
{
  return (cc != NULL) ? static_cast<int>(cc->isSetUnit()) : 0;
}


LIBSBML_EXTERN
int
CoordinateComponent_isSetBoundaryMin(const CoordinateComponent_t * cc)
{
  return (cc != NULL) ? static_cast<int>(cc->isSetBoundaryMin()) : 0;
}


LIBSBML_EXTERN
int
CoordinateComponent_isSetBoundaryMax(const CoordinateComponent_t * cc)
{
  return (cc != NULL) ? static_cast<int>(cc->isSetBoundaryMax()) : 0;
}


LIBSBML_EXTERN
int
CoordinateComponent_setId(CoordinateComponent_t * cc, const char * id)
{
  if (cc != NULL)
    return (id == NULL) ? cc->setId("") : cc->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CoordinateComponent_setType(CoordinateComponent_t * cc, CoordinateKind_t type)
{
  if (cc != NULL)
    return cc->setType(type);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CoordinateComponent_setUnit(CoordinateComponent_t * cc, const char * unit)
{
  if (cc != NULL)
    return (unit == NULL) ? cc->setUnit("") : cc->setUnit(unit);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CoordinateComponent_setBoundaryMin(CoordinateComponent_t * cc, Boundary_t* boundaryMin)
{
	return (cc != NULL) ? cc->setBoundaryMin(boundaryMin) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CoordinateComponent_setBoundaryMax(CoordinateComponent_t * cc, Boundary_t* boundaryMax)
{
	return (cc != NULL) ? cc->setBoundaryMax(boundaryMax) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CoordinateComponent_unsetId(CoordinateComponent_t * cc)
{
  return (cc != NULL) ? cc->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CoordinateComponent_unsetType(CoordinateComponent_t * cc)
{
  return (cc != NULL) ? cc->unsetType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CoordinateComponent_unsetUnit(CoordinateComponent_t * cc)
{
  return (cc != NULL) ? cc->unsetUnit() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CoordinateComponent_hasRequiredAttributes(const CoordinateComponent_t * cc)
{
  return (cc != NULL) ? static_cast<int>(cc->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
CoordinateComponent_hasRequiredElements(const CoordinateComponent_t * cc)
{
	return (cc != NULL) ? static_cast<int>(cc->hasRequiredElements()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
CoordinateComponent_t *
ListOfCoordinateComponents_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfCoordinateComponents *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
CoordinateComponent_t *
ListOfCoordinateComponents_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfCoordinateComponents *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


