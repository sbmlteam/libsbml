/**
 * @file Domain.cpp
 * @brief Implementation of the Domain class.
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
#include <sbml/packages/spatial/sbml/Domain.h>
#include <sbml/packages/spatial/sbml/ListOfDomains.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new Domain using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
Domain::Domain(unsigned int level,
               unsigned int version,
               unsigned int pkgVersion)
  : SBase(level, version)
  , mDomainType ("")
  , mInteriorPoints (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new Domain using the given SpatialPkgNamespaces object.
 */
Domain::Domain(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mDomainType ("")
  , mInteriorPoints (spatialns)
{
  setElementNamespace(spatialns->getURI());
  connectToChild();
  loadPlugins(spatialns);
}


/*
 * Copy constructor for Domain.
 */
Domain::Domain(const Domain& orig)
  : SBase( orig )
  , mDomainType ( orig.mDomainType )
  , mInteriorPoints ( orig.mInteriorPoints )
{
  connectToChild();
}


/*
 * Assignment operator for Domain.
 */
Domain&
Domain::operator=(const Domain& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mDomainType = rhs.mDomainType;
    mInteriorPoints = rhs.mInteriorPoints;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Domain object.
 */
Domain*
Domain::clone() const
{
  return new Domain(*this);
}


/*
 * Destructor for Domain.
 */
Domain::~Domain()
{
}


/*
 * Returns the value of the "id" attribute of this Domain.
 */
const std::string&
Domain::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this Domain.
 */
const std::string&
Domain::getName() const
{
  return mName;
}


/*
 * Returns the value of the "domainType" attribute of this Domain.
 */
const std::string&
Domain::getDomainType() const
{
  return mDomainType;
}


/*
 * Predicate returning @c true if this Domain's "id" attribute is set.
 */
bool
Domain::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this Domain's "name" attribute is set.
 */
bool
Domain::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this Domain's "domainType" attribute is set.
 */
bool
Domain::isSetDomainType() const
{
  return (mDomainType.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this Domain.
 */
int
Domain::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this Domain.
 */
int
Domain::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "domainType" attribute of this Domain.
 */
int
Domain::setDomainType(const std::string& domainType)
{
  if (!(SyntaxChecker::isValidInternalSId(domainType)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDomainType = domainType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "id" attribute of this Domain.
 */
int
Domain::unsetId()
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
 * Unsets the value of the "name" attribute of this Domain.
 */
int
Domain::unsetName()
{
  mName.erase();

  if (mName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "domainType" attribute of this Domain.
 */
int
Domain::unsetDomainType()
{
  mDomainType.erase();

  if (mDomainType.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the ListOfInteriorPoints from this Domain.
 */
const ListOfInteriorPoints*
Domain::getListOfInteriorPoints() const
{
  return &mInteriorPoints;
}


/*
 * Returns the ListOfInteriorPoints from this Domain.
 */
ListOfInteriorPoints*
Domain::getListOfInteriorPoints()
{
  return &mInteriorPoints;
}


/*
 * Get an InteriorPoint from the Domain.
 */
InteriorPoint*
Domain::getInteriorPoint(unsigned int n)
{
  return mInteriorPoints.get(n);
}


/*
 * Get an InteriorPoint from the Domain.
 */
const InteriorPoint*
Domain::getInteriorPoint(unsigned int n) const
{
  return mInteriorPoints.get(n);
}


/*
 * Adds a copy of the given InteriorPoint to this Domain.
 */
int
Domain::addInteriorPoint(const InteriorPoint* ip)
{
  if (ip == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (ip->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != ip->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != ip->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(ip)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mInteriorPoints.append(ip);
  }
}


/*
 * Get the number of InteriorPoint objects in this Domain.
 */
unsigned int
Domain::getNumInteriorPoints() const
{
  return mInteriorPoints.size();
}


/*
 * Creates a new InteriorPoint object, adds it to this Domain object and
 * returns the InteriorPoint object created.
 */
InteriorPoint*
Domain::createInteriorPoint()
{
  InteriorPoint* ip = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    ip = new InteriorPoint(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (ip != NULL)
  {
    mInteriorPoints.appendAndOwn(ip);
  }

  return ip;
}


/*
 * Removes the nth InteriorPoint from this Domain and returns a pointer to it.
 */
InteriorPoint*
Domain::removeInteriorPoint(unsigned int n)
{
  return mInteriorPoints.remove(n);
}


/*
 * @copydoc doc_renamesidref_common
 */
void
Domain::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetDomainType() && mDomainType == oldid)
  {
    setDomainType(newid);
  }
}


/*
 * Returns the XML element name of this Domain object.
 */
const std::string&
Domain::getElementName() const
{
  static const string name = "domain";
  return name;
}


/*
 * Returns the libSBML type code for this Domain object.
 */
int
Domain::getTypeCode() const
{
  return SBML_SPATIAL_DOMAIN;
}


/*
 * Predicate returning @c true if all the required attributes for this Domain
 * object have been set.
 */
bool
Domain::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetId() == false)
  {
    allPresent = false;
  }

  if (isSetDomainType() == false)
  {
    allPresent = false;
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
Domain::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (getNumInteriorPoints() > 0)
  {
    mInteriorPoints.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
Domain::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mInteriorPoints.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
Domain::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  mInteriorPoints.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
Domain::connectToChild()
{
  SBase::connectToChild();

  mInteriorPoints.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
Domain::enablePackageInternal(const std::string& pkgURI,
                              const std::string& pkgPrefix,
                              bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mInteriorPoints.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
Domain::updateSBMLNamespace(const std::string& package,
                            unsigned int level,
                            unsigned int version)
{
  SBase::updateSBMLNamespace(package, level, version);

  mInteriorPoints.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Domain.
 */
int
Domain::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Domain.
 */
int
Domain::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Domain.
 */
int
Domain::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Domain.
 */
int
Domain::getAttribute(const std::string& attributeName,
                     unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Domain.
 */
int
Domain::getAttribute(const std::string& attributeName,
                     std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "domainType")
  {
    value = getDomainType();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Domain's attribute "attributeName" is
 * set.
 */
bool
Domain::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }
  else if (attributeName == "domainType")
  {
    value = isSetDomainType();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Domain.
 */
int
Domain::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Domain.
 */
int
Domain::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Domain.
 */
int
Domain::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Domain.
 */
int
Domain::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Domain.
 */
int
Domain::setAttribute(const std::string& attributeName,
                     const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }
  else if (attributeName == "domainType")
  {
    return_value = setDomainType(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Domain.
 */
int
Domain::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }
  else if (attributeName == "domainType")
  {
    value = unsetDomainType();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this Domain.
 */
SBase*
Domain::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "interiorPoint")
  {
    return createInteriorPoint();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this Domain.
 */
int
Domain::addChildObject(const std::string& elementName, const SBase* element)
{
  if (elementName == "interiorPoint" && element->getTypeCode() ==
    SBML_SPATIAL_INTERIORPOINT)
  {
    return addInteriorPoint((const InteriorPoint*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * Domain.
 */
SBase*
Domain::removeChildObject(const std::string& elementName,
                          const std::string& id)
{
  if (elementName == "interiorPoint")
  {
    for (unsigned int i = 0; i < getNumInteriorPoints(); i++)
    {
      if (getInteriorPoint(i)->getId() == id)
      {
        return removeInteriorPoint(i);
      }
    }
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this Domain.
 */
unsigned int
Domain::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "interiorPoint")
  {
    return getNumInteriorPoints();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this Domain.
 */
SBase*
Domain::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "interiorPoint")
  {
    return getInteriorPoint(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
Domain::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mInteriorPoints.getElementBySId(id);

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
Domain::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mInteriorPoints.getMetaId() == metaid)
  {
    return &mInteriorPoints;
  }

  obj = mInteriorPoints.getElementByMetaId(metaid);

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
Domain::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mInteriorPoints, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
Domain::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();

  if (name == "listOfInteriorPoints")
  {
    if (mInteriorPoints.size() != 0)
    {
      getErrorLog()->logPackageError("spatial", SpatialDomainAllowedElements,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(),
          getColumn());
    }

    obj = &mInteriorPoints;
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
Domain::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("domainType");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Domain::readAttributes(const XMLAttributes& attributes,
                       const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfDomains*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialDomainAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialGeometryLODomainsAllowedCoreAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialDomainAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial", SpatialDomainAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // id SId (use = "required" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<Domain>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("spatial", SpatialIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from the <Domain> "
      "element.";
    log->logPackageError("spatial", SpatialDomainAllowedAttributes, pkgVersion,
      level, version, message, getLine(), getColumn());
  }

  // 
  // name string (use = "optional" )
  // 

  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<Domain>");
    }
  }

  // 
  // domainType SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("domainType", mDomainType);

  if (assigned == true)
  {
    if (mDomainType.empty() == true)
    {
      logEmptyString(mDomainType, level, version, "<Domain>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomainType) == false)
    {
      std::string msg = "The domainType attribute on the <" + getElementName()
        + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mDomainType + "', which does not conform to the "
        "syntax.";
      log->logPackageError("spatial", SpatialDomainDomainTypeMustBeDomainType,
        pkgVersion, level, version, msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domainType' is missing from the "
      "<Domain> element.";
    log->logPackageError("spatial", SpatialDomainAllowedAttributes, pkgVersion,
      level, version, message, getLine(), getColumn());
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
Domain::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  if (isSetDomainType() == true)
  {
    stream.writeAttribute("domainType", getPrefix(), mDomainType);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new Domain_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
Domain_t *
Domain_create(unsigned int level,
              unsigned int version,
              unsigned int pkgVersion)
{
  return new Domain(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Domain_t object.
 */
LIBSBML_EXTERN
Domain_t*
Domain_clone(const Domain_t* d)
{
  if (d != NULL)
  {
    return static_cast<Domain_t*>(d->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Domain_t object.
 */
LIBSBML_EXTERN
void
Domain_free(Domain_t* d)
{
  if (d != NULL)
  {
    delete d;
  }
}


/*
 * Returns the value of the "id" attribute of this Domain_t.
 */
LIBSBML_EXTERN
char *
Domain_getId(const Domain_t * d)
{
  if (d == NULL)
  {
    return NULL;
  }

  return d->getId().empty() ? NULL : safe_strdup(d->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this Domain_t.
 */
LIBSBML_EXTERN
char *
Domain_getName(const Domain_t * d)
{
  if (d == NULL)
  {
    return NULL;
  }

  return d->getName().empty() ? NULL : safe_strdup(d->getName().c_str());
}


/*
 * Returns the value of the "domainType" attribute of this Domain_t.
 */
LIBSBML_EXTERN
char *
Domain_getDomainType(const Domain_t * d)
{
  if (d == NULL)
  {
    return NULL;
  }

  return d->getDomainType().empty() ? NULL :
    safe_strdup(d->getDomainType().c_str());
}


/*
 * Predicate returning @c 1 (true) if this Domain_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
Domain_isSetId(const Domain_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Domain_t's "name" attribute is set.
 */
LIBSBML_EXTERN
int
Domain_isSetName(const Domain_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Domain_t's "domainType" attribute is
 * set.
 */
LIBSBML_EXTERN
int
Domain_isSetDomainType(const Domain_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetDomainType()) : 0;
}


/*
 * Sets the value of the "id" attribute of this Domain_t.
 */
LIBSBML_EXTERN
int
Domain_setId(Domain_t * d, const char * id)
{
  return (d != NULL) ? d->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this Domain_t.
 */
LIBSBML_EXTERN
int
Domain_setName(Domain_t * d, const char * name)
{
  return (d != NULL) ? d->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "domainType" attribute of this Domain_t.
 */
LIBSBML_EXTERN
int
Domain_setDomainType(Domain_t * d, const char * domainType)
{
  return (d != NULL) ? d->setDomainType(domainType) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this Domain_t.
 */
LIBSBML_EXTERN
int
Domain_unsetId(Domain_t * d)
{
  return (d != NULL) ? d->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this Domain_t.
 */
LIBSBML_EXTERN
int
Domain_unsetName(Domain_t * d)
{
  return (d != NULL) ? d->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "domainType" attribute of this Domain_t.
 */
LIBSBML_EXTERN
int
Domain_unsetDomainType(Domain_t * d)
{
  return (d != NULL) ? d->unsetDomainType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing InteriorPoint_t objects from this Domain_t.
 */
LIBSBML_EXTERN
ListOf_t*
Domain_getListOfInteriorPoints(Domain_t* d)
{
  return (d != NULL) ? d->getListOfInteriorPoints() : NULL;
}


/*
 * Get an InteriorPoint_t from the Domain_t.
 */
LIBSBML_EXTERN
InteriorPoint_t*
Domain_getInteriorPoint(Domain_t* d, unsigned int n)
{
  return (d != NULL) ? d->getInteriorPoint(n) : NULL;
}


/*
 * Adds a copy of the given InteriorPoint_t to this Domain_t.
 */
LIBSBML_EXTERN
int
Domain_addInteriorPoint(Domain_t* d, const InteriorPoint_t* ip)
{
  return (d != NULL) ? d->addInteriorPoint(ip) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of InteriorPoint_t objects in this Domain_t.
 */
LIBSBML_EXTERN
unsigned int
Domain_getNumInteriorPoints(Domain_t* d)
{
  return (d != NULL) ? d->getNumInteriorPoints() : SBML_INT_MAX;
}


/*
 * Creates a new InteriorPoint_t object, adds it to this Domain_t object and
 * returns the InteriorPoint_t object created.
 */
LIBSBML_EXTERN
InteriorPoint_t*
Domain_createInteriorPoint(Domain_t* d)
{
  return (d != NULL) ? d->createInteriorPoint() : NULL;
}


/*
 * Removes the nth InteriorPoint_t from this Domain_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
InteriorPoint_t*
Domain_removeInteriorPoint(Domain_t* d, unsigned int n)
{
  return (d != NULL) ? d->removeInteriorPoint(n) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Domain_t object have been set.
 */
LIBSBML_EXTERN
int
Domain_hasRequiredAttributes(const Domain_t * d)
{
  return (d != NULL) ? static_cast<int>(d->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


