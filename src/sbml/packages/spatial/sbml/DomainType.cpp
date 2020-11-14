/**
 * @file DomainType.cpp
 * @brief Implementation of the DomainType class.
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
#include <sbml/packages/spatial/sbml/DomainType.h>
#include <sbml/packages/spatial/sbml/ListOfDomainTypes.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DomainType using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
DomainType::DomainType(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion)
  : SBase(level, version)
  , mSpatialDimensions (SBML_INT_MAX)
  , mIsSetSpatialDimensions (false)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new DomainType using the given SpatialPkgNamespaces object.
 */
DomainType::DomainType(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mSpatialDimensions (SBML_INT_MAX)
  , mIsSetSpatialDimensions (false)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for DomainType.
 */
DomainType::DomainType(const DomainType& orig)
  : SBase( orig )
  , mSpatialDimensions ( orig.mSpatialDimensions )
  , mIsSetSpatialDimensions ( orig.mIsSetSpatialDimensions )
{
}


/*
 * Assignment operator for DomainType.
 */
DomainType&
DomainType::operator=(const DomainType& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mSpatialDimensions = rhs.mSpatialDimensions;
    mIsSetSpatialDimensions = rhs.mIsSetSpatialDimensions;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DomainType object.
 */
DomainType*
DomainType::clone() const
{
  return new DomainType(*this);
}


/*
 * Destructor for DomainType.
 */
DomainType::~DomainType()
{
}


/*
 * Returns the value of the "id" attribute of this DomainType.
 */
const std::string&
DomainType::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this DomainType.
 */
const std::string&
DomainType::getName() const
{
  return mName;
}


/*
 * Returns the value of the "spatialDimensions" attribute of this DomainType.
 */
int
DomainType::getSpatialDimensions() const
{
  return mSpatialDimensions;
}


/*
 * Predicate returning @c true if this DomainType's "id" attribute is set.
 */
bool
DomainType::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DomainType's "name" attribute is set.
 */
bool
DomainType::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this DomainType's "spatialDimensions"
 * attribute is set.
 */
bool
DomainType::isSetSpatialDimensions() const
{
  return mIsSetSpatialDimensions;
}


/*
 * Sets the value of the "id" attribute of this DomainType.
 */
int
DomainType::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DomainType.
 */
int
DomainType::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "spatialDimensions" attribute of this DomainType.
 */
int
DomainType::setSpatialDimensions(int spatialDimensions)
{
  mSpatialDimensions = spatialDimensions;
  mIsSetSpatialDimensions = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DomainType.
 */
int
DomainType::unsetId()
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
 * Unsets the value of the "name" attribute of this DomainType.
 */
int
DomainType::unsetName()
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
 * Unsets the value of the "spatialDimensions" attribute of this DomainType.
 */
int
DomainType::unsetSpatialDimensions()
{
  mSpatialDimensions = SBML_INT_MAX;
  mIsSetSpatialDimensions = false;

  if (isSetSpatialDimensions() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the XML element name of this DomainType object.
 */
const std::string&
DomainType::getElementName() const
{
  static const string name = "domainType";
  return name;
}


/*
 * Returns the libSBML type code for this DomainType object.
 */
int
DomainType::getTypeCode() const
{
  return SBML_SPATIAL_DOMAINTYPE;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DomainType object have been set.
 */
bool
DomainType::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetId() == false)
  {
    allPresent = false;
  }

  if (isSetSpatialDimensions() == false)
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
DomainType::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DomainType::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DomainType::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DomainType::enablePackageInternal(const std::string& pkgURI,
                                  const std::string& pkgPrefix,
                                  bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DomainType.
 */
int
DomainType::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DomainType.
 */
int
DomainType::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "spatialDimensions")
  {
    value = getSpatialDimensions();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DomainType.
 */
int
DomainType::getAttribute(const std::string& attributeName,
                         double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DomainType.
 */
int
DomainType::getAttribute(const std::string& attributeName,
                         unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DomainType.
 */
int
DomainType::getAttribute(const std::string& attributeName,
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

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DomainType's attribute "attributeName"
 * is set.
 */
bool
DomainType::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "spatialDimensions")
  {
    value = isSetSpatialDimensions();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DomainType.
 */
int
DomainType::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DomainType.
 */
int
DomainType::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "spatialDimensions")
  {
    return_value = setSpatialDimensions(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DomainType.
 */
int
DomainType::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DomainType.
 */
int
DomainType::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DomainType.
 */
int
DomainType::setAttribute(const std::string& attributeName,
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

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this DomainType.
 */
int
DomainType::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "spatialDimensions")
  {
    value = unsetSpatialDimensions();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DomainType::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("spatialDimensions");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DomainType::readAttributes(const XMLAttributes& attributes,
                           const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfDomainTypes*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialDomainTypeAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialGeometryLODomainTypesAllowedCoreAttributes, pkgVersion, level,
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
        log->logPackageError("spatial", SpatialDomainTypeAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial", SpatialDomainTypeAllowedCoreAttributes,
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
      logEmptyString(mId, level, version, "<DomainType>");
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
    std::string message = "Spatial attribute 'id' is missing from the "
      "<DomainType> element.";
    log->logPackageError("spatial", SpatialDomainTypeAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }

  // 
  // name string (use = "optional" )
  // 

  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<DomainType>");
    }
  }

  // 
  // spatialDimensions int (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetSpatialDimensions = attributes.readInto("spatialDimensions",
    mSpatialDimensions);

  if ( mIsSetSpatialDimensions == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'spatialDimensions' from the "
        "<DomainType> element must be an integer.";
      log->logPackageError("spatial",
        SpatialDomainTypeSpatialDimensionsMustBeInteger, pkgVersion, level,
          version, message, getLine(), getColumn());
    }
    else
    {
      std::string message = "Spatial attribute 'spatialDimensions' is missing "
        "from the <DomainType> element.";
      log->logPackageError("spatial", SpatialDomainTypeAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DomainType::writeAttributes(XMLOutputStream& stream) const
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

  if (isSetSpatialDimensions() == true)
  {
    stream.writeAttribute("spatialDimensions", getPrefix(),
      mSpatialDimensions);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DomainType_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
DomainType_t *
DomainType_create(unsigned int level,
                  unsigned int version,
                  unsigned int pkgVersion)
{
  return new DomainType(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DomainType_t object.
 */
LIBSBML_EXTERN
DomainType_t*
DomainType_clone(const DomainType_t* dt)
{
  if (dt != NULL)
  {
    return static_cast<DomainType_t*>(dt->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DomainType_t object.
 */
LIBSBML_EXTERN
void
DomainType_free(DomainType_t* dt)
{
  if (dt != NULL)
  {
    delete dt;
  }
}


/*
 * Returns the value of the "id" attribute of this DomainType_t.
 */
LIBSBML_EXTERN
char *
DomainType_getId(const DomainType_t * dt)
{
  if (dt == NULL)
  {
    return NULL;
  }

  return dt->getId().empty() ? NULL : safe_strdup(dt->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this DomainType_t.
 */
LIBSBML_EXTERN
char *
DomainType_getName(const DomainType_t * dt)
{
  if (dt == NULL)
  {
    return NULL;
  }

  return dt->getName().empty() ? NULL : safe_strdup(dt->getName().c_str());
}


/*
 * Returns the value of the "spatialDimensions" attribute of this DomainType_t.
 */
LIBSBML_EXTERN
int
DomainType_getSpatialDimensions(const DomainType_t * dt)
{
  return (dt != NULL) ? dt->getSpatialDimensions() : SBML_INT_MAX;
}


/*
 * Predicate returning @c 1 (true) if this DomainType_t's "id" attribute is
 * set.
 */
LIBSBML_EXTERN
int
DomainType_isSetId(const DomainType_t * dt)
{
  return (dt != NULL) ? static_cast<int>(dt->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DomainType_t's "name" attribute is
 * set.
 */
LIBSBML_EXTERN
int
DomainType_isSetName(const DomainType_t * dt)
{
  return (dt != NULL) ? static_cast<int>(dt->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DomainType_t's "spatialDimensions"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DomainType_isSetSpatialDimensions(const DomainType_t * dt)
{
  return (dt != NULL) ? static_cast<int>(dt->isSetSpatialDimensions()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DomainType_t.
 */
LIBSBML_EXTERN
int
DomainType_setId(DomainType_t * dt, const char * id)
{
  return (dt != NULL) ? dt->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this DomainType_t.
 */
LIBSBML_EXTERN
int
DomainType_setName(DomainType_t * dt, const char * name)
{
  return (dt != NULL) ? dt->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "spatialDimensions" attribute of this DomainType_t.
 */
LIBSBML_EXTERN
int
DomainType_setSpatialDimensions(DomainType_t * dt, int spatialDimensions)
{
  return (dt != NULL) ? dt->setSpatialDimensions(spatialDimensions) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this DomainType_t.
 */
LIBSBML_EXTERN
int
DomainType_unsetId(DomainType_t * dt)
{
  return (dt != NULL) ? dt->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this DomainType_t.
 */
LIBSBML_EXTERN
int
DomainType_unsetName(DomainType_t * dt)
{
  return (dt != NULL) ? dt->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "spatialDimensions" attribute of this DomainType_t.
 */
LIBSBML_EXTERN
int
DomainType_unsetSpatialDimensions(DomainType_t * dt)
{
  return (dt != NULL) ? dt->unsetSpatialDimensions() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DomainType_t object have been set.
 */
LIBSBML_EXTERN
int
DomainType_hasRequiredAttributes(const DomainType_t * dt)
{
  return (dt != NULL) ? static_cast<int>(dt->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


