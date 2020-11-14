/**
 * @file GeometryDefinition.cpp
 * @brief Implementation of the GeometryDefinition class.
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
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/sbml/ListOfGeometryDefinitions.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>

#include <sbml/packages/spatial/sbml/AnalyticGeometry.h>
#include <sbml/packages/spatial/sbml/SampledFieldGeometry.h>
#include <sbml/packages/spatial/sbml/CSGeometry.h>
#include <sbml/packages/spatial/sbml/ParametricGeometry.h>
#include <sbml/packages/spatial/sbml/MixedGeometry.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new GeometryDefinition using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
GeometryDefinition::GeometryDefinition(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion)
  : SBase(level, version)
  , mIsActive (false)
  , mIsSetIsActive (false)
  , mElementName("geometryDefinition")
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new GeometryDefinition using the given SpatialPkgNamespaces
 * object.
 */
GeometryDefinition::GeometryDefinition(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mIsActive (false)
  , mIsSetIsActive (false)
  , mElementName("geometryDefinition")
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for GeometryDefinition.
 */
GeometryDefinition::GeometryDefinition(const GeometryDefinition& orig)
  : SBase( orig )
  , mIsActive ( orig.mIsActive )
  , mIsSetIsActive ( orig.mIsSetIsActive )
  , mElementName ( orig.mElementName )
{
}


/*
 * Assignment operator for GeometryDefinition.
 */
GeometryDefinition&
GeometryDefinition::operator=(const GeometryDefinition& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mIsActive = rhs.mIsActive;
    mIsSetIsActive = rhs.mIsSetIsActive;
    mElementName = rhs.mElementName;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this GeometryDefinition object.
 */
GeometryDefinition*
GeometryDefinition::clone() const
{
  return new GeometryDefinition(*this);
}


/*
 * Destructor for GeometryDefinition.
 */
GeometryDefinition::~GeometryDefinition()
{
}


/*
 * Returns the value of the "id" attribute of this GeometryDefinition.
 */
const std::string&
GeometryDefinition::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this GeometryDefinition.
 */
const std::string&
GeometryDefinition::getName() const
{
  return mName;
}


/*
 * Returns the value of the "isActive" attribute of this GeometryDefinition.
 */
bool
GeometryDefinition::getIsActive() const
{
  return mIsActive;
}


/*
 * Predicate returning @c true if this GeometryDefinition's "id" attribute is
 * set.
 */
bool
GeometryDefinition::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this GeometryDefinition's "name" attribute is
 * set.
 */
bool
GeometryDefinition::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this GeometryDefinition's "isActive"
 * attribute is set.
 */
bool
GeometryDefinition::isSetIsActive() const
{
  return mIsSetIsActive;
}


/*
 * Sets the value of the "id" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "isActive" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::setIsActive(bool isActive)
{
  mIsActive = isActive;
  mIsSetIsActive = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::unsetId()
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
 * Unsets the value of the "name" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::unsetName()
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
 * Unsets the value of the "isActive" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::unsetIsActive()
{
  mIsActive = false;
  mIsSetIsActive = false;

  if (isSetIsActive() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Predicate returning @c true if this abstract "GeometryDefinition" is of type
 * AnalyticGeometry
 */
bool
GeometryDefinition::isAnalyticGeometry() const
{
  return dynamic_cast<const AnalyticGeometry*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "GeometryDefinition" is of type
 * SampledFieldGeometry
 */
bool
GeometryDefinition::isSampledFieldGeometry() const
{
  return dynamic_cast<const SampledFieldGeometry*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "GeometryDefinition" is of type
 * CSGeometry
 */
bool
GeometryDefinition::isCSGeometry() const
{
  return dynamic_cast<const CSGeometry*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "GeometryDefinition" is of type
 * ParametricGeometry
 */
bool
GeometryDefinition::isParametricGeometry() const
{
  return dynamic_cast<const ParametricGeometry*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "GeometryDefinition" is of type
 * MixedGeometry
 */
bool
GeometryDefinition::isMixedGeometry() const
{
  return dynamic_cast<const MixedGeometry*>(this) != NULL;
}


/*
 * Returns the XML element name of this GeometryDefinition object.
 */
const std::string&
GeometryDefinition::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this GeometryDefinition object.
 */
void
GeometryDefinition::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this GeometryDefinition object.
 */
int
GeometryDefinition::getTypeCode() const
{
  return SBML_SPATIAL_GEOMETRYDEFINITION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * GeometryDefinition object have been set.
 */
bool
GeometryDefinition::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetId() == false)
  {
    allPresent = false;
  }

  if (isSetIsActive() == false)
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
GeometryDefinition::writeElements(XMLOutputStream& stream) const
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
GeometryDefinition::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
GeometryDefinition::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
GeometryDefinition::enablePackageInternal(const std::string& pkgURI,
                                          const std::string& pkgPrefix,
                                          bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::getAttribute(const std::string& attributeName,
                                 bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "isActive")
  {
    value = getIsActive();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::getAttribute(const std::string& attributeName,
                                 int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::getAttribute(const std::string& attributeName,
                                 double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::getAttribute(const std::string& attributeName,
                                 unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::getAttribute(const std::string& attributeName,
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
 * Predicate returning @c true if this GeometryDefinition's attribute
 * "attributeName" is set.
 */
bool
GeometryDefinition::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "isActive")
  {
    value = isSetIsActive();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "isActive")
  {
    return_value = setIsActive(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::setAttribute(const std::string& attributeName,
                                 double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::setAttribute(const std::string& attributeName,
                                 unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GeometryDefinition.
 */
int
GeometryDefinition::setAttribute(const std::string& attributeName,
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
 * Unsets the value of the "attributeName" attribute of this
 * GeometryDefinition.
 */
int
GeometryDefinition::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "isActive")
  {
    value = unsetIsActive();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
GeometryDefinition::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("isActive");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
GeometryDefinition::readAttributes(const XMLAttributes& attributes,
                                   const ExpectedAttributes&
                                     expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfGeometryDefinitions*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial",
          SpatialGeometryDefinitionAllowedAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialGeometryLOGeometryDefinitionsAllowedCoreAttributes, pkgVersion,
            level, version, details, getLine(), getColumn());
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
        log->logPackageError("spatial",
          SpatialGeometryDefinitionAllowedAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialGeometryDefinitionAllowedCoreAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
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
      logEmptyString(mId, level, version, "<GeometryDefinition>");
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
      "<GeometryDefinition> element.";
    log->logPackageError("spatial", SpatialGeometryDefinitionAllowedAttributes,
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
      logEmptyString(mName, level, version, "<GeometryDefinition>");
    }
  }

  // 
  // isActive bool (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetIsActive = attributes.readInto("isActive", mIsActive);

  if (mIsSetIsActive == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      log->logPackageError("spatial",
        SpatialGeometryDefinitionIsActiveMustBeBoolean, pkgVersion, level,
          version);
    }
    else
    {
      std::string message = "Spatial attribute 'isActive' is missing from the "
        "<GeometryDefinition> element.";
      log->logPackageError("spatial",
        SpatialGeometryDefinitionAllowedAttributes, pkgVersion, level, version,
          message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
GeometryDefinition::writeAttributes(XMLOutputStream& stream) const
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

  if (isSetIsActive() == true)
  {
    stream.writeAttribute("isActive", getPrefix(), mIsActive);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new AnalyticGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
AnalyticGeometry_t *
GeometryDefinition_createAnalyticGeometry(unsigned int level,
                                          unsigned int version,
                                          unsigned int pkgVersion)
{
  return new AnalyticGeometry(level, version, pkgVersion);
}


/*
 * Creates a new SampledFieldGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
SampledFieldGeometry_t *
GeometryDefinition_createSampledFieldGeometry(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion)
{
  return new SampledFieldGeometry(level, version, pkgVersion);
}


/*
 * Creates a new CSGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGeometry_t *
GeometryDefinition_createCSGeometry(unsigned int level,
                                    unsigned int version,
                                    unsigned int pkgVersion)
{
  return new CSGeometry(level, version, pkgVersion);
}


/*
 * Creates a new ParametricGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
ParametricGeometry_t *
GeometryDefinition_createParametricGeometry(unsigned int level,
                                            unsigned int version,
                                            unsigned int pkgVersion)
{
  return new ParametricGeometry(level, version, pkgVersion);
}


/*
 * Creates a new MixedGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
MixedGeometry_t *
GeometryDefinition_createMixedGeometry(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion)
{
  return new MixedGeometry(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this GeometryDefinition_t object.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
GeometryDefinition_clone(const GeometryDefinition_t* gd)
{
  if (gd != NULL)
  {
    return static_cast<GeometryDefinition_t*>(gd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this GeometryDefinition_t object.
 */
LIBSBML_EXTERN
void
GeometryDefinition_free(GeometryDefinition_t* gd)
{
  if (gd != NULL)
  {
    delete gd;
  }
}


/*
 * Returns the value of the "id" attribute of this GeometryDefinition_t.
 */
LIBSBML_EXTERN
char *
GeometryDefinition_getId(const GeometryDefinition_t * gd)
{
  if (gd == NULL)
  {
    return NULL;
  }

  return gd->getId().empty() ? NULL : safe_strdup(gd->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this GeometryDefinition_t.
 */
LIBSBML_EXTERN
char *
GeometryDefinition_getName(const GeometryDefinition_t * gd)
{
  if (gd == NULL)
  {
    return NULL;
  }

  return gd->getName().empty() ? NULL : safe_strdup(gd->getName().c_str());
}


/*
 * Returns the value of the "isActive" attribute of this GeometryDefinition_t.
 */
LIBSBML_EXTERN
int
GeometryDefinition_getIsActive(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->getIsActive()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this GeometryDefinition_t's "id"
 * attribute is set.
 */
LIBSBML_EXTERN
int
GeometryDefinition_isSetId(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this GeometryDefinition_t's "name"
 * attribute is set.
 */
LIBSBML_EXTERN
int
GeometryDefinition_isSetName(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this GeometryDefinition_t's "isActive"
 * attribute is set.
 */
LIBSBML_EXTERN
int
GeometryDefinition_isSetIsActive(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->isSetIsActive()) : 0;
}


/*
 * Sets the value of the "id" attribute of this GeometryDefinition_t.
 */
LIBSBML_EXTERN
int
GeometryDefinition_setId(GeometryDefinition_t * gd, const char * id)
{
  return (gd != NULL) ? gd->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this GeometryDefinition_t.
 */
LIBSBML_EXTERN
int
GeometryDefinition_setName(GeometryDefinition_t * gd, const char * name)
{
  return (gd != NULL) ? gd->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "isActive" attribute of this GeometryDefinition_t.
 */
LIBSBML_EXTERN
int
GeometryDefinition_setIsActive(GeometryDefinition_t * gd, int isActive)
{
  return (gd != NULL) ? gd->setIsActive(isActive) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this GeometryDefinition_t.
 */
LIBSBML_EXTERN
int
GeometryDefinition_unsetId(GeometryDefinition_t * gd)
{
  return (gd != NULL) ? gd->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this GeometryDefinition_t.
 */
LIBSBML_EXTERN
int
GeometryDefinition_unsetName(GeometryDefinition_t * gd)
{
  return (gd != NULL) ? gd->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "isActive" attribute of this GeometryDefinition_t.
 */
LIBSBML_EXTERN
int
GeometryDefinition_unsetIsActive(GeometryDefinition_t * gd)
{
  return (gd != NULL) ? gd->unsetIsActive() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this GeometryDefinition_t is of type
 * AnalyticGeometry_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isAnalyticGeometry(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->isAnalyticGeometry()) : 0;
}


/*
 * Predicate returning @c 1 if this GeometryDefinition_t is of type
 * SampledFieldGeometry_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isSampledFieldGeometry(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->isSampledFieldGeometry()) : 0;
}


/*
 * Predicate returning @c 1 if this GeometryDefinition_t is of type
 * CSGeometry_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isCSGeometry(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->isCSGeometry()) : 0;
}


/*
 * Predicate returning @c 1 if this GeometryDefinition_t is of type
 * ParametricGeometry_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isParametricGeometry(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->isParametricGeometry()) : 0;
}


/*
 * Predicate returning @c 1 if this GeometryDefinition_t is of type
 * MixedGeometry_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isMixedGeometry(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->isMixedGeometry()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GeometryDefinition_t object have been set.
 */
LIBSBML_EXTERN
int
GeometryDefinition_hasRequiredAttributes(const GeometryDefinition_t * gd)
{
  return (gd != NULL) ? static_cast<int>(gd->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


