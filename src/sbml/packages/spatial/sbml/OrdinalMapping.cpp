/**
 * @file OrdinalMapping.cpp
 * @brief Implementation of the OrdinalMapping class.
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
#include <sbml/packages/spatial/sbml/OrdinalMapping.h>
#include <sbml/packages/spatial/sbml/ListOfOrdinalMappings.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new OrdinalMapping using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
OrdinalMapping::OrdinalMapping(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion)
  : SBase(level, version)
  , mGeometryDefinition ("")
  , mOrdinal (SBML_INT_MAX)
  , mIsSetOrdinal (false)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new OrdinalMapping using the given SpatialPkgNamespaces object.
 */
OrdinalMapping::OrdinalMapping(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mGeometryDefinition ("")
  , mOrdinal (SBML_INT_MAX)
  , mIsSetOrdinal (false)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for OrdinalMapping.
 */
OrdinalMapping::OrdinalMapping(const OrdinalMapping& orig)
  : SBase( orig )
  , mGeometryDefinition ( orig.mGeometryDefinition )
  , mOrdinal ( orig.mOrdinal )
  , mIsSetOrdinal ( orig.mIsSetOrdinal )
{
}


/*
 * Assignment operator for OrdinalMapping.
 */
OrdinalMapping&
OrdinalMapping::operator=(const OrdinalMapping& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mGeometryDefinition = rhs.mGeometryDefinition;
    mOrdinal = rhs.mOrdinal;
    mIsSetOrdinal = rhs.mIsSetOrdinal;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this OrdinalMapping object.
 */
OrdinalMapping*
OrdinalMapping::clone() const
{
  return new OrdinalMapping(*this);
}


/*
 * Destructor for OrdinalMapping.
 */
OrdinalMapping::~OrdinalMapping()
{
}


/*
 * Returns the value of the "geometryDefinition" attribute of this
 * OrdinalMapping.
 */
const std::string&
OrdinalMapping::getGeometryDefinition() const
{
  return mGeometryDefinition;
}


/*
 * Returns the value of the "ordinal" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::getOrdinal() const
{
  return mOrdinal;
}


/*
 * Predicate returning @c true if this OrdinalMapping's "geometryDefinition"
 * attribute is set.
 */
bool
OrdinalMapping::isSetGeometryDefinition() const
{
  return (mGeometryDefinition.empty() == false);
}


/*
 * Predicate returning @c true if this OrdinalMapping's "ordinal" attribute is
 * set.
 */
bool
OrdinalMapping::isSetOrdinal() const
{
  return mIsSetOrdinal;
}


/*
 * Sets the value of the "geometryDefinition" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::setGeometryDefinition(const std::string& geometryDefinition)
{
  if (!(SyntaxChecker::isValidInternalSId(geometryDefinition)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mGeometryDefinition = geometryDefinition;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "ordinal" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::setOrdinal(int ordinal)
{
  mOrdinal = ordinal;
  mIsSetOrdinal = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "geometryDefinition" attribute of this
 * OrdinalMapping.
 */
int
OrdinalMapping::unsetGeometryDefinition()
{
  mGeometryDefinition.erase();

  if (mGeometryDefinition.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "ordinal" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::unsetOrdinal()
{
  mOrdinal = SBML_INT_MAX;
  mIsSetOrdinal = false;

  if (isSetOrdinal() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * @copydoc doc_renamesidref_common
 */
void
OrdinalMapping::renameSIdRefs(const std::string& oldid,
                              const std::string& newid)
{
  if (isSetGeometryDefinition() && mGeometryDefinition == oldid)
  {
    setGeometryDefinition(newid);
  }
}


/*
 * Returns the XML element name of this OrdinalMapping object.
 */
const std::string&
OrdinalMapping::getElementName() const
{
  static const string name = "ordinalMapping";
  return name;
}


/*
 * Returns the libSBML type code for this OrdinalMapping object.
 */
int
OrdinalMapping::getTypeCode() const
{
  return SBML_SPATIAL_ORDINALMAPPING;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * OrdinalMapping object have been set.
 */
bool
OrdinalMapping::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetGeometryDefinition() == false)
  {
    allPresent = false;
  }

  if (isSetOrdinal() == false)
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
OrdinalMapping::writeElements(XMLOutputStream& stream) const
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
OrdinalMapping::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
OrdinalMapping::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
OrdinalMapping::enablePackageInternal(const std::string& pkgURI,
                                      const std::string& pkgPrefix,
                                      bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::getAttribute(const std::string& attributeName,
                             bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::getAttribute(const std::string& attributeName,
                             int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "ordinal")
  {
    value = getOrdinal();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::getAttribute(const std::string& attributeName,
                             double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::getAttribute(const std::string& attributeName,
                             unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::getAttribute(const std::string& attributeName,
                             std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "geometryDefinition")
  {
    value = getGeometryDefinition();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this OrdinalMapping's attribute
 * "attributeName" is set.
 */
bool
OrdinalMapping::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "geometryDefinition")
  {
    value = isSetGeometryDefinition();
  }
  else if (attributeName == "ordinal")
  {
    value = isSetOrdinal();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "ordinal")
  {
    return_value = setOrdinal(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::setAttribute(const std::string& attributeName,
                             unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::setAttribute(const std::string& attributeName,
                             const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "geometryDefinition")
  {
    return_value = setGeometryDefinition(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this OrdinalMapping.
 */
int
OrdinalMapping::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "geometryDefinition")
  {
    value = unsetGeometryDefinition();
  }
  else if (attributeName == "ordinal")
  {
    value = unsetOrdinal();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
OrdinalMapping::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("geometryDefinition");

  attributes.add("ordinal");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
OrdinalMapping::readAttributes(const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfOrdinalMappings*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialOrdinalMappingAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialMixedGeometryLOOrdinalMappingsAllowedCoreAttributes, pkgVersion,
            level, version, details);
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
        log->logPackageError("spatial", SpatialOrdinalMappingAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialOrdinalMappingAllowedCoreAttributes, pkgVersion, level, version,
            details);
      }
    }
  }

  // 
  // geometryDefinition SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("geometryDefinition", mGeometryDefinition);

  if (assigned == true)
  {
    if (mGeometryDefinition.empty() == true)
    {
      logEmptyString(mGeometryDefinition, level, version, "<OrdinalMapping>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mGeometryDefinition) == false)
    {
      std::string msg = "The geometryDefinition attribute on the <" +
        getElementName() + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mGeometryDefinition + "', which does not conform to the "
        "syntax.";
      log->logPackageError("spatial",
        SpatialOrdinalMappingGeometryDefinitionMustBeGeometryDefinition,
          pkgVersion, level, version, msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'geometryDefinition' is missing "
      "from the <OrdinalMapping> element.";
    log->logPackageError("spatial", SpatialOrdinalMappingAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // ordinal int (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetOrdinal = attributes.readInto("ordinal", mOrdinal);

  if ( mIsSetOrdinal == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'ordinal' from the "
        "<OrdinalMapping> element must be an integer.";
      log->logPackageError("spatial",
        SpatialOrdinalMappingOrdinalMustBeInteger, pkgVersion, level, version,
          message);
    }
    else
    {
      std::string message = "Spatial attribute 'ordinal' is missing from the "
        "<OrdinalMapping> element.";
      log->logPackageError("spatial", SpatialOrdinalMappingAllowedAttributes,
        pkgVersion, level, version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
OrdinalMapping::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetGeometryDefinition() == true)
  {
    stream.writeAttribute("geometryDefinition", getPrefix(),
      mGeometryDefinition);
  }

  if (isSetOrdinal() == true)
  {
    stream.writeAttribute("ordinal", getPrefix(), mOrdinal);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new OrdinalMapping_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
OrdinalMapping_t *
OrdinalMapping_create(unsigned int level,
                      unsigned int version,
                      unsigned int pkgVersion)
{
  return new OrdinalMapping(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this OrdinalMapping_t object.
 */
LIBSBML_EXTERN
OrdinalMapping_t*
OrdinalMapping_clone(const OrdinalMapping_t* om)
{
  if (om != NULL)
  {
    return static_cast<OrdinalMapping_t*>(om->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this OrdinalMapping_t object.
 */
LIBSBML_EXTERN
void
OrdinalMapping_free(OrdinalMapping_t* om)
{
  if (om != NULL)
  {
    delete om;
  }
}


/*
 * Returns the value of the "geometryDefinition" attribute of this
 * OrdinalMapping_t.
 */
LIBSBML_EXTERN
char *
OrdinalMapping_getGeometryDefinition(const OrdinalMapping_t * om)
{
  if (om == NULL)
  {
    return NULL;
  }

  return om->getGeometryDefinition().empty() ? NULL :
    safe_strdup(om->getGeometryDefinition().c_str());
}


/*
 * Returns the value of the "ordinal" attribute of this OrdinalMapping_t.
 */
LIBSBML_EXTERN
int
OrdinalMapping_getOrdinal(const OrdinalMapping_t * om)
{
  return (om != NULL) ? om->getOrdinal() : SBML_INT_MAX;
}


/*
 * Predicate returning @c 1 (true) if this OrdinalMapping_t's
 * "geometryDefinition" attribute is set.
 */
LIBSBML_EXTERN
int
OrdinalMapping_isSetGeometryDefinition(const OrdinalMapping_t * om)
{
  return (om != NULL) ? static_cast<int>(om->isSetGeometryDefinition()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this OrdinalMapping_t's "ordinal"
 * attribute is set.
 */
LIBSBML_EXTERN
int
OrdinalMapping_isSetOrdinal(const OrdinalMapping_t * om)
{
  return (om != NULL) ? static_cast<int>(om->isSetOrdinal()) : 0;
}


/*
 * Sets the value of the "geometryDefinition" attribute of this
 * OrdinalMapping_t.
 */
LIBSBML_EXTERN
int
OrdinalMapping_setGeometryDefinition(OrdinalMapping_t * om,
                                     const char * geometryDefinition)
{
  return (om != NULL) ? om->setGeometryDefinition(geometryDefinition) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "ordinal" attribute of this OrdinalMapping_t.
 */
LIBSBML_EXTERN
int
OrdinalMapping_setOrdinal(OrdinalMapping_t * om, int ordinal)
{
  return (om != NULL) ? om->setOrdinal(ordinal) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "geometryDefinition" attribute of this
 * OrdinalMapping_t.
 */
LIBSBML_EXTERN
int
OrdinalMapping_unsetGeometryDefinition(OrdinalMapping_t * om)
{
  return (om != NULL) ? om->unsetGeometryDefinition() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "ordinal" attribute of this OrdinalMapping_t.
 */
LIBSBML_EXTERN
int
OrdinalMapping_unsetOrdinal(OrdinalMapping_t * om)
{
  return (om != NULL) ? om->unsetOrdinal() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * OrdinalMapping_t object have been set.
 */
LIBSBML_EXTERN
int
OrdinalMapping_hasRequiredAttributes(const OrdinalMapping_t * om)
{
  return (om != NULL) ? static_cast<int>(om->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


