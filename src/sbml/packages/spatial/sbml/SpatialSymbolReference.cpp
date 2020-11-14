/**
 * @file SpatialSymbolReference.cpp
 * @brief Implementation of the SpatialSymbolReference class.
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
#include <sbml/packages/spatial/sbml/SpatialSymbolReference.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new SpatialSymbolReference using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
SpatialSymbolReference::SpatialSymbolReference(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion)
  : SBase(level, version)
  , mSpatialRef ("")
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new SpatialSymbolReference using the given SpatialPkgNamespaces
 * object.
 */
SpatialSymbolReference::SpatialSymbolReference(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mSpatialRef ("")
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for SpatialSymbolReference.
 */
SpatialSymbolReference::SpatialSymbolReference(const SpatialSymbolReference&
  orig)
  : SBase( orig )
  , mSpatialRef ( orig.mSpatialRef )
{
}


/*
 * Assignment operator for SpatialSymbolReference.
 */
SpatialSymbolReference&
SpatialSymbolReference::operator=(const SpatialSymbolReference& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mSpatialRef = rhs.mSpatialRef;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialSymbolReference object.
 */
SpatialSymbolReference*
SpatialSymbolReference::clone() const
{
  return new SpatialSymbolReference(*this);
}


/*
 * Destructor for SpatialSymbolReference.
 */
SpatialSymbolReference::~SpatialSymbolReference()
{
}


/*
 * Returns the value of the "spatialRef" attribute of this
 * SpatialSymbolReference.
 */
const std::string&
SpatialSymbolReference::getSpatialRef() const
{
  return mSpatialRef;
}


/*
 * Predicate returning @c true if this SpatialSymbolReference's "spatialRef"
 * attribute is set.
 */
bool
SpatialSymbolReference::isSetSpatialRef() const
{
  return (mSpatialRef.empty() == false);
}


/*
 * Sets the value of the "spatialRef" attribute of this SpatialSymbolReference.
 */
int
SpatialSymbolReference::setSpatialRef(const std::string& spatialRef)
{
  if (!(SyntaxChecker::isValidInternalSId(spatialRef)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSpatialRef = spatialRef;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "spatialRef" attribute of this
 * SpatialSymbolReference.
 */
int
SpatialSymbolReference::unsetSpatialRef()
{
  mSpatialRef.erase();

  if (mSpatialRef.empty() == true)
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
SpatialSymbolReference::renameSIdRefs(const std::string& oldid,
                                      const std::string& newid)
{
  if (isSetSpatialRef() && mSpatialRef == oldid)
  {
    setSpatialRef(newid);
  }
}


/*
 * Returns the XML element name of this SpatialSymbolReference object.
 */
const std::string&
SpatialSymbolReference::getElementName() const
{
  static const string name = "spatialSymbolReference";
  return name;
}


/*
 * Returns the libSBML type code for this SpatialSymbolReference object.
 */
int
SpatialSymbolReference::getTypeCode() const
{
  return SBML_SPATIAL_SPATIALSYMBOLREFERENCE;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * SpatialSymbolReference object have been set.
 */
bool
SpatialSymbolReference::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetSpatialRef() == false)
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
SpatialSymbolReference::writeElements(XMLOutputStream& stream) const
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
SpatialSymbolReference::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
SpatialSymbolReference::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
SpatialSymbolReference::enablePackageInternal(const std::string& pkgURI,
                                              const std::string& pkgPrefix,
                                              bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSymbolReference.
 */
int
SpatialSymbolReference::getAttribute(const std::string& attributeName,
                                     bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSymbolReference.
 */
int
SpatialSymbolReference::getAttribute(const std::string& attributeName,
                                     int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSymbolReference.
 */
int
SpatialSymbolReference::getAttribute(const std::string& attributeName,
                                     double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSymbolReference.
 */
int
SpatialSymbolReference::getAttribute(const std::string& attributeName,
                                     unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSymbolReference.
 */
int
SpatialSymbolReference::getAttribute(const std::string& attributeName,
                                     std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "spatialRef")
  {
    value = getSpatialRef();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this SpatialSymbolReference's attribute
 * "attributeName" is set.
 */
bool
SpatialSymbolReference::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "spatialRef")
  {
    value = isSetSpatialRef();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSymbolReference.
 */
int
SpatialSymbolReference::setAttribute(const std::string& attributeName,
                                     bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSymbolReference.
 */
int
SpatialSymbolReference::setAttribute(const std::string& attributeName,
                                     int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSymbolReference.
 */
int
SpatialSymbolReference::setAttribute(const std::string& attributeName,
                                     double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSymbolReference.
 */
int
SpatialSymbolReference::setAttribute(const std::string& attributeName,
                                     unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSymbolReference.
 */
int
SpatialSymbolReference::setAttribute(const std::string& attributeName,
                                     const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "spatialRef")
  {
    return_value = setSpatialRef(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * SpatialSymbolReference.
 */
int
SpatialSymbolReference::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "spatialRef")
  {
    value = unsetSpatialRef();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
SpatialSymbolReference::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("spatialRef");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
SpatialSymbolReference::readAttributes(const XMLAttributes& attributes,
                                       const ExpectedAttributes&
                                         expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

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
          SpatialSpatialSymbolReferenceAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialSpatialSymbolReferenceAllowedCoreAttributes, pkgVersion, level,
            version, details);
      }
    }
  }

  // 
  // spatialRef SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("spatialRef", mSpatialRef);

  if (assigned == true)
  {
    if (mSpatialRef.empty() == true)
    {
      logEmptyString(mSpatialRef, level, version, "<SpatialSymbolReference>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mSpatialRef) == false)
    {
      std::string msg = "The spatialRef attribute on the <" + getElementName()
        + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mSpatialRef + "', which does not conform to the "
        "syntax.";
      log->logPackageError("spatial",
        SpatialSpatialSymbolReferenceSpatialRefMustReferenceMath, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'spatialRef' is missing from the "
      "<SpatialSymbolReference> element.";
    log->logPackageError("spatial",
      SpatialSpatialSymbolReferenceAllowedAttributes, pkgVersion, level, version,
        message);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
SpatialSymbolReference::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetSpatialRef() == true)
  {
    stream.writeAttribute("spatialRef", getPrefix(), mSpatialRef);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new SpatialSymbolReference_t using the given SBML Level, Version
 * and &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
SpatialSymbolReference_t *
SpatialSymbolReference_create(unsigned int level,
                              unsigned int version,
                              unsigned int pkgVersion)
{
  return new SpatialSymbolReference(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this SpatialSymbolReference_t object.
 */
LIBSBML_EXTERN
SpatialSymbolReference_t*
SpatialSymbolReference_clone(const SpatialSymbolReference_t* ssr)
{
  if (ssr != NULL)
  {
    return static_cast<SpatialSymbolReference_t*>(ssr->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this SpatialSymbolReference_t object.
 */
LIBSBML_EXTERN
void
SpatialSymbolReference_free(SpatialSymbolReference_t* ssr)
{
  if (ssr != NULL)
  {
    delete ssr;
  }
}


/*
 * Returns the value of the "spatialRef" attribute of this
 * SpatialSymbolReference_t.
 */
LIBSBML_EXTERN
char *
SpatialSymbolReference_getSpatialRef(const SpatialSymbolReference_t * ssr)
{
  if (ssr == NULL)
  {
    return NULL;
  }

  return ssr->getSpatialRef().empty() ? NULL :
    safe_strdup(ssr->getSpatialRef().c_str());
}


/*
 * Predicate returning @c 1 (true) if this SpatialSymbolReference_t's
 * "spatialRef" attribute is set.
 */
LIBSBML_EXTERN
int
SpatialSymbolReference_isSetSpatialRef(const SpatialSymbolReference_t * ssr)
{
  return (ssr != NULL) ? static_cast<int>(ssr->isSetSpatialRef()) : 0;
}


/*
 * Sets the value of the "spatialRef" attribute of this
 * SpatialSymbolReference_t.
 */
LIBSBML_EXTERN
int
SpatialSymbolReference_setSpatialRef(SpatialSymbolReference_t * ssr,
                                     const char * spatialRef)
{
  return (ssr != NULL) ? ssr->setSpatialRef(spatialRef) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "spatialRef" attribute of this
 * SpatialSymbolReference_t.
 */
LIBSBML_EXTERN
int
SpatialSymbolReference_unsetSpatialRef(SpatialSymbolReference_t * ssr)
{
  return (ssr != NULL) ? ssr->unsetSpatialRef() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SpatialSymbolReference_t object have been set.
 */
LIBSBML_EXTERN
int
SpatialSymbolReference_hasRequiredAttributes(const SpatialSymbolReference_t *
  ssr)
{
  return (ssr != NULL) ? static_cast<int>(ssr->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


