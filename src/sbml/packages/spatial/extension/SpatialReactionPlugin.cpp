/**
 * @file SpatialReactionPlugin.cpp
 * @brief Implementation of the SpatialReactionPlugin class.
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
#include <sbml/packages/spatial/extension/SpatialReactionPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/Model.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new SpatialReactionPlugin using the given URI, prefix and package
 * namespace.
 */
SpatialReactionPlugin::SpatialReactionPlugin(const std::string& uri,
                                             const std::string& prefix,
                                             SpatialPkgNamespaces* spatialns)
  : SBasePlugin(uri, prefix, spatialns)
  , mIsLocal (false)
  , mIsSetIsLocal (false)
{
}


/*
 * Copy constructor for SpatialReactionPlugin.
 */
SpatialReactionPlugin::SpatialReactionPlugin(const SpatialReactionPlugin& orig)
  : SBasePlugin( orig )
  , mIsLocal ( orig.mIsLocal )
  , mIsSetIsLocal ( orig.mIsSetIsLocal )
{
}


/*
 * Assignment operator for SpatialReactionPlugin.
 */
SpatialReactionPlugin&
SpatialReactionPlugin::operator=(const SpatialReactionPlugin& rhs)
{
  if (&rhs != this)
  {
    SBasePlugin::operator=(rhs);
    mIsLocal = rhs.mIsLocal;
    mIsSetIsLocal = rhs.mIsSetIsLocal;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialReactionPlugin object.
 */
SpatialReactionPlugin*
SpatialReactionPlugin::clone() const
{
  return new SpatialReactionPlugin(*this);
}


/*
 * Destructor for SpatialReactionPlugin.
 */
SpatialReactionPlugin::~SpatialReactionPlugin()
{
}


/*
 * Returns the value of the "isLocal" attribute of this SpatialReactionPlugin.
 */
bool
SpatialReactionPlugin::getIsLocal() const
{
  return mIsLocal;
}


/*
 * Predicate returning @c true if this SpatialReactionPlugin's "isLocal"
 * attribute is set.
 */
bool
SpatialReactionPlugin::isSetIsLocal() const
{
  return mIsSetIsLocal;
}


/*
 * Sets the value of the "isLocal" attribute of this SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::setIsLocal(bool isLocal)
{
  mIsLocal = isLocal;
  mIsSetIsLocal = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "isLocal" attribute of this SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::unsetIsLocal()
{
  mIsLocal = false;
  mIsSetIsLocal = false;

  if (isSetIsLocal() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Predicate returning @c true if all the required attributes for this
 * SpatialReactionPlugin object have been set.
 */
bool
SpatialReactionPlugin::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetIsLocal() == false)
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
SpatialReactionPlugin::writeElements(XMLOutputStream& stream) const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
SpatialReactionPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
SpatialReactionPlugin::enablePackageInternal(const std::string& pkgURI,
                                             const std::string& pkgPrefix,
                                             bool flag)
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::getAttribute(const std::string& attributeName,
                                    bool& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "isLocal")
  {
    value = getIsLocal();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::getAttribute(const std::string& attributeName,
                                    int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::getAttribute(const std::string& attributeName,
                                    double& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::getAttribute(const std::string& attributeName,
                                    unsigned int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::getAttribute(const std::string& attributeName,
                                    std::string& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this SpatialReactionPlugin's attribute
 * "attributeName" is set.
 */
bool
SpatialReactionPlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBasePlugin::isSetAttribute(attributeName);

  if (attributeName == "isLocal")
  {
    value = isSetIsLocal();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::setAttribute(const std::string& attributeName,
                                    bool value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  if (attributeName == "isLocal")
  {
    return_value = setIsLocal(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::setAttribute(const std::string& attributeName,
                                    int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::setAttribute(const std::string& attributeName,
                                    double value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::setAttribute(const std::string& attributeName,
                                    unsigned int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::setAttribute(const std::string& attributeName,
                                    const std::string& value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * SpatialReactionPlugin.
 */
int
SpatialReactionPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBasePlugin::unsetAttribute(attributeName);

  if (attributeName == "isLocal")
  {
    value = unsetIsLocal();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
SpatialReactionPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBasePlugin::addExpectedAttributes(attributes);

  attributes.add("isLocal");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
SpatialReactionPlugin::readAttributes(const XMLAttributes& attributes,
                                      const ExpectedAttributes&
                                        expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  SBMLErrorLog* log = getErrorLog();

  SBasePlugin::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialReactionAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial", SpatialReactionAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == NotSchemaConformant)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(NotSchemaConformant);
        log->logPackageError("spatial", SpatialReactionAllowedAttributes,
          pkgVersion, level, version, details);
      }
    }
  }

  // 
  // isLocal bool (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetIsLocal = attributes.readInto("isLocal", mIsLocal);

  if (mIsSetIsLocal == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      log->logPackageError("spatial", SpatialReactionIsLocalMustBeBoolean,
        pkgVersion, level, version);
    }
    else
    {
      std::string message = "Spatial attribute 'isLocal' is missing from the "
        "<SpatialReactionPlugin> element.";
      log->logPackageError("spatial", SpatialReactionAllowedAttributes,
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
SpatialReactionPlugin::writeAttributes(XMLOutputStream& stream) const
{
  SBasePlugin::writeAttributes(stream);

  if (isSetIsLocal() == true)
  {
    stream.writeAttribute("isLocal", getPrefix(), mIsLocal);
  }
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Returns the value of the "isLocal" attribute of this
 * SpatialReactionPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialReactionPlugin_getIsLocal(const SpatialReactionPlugin_t * srp)
{
  return (srp != NULL) ? static_cast<int>(srp->getIsLocal()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SpatialReactionPlugin_t's "isLocal"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SpatialReactionPlugin_isSetIsLocal(const SpatialReactionPlugin_t * srp)
{
  return (srp != NULL) ? static_cast<int>(srp->isSetIsLocal()) : 0;
}


/*
 * Sets the value of the "isLocal" attribute of this SpatialReactionPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialReactionPlugin_setIsLocal(SpatialReactionPlugin_t * srp, int isLocal)
{
  return (srp != NULL) ? srp->setIsLocal(isLocal) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "isLocal" attribute of this SpatialReactionPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialReactionPlugin_unsetIsLocal(SpatialReactionPlugin_t * srp)
{
  return (srp != NULL) ? srp->unsetIsLocal() : LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END


