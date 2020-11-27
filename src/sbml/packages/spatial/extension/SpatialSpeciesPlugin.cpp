/**
 * @file SpatialSpeciesPlugin.cpp
 * @brief Implementation of the SpatialSpeciesPlugin class.
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
#include <sbml/packages/spatial/extension/SpatialSpeciesPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/Model.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new SpatialSpeciesPlugin using the given URI, prefix and package
 * namespace.
 */
SpatialSpeciesPlugin::SpatialSpeciesPlugin(const std::string& uri,
                                           const std::string& prefix,
                                           SpatialPkgNamespaces* spatialns)
  : SBasePlugin(uri, prefix, spatialns)
  , mIsSpatial (false)
  , mIsSetIsSpatial (false)
{
}


/*
 * Copy constructor for SpatialSpeciesPlugin.
 */
SpatialSpeciesPlugin::SpatialSpeciesPlugin(const SpatialSpeciesPlugin& orig)
  : SBasePlugin( orig )
  , mIsSpatial ( orig.mIsSpatial )
  , mIsSetIsSpatial ( orig.mIsSetIsSpatial )
{
}


/*
 * Assignment operator for SpatialSpeciesPlugin.
 */
SpatialSpeciesPlugin&
SpatialSpeciesPlugin::operator=(const SpatialSpeciesPlugin& rhs)
{
  if (&rhs != this)
  {
    SBasePlugin::operator=(rhs);
    mIsSpatial = rhs.mIsSpatial;
    mIsSetIsSpatial = rhs.mIsSetIsSpatial;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialSpeciesPlugin object.
 */
SpatialSpeciesPlugin*
SpatialSpeciesPlugin::clone() const
{
  return new SpatialSpeciesPlugin(*this);
}


/*
 * Destructor for SpatialSpeciesPlugin.
 */
SpatialSpeciesPlugin::~SpatialSpeciesPlugin()
{
}


/*
 * Returns the value of the "isSpatial" attribute of this SpatialSpeciesPlugin.
 */
bool
SpatialSpeciesPlugin::getIsSpatial() const
{
  return mIsSpatial;
}


/*
 * Predicate returning @c true if this SpatialSpeciesPlugin's "isSpatial"
 * attribute is set.
 */
bool
SpatialSpeciesPlugin::isSetIsSpatial() const
{
  return mIsSetIsSpatial;
}


/*
 * Sets the value of the "isSpatial" attribute of this SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::setIsSpatial(bool isSpatial)
{
  mIsSpatial = isSpatial;
  mIsSetIsSpatial = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "isSpatial" attribute of this SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::unsetIsSpatial()
{
  mIsSpatial = false;
  mIsSetIsSpatial = false;

  if (isSetIsSpatial() == false)
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
 * SpatialSpeciesPlugin object have been set.
 */
bool
SpatialSpeciesPlugin::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
SpatialSpeciesPlugin::writeElements(XMLOutputStream& stream) const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
SpatialSpeciesPlugin::accept(SBMLVisitor& v) const
{
  const Species* s = static_cast<const Species*>(this->getParentSBMLObject());
  v.visit(*s);
  v.leave(*s);

  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
SpatialSpeciesPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
SpatialSpeciesPlugin::enablePackageInternal(const std::string& pkgURI,
                                            const std::string& pkgPrefix,
                                            bool flag)
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::getAttribute(const std::string& attributeName,
                                   bool& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "isSpatial")
  {
    value = getIsSpatial();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::getAttribute(const std::string& attributeName,
                                   int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::getAttribute(const std::string& attributeName,
                                   double& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::getAttribute(const std::string& attributeName,
                                   unsigned int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::getAttribute(const std::string& attributeName,
                                   std::string& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this SpatialSpeciesPlugin's attribute
 * "attributeName" is set.
 */
bool
SpatialSpeciesPlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBasePlugin::isSetAttribute(attributeName);

  if (attributeName == "isSpatial")
  {
    value = isSetIsSpatial();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::setAttribute(const std::string& attributeName,
                                   bool value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  if (attributeName == "isSpatial")
  {
    return_value = setIsSpatial(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::setAttribute(const std::string& attributeName,
                                   int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::setAttribute(const std::string& attributeName,
                                   double value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::setAttribute(const std::string& attributeName,
                                   unsigned int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::setAttribute(const std::string& attributeName,
                                   const std::string& value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * SpatialSpeciesPlugin.
 */
int
SpatialSpeciesPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBasePlugin::unsetAttribute(attributeName);

  if (attributeName == "isSpatial")
  {
    value = unsetIsSpatial();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
SpatialSpeciesPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBasePlugin::addExpectedAttributes(attributes);

  attributes.add("isSpatial");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
SpatialSpeciesPlugin::readAttributes(const XMLAttributes& attributes,
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
        log->logPackageError("spatial", SpatialSpeciesAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial", SpatialSpeciesAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == NotSchemaConformant)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(NotSchemaConformant);
        log->logPackageError("spatial", SpatialSpeciesAllowedAttributes,
          pkgVersion, level, version, details);
      }
    }
  }

  // 
  // isSpatial bool (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetIsSpatial = attributes.readInto("isSpatial", mIsSpatial);

  if (mIsSetIsSpatial == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      log->logPackageError("spatial", SpatialSpeciesIsSpatialMustBeBoolean,
        pkgVersion, level, version);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
SpatialSpeciesPlugin::writeAttributes(XMLOutputStream& stream) const
{
  SBasePlugin::writeAttributes(stream);

  if (isSetIsSpatial() == true)
  {
    stream.writeAttribute("isSpatial", getPrefix(), mIsSpatial);
  }
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Returns the value of the "isSpatial" attribute of this
 * SpatialSpeciesPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialSpeciesPlugin_getIsSpatial(const SpatialSpeciesPlugin_t * ssp)
{
  return (ssp != NULL) ? static_cast<int>(ssp->getIsSpatial()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SpatialSpeciesPlugin_t's "isSpatial"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SpatialSpeciesPlugin_isSetIsSpatial(const SpatialSpeciesPlugin_t * ssp)
{
  return (ssp != NULL) ? static_cast<int>(ssp->isSetIsSpatial()) : 0;
}


/*
 * Sets the value of the "isSpatial" attribute of this SpatialSpeciesPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialSpeciesPlugin_setIsSpatial(SpatialSpeciesPlugin_t * ssp, int isSpatial)
{
  return (ssp != NULL) ? ssp->setIsSpatial(isSpatial) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "isSpatial" attribute of this
 * SpatialSpeciesPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialSpeciesPlugin_unsetIsSpatial(SpatialSpeciesPlugin_t * ssp)
{
  return (ssp != NULL) ? ssp->unsetIsSpatial() : LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END


