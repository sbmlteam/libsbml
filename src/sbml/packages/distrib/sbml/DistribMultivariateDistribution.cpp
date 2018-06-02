/**
 * @file DistribMultivariateDistribution.cpp
 * @brief Implementation of the DistribMultivariateDistribution class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/packages/distrib/sbml/DistribMultivariateDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribMultivariateDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribMultivariateDistribution::DistribMultivariateDistribution(
                                                                 unsigned int
                                                                   level,
                                                                 unsigned int
                                                                   version,
                                                                 unsigned int
                                                                   pkgVersion)
  : DistribDistribution(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new DistribMultivariateDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribMultivariateDistribution::DistribMultivariateDistribution(DistribPkgNamespaces
  *distribns)
  : DistribDistribution(distribns)
{
  setElementNamespace(distribns->getURI());
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribMultivariateDistribution.
 */
DistribMultivariateDistribution::DistribMultivariateDistribution(const
  DistribMultivariateDistribution& orig)
  : DistribDistribution( orig )
{
}


/*
 * Assignment operator for DistribMultivariateDistribution.
 */
DistribMultivariateDistribution&
DistribMultivariateDistribution::operator=(const
  DistribMultivariateDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribDistribution::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribMultivariateDistribution
 * object.
 */
DistribMultivariateDistribution*
DistribMultivariateDistribution::clone() const
{
  return new DistribMultivariateDistribution(*this);
}


/*
 * Destructor for DistribMultivariateDistribution.
 */
DistribMultivariateDistribution::~DistribMultivariateDistribution()
{
}


/*
 * Returns the XML element name of this DistribMultivariateDistribution object.
 */
const std::string&
DistribMultivariateDistribution::getElementName() const
{
  static const string name = "distribMultivariateDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribMultivariateDistribution
 * object.
 */
int
DistribMultivariateDistribution::getTypeCode() const
{
  return SBML_DISTRIB_MULTIVARIATEDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribMultivariateDistribution object have been set.
 */
bool
DistribMultivariateDistribution::hasRequiredAttributes() const
{
  bool allPresent = DistribDistribution::hasRequiredAttributes();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribMultivariateDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribDistribution::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribMultivariateDistribution::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribMultivariateDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribDistribution::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribMultivariateDistribution::enablePackageInternal(
                                                       const std::string&
                                                         pkgURI,
                                                       const std::string&
                                                         pkgPrefix,
                                                       bool flag)
{
  DistribDistribution::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribMultivariateDistribution.
 */
int
DistribMultivariateDistribution::getAttribute(const std::string& attributeName,
                                              bool& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribMultivariateDistribution.
 */
int
DistribMultivariateDistribution::getAttribute(const std::string& attributeName,
                                              int& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribMultivariateDistribution.
 */
int
DistribMultivariateDistribution::getAttribute(const std::string& attributeName,
                                              double& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribMultivariateDistribution.
 */
int
DistribMultivariateDistribution::getAttribute(const std::string& attributeName,
                                              unsigned int& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribMultivariateDistribution.
 */
int
DistribMultivariateDistribution::getAttribute(const std::string& attributeName,
                                              std::string& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribMultivariateDistribution's
 * attribute "attributeName" is set.
 */
bool
DistribMultivariateDistribution::isSetAttribute(const std::string&
  attributeName) const
{
  bool value = DistribDistribution::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribMultivariateDistribution.
 */
int
DistribMultivariateDistribution::setAttribute(const std::string& attributeName,
                                              bool value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribMultivariateDistribution.
 */
int
DistribMultivariateDistribution::setAttribute(const std::string& attributeName,
                                              int value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribMultivariateDistribution.
 */
int
DistribMultivariateDistribution::setAttribute(const std::string& attributeName,
                                              double value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribMultivariateDistribution.
 */
int
DistribMultivariateDistribution::setAttribute(const std::string& attributeName,
                                              unsigned int value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribMultivariateDistribution.
 */
int
DistribMultivariateDistribution::setAttribute(const std::string& attributeName,
                                              const std::string& value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribMultivariateDistribution.
 */
int
DistribMultivariateDistribution::unsetAttribute(const std::string&
  attributeName)
{
  int value = DistribDistribution::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribMultivariateDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribDistribution::createObject(stream);

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DistribMultivariateDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribDistribution::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
  }

  if (level == 3 && coreVersion == 2 && pkgVersion == 1)
  {
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribMultivariateDistribution::readAttributes(
                                                const XMLAttributes&
                                                  attributes,
                                                const ExpectedAttributes&
                                                  expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribDistribution::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib", DistribUnknown, pkgVersion, level,
          version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribMultivariateDistributionAllowedCoreAttributes,
            pkgVersion, level, version, details);
      }
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    readL3V1V1Attributes(attributes);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    readL3V2V1Attributes(attributes);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribMultivariateDistribution::readL3V1V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribMultivariateDistribution::readL3V2V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribMultivariateDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribDistribution::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    writeL3V1V1Attributes(stream);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    writeL3V2V1Attributes(stream);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribMultivariateDistribution::writeL3V1V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribMultivariateDistribution::writeL3V2V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribMultivariateDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribMultivariateDistribution_t *
DistribMultivariateDistribution_create(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion)
{
  return new DistribMultivariateDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribMultivariateDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribMultivariateDistribution_t*
DistribMultivariateDistribution_clone(const DistribMultivariateDistribution_t*
  dmd)
{
  if (dmd != NULL)
  {
    return static_cast<DistribMultivariateDistribution_t*>(dmd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribMultivariateDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribMultivariateDistribution_free(DistribMultivariateDistribution_t* dmd)
{
  if (dmd != NULL)
  {
    delete dmd;
  }
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribMultivariateDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribMultivariateDistribution_hasRequiredAttributes(const
  DistribMultivariateDistribution_t * dmd)
{
  return (dmd != NULL) ? static_cast<int>(dmd->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


