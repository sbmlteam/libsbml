/**
 * @file DistribCategoricalUnivariateDistribution.cpp
 * @brief Implementation of the DistribCategoricalUnivariateDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribCategoricalUnivariateDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>

#include <sbml/packages/distrib/sbml/DistribBernoulliDistribution.h>
#include <sbml/packages/distrib/sbml/DistribCategoricalDistribution.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribCategoricalUnivariateDistribution using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
DistribCategoricalUnivariateDistribution::DistribCategoricalUnivariateDistribution(
                                                                                   unsigned int level,
                                                                                   unsigned int version,
                                                                                   unsigned int pkgVersion)
  : DistribUnivariateDistribution(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new DistribCategoricalUnivariateDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribCategoricalUnivariateDistribution::DistribCategoricalUnivariateDistribution(DistribPkgNamespaces
  *distribns)
  : DistribUnivariateDistribution(distribns)
{
  setElementNamespace(distribns->getURI());
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribCategoricalUnivariateDistribution.
 */
DistribCategoricalUnivariateDistribution::DistribCategoricalUnivariateDistribution(const
  DistribCategoricalUnivariateDistribution& orig)
  : DistribUnivariateDistribution( orig )
{
}


/*
 * Assignment operator for DistribCategoricalUnivariateDistribution.
 */
DistribCategoricalUnivariateDistribution&
DistribCategoricalUnivariateDistribution::operator=(const
  DistribCategoricalUnivariateDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribUnivariateDistribution::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this
 * DistribCategoricalUnivariateDistribution object.
 */
DistribCategoricalUnivariateDistribution*
DistribCategoricalUnivariateDistribution::clone() const
{
  return new DistribCategoricalUnivariateDistribution(*this);
}


/*
 * Destructor for DistribCategoricalUnivariateDistribution.
 */
DistribCategoricalUnivariateDistribution::~DistribCategoricalUnivariateDistribution()
{
}


/*
 * Predicate returning @c true if this abstract
 * "DistribCategoricalUnivariateDistribution" is of type
 * DistribBernoulliDistribution
 */
bool
DistribCategoricalUnivariateDistribution::isDistribBernoulliDistribution()
  const
{
  return dynamic_cast<const DistribBernoulliDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribCategoricalUnivariateDistribution" is of type
 * DistribCategoricalDistribution
 */
bool
DistribCategoricalUnivariateDistribution::isDistribCategoricalDistribution()
  const
{
  return dynamic_cast<const DistribCategoricalDistribution*>(this) != NULL;
}


/*
 * Returns the XML element name of this
 * DistribCategoricalUnivariateDistribution object.
 */
const std::string&
DistribCategoricalUnivariateDistribution::getElementName() const
{
  static const string name = "categoricalUnivariateDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this
 * DistribCategoricalUnivariateDistribution object.
 */
int
DistribCategoricalUnivariateDistribution::getTypeCode() const
{
  return SBML_DISTRIB_CATEGORICALUNIVARIATEDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribCategoricalUnivariateDistribution object have been set.
 */
bool
DistribCategoricalUnivariateDistribution::hasRequiredAttributes() const
{
  bool allPresent = DistribUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribCategoricalUnivariateDistribution::writeElements(XMLOutputStream&
  stream) const
{
  DistribUnivariateDistribution::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribCategoricalUnivariateDistribution::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribCategoricalUnivariateDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribUnivariateDistribution::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribCategoricalUnivariateDistribution::enablePackageInternal(
                                                                const
                                                                  std::string&
                                                                    pkgURI,
                                                                const
                                                                  std::string&
                                                                    pkgPrefix,
                                                                bool flag)
{
  DistribUnivariateDistribution::enablePackageInternal(pkgURI, pkgPrefix,
    flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribCategoricalUnivariateDistribution.
 */
int
DistribCategoricalUnivariateDistribution::getAttribute(
                                                       const std::string&
                                                         attributeName,
                                                       bool& value) const
{
  int return_value = DistribUnivariateDistribution::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribCategoricalUnivariateDistribution.
 */
int
DistribCategoricalUnivariateDistribution::getAttribute(
                                                       const std::string&
                                                         attributeName,
                                                       int& value) const
{
  int return_value = DistribUnivariateDistribution::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribCategoricalUnivariateDistribution.
 */
int
DistribCategoricalUnivariateDistribution::getAttribute(
                                                       const std::string&
                                                         attributeName,
                                                       double& value) const
{
  int return_value = DistribUnivariateDistribution::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribCategoricalUnivariateDistribution.
 */
int
DistribCategoricalUnivariateDistribution::getAttribute(
                                                       const std::string&
                                                         attributeName,
                                                       unsigned int& value)
                                                         const
{
  int return_value = DistribUnivariateDistribution::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribCategoricalUnivariateDistribution.
 */
int
DistribCategoricalUnivariateDistribution::getAttribute(
                                                       const std::string&
                                                         attributeName,
                                                       std::string& value)
                                                         const
{
  int return_value = DistribUnivariateDistribution::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this
 * DistribCategoricalUnivariateDistribution's attribute "attributeName" is set.
 */
bool
DistribCategoricalUnivariateDistribution::isSetAttribute(const std::string&
  attributeName) const
{
  bool value = DistribUnivariateDistribution::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribCategoricalUnivariateDistribution.
 */
int
DistribCategoricalUnivariateDistribution::setAttribute(
                                                       const std::string&
                                                         attributeName,
                                                       bool value)
{
  int return_value = DistribUnivariateDistribution::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribCategoricalUnivariateDistribution.
 */
int
DistribCategoricalUnivariateDistribution::setAttribute(
                                                       const std::string&
                                                         attributeName,
                                                       int value)
{
  int return_value = DistribUnivariateDistribution::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribCategoricalUnivariateDistribution.
 */
int
DistribCategoricalUnivariateDistribution::setAttribute(
                                                       const std::string&
                                                         attributeName,
                                                       double value)
{
  int return_value = DistribUnivariateDistribution::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribCategoricalUnivariateDistribution.
 */
int
DistribCategoricalUnivariateDistribution::setAttribute(
                                                       const std::string&
                                                         attributeName,
                                                       unsigned int value)
{
  int return_value = DistribUnivariateDistribution::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribCategoricalUnivariateDistribution.
 */
int
DistribCategoricalUnivariateDistribution::setAttribute(
                                                       const std::string&
                                                         attributeName,
                                                       const std::string&
                                                         value)
{
  int return_value = DistribUnivariateDistribution::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribCategoricalUnivariateDistribution.
 */
int
DistribCategoricalUnivariateDistribution::unsetAttribute(const std::string&
  attributeName)
{
  int value = DistribUnivariateDistribution::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribCategoricalUnivariateDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribUnivariateDistribution::createObject(stream);

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DistribCategoricalUnivariateDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribUnivariateDistribution::addExpectedAttributes(attributes);

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
DistribCategoricalUnivariateDistribution::readAttributes(
                                                         const XMLAttributes&
                                                           attributes,
                                                         const
                                                           ExpectedAttributes&
                                                             expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribUnivariateDistribution::readAttributes(attributes,
    expectedAttributes);

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
          DistribDistribCategoricalUnivariateDistributionAllowedCoreAttributes,
            pkgVersion, level, version, details);
      }
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    readL3V1V1Attributes(attributes);
  }

  else
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
DistribCategoricalUnivariateDistribution::readL3V1V1Attributes(const
  XMLAttributes& attributes)
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
DistribCategoricalUnivariateDistribution::readL3V2V1Attributes(const
  XMLAttributes& attributes)
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
DistribCategoricalUnivariateDistribution::writeAttributes(XMLOutputStream&
  stream) const
{
  DistribUnivariateDistribution::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    writeL3V1V1Attributes(stream);
  }

  else
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
DistribCategoricalUnivariateDistribution::writeL3V1V1Attributes(XMLOutputStream&
  stream) const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribCategoricalUnivariateDistribution::writeL3V2V1Attributes(XMLOutputStream&
  stream) const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribCategoricalUnivariateDistribution_t using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribCategoricalUnivariateDistribution_t *
DistribCategoricalUnivariateDistribution_create(unsigned int level,
                                                unsigned int version,
                                                unsigned int pkgVersion)
{
  return new DistribCategoricalUnivariateDistribution(level, version,
    pkgVersion);
}


/*
 * Creates and returns a deep copy of this
 * DistribCategoricalUnivariateDistribution_t object.
 */
LIBSBML_EXTERN
DistribCategoricalUnivariateDistribution_t*
DistribCategoricalUnivariateDistribution_clone(const
  DistribCategoricalUnivariateDistribution_t* dcud)
{
  if (dcud != NULL)
  {
    return
      static_cast<DistribCategoricalUnivariateDistribution_t*>(dcud->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribCategoricalUnivariateDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribCategoricalUnivariateDistribution_free(DistribCategoricalUnivariateDistribution_t*
  dcud)
{
  if (dcud != NULL)
  {
    delete dcud;
  }
}


/*
 * Predicate returning @c 1 if this DistribCategoricalUnivariateDistribution_t
 * is of type DistribBernoulliDistribution_t
 */
LIBSBML_EXTERN
int
DistribCategoricalUnivariateDistribution_isDistribBernoulliDistribution(const
  DistribCategoricalUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ?
    static_cast<int>(dcud->isDistribBernoulliDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribCategoricalUnivariateDistribution_t
 * is of type DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
int
DistribCategoricalUnivariateDistribution_isDistribCategoricalDistribution(const
  DistribCategoricalUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ?
    static_cast<int>(dcud->isDistribCategoricalDistribution()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribCategoricalUnivariateDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribCategoricalUnivariateDistribution_hasRequiredAttributes(const
  DistribCategoricalUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


