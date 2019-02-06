/**
 * @file DistribUnivariateDistribution.cpp
 * @brief Implementation of the DistribUnivariateDistribution class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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
#include <sbml/packages/distrib/sbml/DistribUnivariateDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>

#include <sbml/packages/distrib/sbml/DistribBetaDistribution.h>
#include <sbml/packages/distrib/sbml/DistribCauchyDistribution.h>
#include <sbml/packages/distrib/sbml/DistribChiSquareDistribution.h>
#include <sbml/packages/distrib/sbml/DistribExponentialDistribution.h>
#include <sbml/packages/distrib/sbml/DistribFDistribution.h>
#include <sbml/packages/distrib/sbml/DistribGammaDistribution.h>
#include <sbml/packages/distrib/sbml/DistribInverseGammaDistribution.h>
#include <sbml/packages/distrib/sbml/DistribLaPlaceDistribution.h>
#include <sbml/packages/distrib/sbml/DistribLogNormalDistribution.h>
#include <sbml/packages/distrib/sbml/DistribLogisticDistribution.h>
#include <sbml/packages/distrib/sbml/DistribNormalDistribution.h>
#include <sbml/packages/distrib/sbml/DistribParetoDistribution.h>
#include <sbml/packages/distrib/sbml/DistribRayleighDistribution.h>
#include <sbml/packages/distrib/sbml/DistribStudentTDistribution.h>
#include <sbml/packages/distrib/sbml/DistribUniformDistribution.h>
#include <sbml/packages/distrib/sbml/DistribWeibullDistribution.h>
#include <sbml/packages/distrib/sbml/DistribBinomialDistribution.h>
#include <sbml/packages/distrib/sbml/DistribGeometricDistribution.h>
#include <sbml/packages/distrib/sbml/DistribHypergeometricDistribution.h>
#include <sbml/packages/distrib/sbml/DistribNegativeBinomialDistribution.h>
#include <sbml/packages/distrib/sbml/DistribPoissonDistribution.h>
#include <sbml/packages/distrib/sbml/DistribBernoulliDistribution.h>
#include <sbml/packages/distrib/sbml/DistribCategoricalDistribution.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribUnivariateDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribUnivariateDistribution::DistribUnivariateDistribution(
                                                             unsigned int
                                                               level,
                                                             unsigned int
                                                               version,
                                                             unsigned int
                                                               pkgVersion)
  : DistribDistribution(level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new DistribUnivariateDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribUnivariateDistribution::DistribUnivariateDistribution(DistribPkgNamespaces
  *distribns)
  : DistribDistribution(distribns)
{
  setElementNamespace(distribns->getURI());
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribUnivariateDistribution.
 */
DistribUnivariateDistribution::DistribUnivariateDistribution(const
  DistribUnivariateDistribution& orig)
  : DistribDistribution( orig )
{
}


/*
 * Assignment operator for DistribUnivariateDistribution.
 */
DistribUnivariateDistribution&
DistribUnivariateDistribution::operator=(const DistribUnivariateDistribution&
  rhs)
{
  if (&rhs != this)
  {
    DistribDistribution::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribUnivariateDistribution
 * object.
 */
DistribUnivariateDistribution*
DistribUnivariateDistribution::clone() const
{
  return new DistribUnivariateDistribution(*this);
}


/*
 * Destructor for DistribUnivariateDistribution.
 */
DistribUnivariateDistribution::~DistribUnivariateDistribution()
{
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribBetaDistribution
 */
bool
DistribUnivariateDistribution::isDistribBetaDistribution() const
{
  return dynamic_cast<const DistribBetaDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribCauchyDistribution
 */
bool
DistribUnivariateDistribution::isDistribCauchyDistribution() const
{
  return dynamic_cast<const DistribCauchyDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribChiSquareDistribution
 */
bool
DistribUnivariateDistribution::isDistribChiSquareDistribution() const
{
  return dynamic_cast<const DistribChiSquareDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribExponentialDistribution
 */
bool
DistribUnivariateDistribution::isDistribExponentialDistribution() const
{
  return dynamic_cast<const DistribExponentialDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribFDistribution
 */
bool
DistribUnivariateDistribution::isDistribFDistribution() const
{
  return dynamic_cast<const DistribFDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribGammaDistribution
 */
bool
DistribUnivariateDistribution::isDistribGammaDistribution() const
{
  return dynamic_cast<const DistribGammaDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribInverseGammaDistribution
 */
bool
DistribUnivariateDistribution::isDistribInverseGammaDistribution() const
{
  return dynamic_cast<const DistribInverseGammaDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribLaPlaceDistribution
 */
bool
DistribUnivariateDistribution::isDistribLaPlaceDistribution() const
{
  return dynamic_cast<const DistribLaPlaceDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribLogNormalDistribution
 */
bool
DistribUnivariateDistribution::isDistribLogNormalDistribution() const
{
  return dynamic_cast<const DistribLogNormalDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribLogisticDistribution
 */
bool
DistribUnivariateDistribution::isDistribLogisticDistribution() const
{
  return dynamic_cast<const DistribLogisticDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribNormalDistribution
 */
bool
DistribUnivariateDistribution::isDistribNormalDistribution() const
{
  return dynamic_cast<const DistribNormalDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribParetoDistribution
 */
bool
DistribUnivariateDistribution::isDistribParetoDistribution() const
{
  return dynamic_cast<const DistribParetoDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribRayleighDistribution
 */
bool
DistribUnivariateDistribution::isDistribRayleighDistribution() const
{
  return dynamic_cast<const DistribRayleighDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribStudentTDistribution
 */
bool
DistribUnivariateDistribution::isDistribStudentTDistribution() const
{
  return dynamic_cast<const DistribStudentTDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribUniformDistribution
 */
bool
DistribUnivariateDistribution::isDistribUniformDistribution() const
{
  return dynamic_cast<const DistribUniformDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribWeibullDistribution
 */
bool
DistribUnivariateDistribution::isDistribWeibullDistribution() const
{
  return dynamic_cast<const DistribWeibullDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribBinomialDistribution
 */
bool
DistribUnivariateDistribution::isDistribBinomialDistribution() const
{
  return dynamic_cast<const DistribBinomialDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribGeometricDistribution
 */
bool
DistribUnivariateDistribution::isDistribGeometricDistribution() const
{
  return dynamic_cast<const DistribGeometricDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribHypergeometricDistribution
 */
bool
DistribUnivariateDistribution::isDistribHypergeometricDistribution() const
{
  return dynamic_cast<const DistribHypergeometricDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribNegativeBinomialDistribution
 */
bool
DistribUnivariateDistribution::isDistribNegativeBinomialDistribution() const
{
  return dynamic_cast<const DistribNegativeBinomialDistribution*>(this) !=
    NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribPoissonDistribution
 */
bool
DistribUnivariateDistribution::isDistribPoissonDistribution() const
{
  return dynamic_cast<const DistribPoissonDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribBernoulliDistribution
 */
bool
DistribUnivariateDistribution::isDistribBernoulliDistribution() const
{
  return dynamic_cast<const DistribBernoulliDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUnivariateDistribution"
 * is of type DistribCategoricalDistribution
 */
bool
DistribUnivariateDistribution::isDistribCategoricalDistribution() const
{
  return dynamic_cast<const DistribCategoricalDistribution*>(this) != NULL;
}


/*
 * Returns the XML element name of this DistribUnivariateDistribution object.
 */
const std::string&
DistribUnivariateDistribution::getElementName() const
{
  static const string name = "distribUnivariateDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribUnivariateDistribution object.
 */
int
DistribUnivariateDistribution::getTypeCode() const
{
  return SBML_DISTRIB_UNIVARIATEDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribUnivariateDistribution object have been set.
 */
bool
DistribUnivariateDistribution::hasRequiredAttributes() const
{
  bool allPresent = DistribDistribution::hasRequiredAttributes();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribUnivariateDistribution::writeElements(XMLOutputStream& stream) const
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
DistribUnivariateDistribution::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribUnivariateDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribDistribution::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribUnivariateDistribution::enablePackageInternal(const std::string& pkgURI,
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
 * DistribUnivariateDistribution.
 */
int
DistribUnivariateDistribution::getAttribute(const std::string& attributeName,
                                            bool& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUnivariateDistribution.
 */
int
DistribUnivariateDistribution::getAttribute(const std::string& attributeName,
                                            int& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUnivariateDistribution.
 */
int
DistribUnivariateDistribution::getAttribute(const std::string& attributeName,
                                            double& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUnivariateDistribution.
 */
int
DistribUnivariateDistribution::getAttribute(const std::string& attributeName,
                                            unsigned int& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUnivariateDistribution.
 */
int
DistribUnivariateDistribution::getAttribute(const std::string& attributeName,
                                            std::string& value) const
{
  int return_value = DistribDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribUnivariateDistribution's
 * attribute "attributeName" is set.
 */
bool
DistribUnivariateDistribution::isSetAttribute(const std::string& attributeName)
  const
{
  bool value = DistribDistribution::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUnivariateDistribution.
 */
int
DistribUnivariateDistribution::setAttribute(const std::string& attributeName,
                                            bool value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUnivariateDistribution.
 */
int
DistribUnivariateDistribution::setAttribute(const std::string& attributeName,
                                            int value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUnivariateDistribution.
 */
int
DistribUnivariateDistribution::setAttribute(const std::string& attributeName,
                                            double value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUnivariateDistribution.
 */
int
DistribUnivariateDistribution::setAttribute(const std::string& attributeName,
                                            unsigned int value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUnivariateDistribution.
 */
int
DistribUnivariateDistribution::setAttribute(const std::string& attributeName,
                                            const std::string& value)
{
  int return_value = DistribDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribUnivariateDistribution.
 */
int
DistribUnivariateDistribution::unsetAttribute(const std::string& attributeName)
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
DistribUnivariateDistribution::createObject(XMLInputStream& stream)
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
DistribUnivariateDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribDistribution::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribUnivariateDistribution::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribUnivariateDistributionAllowedCoreAttributes, pkgVersion,
            level, version, details);
      }
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribUnivariateDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribDistribution::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribUnivariateDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribUnivariateDistribution_t *
DistribUnivariateDistribution_create(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion)
{
  return new DistribUnivariateDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribUnivariateDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribUnivariateDistribution_t*
DistribUnivariateDistribution_clone(const DistribUnivariateDistribution_t* dud)
{
  if (dud != NULL)
  {
    return static_cast<DistribUnivariateDistribution_t*>(dud->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribUnivariateDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribUnivariateDistribution_free(DistribUnivariateDistribution_t* dud)
{
  if (dud != NULL)
  {
    delete dud;
  }
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribBetaDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribBetaDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribCauchyDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribCauchyDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribChiSquareDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribChiSquareDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ?
    static_cast<int>(dud->isDistribChiSquareDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribExponentialDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribExponentialDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ?
    static_cast<int>(dud->isDistribExponentialDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribFDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribFDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribFDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribGammaDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribGammaDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribGammaDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribInverseGammaDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribInverseGammaDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ?
    static_cast<int>(dud->isDistribInverseGammaDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribLaPlaceDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribLaPlaceDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribLaPlaceDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribLogNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribLogNormalDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ?
    static_cast<int>(dud->isDistribLogNormalDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribLogisticDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribLogisticDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribLogisticDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribNormalDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribNormalDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribParetoDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribParetoDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribParetoDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribRayleighDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribRayleighDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribRayleighDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribStudentTDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribStudentTDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribUniformDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribUniformDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribUniformDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribWeibullDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribWeibullDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribWeibullDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribBinomialDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribBinomialDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribBinomialDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribGeometricDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ?
    static_cast<int>(dud->isDistribGeometricDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribHypergeometricDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ?
    static_cast<int>(dud->isDistribHypergeometricDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribNegativeBinomialDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribNegativeBinomialDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ?
    static_cast<int>(dud->isDistribNegativeBinomialDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribPoissonDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isDistribPoissonDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribBernoulliDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribBernoulliDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ?
    static_cast<int>(dud->isDistribBernoulliDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribCategoricalDistribution(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ?
    static_cast<int>(dud->isDistribCategoricalDistribution()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUnivariateDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_hasRequiredAttributes(const
  DistribUnivariateDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


