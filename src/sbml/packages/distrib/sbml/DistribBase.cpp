/**
 * @file DistribBase.cpp
 * @brief Implementation of the DistribBase class.
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
#include <sbml/packages/distrib/sbml/DistribBase.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>

#include <sbml/packages/distrib/sbml/DistribDrawFromDistribution.h>
#include <sbml/packages/distrib/sbml/DistribInput.h>
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
#include <sbml/packages/distrib/sbml/DistribMultivariateDistribution.h>
#include <sbml/packages/distrib/sbml/DistribExternalDistribution.h>
#include <sbml/packages/distrib/sbml/DistribUncertBound.h>
#include <sbml/packages/distrib/sbml/DistribExternalParameter.h>
#include <sbml/packages/distrib/sbml/DistribCategory.h>
#include <sbml/packages/distrib/sbml/DistribUncertainty.h>
#include <sbml/packages/distrib/sbml/DistribUncertStatistics.h>
#include <sbml/packages/distrib/sbml/DistribUncertStatisticSpan.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribBase using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
DistribBase::DistribBase(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
  : SBase(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new DistribBase using the given DistribPkgNamespaces object.
 */
DistribBase::DistribBase(DistribPkgNamespaces *distribns)
  : SBase(distribns)
{
  setElementNamespace(distribns->getURI());
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribBase.
 */
DistribBase::DistribBase(const DistribBase& orig)
  : SBase( orig )
{
}


/*
 * Assignment operator for DistribBase.
 */
DistribBase&
DistribBase::operator=(const DistribBase& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribBase object.
 */
DistribBase*
DistribBase::clone() const
{
  return new DistribBase(*this);
}


/*
 * Destructor for DistribBase.
 */
DistribBase::~DistribBase()
{
}


/*
 * Returns the value of the "id" attribute of this DistribBase.
 */
const std::string&
DistribBase::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this DistribBase.
 */
const std::string&
DistribBase::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribBase's "id" attribute is set.
 */
bool
DistribBase::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribBase's "name" attribute is set.
 */
bool
DistribBase::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this DistribBase.
 */
int
DistribBase::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DistribBase.
 */
int
DistribBase::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DistribBase.
 */
int
DistribBase::unsetId()
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
 * Unsets the value of the "name" attribute of this DistribBase.
 */
int
DistribBase::unsetName()
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
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribDrawFromDistribution
 */
bool
DistribBase::isDistribDrawFromDistribution() const
{
  return dynamic_cast<const DistribDrawFromDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribInput
 */
bool
DistribBase::isDistribInput() const
{
  return dynamic_cast<const DistribInput*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribBetaDistribution
 */
bool
DistribBase::isDistribBetaDistribution() const
{
  return dynamic_cast<const DistribBetaDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribCauchyDistribution
 */
bool
DistribBase::isDistribCauchyDistribution() const
{
  return dynamic_cast<const DistribCauchyDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribChiSquareDistribution
 */
bool
DistribBase::isDistribChiSquareDistribution() const
{
  return dynamic_cast<const DistribChiSquareDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribExponentialDistribution
 */
bool
DistribBase::isDistribExponentialDistribution() const
{
  return dynamic_cast<const DistribExponentialDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribFDistribution
 */
bool
DistribBase::isDistribFDistribution() const
{
  return dynamic_cast<const DistribFDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribGammaDistribution
 */
bool
DistribBase::isDistribGammaDistribution() const
{
  return dynamic_cast<const DistribGammaDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribInverseGammaDistribution
 */
bool
DistribBase::isDistribInverseGammaDistribution() const
{
  return dynamic_cast<const DistribInverseGammaDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribLaPlaceDistribution
 */
bool
DistribBase::isDistribLaPlaceDistribution() const
{
  return dynamic_cast<const DistribLaPlaceDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribLogNormalDistribution
 */
bool
DistribBase::isDistribLogNormalDistribution() const
{
  return dynamic_cast<const DistribLogNormalDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribLogisticDistribution
 */
bool
DistribBase::isDistribLogisticDistribution() const
{
  return dynamic_cast<const DistribLogisticDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribNormalDistribution
 */
bool
DistribBase::isDistribNormalDistribution() const
{
  return dynamic_cast<const DistribNormalDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribParetoDistribution
 */
bool
DistribBase::isDistribParetoDistribution() const
{
  return dynamic_cast<const DistribParetoDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribRayleighDistribution
 */
bool
DistribBase::isDistribRayleighDistribution() const
{
  return dynamic_cast<const DistribRayleighDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribStudentTDistribution
 */
bool
DistribBase::isDistribStudentTDistribution() const
{
  return dynamic_cast<const DistribStudentTDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribUniformDistribution
 */
bool
DistribBase::isDistribUniformDistribution() const
{
  return dynamic_cast<const DistribUniformDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribWeibullDistribution
 */
bool
DistribBase::isDistribWeibullDistribution() const
{
  return dynamic_cast<const DistribWeibullDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribBinomialDistribution
 */
bool
DistribBase::isDistribBinomialDistribution() const
{
  return dynamic_cast<const DistribBinomialDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribGeometricDistribution
 */
bool
DistribBase::isDistribGeometricDistribution() const
{
  return dynamic_cast<const DistribGeometricDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribHypergeometricDistribution
 */
bool
DistribBase::isDistribHypergeometricDistribution() const
{
  return dynamic_cast<const DistribHypergeometricDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribNegativeBinomialDistribution
 */
bool
DistribBase::isDistribNegativeBinomialDistribution() const
{
  return dynamic_cast<const DistribNegativeBinomialDistribution*>(this) !=
    NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribPoissonDistribution
 */
bool
DistribBase::isDistribPoissonDistribution() const
{
  return dynamic_cast<const DistribPoissonDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribBernoulliDistribution
 */
bool
DistribBase::isDistribBernoulliDistribution() const
{
  return dynamic_cast<const DistribBernoulliDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribCategoricalDistribution
 */
bool
DistribBase::isDistribCategoricalDistribution() const
{
  return dynamic_cast<const DistribCategoricalDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribMultivariateDistribution
 */
bool
DistribBase::isDistribMultivariateDistribution() const
{
  return dynamic_cast<const DistribMultivariateDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribExternalDistribution
 */
bool
DistribBase::isDistribExternalDistribution() const
{
  return dynamic_cast<const DistribExternalDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribUncertBound
 */
bool
DistribBase::isDistribUncertBound() const
{
  return dynamic_cast<const DistribUncertBound*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribExternalParameter
 */
bool
DistribBase::isDistribExternalParameter() const
{
  return dynamic_cast<const DistribExternalParameter*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribCategory
 */
bool
DistribBase::isDistribCategory() const
{
  return dynamic_cast<const DistribCategory*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribUncertainty
 */
bool
DistribBase::isDistribUncertainty() const
{
  return dynamic_cast<const DistribUncertainty*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribUncertStatistics
 */
bool
DistribBase::isDistribUncertStatistics() const
{
  return dynamic_cast<const DistribUncertStatistics*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribBase" is of type
 * DistribUncertStatisticSpan
 */
bool
DistribBase::isDistribUncertStatisticSpan() const
{
  return dynamic_cast<const DistribUncertStatisticSpan*>(this) != NULL;
}


/*
 * Returns the XML element name of this DistribBase object.
 */
const std::string&
DistribBase::getElementName() const
{
  static const string name = "distribBase";
  return name;
}


/*
 * Returns the libSBML type code for this DistribBase object.
 */
int
DistribBase::getTypeCode() const
{
  return SBML_DISTRIB_DISTRIBBASE;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribBase object have been set.
 */
bool
DistribBase::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribBase::writeElements(XMLOutputStream& stream) const
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
DistribBase::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribBase::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribBase::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix,
                                   bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName,
                          double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName,
                          unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::getAttribute(const std::string& attributeName,
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
 * Predicate returning @c true if this DistribBase's attribute "attributeName"
 * is set.
 */
bool
DistribBase::isSetAttribute(const std::string& attributeName) const
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

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName,
                          unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::setAttribute(const std::string& attributeName,
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
 * Unsets the value of the "attributeName" attribute of this DistribBase.
 */
int
DistribBase::unsetAttribute(const std::string& attributeName)
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

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DistribBase::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
    attributes.add("id");
    attributes.add("name");
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
DistribBase::readAttributes(const XMLAttributes& attributes,
                            const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();
  int origNumErrs = log->getNumErrors();

  SBase::readAttributes(attributes, expectedAttributes);

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    readL3V1V1Attributes(attributes);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    readL3V2V1Attributes(attributes, origNumErrs);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribBase::readL3V1V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  if (attributes.getIndex("id", "") >= 0)
  {
    string details = "The <distrib:";
    details += getElementName() + "> element with the 'id' with value '"
      + attributes.getValue("id", "") + "' must use 'distrib:id' instead.";
    log->logPackageError("distrib", DistribIdL3v1NamespaceRule,
      pkgVersion, level, version, details);
  }
  if (attributes.getIndex("name", "") >= 0)
  {
    string details = "The <distrib:";
    details += getElementName() + "> element with the 'name' with value '"
      + attributes.getValue("name", "") + "' must use 'distrib:name' instead.";
    log->logPackageError("distrib", DistribNameL3v1NamespaceRule,
      pkgVersion, level, version, "");
  }
  // 
  // id SId (use = "optional" )
  // 

  XMLTriple tripleID("id", mURI, getPrefix());
  assigned = attributes.readInto(tripleID, mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribBase>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("distrib", DistribIdSyntaxRule, pkgVersion, level,
        version, "The distrib:id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  XMLTriple tripleNAME("name", mURI, getPrefix());
  assigned = attributes.readInto(tripleNAME, mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<DistribBase>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribBase::readL3V2V1Attributes(const XMLAttributes& attributes, int origNumErrs)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  if (attributes.getIndex("id", mURI) >= 0)
  {
    string details = "The <distrib:";
    details += getElementName() + "> element with the 'distrib:id' with value '"
      + attributes.getValue("id", mURI)
      + "' must use the 'id' attribute instead.";
    log->logPackageError("distrib", DistribIdL3v2NamespaceRule,
      pkgVersion, level, version, details);
    //Remove the original error:
    for (int n = log->getNumErrors(); n > origNumErrs; n--)
    {
      if (log->getError(n - 1)->getErrorId() == UnknownPackageAttribute)
      {
        log->remove(UnknownPackageAttribute);
      }
    }
  }
  if (attributes.getIndex("name", mURI) >= 0)
  {
    string details = "The <distrib:";
    details += getElementName() + "> element with the 'distrib:name' with value '"
      + attributes.getValue("name", mURI)
      + "' must use the 'name' attribute instead.";
    log->logPackageError("distrib", DistribNameL3v2NamespaceRule,
      pkgVersion, level, version, details);
    //Remove the original error:
    for (int n = log->getNumErrors(); n > origNumErrs; n--)
    {
      if (log->getError(n - 1)->getErrorId() == UnknownPackageAttribute)
      {
        log->remove(UnknownPackageAttribute);
      }
    }
  }
  // 
  // id SId (use = "optional" )
  // 

  // read by SBase

  // 
  // name string (use = "optional" )
  // 

  // read by SBase;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribBase::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

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
DistribBase::writeL3V1V1Attributes(XMLOutputStream& stream) const
{
  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribBase::writeL3V2V1Attributes(XMLOutputStream& stream) const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribDrawFromDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribDrawFromDistribution(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion)
{
  return new DistribDrawFromDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribInput (DistribBase_t) using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribInput(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion)
{
  return new DistribInput(level, version, pkgVersion);
}


/*
 * Creates a new DistribBetaDistribution (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribBetaDistribution(unsigned int level,
                                          unsigned int version,
                                          unsigned int pkgVersion)
{
  return new DistribBetaDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribCauchyDistribution (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribCauchyDistribution(unsigned int level,
                                            unsigned int version,
                                            unsigned int pkgVersion)
{
  return new DistribCauchyDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribChiSquareDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribChiSquareDistribution(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion)
{
  return new DistribChiSquareDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribExponentialDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribExponentialDistribution(unsigned int level,
                                                 unsigned int version,
                                                 unsigned int pkgVersion)
{
  return new DistribExponentialDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribFDistribution (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribFDistribution(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion)
{
  return new DistribFDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribGammaDistribution (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribGammaDistribution(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion)
{
  return new DistribGammaDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribInverseGammaDistribution (DistribBase_t) using the
 * given SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribInverseGammaDistribution(unsigned int level,
                                                  unsigned int version,
                                                  unsigned int pkgVersion)
{
  return new DistribInverseGammaDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribLaPlaceDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribLaPlaceDistribution(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion)
{
  return new DistribLaPlaceDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribLogNormalDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribLogNormalDistribution(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion)
{
  return new DistribLogNormalDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribLogisticDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribLogisticDistribution(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion)
{
  return new DistribLogisticDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribNormalDistribution (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribNormalDistribution(unsigned int level,
                                            unsigned int version,
                                            unsigned int pkgVersion)
{
  return new DistribNormalDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribParetoDistribution (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribParetoDistribution(unsigned int level,
                                            unsigned int version,
                                            unsigned int pkgVersion)
{
  return new DistribParetoDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribRayleighDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribRayleighDistribution(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion)
{
  return new DistribRayleighDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribStudentTDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribStudentTDistribution(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion)
{
  return new DistribStudentTDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribUniformDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribUniformDistribution(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion)
{
  return new DistribUniformDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribWeibullDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribWeibullDistribution(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion)
{
  return new DistribWeibullDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribBinomialDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribBinomialDistribution(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion)
{
  return new DistribBinomialDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribGeometricDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribGeometricDistribution(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion)
{
  return new DistribGeometricDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribHypergeometricDistribution (DistribBase_t) using the
 * given SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribHypergeometricDistribution(unsigned int level,
                                                    unsigned int version,
                                                    unsigned int pkgVersion)
{
  return new DistribHypergeometricDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribNegativeBinomialDistribution (DistribBase_t) using the
 * given SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribNegativeBinomialDistribution(unsigned int level,
                                                      unsigned int version,
                                                      unsigned int pkgVersion)
{
  return new DistribNegativeBinomialDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribPoissonDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribPoissonDistribution(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion)
{
  return new DistribPoissonDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribBernoulliDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribBernoulliDistribution(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion)
{
  return new DistribBernoulliDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribCategoricalDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribCategoricalDistribution(unsigned int level,
                                                 unsigned int version,
                                                 unsigned int pkgVersion)
{
  return new DistribCategoricalDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribMultivariateDistribution (DistribBase_t) using the
 * given SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribMultivariateDistribution(unsigned int level,
                                                  unsigned int version,
                                                  unsigned int pkgVersion)
{
  return new DistribMultivariateDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribExternalDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribExternalDistribution(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion)
{
  return new DistribExternalDistribution(level, version, pkgVersion);
}


/*
 * Creates a new DistribUncertBound (DistribBase_t) using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribUncertBound(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion)
{
  return new DistribUncertBound(level, version, pkgVersion);
}


/*
 * Creates a new DistribExternalParameter (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribExternalParameter(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion)
{
  return new DistribExternalParameter(level, version, pkgVersion);
}


/*
 * Creates a new DistribCategory (DistribBase_t) using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribCategory(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion)
{
  return new DistribCategory(level, version, pkgVersion);
}


/*
 * Creates a new DistribUncertainty (DistribBase_t) using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribUncertainty(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion)
{
  return new DistribUncertainty(level, version, pkgVersion);
}


/*
 * Creates a new DistribUncertStatistics (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribUncertStatistics(unsigned int level,
                                          unsigned int version,
                                          unsigned int pkgVersion)
{
  return new DistribUncertStatistics(level, version, pkgVersion);
}


/*
 * Creates a new DistribUncertStatisticSpan (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribUncertStatisticSpan(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion)
{
  return new DistribUncertStatisticSpan(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribBase_t object.
 */
LIBSBML_EXTERN
DistribBase_t*
DistribBase_clone(const DistribBase_t* db)
{
  if (db != NULL)
  {
    return static_cast<DistribBase_t*>(db->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribBase_t object.
 */
LIBSBML_EXTERN
void
DistribBase_free(DistribBase_t* db)
{
  if (db != NULL)
  {
    delete db;
  }
}


/*
 * Returns the value of the "id" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
char *
DistribBase_getId(const DistribBase_t * db)
{
  if (db == NULL)
  {
    return NULL;
  }

  return db->getId().empty() ? NULL : safe_strdup(db->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
char *
DistribBase_getName(const DistribBase_t * db)
{
  if (db == NULL)
  {
    return NULL;
  }

  return db->getName().empty() ? NULL : safe_strdup(db->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribBase_t's "id" attribute is
 * set.
 */
LIBSBML_EXTERN
int
DistribBase_isSetId(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribBase_t's "name" attribute is
 * set.
 */
LIBSBML_EXTERN
int
DistribBase_isSetName(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
int
DistribBase_setId(DistribBase_t * db, const char * id)
{
  return (db != NULL) ? db->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
int
DistribBase_setName(DistribBase_t * db, const char * name)
{
  return (db != NULL) ? db->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
int
DistribBase_unsetId(DistribBase_t * db)
{
  return (db != NULL) ? db->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this DistribBase_t.
 */
LIBSBML_EXTERN
int
DistribBase_unsetName(DistribBase_t * db)
{
  return (db != NULL) ? db->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribDrawFromDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribDrawFromDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type DistribInput_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribInput(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribInput()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribBetaDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribBetaDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribCauchyDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribCauchyDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribChiSquareDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribChiSquareDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribChiSquareDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribExponentialDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribExponentialDistribution(const DistribBase_t * db)
{
  return (db != NULL) ?
    static_cast<int>(db->isDistribExponentialDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribFDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribFDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribFDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribGammaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribGammaDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribGammaDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribInverseGammaDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribInverseGammaDistribution(const DistribBase_t * db)
{
  return (db != NULL) ?
    static_cast<int>(db->isDistribInverseGammaDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribLaPlaceDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribLaPlaceDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribLaPlaceDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribLogNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribLogNormalDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribLogNormalDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribLogisticDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribLogisticDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribLogisticDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribNormalDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribNormalDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribParetoDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribParetoDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribParetoDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribRayleighDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribRayleighDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribRayleighDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribStudentTDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribStudentTDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribUniformDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribUniformDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribUniformDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribWeibullDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribWeibullDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribWeibullDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribBinomialDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribBinomialDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribBinomialDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribGeometricDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribGeometricDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribHypergeometricDistribution(const DistribBase_t * db)
{
  return (db != NULL) ?
    static_cast<int>(db->isDistribHypergeometricDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribNegativeBinomialDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribNegativeBinomialDistribution(const DistribBase_t * db)
{
  return (db != NULL) ?
    static_cast<int>(db->isDistribNegativeBinomialDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribPoissonDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribPoissonDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribBernoulliDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribBernoulliDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribBernoulliDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribCategoricalDistribution(const DistribBase_t * db)
{
  return (db != NULL) ?
    static_cast<int>(db->isDistribCategoricalDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribMultivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribMultivariateDistribution(const DistribBase_t * db)
{
  return (db != NULL) ?
    static_cast<int>(db->isDistribMultivariateDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribExternalDistribution(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribExternalDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribUncertBound(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribUncertBound()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribExternalParameter_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribExternalParameter(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribExternalParameter()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type DistribCategory_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribCategory(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribCategory()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribUncertainty(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribUncertainty()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribUncertStatistics(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribUncertStatistics()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribUncertStatisticSpan(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->isDistribUncertStatisticSpan()) :
    0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribBase_t object have been set.
 */
LIBSBML_EXTERN
int
DistribBase_hasRequiredAttributes(const DistribBase_t * db)
{
  return (db != NULL) ? static_cast<int>(db->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


