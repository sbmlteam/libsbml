/**
 * @file DistribUncertStatisticSpan.cpp
 * @brief Implementation of the DistribUncertStatisticSpan class.
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
#include <sbml/packages/distrib/sbml/DistribUncertStatisticSpan.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribUncertStatisticSpan using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
DistribUncertStatisticSpan::DistribUncertStatisticSpan(unsigned int level,
                                                       unsigned int version,
                                                       unsigned int pkgVersion)
  : DistribBase(level, version, pkgVersion)
  , mVarLower ("")
  , mValueLower (util_NaN())
  , mIsSetValueLower (false)
  , mVarUpper ("")
  , mValueUpper (util_NaN())
  , mIsSetValueUpper (false)
  , mElementName("uncertStatisticSpan")
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new DistribUncertStatisticSpan using the given
 * DistribPkgNamespaces object.
 */
DistribUncertStatisticSpan::DistribUncertStatisticSpan(DistribPkgNamespaces
  *distribns)
  : DistribBase(distribns)
  , mVarLower ("")
  , mValueLower (util_NaN())
  , mIsSetValueLower (false)
  , mVarUpper ("")
  , mValueUpper (util_NaN())
  , mIsSetValueUpper (false)
  , mElementName("uncertStatisticSpan")
{
  setElementNamespace(distribns->getURI());
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribUncertStatisticSpan.
 */
DistribUncertStatisticSpan::DistribUncertStatisticSpan(const
  DistribUncertStatisticSpan& orig)
  : DistribBase( orig )
  , mVarLower ( orig.mVarLower )
  , mValueLower ( orig.mValueLower )
  , mIsSetValueLower ( orig.mIsSetValueLower )
  , mVarUpper ( orig.mVarUpper )
  , mValueUpper ( orig.mValueUpper )
  , mIsSetValueUpper ( orig.mIsSetValueUpper )
  , mElementName ( orig.mElementName )
{
}


/*
 * Assignment operator for DistribUncertStatisticSpan.
 */
DistribUncertStatisticSpan&
DistribUncertStatisticSpan::operator=(const DistribUncertStatisticSpan& rhs)
{
  if (&rhs != this)
  {
    DistribBase::operator=(rhs);
    mVarLower = rhs.mVarLower;
    mValueLower = rhs.mValueLower;
    mIsSetValueLower = rhs.mIsSetValueLower;
    mVarUpper = rhs.mVarUpper;
    mValueUpper = rhs.mValueUpper;
    mIsSetValueUpper = rhs.mIsSetValueUpper;
    mElementName = rhs.mElementName;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribUncertStatisticSpan object.
 */
DistribUncertStatisticSpan*
DistribUncertStatisticSpan::clone() const
{
  return new DistribUncertStatisticSpan(*this);
}


/*
 * Destructor for DistribUncertStatisticSpan.
 */
DistribUncertStatisticSpan::~DistribUncertStatisticSpan()
{
}


/*
 * Returns the value of the "varLower" attribute of this
 * DistribUncertStatisticSpan.
 */
const std::string&
DistribUncertStatisticSpan::getVarLower() const
{
  return mVarLower;
}


/*
 * Returns the value of the "valueLower" attribute of this
 * DistribUncertStatisticSpan.
 */
double
DistribUncertStatisticSpan::getValueLower() const
{
  return mValueLower;
}


/*
 * Returns the value of the "varUpper" attribute of this
 * DistribUncertStatisticSpan.
 */
const std::string&
DistribUncertStatisticSpan::getVarUpper() const
{
  return mVarUpper;
}


/*
 * Returns the value of the "valueUpper" attribute of this
 * DistribUncertStatisticSpan.
 */
double
DistribUncertStatisticSpan::getValueUpper() const
{
  return mValueUpper;
}


/*
 * Predicate returning @c true if this DistribUncertStatisticSpan's "varLower"
 * attribute is set.
 */
bool
DistribUncertStatisticSpan::isSetVarLower() const
{
  return (mVarLower.empty() == false);
}


/*
 * Predicate returning @c true if this DistribUncertStatisticSpan's
 * "valueLower" attribute is set.
 */
bool
DistribUncertStatisticSpan::isSetValueLower() const
{
  return mIsSetValueLower;
}


/*
 * Predicate returning @c true if this DistribUncertStatisticSpan's "varUpper"
 * attribute is set.
 */
bool
DistribUncertStatisticSpan::isSetVarUpper() const
{
  return (mVarUpper.empty() == false);
}


/*
 * Predicate returning @c true if this DistribUncertStatisticSpan's
 * "valueUpper" attribute is set.
 */
bool
DistribUncertStatisticSpan::isSetValueUpper() const
{
  return mIsSetValueUpper;
}


/*
 * Sets the value of the "varLower" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::setVarLower(const std::string& varLower)
{
  if (!(SyntaxChecker::isValidInternalSId(varLower)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mVarLower = varLower;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "valueLower" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::setValueLower(double valueLower)
{
  mValueLower = valueLower;
  mIsSetValueLower = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "varUpper" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::setVarUpper(const std::string& varUpper)
{
  if (!(SyntaxChecker::isValidInternalSId(varUpper)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mVarUpper = varUpper;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "valueUpper" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::setValueUpper(double valueUpper)
{
  mValueUpper = valueUpper;
  mIsSetValueUpper = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "varLower" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::unsetVarLower()
{
  mVarLower.erase();

  if (mVarLower.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "valueLower" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::unsetValueLower()
{
  mValueLower = util_NaN();
  mIsSetValueLower = false;

  if (isSetValueLower() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "varUpper" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::unsetVarUpper()
{
  mVarUpper.erase();

  if (mVarUpper.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "valueUpper" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::unsetValueUpper()
{
  mValueUpper = util_NaN();
  mIsSetValueUpper = false;

  if (isSetValueUpper() == false)
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
DistribUncertStatisticSpan::renameSIdRefs(const std::string& oldid,
                                          const std::string& newid)
{
  if (isSetVarLower() && mVarLower == oldid)
  {
    setVarLower(newid);
  }

  if (isSetVarUpper() && mVarUpper == oldid)
  {
    setVarUpper(newid);
  }

  if (isSetVarLower() && mVarLower == oldid)
  {
    setVarLower(newid);
  }

  if (isSetVarUpper() && mVarUpper == oldid)
  {
    setVarUpper(newid);
  }
}


/*
 * Returns the XML element name of this DistribUncertStatisticSpan object.
 */
const std::string&
DistribUncertStatisticSpan::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this DistribUncertStatisticSpan object.
 */
void
DistribUncertStatisticSpan::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this DistribUncertStatisticSpan object.
 */
int
DistribUncertStatisticSpan::getTypeCode() const
{
  return SBML_DISTRIB_UNCERTSTATISTICSPAN;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribUncertStatisticSpan object have been set.
 */
bool
DistribUncertStatisticSpan::hasRequiredAttributes() const
{
  bool allPresent = DistribBase::hasRequiredAttributes();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribUncertStatisticSpan::writeElements(XMLOutputStream& stream) const
{
  DistribBase::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribUncertStatisticSpan::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribUncertStatisticSpan::setSBMLDocument(SBMLDocument* d)
{
  DistribBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribUncertStatisticSpan::enablePackageInternal(const std::string& pkgURI,
                                                  const std::string& pkgPrefix,
                                                  bool flag)
{
  DistribBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::getAttribute(const std::string& attributeName,
                                         bool& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::getAttribute(const std::string& attributeName,
                                         int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::getAttribute(const std::string& attributeName,
                                         double& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "valueLower")
  {
    value = getValueLower();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "valueUpper")
  {
    value = getValueUpper();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::getAttribute(const std::string& attributeName,
                                         unsigned int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::getAttribute(const std::string& attributeName,
                                         std::string& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "varLower")
  {
    value = getVarLower();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "varUpper")
  {
    value = getVarUpper();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribUncertStatisticSpan's attribute
 * "attributeName" is set.
 */
bool
DistribUncertStatisticSpan::isSetAttribute(const std::string& attributeName)
  const
{
  bool value = DistribBase::isSetAttribute(attributeName);

  if (attributeName == "varLower")
  {
    value = isSetVarLower();
  }
  else if (attributeName == "valueLower")
  {
    value = isSetValueLower();
  }
  else if (attributeName == "varUpper")
  {
    value = isSetVarUpper();
  }
  else if (attributeName == "valueUpper")
  {
    value = isSetValueUpper();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::setAttribute(const std::string& attributeName,
                                         bool value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::setAttribute(const std::string& attributeName,
                                         int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::setAttribute(const std::string& attributeName,
                                         double value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  if (attributeName == "valueLower")
  {
    return_value = setValueLower(value);
  }
  else if (attributeName == "valueUpper")
  {
    return_value = setValueUpper(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::setAttribute(const std::string& attributeName,
                                         unsigned int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::setAttribute(const std::string& attributeName,
                                         const std::string& value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  if (attributeName == "varLower")
  {
    return_value = setVarLower(value);
  }
  else if (attributeName == "varUpper")
  {
    return_value = setVarUpper(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribUncertStatisticSpan.
 */
int
DistribUncertStatisticSpan::unsetAttribute(const std::string& attributeName)
{
  int value = DistribBase::unsetAttribute(attributeName);

  if (attributeName == "varLower")
  {
    value = unsetVarLower();
  }
  else if (attributeName == "valueLower")
  {
    value = unsetValueLower();
  }
  else if (attributeName == "varUpper")
  {
    value = unsetVarUpper();
  }
  else if (attributeName == "valueUpper")
  {
    value = unsetValueUpper();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribUncertStatisticSpan::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribBase::createObject(stream);

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DistribUncertStatisticSpan::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribBase::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
    attributes.add("varLower");
    attributes.add("valueLower");
    attributes.add("varUpper");
    attributes.add("valueUpper");
  }

  if (level == 3 && coreVersion == 2 && pkgVersion == 1)
  {
    attributes.add("varLower");
    attributes.add("valueLower");
    attributes.add("varUpper");
    attributes.add("valueUpper");
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribUncertStatisticSpan::readAttributes(const XMLAttributes& attributes,
                                           const ExpectedAttributes&
                                             expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib",
          DistribDistribUncertStatisticSpanAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribUncertStatisticSpanAllowedCoreAttributes, pkgVersion,
            level, version, details);
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
DistribUncertStatisticSpan::readL3V1V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
  unsigned int numErrs;

  // 
  // varLower SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("varLower", mVarLower);

  if (assigned == true)
  {
    if (mVarLower.empty() == true)
    {
      logEmptyString(mVarLower, level, version,
        "<DistribUncertStatisticSpan>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVarLower) == false)
    {
      std::string msg = "The varLower attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mVarLower + "', which does not conform to the syntax.";
      log->logPackageError("distrib",
        DistribDistribUncertStatisticSpanVarLowerMustBeSBase, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }

  // 
  // valueLower double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetValueLower = attributes.readInto("valueLower", mValueLower);

  if ( mIsSetValueLower == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Distrib attribute 'valueLower' from the "
        "<DistribUncertStatisticSpan> element must be an integer.";
      log->logPackageError("distrib",
        DistribDistribUncertStatisticSpanValueLowerMustBeDouble, pkgVersion,
          level, version, message);
    }
  }

  // 
  // varUpper SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("varUpper", mVarUpper);

  if (assigned == true)
  {
    if (mVarUpper.empty() == true)
    {
      logEmptyString(mVarUpper, level, version,
        "<DistribUncertStatisticSpan>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVarUpper) == false)
    {
      std::string msg = "The varUpper attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mVarUpper + "', which does not conform to the syntax.";
      log->logPackageError("distrib",
        DistribDistribUncertStatisticSpanVarUpperMustBeSBase, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }

  // 
  // valueUpper double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetValueUpper = attributes.readInto("valueUpper", mValueUpper);

  if ( mIsSetValueUpper == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Distrib attribute 'valueUpper' from the "
        "<DistribUncertStatisticSpan> element must be an integer.";
      log->logPackageError("distrib",
        DistribDistribUncertStatisticSpanValueUpperMustBeDouble, pkgVersion,
          level, version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribUncertStatisticSpan::readL3V2V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
  unsigned int numErrs;

  // 
  // varLower SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("varLower", mVarLower);

  if (assigned == true)
  {
    if (mVarLower.empty() == true)
    {
      logEmptyString(mVarLower, level, version,
        "<DistribUncertStatisticSpan>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVarLower) == false)
    {
      std::string msg = "The varLower attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mVarLower + "', which does not conform to the syntax.";
      log->logPackageError("distrib",
        DistribDistribUncertStatisticSpanVarLowerMustBeSBase, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }

  // 
  // valueLower double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetValueLower = attributes.readInto("valueLower", mValueLower);

  if ( mIsSetValueLower == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Distrib attribute 'valueLower' from the "
        "<DistribUncertStatisticSpan> element must be an integer.";
      log->logPackageError("distrib",
        DistribDistribUncertStatisticSpanValueLowerMustBeDouble, pkgVersion,
          level, version, message);
    }
  }

  // 
  // varUpper SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("varUpper", mVarUpper);

  if (assigned == true)
  {
    if (mVarUpper.empty() == true)
    {
      logEmptyString(mVarUpper, level, version,
        "<DistribUncertStatisticSpan>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVarUpper) == false)
    {
      std::string msg = "The varUpper attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mVarUpper + "', which does not conform to the syntax.";
      log->logPackageError("distrib",
        DistribDistribUncertStatisticSpanVarUpperMustBeSBase, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }

  // 
  // valueUpper double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetValueUpper = attributes.readInto("valueUpper", mValueUpper);

  if ( mIsSetValueUpper == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Distrib attribute 'valueUpper' from the "
        "<DistribUncertStatisticSpan> element must be an integer.";
      log->logPackageError("distrib",
        DistribDistribUncertStatisticSpanValueUpperMustBeDouble, pkgVersion,
          level, version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribUncertStatisticSpan::writeAttributes(XMLOutputStream& stream) const
{
  DistribBase::writeAttributes(stream);

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
DistribUncertStatisticSpan::writeL3V1V1Attributes(XMLOutputStream& stream)
  const
{
  if (isSetVarLower() == true)
  {
    stream.writeAttribute("varLower", getPrefix(), mVarLower);
  }

  if (isSetValueLower() == true)
  {
    stream.writeAttribute("valueLower", getPrefix(), mValueLower);
  }

  if (isSetVarUpper() == true)
  {
    stream.writeAttribute("varUpper", getPrefix(), mVarUpper);
  }

  if (isSetValueUpper() == true)
  {
    stream.writeAttribute("valueUpper", getPrefix(), mValueUpper);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribUncertStatisticSpan::writeL3V2V1Attributes(XMLOutputStream& stream)
  const
{
  if (isSetVarLower() == true)
  {
    stream.writeAttribute("varLower", getPrefix(), mVarLower);
  }

  if (isSetValueLower() == true)
  {
    stream.writeAttribute("valueLower", getPrefix(), mValueLower);
  }

  if (isSetVarUpper() == true)
  {
    stream.writeAttribute("varUpper", getPrefix(), mVarUpper);
  }

  if (isSetValueUpper() == true)
  {
    stream.writeAttribute("valueUpper", getPrefix(), mValueUpper);
  }
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribUncertStatisticSpan_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribUncertStatisticSpan_t *
DistribUncertStatisticSpan_create(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion)
{
  return new DistribUncertStatisticSpan(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribUncertStatisticSpan_t object.
 */
LIBSBML_EXTERN
DistribUncertStatisticSpan_t*
DistribUncertStatisticSpan_clone(const DistribUncertStatisticSpan_t* duss)
{
  if (duss != NULL)
  {
    return static_cast<DistribUncertStatisticSpan_t*>(duss->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribUncertStatisticSpan_t object.
 */
LIBSBML_EXTERN
void
DistribUncertStatisticSpan_free(DistribUncertStatisticSpan_t* duss)
{
  if (duss != NULL)
  {
    delete duss;
  }
}


/*
 * Returns the value of the "varLower" attribute of this
 * DistribUncertStatisticSpan_t.
 */
LIBSBML_EXTERN
char *
DistribUncertStatisticSpan_getVarLower(const DistribUncertStatisticSpan_t *
  duss)
{
  if (duss == NULL)
  {
    return NULL;
  }

  return duss->getVarLower().empty() ? NULL :
    safe_strdup(duss->getVarLower().c_str());
}


/*
 * Returns the value of the "valueLower" attribute of this
 * DistribUncertStatisticSpan_t.
 */
LIBSBML_EXTERN
double
DistribUncertStatisticSpan_getValueLower(const DistribUncertStatisticSpan_t *
  duss)
{
  return (duss != NULL) ? duss->getValueLower() : util_NaN();
}


/*
 * Returns the value of the "varUpper" attribute of this
 * DistribUncertStatisticSpan_t.
 */
LIBSBML_EXTERN
char *
DistribUncertStatisticSpan_getVarUpper(const DistribUncertStatisticSpan_t *
  duss)
{
  if (duss == NULL)
  {
    return NULL;
  }

  return duss->getVarUpper().empty() ? NULL :
    safe_strdup(duss->getVarUpper().c_str());
}


/*
 * Returns the value of the "valueUpper" attribute of this
 * DistribUncertStatisticSpan_t.
 */
LIBSBML_EXTERN
double
DistribUncertStatisticSpan_getValueUpper(const DistribUncertStatisticSpan_t *
  duss)
{
  return (duss != NULL) ? duss->getValueUpper() : util_NaN();
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatisticSpan_t's
 * "varLower" attribute is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_isSetVarLower(const DistribUncertStatisticSpan_t *
  duss)
{
  return (duss != NULL) ? static_cast<int>(duss->isSetVarLower()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatisticSpan_t's
 * "valueLower" attribute is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_isSetValueLower(const DistribUncertStatisticSpan_t *
  duss)
{
  return (duss != NULL) ? static_cast<int>(duss->isSetValueLower()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatisticSpan_t's
 * "varUpper" attribute is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_isSetVarUpper(const DistribUncertStatisticSpan_t *
  duss)
{
  return (duss != NULL) ? static_cast<int>(duss->isSetVarUpper()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatisticSpan_t's
 * "valueUpper" attribute is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_isSetValueUpper(const DistribUncertStatisticSpan_t *
  duss)
{
  return (duss != NULL) ? static_cast<int>(duss->isSetValueUpper()) : 0;
}


/*
 * Sets the value of the "varLower" attribute of this
 * DistribUncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_setVarLower(DistribUncertStatisticSpan_t * duss,
                                       const char * varLower)
{
  return (duss != NULL) ? duss->setVarLower(varLower) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "valueLower" attribute of this
 * DistribUncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_setValueLower(DistribUncertStatisticSpan_t * duss,
                                         double valueLower)
{
  return (duss != NULL) ? duss->setValueLower(valueLower) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "varUpper" attribute of this
 * DistribUncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_setVarUpper(DistribUncertStatisticSpan_t * duss,
                                       const char * varUpper)
{
  return (duss != NULL) ? duss->setVarUpper(varUpper) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "valueUpper" attribute of this
 * DistribUncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_setValueUpper(DistribUncertStatisticSpan_t * duss,
                                         double valueUpper)
{
  return (duss != NULL) ? duss->setValueUpper(valueUpper) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "varLower" attribute of this
 * DistribUncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_unsetVarLower(DistribUncertStatisticSpan_t * duss)
{
  return (duss != NULL) ? duss->unsetVarLower() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "valueLower" attribute of this
 * DistribUncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_unsetValueLower(DistribUncertStatisticSpan_t * duss)
{
  return (duss != NULL) ? duss->unsetValueLower() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "varUpper" attribute of this
 * DistribUncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_unsetVarUpper(DistribUncertStatisticSpan_t * duss)
{
  return (duss != NULL) ? duss->unsetVarUpper() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "valueUpper" attribute of this
 * DistribUncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_unsetValueUpper(DistribUncertStatisticSpan_t * duss)
{
  return (duss != NULL) ? duss->unsetValueUpper() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUncertStatisticSpan_t object have been set.
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_hasRequiredAttributes(const
  DistribUncertStatisticSpan_t * duss)
{
  return (duss != NULL) ? static_cast<int>(duss->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


