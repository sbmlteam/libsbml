/**
 * @file UncertStatisticSpan.cpp
 * @brief Implementation of the UncertStatisticSpan class.
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
#include <sbml/packages/distrib/sbml/UncertStatisticSpan.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new UncertStatisticSpan using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
UncertStatisticSpan::UncertStatisticSpan(unsigned int level,
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
 * Creates a new UncertStatisticSpan using the given DistribPkgNamespaces
 * object.
 */
UncertStatisticSpan::UncertStatisticSpan(DistribPkgNamespaces *distribns)
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
 * Copy constructor for UncertStatisticSpan.
 */
UncertStatisticSpan::UncertStatisticSpan(const UncertStatisticSpan& orig)
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
 * Assignment operator for UncertStatisticSpan.
 */
UncertStatisticSpan&
UncertStatisticSpan::operator=(const UncertStatisticSpan& rhs)
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
 * Creates and returns a deep copy of this UncertStatisticSpan object.
 */
UncertStatisticSpan*
UncertStatisticSpan::clone() const
{
  return new UncertStatisticSpan(*this);
}


/*
 * Destructor for UncertStatisticSpan.
 */
UncertStatisticSpan::~UncertStatisticSpan()
{
}


/*
 * Returns the value of the "varLower" attribute of this UncertStatisticSpan.
 */
const std::string&
UncertStatisticSpan::getVarLower() const
{
  return mVarLower;
}


/*
 * Returns the value of the "valueLower" attribute of this UncertStatisticSpan.
 */
double
UncertStatisticSpan::getValueLower() const
{
  return mValueLower;
}


/*
 * Returns the value of the "varUpper" attribute of this UncertStatisticSpan.
 */
const std::string&
UncertStatisticSpan::getVarUpper() const
{
  return mVarUpper;
}


/*
 * Returns the value of the "valueUpper" attribute of this UncertStatisticSpan.
 */
double
UncertStatisticSpan::getValueUpper() const
{
  return mValueUpper;
}


/*
 * Predicate returning @c true if this UncertStatisticSpan's "varLower"
 * attribute is set.
 */
bool
UncertStatisticSpan::isSetVarLower() const
{
  return (mVarLower.empty() == false);
}


/*
 * Predicate returning @c true if this UncertStatisticSpan's "valueLower"
 * attribute is set.
 */
bool
UncertStatisticSpan::isSetValueLower() const
{
  return mIsSetValueLower;
}


/*
 * Predicate returning @c true if this UncertStatisticSpan's "varUpper"
 * attribute is set.
 */
bool
UncertStatisticSpan::isSetVarUpper() const
{
  return (mVarUpper.empty() == false);
}


/*
 * Predicate returning @c true if this UncertStatisticSpan's "valueUpper"
 * attribute is set.
 */
bool
UncertStatisticSpan::isSetValueUpper() const
{
  return mIsSetValueUpper;
}


/*
 * Sets the value of the "varLower" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::setVarLower(const std::string& varLower)
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
 * Sets the value of the "valueLower" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::setValueLower(double valueLower)
{
  mValueLower = valueLower;
  mIsSetValueLower = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "varUpper" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::setVarUpper(const std::string& varUpper)
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
 * Sets the value of the "valueUpper" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::setValueUpper(double valueUpper)
{
  mValueUpper = valueUpper;
  mIsSetValueUpper = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "varLower" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::unsetVarLower()
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
 * Unsets the value of the "valueLower" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::unsetValueLower()
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
 * Unsets the value of the "varUpper" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::unsetVarUpper()
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
 * Unsets the value of the "valueUpper" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::unsetValueUpper()
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
UncertStatisticSpan::renameSIdRefs(const std::string& oldid,
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
}


/*
 * Returns the XML element name of this UncertStatisticSpan object.
 */
const std::string&
UncertStatisticSpan::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this UncertStatisticSpan object.
 */
void
UncertStatisticSpan::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this UncertStatisticSpan object.
 */
int
UncertStatisticSpan::getTypeCode() const
{
  return SBML_DISTRIB_UNCERTSTATISTICSPAN;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * UncertStatisticSpan object have been set.
 */
bool
UncertStatisticSpan::hasRequiredAttributes() const
{
  bool allPresent = DistribBase::hasRequiredAttributes();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
UncertStatisticSpan::writeElements(XMLOutputStream& stream) const
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
UncertStatisticSpan::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
UncertStatisticSpan::setSBMLDocument(SBMLDocument* d)
{
  DistribBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
UncertStatisticSpan::enablePackageInternal(const std::string& pkgURI,
                                           const std::string& pkgPrefix,
                                           bool flag)
{
  DistribBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::getAttribute(const std::string& attributeName,
                                  bool& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::getAttribute(const std::string& attributeName,
                                  int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::getAttribute(const std::string& attributeName,
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
 * Gets the value of the "attributeName" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::getAttribute(const std::string& attributeName,
                                  unsigned int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::getAttribute(const std::string& attributeName,
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
 * Predicate returning @c true if this UncertStatisticSpan's attribute
 * "attributeName" is set.
 */
bool
UncertStatisticSpan::isSetAttribute(const std::string& attributeName) const
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
 * Sets the value of the "attributeName" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::setAttribute(const std::string& attributeName,
                                  bool value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::setAttribute(const std::string& attributeName, int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::setAttribute(const std::string& attributeName,
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
 * Sets the value of the "attributeName" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::setAttribute(const std::string& attributeName,
                                  unsigned int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertStatisticSpan.
 */
int
UncertStatisticSpan::setAttribute(const std::string& attributeName,
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
 * UncertStatisticSpan.
 */
int
UncertStatisticSpan::unsetAttribute(const std::string& attributeName)
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
UncertStatisticSpan::createObject(XMLInputStream& stream)
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
UncertStatisticSpan::addExpectedAttributes(ExpectedAttributes& attributes)
{
  DistribBase::addExpectedAttributes(attributes);

  attributes.add("varLower");

  attributes.add("valueLower");

  attributes.add("varUpper");

  attributes.add("valueUpper");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
UncertStatisticSpan::readAttributes(const XMLAttributes& attributes,
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
          DistribUncertStatisticSpanAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribUncertStatisticSpanAllowedCoreAttributes, pkgVersion, level,
            version, details);
      }
    }
  }

  // 
  // varLower SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("varLower", mVarLower);

  if (assigned == true)
  {
    if (mVarLower.empty() == true)
    {
      logEmptyString(mVarLower, level, version, "<UncertStatisticSpan>");
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
        DistribUncertStatisticSpanVarLowerMustBeSBase, pkgVersion, level,
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
        "<UncertStatisticSpan> element must be an integer.";
      log->logPackageError("distrib",
        DistribUncertStatisticSpanValueLowerMustBeDouble, pkgVersion, level,
          version, message);
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
      logEmptyString(mVarUpper, level, version, "<UncertStatisticSpan>");
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
        DistribUncertStatisticSpanVarUpperMustBeSBase, pkgVersion, level,
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
        "<UncertStatisticSpan> element must be an integer.";
      log->logPackageError("distrib",
        DistribUncertStatisticSpanValueUpperMustBeDouble, pkgVersion, level,
          version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
UncertStatisticSpan::writeAttributes(XMLOutputStream& stream) const
{
  DistribBase::writeAttributes(stream);

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

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new UncertStatisticSpan_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
UncertStatisticSpan_t *
UncertStatisticSpan_create(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion)
{
  return new UncertStatisticSpan(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this UncertStatisticSpan_t object.
 */
LIBSBML_EXTERN
UncertStatisticSpan_t*
UncertStatisticSpan_clone(const UncertStatisticSpan_t* uss)
{
  if (uss != NULL)
  {
    return static_cast<UncertStatisticSpan_t*>(uss->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this UncertStatisticSpan_t object.
 */
LIBSBML_EXTERN
void
UncertStatisticSpan_free(UncertStatisticSpan_t* uss)
{
  if (uss != NULL)
  {
    delete uss;
  }
}


/*
 * Returns the value of the "varLower" attribute of this UncertStatisticSpan_t.
 */
LIBSBML_EXTERN
char *
UncertStatisticSpan_getVarLower(const UncertStatisticSpan_t * uss)
{
  if (uss == NULL)
  {
    return NULL;
  }

  return uss->getVarLower().empty() ? NULL :
    safe_strdup(uss->getVarLower().c_str());
}


/*
 * Returns the value of the "valueLower" attribute of this
 * UncertStatisticSpan_t.
 */
LIBSBML_EXTERN
double
UncertStatisticSpan_getValueLower(const UncertStatisticSpan_t * uss)
{
  return (uss != NULL) ? uss->getValueLower() : util_NaN();
}


/*
 * Returns the value of the "varUpper" attribute of this UncertStatisticSpan_t.
 */
LIBSBML_EXTERN
char *
UncertStatisticSpan_getVarUpper(const UncertStatisticSpan_t * uss)
{
  if (uss == NULL)
  {
    return NULL;
  }

  return uss->getVarUpper().empty() ? NULL :
    safe_strdup(uss->getVarUpper().c_str());
}


/*
 * Returns the value of the "valueUpper" attribute of this
 * UncertStatisticSpan_t.
 */
LIBSBML_EXTERN
double
UncertStatisticSpan_getValueUpper(const UncertStatisticSpan_t * uss)
{
  return (uss != NULL) ? uss->getValueUpper() : util_NaN();
}


/*
 * Predicate returning @c 1 (true) if this UncertStatisticSpan_t's "varLower"
 * attribute is set.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_isSetVarLower(const UncertStatisticSpan_t * uss)
{
  return (uss != NULL) ? static_cast<int>(uss->isSetVarLower()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UncertStatisticSpan_t's "valueLower"
 * attribute is set.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_isSetValueLower(const UncertStatisticSpan_t * uss)
{
  return (uss != NULL) ? static_cast<int>(uss->isSetValueLower()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UncertStatisticSpan_t's "varUpper"
 * attribute is set.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_isSetVarUpper(const UncertStatisticSpan_t * uss)
{
  return (uss != NULL) ? static_cast<int>(uss->isSetVarUpper()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UncertStatisticSpan_t's "valueUpper"
 * attribute is set.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_isSetValueUpper(const UncertStatisticSpan_t * uss)
{
  return (uss != NULL) ? static_cast<int>(uss->isSetValueUpper()) : 0;
}


/*
 * Sets the value of the "varLower" attribute of this UncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_setVarLower(UncertStatisticSpan_t * uss,
                                const char * varLower)
{
  return (uss != NULL) ? uss->setVarLower(varLower) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "valueLower" attribute of this UncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_setValueLower(UncertStatisticSpan_t * uss,
                                  double valueLower)
{
  return (uss != NULL) ? uss->setValueLower(valueLower) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "varUpper" attribute of this UncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_setVarUpper(UncertStatisticSpan_t * uss,
                                const char * varUpper)
{
  return (uss != NULL) ? uss->setVarUpper(varUpper) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "valueUpper" attribute of this UncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_setValueUpper(UncertStatisticSpan_t * uss,
                                  double valueUpper)
{
  return (uss != NULL) ? uss->setValueUpper(valueUpper) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "varLower" attribute of this UncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_unsetVarLower(UncertStatisticSpan_t * uss)
{
  return (uss != NULL) ? uss->unsetVarLower() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "valueLower" attribute of this
 * UncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_unsetValueLower(UncertStatisticSpan_t * uss)
{
  return (uss != NULL) ? uss->unsetValueLower() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "varUpper" attribute of this UncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_unsetVarUpper(UncertStatisticSpan_t * uss)
{
  return (uss != NULL) ? uss->unsetVarUpper() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "valueUpper" attribute of this
 * UncertStatisticSpan_t.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_unsetValueUpper(UncertStatisticSpan_t * uss)
{
  return (uss != NULL) ? uss->unsetValueUpper() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * UncertStatisticSpan_t object have been set.
 */
LIBSBML_EXTERN
int
UncertStatisticSpan_hasRequiredAttributes(const UncertStatisticSpan_t * uss)
{
  return (uss != NULL) ? static_cast<int>(uss->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


