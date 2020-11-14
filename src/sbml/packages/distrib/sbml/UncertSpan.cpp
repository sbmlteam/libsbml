/**
 * @file UncertSpan.cpp
 * @brief Implementation of the UncertSpan class.
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
#include <sbml/packages/distrib/sbml/UncertSpan.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new UncertSpan using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
UncertSpan::UncertSpan(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion)
  : UncertParameter(level, version, pkgVersion)
  , mVarLower ("")
  , mValueLower (util_NaN())
  , mIsSetValueLower (false)
  , mVarUpper ("")
  , mValueUpper (util_NaN())
  , mIsSetValueUpper (false)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new UncertSpan using the given DistribPkgNamespaces object.
 */
UncertSpan::UncertSpan(DistribPkgNamespaces *distribns)
  : UncertParameter(distribns)
  , mVarLower ("")
  , mValueLower (util_NaN())
  , mIsSetValueLower (false)
  , mVarUpper ("")
  , mValueUpper (util_NaN())
  , mIsSetValueUpper (false)
{
  setElementNamespace(distribns->getURI());
  loadPlugins(distribns);
}


/*
 * Copy constructor for UncertSpan.
 */
UncertSpan::UncertSpan(const UncertSpan& orig)
  : UncertParameter( orig )
  , mVarLower ( orig.mVarLower )
  , mValueLower ( orig.mValueLower )
  , mIsSetValueLower ( orig.mIsSetValueLower )
  , mVarUpper ( orig.mVarUpper )
  , mValueUpper ( orig.mValueUpper )
  , mIsSetValueUpper ( orig.mIsSetValueUpper )
{
}


/*
 * Assignment operator for UncertSpan.
 */
UncertSpan&
UncertSpan::operator=(const UncertSpan& rhs)
{
  if (&rhs != this)
  {
    UncertParameter::operator=(rhs);
    mVarLower = rhs.mVarLower;
    mValueLower = rhs.mValueLower;
    mIsSetValueLower = rhs.mIsSetValueLower;
    mVarUpper = rhs.mVarUpper;
    mValueUpper = rhs.mValueUpper;
    mIsSetValueUpper = rhs.mIsSetValueUpper;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this UncertSpan object.
 */
UncertSpan*
UncertSpan::clone() const
{
  return new UncertSpan(*this);
}


/*
 * Destructor for UncertSpan.
 */
UncertSpan::~UncertSpan()
{
}


/*
 * Returns the value of the "varLower" attribute of this UncertSpan.
 */
const std::string&
UncertSpan::getVarLower() const
{
  return mVarLower;
}


/*
 * Returns the value of the "valueLower" attribute of this UncertSpan.
 */
double
UncertSpan::getValueLower() const
{
  return mValueLower;
}


/*
 * Returns the value of the "varUpper" attribute of this UncertSpan.
 */
const std::string&
UncertSpan::getVarUpper() const
{
  return mVarUpper;
}


/*
 * Returns the value of the "valueUpper" attribute of this UncertSpan.
 */
double
UncertSpan::getValueUpper() const
{
  return mValueUpper;
}


/*
 * Predicate returning @c true if this UncertSpan's "varLower" attribute is
 * set.
 */
bool
UncertSpan::isSetVarLower() const
{
  return (mVarLower.empty() == false);
}


/*
 * Predicate returning @c true if this UncertSpan's "valueLower" attribute is
 * set.
 */
bool
UncertSpan::isSetValueLower() const
{
  return mIsSetValueLower;
}


/*
 * Predicate returning @c true if this UncertSpan's "varUpper" attribute is
 * set.
 */
bool
UncertSpan::isSetVarUpper() const
{
  return (mVarUpper.empty() == false);
}


/*
 * Predicate returning @c true if this UncertSpan's "valueUpper" attribute is
 * set.
 */
bool
UncertSpan::isSetValueUpper() const
{
  return mIsSetValueUpper;
}


/*
 * Sets the value of the "varLower" attribute of this UncertSpan.
 */
int
UncertSpan::setVarLower(const std::string& varLower)
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
 * Sets the value of the "valueLower" attribute of this UncertSpan.
 */
int
UncertSpan::setValueLower(double valueLower)
{
  mValueLower = valueLower;
  mIsSetValueLower = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "varUpper" attribute of this UncertSpan.
 */
int
UncertSpan::setVarUpper(const std::string& varUpper)
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
 * Sets the value of the "valueUpper" attribute of this UncertSpan.
 */
int
UncertSpan::setValueUpper(double valueUpper)
{
  mValueUpper = valueUpper;
  mIsSetValueUpper = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "varLower" attribute of this UncertSpan.
 */
int
UncertSpan::unsetVarLower()
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
 * Unsets the value of the "valueLower" attribute of this UncertSpan.
 */
int
UncertSpan::unsetValueLower()
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
 * Unsets the value of the "varUpper" attribute of this UncertSpan.
 */
int
UncertSpan::unsetVarUpper()
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
 * Unsets the value of the "valueUpper" attribute of this UncertSpan.
 */
int
UncertSpan::unsetValueUpper()
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
UncertSpan::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  UncertParameter::renameSIdRefs(oldid, newid);
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
 * Returns the XML element name of this UncertSpan object.
 */
const std::string&
UncertSpan::getElementName() const
{
  static const string name = "uncertSpan";
  return name;
}


/*
 * Returns the libSBML type code for this UncertSpan object.
 */
int
UncertSpan::getTypeCode() const
{
  return SBML_DISTRIB_UNCERTSTATISTICSPAN;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * UncertSpan object have been set.
 */
bool
UncertSpan::hasRequiredAttributes() const
{
  bool allPresent = UncertParameter::hasRequiredAttributes();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
UncertSpan::writeElements(XMLOutputStream& stream) const
{
  UncertParameter::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
UncertSpan::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
UncertSpan::setSBMLDocument(SBMLDocument* d)
{
  UncertParameter::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
UncertSpan::enablePackageInternal(const std::string& pkgURI,
                                  const std::string& pkgPrefix,
                                  bool flag)
{
  UncertParameter::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertSpan.
 */
int
UncertSpan::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = UncertParameter::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertSpan.
 */
int
UncertSpan::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = UncertParameter::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertSpan.
 */
int
UncertSpan::getAttribute(const std::string& attributeName,
                         double& value) const
{
  int return_value = UncertParameter::getAttribute(attributeName, value);

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
 * Gets the value of the "attributeName" attribute of this UncertSpan.
 */
int
UncertSpan::getAttribute(const std::string& attributeName,
                         unsigned int& value) const
{
  int return_value = UncertParameter::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertSpan.
 */
int
UncertSpan::getAttribute(const std::string& attributeName,
                         std::string& value) const
{
  int return_value = UncertParameter::getAttribute(attributeName, value);

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
 * Predicate returning @c true if this UncertSpan's attribute "attributeName"
 * is set.
 */
bool
UncertSpan::isSetAttribute(const std::string& attributeName) const
{
  bool value = UncertParameter::isSetAttribute(attributeName);

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
 * Sets the value of the "attributeName" attribute of this UncertSpan.
 */
int
UncertSpan::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = UncertParameter::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertSpan.
 */
int
UncertSpan::setAttribute(const std::string& attributeName, int value)
{
  int return_value = UncertParameter::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertSpan.
 */
int
UncertSpan::setAttribute(const std::string& attributeName, double value)
{
  int return_value = UncertParameter::setAttribute(attributeName, value);

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
 * Sets the value of the "attributeName" attribute of this UncertSpan.
 */
int
UncertSpan::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = UncertParameter::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertSpan.
 */
int
UncertSpan::setAttribute(const std::string& attributeName,
                         const std::string& value)
{
  int return_value = UncertParameter::setAttribute(attributeName, value);

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
 * Unsets the value of the "attributeName" attribute of this UncertSpan.
 */
int
UncertSpan::unsetAttribute(const std::string& attributeName)
{
  int value = UncertParameter::unsetAttribute(attributeName);

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
UncertSpan::createObject(XMLInputStream& stream)
{
  SBase* obj = UncertParameter::createObject(stream);

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
UncertSpan::addExpectedAttributes(ExpectedAttributes& attributes)
{
  UncertParameter::addExpectedAttributes(attributes);

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
UncertSpan::readAttributes(const XMLAttributes& attributes,
                           const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  UncertParameter::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib", DistribUncertSpanAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib", DistribUncertSpanAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
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
      logEmptyString(mVarLower, level, version, "<UncertSpan>");
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
      log->logPackageError("distrib", DistribUncertSpanVarLowerMustBeSBase,
        pkgVersion, level, version, msg, getLine(), getColumn());
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
        "<UncertSpan> element must be an integer.";
      log->logPackageError("distrib", DistribUncertSpanValueLowerMustBeDouble,
        pkgVersion, level, version, message, getLine(), getColumn());
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
      logEmptyString(mVarUpper, level, version, "<UncertSpan>");
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
      log->logPackageError("distrib", DistribUncertSpanVarUpperMustBeSBase,
        pkgVersion, level, version, msg, getLine(), getColumn());
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
        "<UncertSpan> element must be an integer.";
      log->logPackageError("distrib", DistribUncertSpanValueUpperMustBeDouble,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
UncertSpan::writeAttributes(XMLOutputStream& stream) const
{
  UncertParameter::writeAttributes(stream);

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
 * Creates a new UncertSpan_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
UncertSpan_t *
UncertSpan_create(unsigned int level,
                  unsigned int version,
                  unsigned int pkgVersion)
{
  return new UncertSpan(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this UncertSpan_t object.
 */
LIBSBML_EXTERN
UncertSpan_t*
UncertSpan_clone(const UncertSpan_t* us)
{
  if (us != NULL)
  {
    return static_cast<UncertSpan_t*>(us->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this UncertSpan_t object.
 */
LIBSBML_EXTERN
void
UncertSpan_free(UncertSpan_t* us)
{
  if (us != NULL)
  {
    delete us;
  }
}


/*
 * Returns the value of the "varLower" attribute of this UncertSpan_t.
 */
LIBSBML_EXTERN
char *
UncertSpan_getVarLower(const UncertSpan_t * us)
{
  if (us == NULL)
  {
    return NULL;
  }

  return us->getVarLower().empty() ? NULL :
    safe_strdup(us->getVarLower().c_str());
}


/*
 * Returns the value of the "valueLower" attribute of this UncertSpan_t.
 */
LIBSBML_EXTERN
double
UncertSpan_getValueLower(const UncertSpan_t * us)
{
  return (us != NULL) ? us->getValueLower() : util_NaN();
}


/*
 * Returns the value of the "varUpper" attribute of this UncertSpan_t.
 */
LIBSBML_EXTERN
char *
UncertSpan_getVarUpper(const UncertSpan_t * us)
{
  if (us == NULL)
  {
    return NULL;
  }

  return us->getVarUpper().empty() ? NULL :
    safe_strdup(us->getVarUpper().c_str());
}


/*
 * Returns the value of the "valueUpper" attribute of this UncertSpan_t.
 */
LIBSBML_EXTERN
double
UncertSpan_getValueUpper(const UncertSpan_t * us)
{
  return (us != NULL) ? us->getValueUpper() : util_NaN();
}


/*
 * Predicate returning @c 1 (true) if this UncertSpan_t's "varLower" attribute
 * is set.
 */
LIBSBML_EXTERN
int
UncertSpan_isSetVarLower(const UncertSpan_t * us)
{
  return (us != NULL) ? static_cast<int>(us->isSetVarLower()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UncertSpan_t's "valueLower"
 * attribute is set.
 */
LIBSBML_EXTERN
int
UncertSpan_isSetValueLower(const UncertSpan_t * us)
{
  return (us != NULL) ? static_cast<int>(us->isSetValueLower()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UncertSpan_t's "varUpper" attribute
 * is set.
 */
LIBSBML_EXTERN
int
UncertSpan_isSetVarUpper(const UncertSpan_t * us)
{
  return (us != NULL) ? static_cast<int>(us->isSetVarUpper()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UncertSpan_t's "valueUpper"
 * attribute is set.
 */
LIBSBML_EXTERN
int
UncertSpan_isSetValueUpper(const UncertSpan_t * us)
{
  return (us != NULL) ? static_cast<int>(us->isSetValueUpper()) : 0;
}


/*
 * Sets the value of the "varLower" attribute of this UncertSpan_t.
 */
LIBSBML_EXTERN
int
UncertSpan_setVarLower(UncertSpan_t * us, const char * varLower)
{
  return (us != NULL) ? us->setVarLower(varLower) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "valueLower" attribute of this UncertSpan_t.
 */
LIBSBML_EXTERN
int
UncertSpan_setValueLower(UncertSpan_t * us, double valueLower)
{
  return (us != NULL) ? us->setValueLower(valueLower) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "varUpper" attribute of this UncertSpan_t.
 */
LIBSBML_EXTERN
int
UncertSpan_setVarUpper(UncertSpan_t * us, const char * varUpper)
{
  return (us != NULL) ? us->setVarUpper(varUpper) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "valueUpper" attribute of this UncertSpan_t.
 */
LIBSBML_EXTERN
int
UncertSpan_setValueUpper(UncertSpan_t * us, double valueUpper)
{
  return (us != NULL) ? us->setValueUpper(valueUpper) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "varLower" attribute of this UncertSpan_t.
 */
LIBSBML_EXTERN
int
UncertSpan_unsetVarLower(UncertSpan_t * us)
{
  return (us != NULL) ? us->unsetVarLower() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "valueLower" attribute of this UncertSpan_t.
 */
LIBSBML_EXTERN
int
UncertSpan_unsetValueLower(UncertSpan_t * us)
{
  return (us != NULL) ? us->unsetValueLower() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "varUpper" attribute of this UncertSpan_t.
 */
LIBSBML_EXTERN
int
UncertSpan_unsetVarUpper(UncertSpan_t * us)
{
  return (us != NULL) ? us->unsetVarUpper() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "valueUpper" attribute of this UncertSpan_t.
 */
LIBSBML_EXTERN
int
UncertSpan_unsetValueUpper(UncertSpan_t * us)
{
  return (us != NULL) ? us->unsetValueUpper() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * UncertSpan_t object have been set.
 */
LIBSBML_EXTERN
int
UncertSpan_hasRequiredAttributes(const UncertSpan_t * us)
{
  return (us != NULL) ? static_cast<int>(us->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


