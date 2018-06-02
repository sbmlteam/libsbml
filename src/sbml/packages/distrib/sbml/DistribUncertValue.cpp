/**
 * @file DistribUncertValue.cpp
 * @brief Implementation of the DistribUncertValue class.
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
#include <sbml/packages/distrib/sbml/DistribUncertValue.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>

#include <sbml/packages/distrib/sbml/DistribUncertBound.h>
#include <sbml/packages/distrib/sbml/DistribExternalParameter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribUncertValue using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
DistribUncertValue::DistribUncertValue(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion)
  : DistribBase(level, version)
  , mValue (util_NaN())
  , mIsSetValue (false)
  , mVar ("")
  , mUnits ("")
  , mElementName("uncertValue")
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new DistribUncertValue using the given DistribPkgNamespaces
 * object.
 */
DistribUncertValue::DistribUncertValue(DistribPkgNamespaces *distribns)
  : DistribBase(distribns)
  , mValue (util_NaN())
  , mIsSetValue (false)
  , mVar ("")
  , mUnits ("")
  , mElementName("uncertValue")
{
  setElementNamespace(distribns->getURI());
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribUncertValue.
 */
DistribUncertValue::DistribUncertValue(const DistribUncertValue& orig)
  : DistribBase( orig )
  , mValue ( orig.mValue )
  , mIsSetValue ( orig.mIsSetValue )
  , mVar ( orig.mVar )
  , mUnits ( orig.mUnits )
  , mElementName ( orig.mElementName )
{
}


/*
 * Assignment operator for DistribUncertValue.
 */
DistribUncertValue&
DistribUncertValue::operator=(const DistribUncertValue& rhs)
{
  if (&rhs != this)
  {
    DistribBase::operator=(rhs);
    mValue = rhs.mValue;
    mIsSetValue = rhs.mIsSetValue;
    mVar = rhs.mVar;
    mUnits = rhs.mUnits;
    mElementName = rhs.mElementName;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribUncertValue object.
 */
DistribUncertValue*
DistribUncertValue::clone() const
{
  return new DistribUncertValue(*this);
}


/*
 * Destructor for DistribUncertValue.
 */
DistribUncertValue::~DistribUncertValue()
{
}


/*
 * Returns the value of the "value" attribute of this DistribUncertValue.
 */
double
DistribUncertValue::getValue() const
{
  return mValue;
}


/*
 * Returns the value of the "var" attribute of this DistribUncertValue.
 */
const std::string&
DistribUncertValue::getVar() const
{
  return mVar;
}


/*
 * Returns the value of the "units" attribute of this DistribUncertValue.
 */
const std::string&
DistribUncertValue::getUnits() const
{
  return mUnits;
}


/*
 * Predicate returning @c true if this DistribUncertValue's "value" attribute
 * is set.
 */
bool
DistribUncertValue::isSetValue() const
{
  return mIsSetValue;
}


/*
 * Predicate returning @c true if this DistribUncertValue's "var" attribute is
 * set.
 */
bool
DistribUncertValue::isSetVar() const
{
  return (mVar.empty() == false);
}


/*
 * Predicate returning @c true if this DistribUncertValue's "units" attribute
 * is set.
 */
bool
DistribUncertValue::isSetUnits() const
{
  return (mUnits.empty() == false);
}


/*
 * Sets the value of the "value" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::setValue(double value)
{
  mValue = value;
  mIsSetValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "var" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::setVar(const std::string& var)
{
  if (!(SyntaxChecker::isValidInternalSId(var)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mVar = var;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "units" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::setUnits(const std::string& units)
{
  if (!(SyntaxChecker::isValidInternalUnitSId(units)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mUnits = units;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "value" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::unsetValue()
{
  mValue = util_NaN();
  mIsSetValue = false;

  if (isSetValue() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "var" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::unsetVar()
{
  mVar.erase();

  if (mVar.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "units" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::unsetUnits()
{
  mUnits.erase();

  if (mUnits.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Predicate returning @c true if this abstract "DistribUncertValue" is of type
 * DistribUncertBound
 */
bool
DistribUncertValue::isDistribUncertBound() const
{
  return dynamic_cast<const DistribUncertBound*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "DistribUncertValue" is of type
 * DistribExternalParameter
 */
bool
DistribUncertValue::isDistribExternalParameter() const
{
  return dynamic_cast<const DistribExternalParameter*>(this) != NULL;
}


/*
 * @copydoc doc_renamesidref_common
 */
void
DistribUncertValue::renameSIdRefs(const std::string& oldid,
                                  const std::string& newid)
{
  if (isSetVar() && mVar == oldid)
  {
    setVar(newid);
  }

  if (isSetVar() && mVar == oldid)
  {
    setVar(newid);
  }

  if (isSetUnits() && mUnits == oldid)
  {
    setUnits(newid);
  }

  if (isSetUnits() && mUnits == oldid)
  {
    setUnits(newid);
  }
}


/*
 * Returns the XML element name of this DistribUncertValue object.
 */
const std::string&
DistribUncertValue::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this DistribUncertValue object.
 */
void
DistribUncertValue::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this DistribUncertValue object.
 */
int
DistribUncertValue::getTypeCode() const
{
  return SBML_DISTRIB_UNCERTVALUE;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribUncertValue object have been set.
 */
bool
DistribUncertValue::hasRequiredAttributes() const
{
  bool allPresent = DistribBase::hasRequiredAttributes();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribUncertValue::writeElements(XMLOutputStream& stream) const
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
DistribUncertValue::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribUncertValue::setSBMLDocument(SBMLDocument* d)
{
  DistribBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribUncertValue::enablePackageInternal(const std::string& pkgURI,
                                          const std::string& pkgPrefix,
                                          bool flag)
{
  DistribBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::getAttribute(const std::string& attributeName,
                                 bool& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::getAttribute(const std::string& attributeName,
                                 int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::getAttribute(const std::string& attributeName,
                                 double& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "value")
  {
    value = getValue();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::getAttribute(const std::string& attributeName,
                                 unsigned int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::getAttribute(const std::string& attributeName,
                                 std::string& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "var")
  {
    value = getVar();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "units")
  {
    value = getUnits();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribUncertValue's attribute
 * "attributeName" is set.
 */
bool
DistribUncertValue::isSetAttribute(const std::string& attributeName) const
{
  bool value = DistribBase::isSetAttribute(attributeName);

  if (attributeName == "value")
  {
    value = isSetValue();
  }
  else if (attributeName == "var")
  {
    value = isSetVar();
  }
  else if (attributeName == "units")
  {
    value = isSetUnits();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::setAttribute(const std::string& attributeName, int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::setAttribute(const std::string& attributeName,
                                 double value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  if (attributeName == "value")
  {
    return_value = setValue(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::setAttribute(const std::string& attributeName,
                                 unsigned int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertValue.
 */
int
DistribUncertValue::setAttribute(const std::string& attributeName,
                                 const std::string& value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  if (attributeName == "var")
  {
    return_value = setVar(value);
  }
  else if (attributeName == "units")
  {
    return_value = setUnits(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribUncertValue.
 */
int
DistribUncertValue::unsetAttribute(const std::string& attributeName)
{
  int value = DistribBase::unsetAttribute(attributeName);

  if (attributeName == "value")
  {
    value = unsetValue();
  }
  else if (attributeName == "var")
  {
    value = unsetVar();
  }
  else if (attributeName == "units")
  {
    value = unsetUnits();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribUncertValue::createObject(XMLInputStream& stream)
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
DistribUncertValue::addExpectedAttributes(ExpectedAttributes& attributes)
{
  DistribBase::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
    attributes.add("value");
    attributes.add("var");
    attributes.add("units");
  }

  if (level == 3 && coreVersion == 2 && pkgVersion == 1)
  {
    attributes.add("value");
    attributes.add("var");
    attributes.add("units");
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribUncertValue::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribUncertValueAllowedAttributes, pkgVersion, level, version,
            details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribUncertValueAllowedCoreAttributes, pkgVersion, level,
            version, details);
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
DistribUncertValue::readL3V1V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
  unsigned int numErrs;

  // 
  // value double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetValue = attributes.readInto("value", mValue);

  if ( mIsSetValue == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Distrib attribute 'value' from the "
        "<DistribUncertValue> element must be an integer.";
      log->logPackageError("distrib",
        DistribDistribUncertValueValueMustBeDouble, pkgVersion, level, version,
          message);
    }
  }

  // 
  // var SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("var", mVar);

  if (assigned == true)
  {
    if (mVar.empty() == true)
    {
      logEmptyString(mVar, level, version, "<DistribUncertValue>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVar) == false)
    {
      std::string msg = "The var attribute on the <" + getElementName() + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mVar + "', which does not conform to the syntax.";
      log->logPackageError("distrib", DistribDistribUncertValueVarMustBeSBase,
        pkgVersion, level, version, msg, getLine(), getColumn());
    }
  }

  // 
  // units UnitSIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("units", mUnits);

  if (assigned == true)
  {
    if (mUnits.empty() == true)
    {
      logEmptyString(mUnits, level, version, "<DistribUncertValue>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mUnits) == false)
    {
      std::string msg = "The units attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mUnits + "', which does not conform to the syntax.";
      log->logPackageError("distrib",
        DistribDistribUncertValueUnitsMustBeUnitSId, pkgVersion, level, version,
          msg, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribUncertValue::readL3V2V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
  unsigned int numErrs;

  // 
  // value double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetValue = attributes.readInto("value", mValue);

  if ( mIsSetValue == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Distrib attribute 'value' from the "
        "<DistribUncertValue> element must be an integer.";
      log->logPackageError("distrib",
        DistribDistribUncertValueValueMustBeDouble, pkgVersion, level, version,
          message);
    }
  }

  // 
  // var SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("var", mVar);

  if (assigned == true)
  {
    if (mVar.empty() == true)
    {
      logEmptyString(mVar, level, version, "<DistribUncertValue>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVar) == false)
    {
      std::string msg = "The var attribute on the <" + getElementName() + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mVar + "', which does not conform to the syntax.";
      log->logPackageError("distrib", DistribDistribUncertValueVarMustBeSBase,
        pkgVersion, level, version, msg, getLine(), getColumn());
    }
  }

  // 
  // units UnitSIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("units", mUnits);

  if (assigned == true)
  {
    if (mUnits.empty() == true)
    {
      logEmptyString(mUnits, level, version, "<DistribUncertValue>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mUnits) == false)
    {
      std::string msg = "The units attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mUnits + "', which does not conform to the syntax.";
      log->logPackageError("distrib",
        DistribDistribUncertValueUnitsMustBeUnitSId, pkgVersion, level, version,
          msg, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribUncertValue::writeAttributes(XMLOutputStream& stream) const
{
  DistribBase::writeAttributes(stream);

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
DistribUncertValue::writeL3V1V1Attributes(XMLOutputStream& stream) const
{
  if (isSetValue() == true)
  {
    stream.writeAttribute("value", getPrefix(), mValue);
  }

  if (isSetVar() == true)
  {
    stream.writeAttribute("var", getPrefix(), mVar);
  }

  if (isSetUnits() == true)
  {
    stream.writeAttribute("units", getPrefix(), mUnits);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribUncertValue::writeL3V2V1Attributes(XMLOutputStream& stream) const
{
  if (isSetValue() == true)
  {
    stream.writeAttribute("value", getPrefix(), mValue);
  }

  if (isSetVar() == true)
  {
    stream.writeAttribute("var", getPrefix(), mVar);
  }

  if (isSetUnits() == true)
  {
    stream.writeAttribute("units", getPrefix(), mUnits);
  }
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribUncertValue_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribUncertValue_t *
DistribUncertValue_create(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion)
{
  return new DistribUncertValue(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribUncertValue_t object.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertValue_clone(const DistribUncertValue_t* duv)
{
  if (duv != NULL)
  {
    return static_cast<DistribUncertValue_t*>(duv->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribUncertValue_t object.
 */
LIBSBML_EXTERN
void
DistribUncertValue_free(DistribUncertValue_t* duv)
{
  if (duv != NULL)
  {
    delete duv;
  }
}


/*
 * Returns the value of the "value" attribute of this DistribUncertValue_t.
 */
LIBSBML_EXTERN
double
DistribUncertValue_getValue(const DistribUncertValue_t * duv)
{
  return (duv != NULL) ? duv->getValue() : util_NaN();
}


/*
 * Returns the value of the "var" attribute of this DistribUncertValue_t.
 */
LIBSBML_EXTERN
char *
DistribUncertValue_getVar(const DistribUncertValue_t * duv)
{
  if (duv == NULL)
  {
    return NULL;
  }

  return duv->getVar().empty() ? NULL : safe_strdup(duv->getVar().c_str());
}


/*
 * Returns the value of the "units" attribute of this DistribUncertValue_t.
 */
LIBSBML_EXTERN
char *
DistribUncertValue_getUnits(const DistribUncertValue_t * duv)
{
  if (duv == NULL)
  {
    return NULL;
  }

  return duv->getUnits().empty() ? NULL : safe_strdup(duv->getUnits().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertValue_t's "value"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DistribUncertValue_isSetValue(const DistribUncertValue_t * duv)
{
  return (duv != NULL) ? static_cast<int>(duv->isSetValue()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertValue_t's "var"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DistribUncertValue_isSetVar(const DistribUncertValue_t * duv)
{
  return (duv != NULL) ? static_cast<int>(duv->isSetVar()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertValue_t's "units"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DistribUncertValue_isSetUnits(const DistribUncertValue_t * duv)
{
  return (duv != NULL) ? static_cast<int>(duv->isSetUnits()) : 0;
}


/*
 * Sets the value of the "value" attribute of this DistribUncertValue_t.
 */
LIBSBML_EXTERN
int
DistribUncertValue_setValue(DistribUncertValue_t * duv, double value)
{
  return (duv != NULL) ? duv->setValue(value) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "var" attribute of this DistribUncertValue_t.
 */
LIBSBML_EXTERN
int
DistribUncertValue_setVar(DistribUncertValue_t * duv, const char * var)
{
  return (duv != NULL) ? duv->setVar(var) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "units" attribute of this DistribUncertValue_t.
 */
LIBSBML_EXTERN
int
DistribUncertValue_setUnits(DistribUncertValue_t * duv, const char * units)
{
  return (duv != NULL) ? duv->setUnits(units) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "value" attribute of this DistribUncertValue_t.
 */
LIBSBML_EXTERN
int
DistribUncertValue_unsetValue(DistribUncertValue_t * duv)
{
  return (duv != NULL) ? duv->unsetValue() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "var" attribute of this DistribUncertValue_t.
 */
LIBSBML_EXTERN
int
DistribUncertValue_unsetVar(DistribUncertValue_t * duv)
{
  return (duv != NULL) ? duv->unsetVar() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "units" attribute of this DistribUncertValue_t.
 */
LIBSBML_EXTERN
int
DistribUncertValue_unsetUnits(DistribUncertValue_t * duv)
{
  return (duv != NULL) ? duv->unsetUnits() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this DistribUncertValue_t is of type
 * DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_isDistribUncertBound(const DistribUncertValue_t * duv)
{
  return (duv != NULL) ? static_cast<int>(duv->isDistribUncertBound()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribUncertValue_t is of type
 * DistribExternalParameter_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_isDistribExternalParameter(const DistribUncertValue_t * duv)
{
  return (duv != NULL) ? static_cast<int>(duv->isDistribExternalParameter()) :
    0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUncertValue_t object have been set.
 */
LIBSBML_EXTERN
int
DistribUncertValue_hasRequiredAttributes(const DistribUncertValue_t * duv)
{
  return (duv != NULL) ? static_cast<int>(duv->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


