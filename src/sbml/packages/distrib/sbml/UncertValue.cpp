/**
 * @file UncertValue.cpp
 * @brief Implementation of the UncertValue class.
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
#include <sbml/packages/distrib/sbml/UncertValue.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>

#include <sbml/packages/distrib/sbml/ExternalParameter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new UncertValue using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
UncertValue::UncertValue(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
  : DistribBase(level, version, pkgVersion)
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
 * Creates a new UncertValue using the given DistribPkgNamespaces object.
 */
UncertValue::UncertValue(DistribPkgNamespaces *distribns)
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
 * Copy constructor for UncertValue.
 */
UncertValue::UncertValue(const UncertValue& orig)
  : DistribBase( orig )
  , mValue ( orig.mValue )
  , mIsSetValue ( orig.mIsSetValue )
  , mVar ( orig.mVar )
  , mUnits ( orig.mUnits )
  , mElementName ( orig.mElementName )
{
}


/*
 * Assignment operator for UncertValue.
 */
UncertValue&
UncertValue::operator=(const UncertValue& rhs)
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
 * Creates and returns a deep copy of this UncertValue object.
 */
UncertValue*
UncertValue::clone() const
{
  return new UncertValue(*this);
}


/*
 * Destructor for UncertValue.
 */
UncertValue::~UncertValue()
{
}


/*
 * Returns the value of the "value" attribute of this UncertValue.
 */
double
UncertValue::getValue() const
{
  return mValue;
}


/*
 * Returns the value of the "var" attribute of this UncertValue.
 */
const std::string&
UncertValue::getVar() const
{
  return mVar;
}


/*
 * Returns the value of the "units" attribute of this UncertValue.
 */
const std::string&
UncertValue::getUnits() const
{
  return mUnits;
}


/*
 * Predicate returning @c true if this UncertValue's "value" attribute is set.
 */
bool
UncertValue::isSetValue() const
{
  return mIsSetValue;
}


/*
 * Predicate returning @c true if this UncertValue's "var" attribute is set.
 */
bool
UncertValue::isSetVar() const
{
  return (mVar.empty() == false);
}


/*
 * Predicate returning @c true if this UncertValue's "units" attribute is set.
 */
bool
UncertValue::isSetUnits() const
{
  return (mUnits.empty() == false);
}


/*
 * Sets the value of the "value" attribute of this UncertValue.
 */
int
UncertValue::setValue(double value)
{
  mValue = value;
  mIsSetValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "var" attribute of this UncertValue.
 */
int
UncertValue::setVar(const std::string& var)
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
 * Sets the value of the "units" attribute of this UncertValue.
 */
int
UncertValue::setUnits(const std::string& units)
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
 * Unsets the value of the "value" attribute of this UncertValue.
 */
int
UncertValue::unsetValue()
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
 * Unsets the value of the "var" attribute of this UncertValue.
 */
int
UncertValue::unsetVar()
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
 * Unsets the value of the "units" attribute of this UncertValue.
 */
int
UncertValue::unsetUnits()
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
 * Predicate returning @c true if this abstract "UncertValue" is of type
 * ExternalParameter
 */
bool
UncertValue::isExternalParameter() const
{
  return dynamic_cast<const ExternalParameter*>(this) != NULL;
}


/*
 * @copydoc doc_renamesidref_common
 */
void
UncertValue::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetVar() && mVar == oldid)
  {
    setVar(newid);
  }

  if (isSetUnits() && mUnits == oldid)
  {
    setUnits(newid);
  }
}


/*
 * Returns the XML element name of this UncertValue object.
 */
const std::string&
UncertValue::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this UncertValue object.
 */
void
UncertValue::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this UncertValue object.
 */
int
UncertValue::getTypeCode() const
{
  return SBML_DISTRIB_UNCERTVALUE;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * UncertValue object have been set.
 */
bool
UncertValue::hasRequiredAttributes() const
{
  bool allPresent = DistribBase::hasRequiredAttributes();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
UncertValue::writeElements(XMLOutputStream& stream) const
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
UncertValue::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
UncertValue::setSBMLDocument(SBMLDocument* d)
{
  DistribBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
UncertValue::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix,
                                   bool flag)
{
  DistribBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertValue.
 */
int
UncertValue::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertValue.
 */
int
UncertValue::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertValue.
 */
int
UncertValue::getAttribute(const std::string& attributeName,
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
 * Gets the value of the "attributeName" attribute of this UncertValue.
 */
int
UncertValue::getAttribute(const std::string& attributeName,
                          unsigned int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this UncertValue.
 */
int
UncertValue::getAttribute(const std::string& attributeName,
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
 * Predicate returning @c true if this UncertValue's attribute "attributeName"
 * is set.
 */
bool
UncertValue::isSetAttribute(const std::string& attributeName) const
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
 * Sets the value of the "attributeName" attribute of this UncertValue.
 */
int
UncertValue::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertValue.
 */
int
UncertValue::setAttribute(const std::string& attributeName, int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertValue.
 */
int
UncertValue::setAttribute(const std::string& attributeName, double value)
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
 * Sets the value of the "attributeName" attribute of this UncertValue.
 */
int
UncertValue::setAttribute(const std::string& attributeName,
                          unsigned int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this UncertValue.
 */
int
UncertValue::setAttribute(const std::string& attributeName,
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
 * Unsets the value of the "attributeName" attribute of this UncertValue.
 */
int
UncertValue::unsetAttribute(const std::string& attributeName)
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
UncertValue::createObject(XMLInputStream& stream)
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
UncertValue::addExpectedAttributes(ExpectedAttributes& attributes)
{
  DistribBase::addExpectedAttributes(attributes);

  attributes.add("value");

  attributes.add("var");

  attributes.add("units");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
UncertValue::readAttributes(const XMLAttributes& attributes,
                            const ExpectedAttributes& expectedAttributes)
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
        log->logPackageError("distrib", DistribUncertValueAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribUncertValueAllowedCoreAttributes, pkgVersion, level, version,
            details);
      }
    }
  }

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
      std::string message = "Distrib attribute 'value' from the <UncertValue> "
        "element must be an integer.";
      log->logPackageError("distrib", DistribUncertValueValueMustBeDouble,
        pkgVersion, level, version, message);
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
      logEmptyString(mVar, level, version, "<UncertValue>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVar) == false)
    {
      std::string msg = "The var attribute on the <" + getElementName() + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mVar + "', which does not conform to the syntax.";
      log->logPackageError("distrib", DistribUncertValueVarMustBeSBase,
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
      logEmptyString(mUnits, level, version, "<UncertValue>");
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
      log->logPackageError("distrib", DistribUncertValueUnitsMustBeUnitSId,
        pkgVersion, level, version, msg, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
UncertValue::writeAttributes(XMLOutputStream& stream) const
{
  DistribBase::writeAttributes(stream);

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

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new UncertValue_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
UncertValue_t *
UncertValue_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion)
{
  return new UncertValue(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this UncertValue_t object.
 */
LIBSBML_EXTERN
UncertValue_t*
UncertValue_clone(const UncertValue_t* uv)
{
  if (uv != NULL)
  {
    return static_cast<UncertValue_t*>(uv->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this UncertValue_t object.
 */
LIBSBML_EXTERN
void
UncertValue_free(UncertValue_t* uv)
{
  if (uv != NULL)
  {
    delete uv;
  }
}


/*
 * Returns the value of the "value" attribute of this UncertValue_t.
 */
LIBSBML_EXTERN
double
UncertValue_getValue(const UncertValue_t * uv)
{
  return (uv != NULL) ? uv->getValue() : util_NaN();
}


/*
 * Returns the value of the "var" attribute of this UncertValue_t.
 */
LIBSBML_EXTERN
char *
UncertValue_getVar(const UncertValue_t * uv)
{
  if (uv == NULL)
  {
    return NULL;
  }

  return uv->getVar().empty() ? NULL : safe_strdup(uv->getVar().c_str());
}


/*
 * Returns the value of the "units" attribute of this UncertValue_t.
 */
LIBSBML_EXTERN
char *
UncertValue_getUnits(const UncertValue_t * uv)
{
  if (uv == NULL)
  {
    return NULL;
  }

  return uv->getUnits().empty() ? NULL : safe_strdup(uv->getUnits().c_str());
}


/*
 * Predicate returning @c 1 (true) if this UncertValue_t's "value" attribute is
 * set.
 */
LIBSBML_EXTERN
int
UncertValue_isSetValue(const UncertValue_t * uv)
{
  return (uv != NULL) ? static_cast<int>(uv->isSetValue()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UncertValue_t's "var" attribute is
 * set.
 */
LIBSBML_EXTERN
int
UncertValue_isSetVar(const UncertValue_t * uv)
{
  return (uv != NULL) ? static_cast<int>(uv->isSetVar()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UncertValue_t's "units" attribute is
 * set.
 */
LIBSBML_EXTERN
int
UncertValue_isSetUnits(const UncertValue_t * uv)
{
  return (uv != NULL) ? static_cast<int>(uv->isSetUnits()) : 0;
}


/*
 * Sets the value of the "value" attribute of this UncertValue_t.
 */
LIBSBML_EXTERN
int
UncertValue_setValue(UncertValue_t * uv, double value)
{
  return (uv != NULL) ? uv->setValue(value) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "var" attribute of this UncertValue_t.
 */
LIBSBML_EXTERN
int
UncertValue_setVar(UncertValue_t * uv, const char * var)
{
  return (uv != NULL) ? uv->setVar(var) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "units" attribute of this UncertValue_t.
 */
LIBSBML_EXTERN
int
UncertValue_setUnits(UncertValue_t * uv, const char * units)
{
  return (uv != NULL) ? uv->setUnits(units) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "value" attribute of this UncertValue_t.
 */
LIBSBML_EXTERN
int
UncertValue_unsetValue(UncertValue_t * uv)
{
  return (uv != NULL) ? uv->unsetValue() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "var" attribute of this UncertValue_t.
 */
LIBSBML_EXTERN
int
UncertValue_unsetVar(UncertValue_t * uv)
{
  return (uv != NULL) ? uv->unsetVar() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "units" attribute of this UncertValue_t.
 */
LIBSBML_EXTERN
int
UncertValue_unsetUnits(UncertValue_t * uv)
{
  return (uv != NULL) ? uv->unsetUnits() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this UncertValue_t is of type
 * ExternalParameter_t
 */
LIBSBML_EXTERN
int
UncertValue_isExternalParameter(const UncertValue_t * uv)
{
  return (uv != NULL) ? static_cast<int>(uv->isExternalParameter()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * UncertValue_t object have been set.
 */
LIBSBML_EXTERN
int
UncertValue_hasRequiredAttributes(const UncertValue_t * uv)
{
  return (uv != NULL) ? static_cast<int>(uv->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


