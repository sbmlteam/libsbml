/**
 * @file UserDefinedConstraintComponent.cpp
 * @brief Implementation of the UserDefinedConstraintComponent class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/packages/fbc/sbml/UserDefinedConstraintComponent.h>
#include <sbml/packages/fbc/sbml/ListOfUserDefinedConstraintComponents.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new UserDefinedConstraintComponent using the given SBML Level,
 * Version and &ldquo;fbc&rdquo; package version.
 */
UserDefinedConstraintComponent::UserDefinedConstraintComponent(
                                                               unsigned int
                                                                 level,
                                                               unsigned int
                                                                 version,
                                                               unsigned int
                                                                 pkgVersion)
  : SBase(level, version)
  , mCoefficient ("")
  , mVariable ("")
  , mVariable2 ("")
  , mVariableType (FBC_VARIABLE_TYPE_INVALID)
{
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new UserDefinedConstraintComponent using the given
 * FbcPkgNamespaces object.
 */
UserDefinedConstraintComponent::UserDefinedConstraintComponent(FbcPkgNamespaces
  *fbcns)
  : SBase(fbcns)
  , mCoefficient ("")
  , mVariable ("")
  , mVariable2 ("")
  , mVariableType (FBC_VARIABLE_TYPE_INVALID)
{
  setElementNamespace(fbcns->getURI());
  loadPlugins(fbcns);
}


/*
 * Copy constructor for UserDefinedConstraintComponent.
 */
UserDefinedConstraintComponent::UserDefinedConstraintComponent(const
  UserDefinedConstraintComponent& orig)
  : SBase( orig )
  , mCoefficient ( orig.mCoefficient )
  , mVariable ( orig.mVariable )
  , mVariable2 ( orig.mVariable2 )
  , mVariableType ( orig.mVariableType )
{
}


/*
 * Assignment operator for UserDefinedConstraintComponent.
 */
UserDefinedConstraintComponent&
UserDefinedConstraintComponent::operator=(const UserDefinedConstraintComponent&
  rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mCoefficient = rhs.mCoefficient;
    mVariable = rhs.mVariable;
    mVariable2 = rhs.mVariable2;
    mVariableType = rhs.mVariableType;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this UserDefinedConstraintComponent
 * object.
 */
UserDefinedConstraintComponent*
UserDefinedConstraintComponent::clone() const
{
  return new UserDefinedConstraintComponent(*this);
}


/*
 * Destructor for UserDefinedConstraintComponent.
 */
UserDefinedConstraintComponent::~UserDefinedConstraintComponent()
{
}


/*
 * Returns the value of the "id" attribute of this
 * UserDefinedConstraintComponent.
 */
const std::string&
UserDefinedConstraintComponent::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * UserDefinedConstraintComponent.
 */
const std::string&
UserDefinedConstraintComponent::getName() const
{
  return mName;
}


/*
 * Returns the value of the "coefficient" attribute of this
 * UserDefinedConstraintComponent.
 */
const std::string&
UserDefinedConstraintComponent::getCoefficient() const
{
  return mCoefficient;
}


/*
 * Returns the value of the "variable" attribute of this
 * UserDefinedConstraintComponent.
 */
const std::string&
UserDefinedConstraintComponent::getVariable() const
{
  return mVariable;
}


/*
 * Returns the value of the "variable2" attribute of this
 * UserDefinedConstraintComponent.
 */
const std::string&
UserDefinedConstraintComponent::getVariable2() const
{
  return mVariable2;
}


/*
 * Returns the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent.
 */
FbcVariableType_t
UserDefinedConstraintComponent::getVariableType() const
{
  return mVariableType;
}


/*
 * Returns the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent.
 */
std::string
UserDefinedConstraintComponent::getVariableTypeAsString() const
{
  std::string code_str = FbcVariableType_toString(mVariableType);
  return code_str;
}


/*
 * Predicate returning @c true if this UserDefinedConstraintComponent's "id"
 * attribute is set.
 */
bool
UserDefinedConstraintComponent::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this UserDefinedConstraintComponent's "name"
 * attribute is set.
 */
bool
UserDefinedConstraintComponent::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this UserDefinedConstraintComponent's
 * "coefficient" attribute is set.
 */
bool
UserDefinedConstraintComponent::isSetCoefficient() const
{
  return (mCoefficient.empty() == false);
}


/*
 * Predicate returning @c true if this UserDefinedConstraintComponent's
 * "variable" attribute is set.
 */
bool
UserDefinedConstraintComponent::isSetVariable() const
{
  return (mVariable.empty() == false);
}


/*
 * Predicate returning @c true if this UserDefinedConstraintComponent's
 * "variable2" attribute is set.
 */
bool
UserDefinedConstraintComponent::isSetVariable2() const
{
  return (mVariable2.empty() == false);
}


/*
 * Predicate returning @c true if this UserDefinedConstraintComponent's
 * "variableType" attribute is set.
 */
bool
UserDefinedConstraintComponent::isSetVariableType() const
{
  return (mVariableType != FBC_VARIABLE_TYPE_INVALID);
}


/*
 * Sets the value of the "id" attribute of this UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::setId(const std::string& id)
{
  unsigned int coreLevel = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (coreLevel == 3 && coreVersion == 1 && pkgVersion == 3)
  {
    return SyntaxChecker::checkAndSetSId(id, mId);
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "name" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::setName(const std::string& name)
{
  unsigned int coreLevel = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (coreLevel == 3 && coreVersion == 1 && pkgVersion == 3)
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "coefficient" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::setCoefficient(const std::string& coefficient)
{
  unsigned int coreLevel = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (coreLevel == 3 && coreVersion == 1 && pkgVersion == 3)
  {
    if (!(SyntaxChecker::isValidInternalSId(coefficient)))
    {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
      mCoefficient = coefficient;
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "variable" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::setVariable(const std::string& variable)
{
  unsigned int coreLevel = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (coreLevel == 3 && coreVersion == 1 && pkgVersion == 3)
  {
    if (!(SyntaxChecker::isValidInternalSId(variable)))
    {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
      mVariable = variable;
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "variable2" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::setVariable2(const std::string& variable)
{
  unsigned int coreLevel = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (coreLevel == 3 && coreVersion == 1 && pkgVersion == 3)
  {
    if (!(SyntaxChecker::isValidInternalSId(variable)))
    {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
      mVariable2 = variable;
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::setVariableType(const FbcVariableType_t
  variableType)
{
  unsigned int coreLevel = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (coreLevel == 3 && coreVersion == 1 && pkgVersion == 3)
  {
    if (FbcVariableType_isValid(variableType) == 0)
    {
      mVariableType = FBC_VARIABLE_TYPE_INVALID;
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
      mVariableType = variableType;
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Sets the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::setVariableType(const std::string&
  variableType)
{
  unsigned int coreLevel = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (coreLevel == 3 && coreVersion == 1 && pkgVersion == 3)
  {
    mVariableType = FbcVariableType_fromString(variableType.c_str());

    if (mVariableType == FBC_VARIABLE_TYPE_INVALID)
    {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
}


/*
 * Unsets the value of the "id" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::unsetId()
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
 * Unsets the value of the "name" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::unsetName()
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
 * Unsets the value of the "coefficient" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::unsetCoefficient()
{
  mCoefficient.erase();

  if (mCoefficient.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "variable" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::unsetVariable()
{
  mVariable.erase();

  if (mVariable.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "variable2" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::unsetVariable2()
{
  mVariable2.erase();

  if (mVariable2.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::unsetVariableType()
{
  mVariableType = FBC_VARIABLE_TYPE_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * @copydoc doc_renamesidref_common
 */
void
UserDefinedConstraintComponent::renameSIdRefs(const std::string& oldid,
                                              const std::string& newid)
{
  if (isSetCoefficient() && mCoefficient == oldid)
  {
    setCoefficient(newid);
  }

  if (isSetVariable() && mVariable == oldid)
  {
    setVariable(newid);
  }

  if (isSetVariable2() && mVariable2 == oldid)
  {
    setVariable2(newid);
  }
}


/*
 * Returns the XML element name of this UserDefinedConstraintComponent object.
 */
const std::string&
UserDefinedConstraintComponent::getElementName() const
{
  static const string name = "userDefinedConstraintComponent";
  return name;
}


/*
 * Returns the libSBML type code for this UserDefinedConstraintComponent
 * object.
 */
int
UserDefinedConstraintComponent::getTypeCode() const
{
  return SBML_FBC_USERDEFINEDCONSTRAINTCOMPONENT;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * UserDefinedConstraintComponent object have been set.
 */
bool
UserDefinedConstraintComponent::hasRequiredAttributes() const
{
  bool allPresent = true;

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1 && pkgVersion == 3)
  {
    if (isSetCoefficient() == false)
    {
      allPresent = false;
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 3)
  {
    if (isSetVariable() == false)
    {
      allPresent = false;
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 3)
  {
    if (isSetVariableType() == false)
    {
      allPresent = false;
    }
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
UserDefinedConstraintComponent::writeElements(XMLOutputStream& stream) const
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
UserDefinedConstraintComponent::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
UserDefinedConstraintComponent::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
UserDefinedConstraintComponent::enablePackageInternal(
                                                      const std::string&
                                                        pkgURI,
                                                      const std::string&
                                                        pkgPrefix,
                                                      bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::getAttribute(const std::string& attributeName,
                                             bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::getAttribute(const std::string& attributeName,
                                             int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::getAttribute(const std::string& attributeName,
                                             double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::getAttribute(const std::string& attributeName,
                                             unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::getAttribute(const std::string& attributeName,
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
  else if (attributeName == "coefficient")
  {
    value = getCoefficient();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "variable")
  {
    value = getVariable();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "variable2")
  {
    value = getVariable2();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "variableType")
  {
    value = getVariableTypeAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this UserDefinedConstraintComponent's
 * attribute "attributeName" is set.
 */
bool
UserDefinedConstraintComponent::isSetAttribute(const std::string&
  attributeName) const
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
  else if (attributeName == "coefficient")
  {
    value = isSetCoefficient();
  }
  else if (attributeName == "variable")
  {
    value = isSetVariable();
  }
  else if (attributeName == "variable2")
  {
    value = isSetVariable2();
  }
  else if (attributeName == "variableType")
  {
    value = isSetVariableType();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::setAttribute(const std::string& attributeName,
                                             bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::setAttribute(const std::string& attributeName,
                                             int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::setAttribute(const std::string& attributeName,
                                             double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::setAttribute(const std::string& attributeName,
                                             unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::setAttribute(const std::string& attributeName,
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
  else if (attributeName == "coefficient")
  {
    return_value = setCoefficient(value);
  }
  else if (attributeName == "variable")
  {
    return_value = setVariable(value);
  }
  else if (attributeName == "variable2")
  {
    return_value = setVariable2(value);
  }
  else if (attributeName == "variableType")
  {
    return_value = setVariableType(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * UserDefinedConstraintComponent.
 */
int
UserDefinedConstraintComponent::unsetAttribute(const std::string&
  attributeName)
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
  else if (attributeName == "coefficient")
  {
    value = unsetCoefficient();
  }
  else if (attributeName == "variable")
  {
    value = unsetVariable();
  }
  else if (attributeName == "variable2")
  {
    value = unsetVariable2();
  }
  else if (attributeName == "variableType")
  {
    value = unsetVariableType();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
UserDefinedConstraintComponent::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  SBase::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 3)
  {
    attributes.add("id");
    attributes.add("name");
    attributes.add("coefficient");
    attributes.add("variable");
    attributes.add("variable2");
    attributes.add("variableType");
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
UserDefinedConstraintComponent::readAttributes(const XMLAttributes& attributes,
                                               const ExpectedAttributes&
                                                 expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() && static_cast<ListOfUserDefinedConstraintComponents*>(getParentSBMLObject())->size()
    < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("fbc",
          FbcUserDefinedConstraintComponentAllowedAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("fbc", FbcUserDefinedConstraintLOUserDefinedConstraintComponentsAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        getErrorLog()->remove(NotSchemaConformant);
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("fbc",
          FbcUserDefinedConstraintComponentAllowedAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("fbc",
          FbcUserDefinedConstraintComponentAllowedCoreAttributes, pkgVersion,
            level, version, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        getErrorLog()->remove(NotSchemaConformant);
      }
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 3)
  {
    readL3V1V3Attributes(attributes);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
UserDefinedConstraintComponent::readL3V1V3Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
  unsigned int numErrs;

  // 
  // id SId (use = "optional" )
  // 

  XMLTriple tripleID("id", mURI, getPrefix());
  assigned = attributes.readInto(tripleID, mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<UserDefinedConstraintComponent>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("fbc", FbcSBMLSIdSyntax, pkgVersion, level, version,
        "The id on the <" + getElementName() + "> is '" + mId + "', which does "
          "not conform to the syntax.", getLine(), getColumn());
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
      logEmptyString(mName, level, version,
        "<UserDefinedConstraintComponent>");
    }
  }

  // 
  // coefficient SIdRef (use = "required" )
  // 

  numErrs = log ? log->getNumErrors() : 0;
  XMLTriple tripleCOEFF("coefficient", mURI, getPrefix());
  assigned = attributes.readInto("coefficient", mCoefficient);

  if (assigned == true)
  {
    if (mCoefficient.empty() == true)
    {
      logEmptyString("coefficient", level, version, "<UserDefinedConstraintComponent>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mCoefficient) == false)
    {
      log->logPackageError("fbc", FbcSBMLSIdSyntax, pkgVersion, level, version,
        "The coefficient on the <" + getElementName() + "> is '" + mCoefficient + "', which does "
          "not conform to the syntax.", getLine(), getColumn());
    }
  }
  else
  {
    if (log)
    {
      std::string message = "Fbc attribute 'coefficient' is missing from the "
        "<UserDefinedConstraintComponent> element.";
      log->logPackageError("fbc",
        FbcUserDefinedConstraintComponentAllowedAttributes, pkgVersion, level,
          version, message, getLine(), getColumn());
    }
  }

  // 
  // variable SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("variable", mVariable);

  if (assigned == true)
  {
    if (mVariable.empty() == true)
    {
      logEmptyString(mVariable, level, version,
        "<UserDefinedConstraintComponent>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVariable) == false)
    {
      std::string msg = "The variable attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mVariable + "', which does not conform to the syntax.";
      log->logPackageError("fbc", FbcSBMLSIdSyntax, pkgVersion, level, version, msg,
        getLine(), getColumn());
    }
  }
  else
  {
    if (log)
    {
      std::string message = "Fbc attribute 'variable' is missing from the "
        "<UserDefinedConstraintComponent> element.";
      log->logPackageError("fbc",
        FbcUserDefinedConstraintComponentAllowedAttributes, pkgVersion, level,
          version, message, getLine(), getColumn());
    }
  }

  // 
  // variable2 SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("variable2", mVariable2);

  if (assigned == true)
  {
    if (mVariable2.empty() == true)
    {
      logEmptyString(mVariable2, level, version,
        "<UserDefinedConstraintComponent>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVariable2) == false)
    {
      std::string msg = "The variable2 attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mVariable2 + "', which does not conform to the syntax.";
      log->logPackageError("fbc", FbcSBMLSIdSyntax, pkgVersion, level, version, msg,
        getLine(), getColumn());
    }
  }

  // 
  // variableType enum (use = "required" )
  // 

  std::string variableType;
  assigned = attributes.readInto("variableType", variableType);

  if (assigned == true)
  {
    if (variableType.empty() == true)
    {
      logEmptyString(variableType, level, version,
        "<UserDefinedConstraintComponent>");
    }
    else
    {
      mVariableType = FbcVariableType_fromString(variableType.c_str());

      if (log && FbcVariableType_isValid(mVariableType) == 0)
      {
        std::string msg = "The variableType on the "
          "<UserDefinedConstraintComponent> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + variableType + "', which is not a valid option.";

        log->logPackageError("fbc", FbcUserDefinedConstraintComponentVariableTypeMustBeFbcVariableTypeEnum,
          pkgVersion, level, version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
    if (log)
    {
      std::string message = "Fbc attribute 'variableType' is missing.";
      log->logPackageError("fbc",
        FbcUserDefinedConstraintComponentAllowedAttributes, pkgVersion, level,
          version, message, getLine(), getColumn());
    }
  }

  // 
  // variable2 SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("variable2", mVariable2);

  if (assigned == true)
  {
    if (mVariable2.empty() == true)
    {
      logEmptyString(mVariable2, level, version,
        "<UserDefinedConstraintComponent>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVariable2) == false)
    {
      std::string msg = "The variable2 attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mVariable2 + "', which does not conform to the syntax.";
      log->logPackageError("fbc", FbcSBMLSIdSyntax, pkgVersion, level, version, msg,
        getLine(), getColumn());
    }
  }

}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
UserDefinedConstraintComponent::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && pkgVersion == 3)
  {
    writeL3V1V3Attributes(stream);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
UserDefinedConstraintComponent::writeL3V1V3Attributes(XMLOutputStream& stream)
  const
{
  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  if (isSetCoefficient() == true)
  {
    stream.writeAttribute("coefficient", getPrefix(), mCoefficient);
  }

  if (isSetVariable() == true)
  {
    stream.writeAttribute("variable", getPrefix(), mVariable);
  }

  if (isSetVariable2() == true)
  {
    stream.writeAttribute("variable2", getPrefix(), mVariable2);
  }

  if (isSetVariableType() == true)
  {
    stream.writeAttribute("variableType", getPrefix(),
      FbcVariableType_toString(mVariableType));
  }
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new UserDefinedConstraintComponent_t using the given SBML Level,
 * Version and &ldquo;fbc&rdquo; package version.
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t *
UserDefinedConstraintComponent_create(unsigned int level,
                                      unsigned int version,
                                      unsigned int pkgVersion)
{
  return new UserDefinedConstraintComponent(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this UserDefinedConstraintComponent_t
 * object.
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraintComponent_clone(const UserDefinedConstraintComponent_t*
  udcc)
{
  if (udcc != NULL)
  {
    return static_cast<UserDefinedConstraintComponent_t*>(udcc->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this UserDefinedConstraintComponent_t object.
 */
LIBSBML_EXTERN
void
UserDefinedConstraintComponent_free(UserDefinedConstraintComponent_t* udcc)
{
  if (udcc != NULL)
  {
    delete udcc;
  }
}


/*
 * Returns the value of the "id" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
char *
UserDefinedConstraintComponent_getId(const UserDefinedConstraintComponent_t *
  udcc)
{
  if (udcc == NULL)
  {
    return NULL;
  }

  return udcc->getId().empty() ? NULL : safe_strdup(udcc->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
char *
UserDefinedConstraintComponent_getName(const UserDefinedConstraintComponent_t *
  udcc)
{
  if (udcc == NULL)
  {
    return NULL;
  }

  return udcc->getName().empty() ? NULL : safe_strdup(udcc->getName().c_str());
}


/*
 * Returns the value of the "coefficient" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
char*
UserDefinedConstraintComponent_getCoefficient(const
  UserDefinedConstraintComponent_t * udcc)
{
  if (udcc == NULL)
  {
    return NULL;
  }

  return udcc->getName().empty() ? NULL : safe_strdup(udcc->getCoefficient().c_str());
}


/*
 * Returns the value of the "variable" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
char *
UserDefinedConstraintComponent_getVariable(const
  UserDefinedConstraintComponent_t * udcc)
{
  if (udcc == NULL)
  {
    return NULL;
  }

  return udcc->getVariable().empty() ? NULL :
    safe_strdup(udcc->getVariable().c_str());
}

/*
 * Returns the value of the "variable" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
char *
UserDefinedConstraintComponent_getVariable2(const
  UserDefinedConstraintComponent_t * udcc)
{
  if (udcc == NULL)
  {
    return NULL;
  }

  return udcc->getVariable2().empty() ? NULL :
    safe_strdup(udcc->getVariable2().c_str());
}


/*
 * Returns the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
FbcVariableType_t
UserDefinedConstraintComponent_getVariableType(const
  UserDefinedConstraintComponent_t * udcc)
{
  if (udcc == NULL)
  {
    return FBC_VARIABLE_TYPE_INVALID;
  }

  return udcc->getVariableType();
}


/*
 * Returns the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
char *
UserDefinedConstraintComponent_getVariableTypeAsString(const
  UserDefinedConstraintComponent_t * udcc)
{
  return (char*)(FbcVariableType_toString(udcc->getVariableType()));
}


/*
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "id" attribute is set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetId(const UserDefinedConstraintComponent_t *
  udcc)
{
  return (udcc != NULL) ? static_cast<int>(udcc->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "name" attribute is set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetName(const UserDefinedConstraintComponent_t
  * udcc)
{
  return (udcc != NULL) ? static_cast<int>(udcc->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "coefficient" attribute is set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetCoefficient(const
  UserDefinedConstraintComponent_t * udcc)
{
  return (udcc != NULL) ? static_cast<int>(udcc->isSetCoefficient()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "variable" attribute is set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetVariable(const
  UserDefinedConstraintComponent_t * udcc)
{
  return (udcc != NULL) ? static_cast<int>(udcc->isSetVariable()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "variable2" attribute is set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetVariable2(const
  UserDefinedConstraintComponent_t * udcc)
{
  return (udcc != NULL) ? static_cast<int>(udcc->isSetVariable2()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "variableType" attribute is set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetVariableType(const
  UserDefinedConstraintComponent_t * udcc)
{
  return (udcc != NULL) ? static_cast<int>(udcc->isSetVariableType()) : 0;
}


/*
 * Sets the value of the "id" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setId(UserDefinedConstraintComponent_t * udcc,
                                     const char * id)
{
  return (udcc != NULL) ? udcc->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setName(UserDefinedConstraintComponent_t * udcc,
                                       const char * name)
{
  return (udcc != NULL) ? udcc->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coefficient" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setCoefficient(
                                              UserDefinedConstraintComponent_t
                                                * udcc,
                                              const char * coefficient)
{
  return (udcc != NULL) ? udcc->setCoefficient(coefficient) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "variable" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setVariable(
                                           UserDefinedConstraintComponent_t *
                                             udcc,
                                           const char * variable)
{
  return (udcc != NULL) ? udcc->setVariable(variable) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "variable2" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setVariable2(
                                           UserDefinedConstraintComponent_t *
                                             udcc,
                                           const char * variable)
{
  return (udcc != NULL) ? udcc->setVariable2(variable) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setVariableType(
                                               UserDefinedConstraintComponent_t
                                                 * udcc,
                                               FbcVariableType_t variableType)
{
  return (udcc != NULL) ? udcc->setVariableType(variableType) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setVariableTypeAsString(
                                                       UserDefinedConstraintComponent_t
                                                         * udcc,
                                                       const char *
                                                         variableType)
{
  return (udcc != NULL) ? udcc->setVariableType(variableType):
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_unsetId(UserDefinedConstraintComponent_t * udcc)
{
  return (udcc != NULL) ? udcc->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_unsetName(UserDefinedConstraintComponent_t *
  udcc)
{
  return (udcc != NULL) ? udcc->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "coefficient" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_unsetCoefficient(UserDefinedConstraintComponent_t
  * udcc)
{
  return (udcc != NULL) ? udcc->unsetCoefficient() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "variable" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_unsetVariable(UserDefinedConstraintComponent_t *
  udcc)
{
  return (udcc != NULL) ? udcc->unsetVariable() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "variable2" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_unsetVariable2(UserDefinedConstraintComponent_t
  * udcc)
{
  return (udcc != NULL) ? udcc->unsetVariable2() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent_t.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_unsetVariableType(UserDefinedConstraintComponent_t
  * udcc)
{
  return (udcc != NULL) ? udcc->unsetVariableType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * UserDefinedConstraintComponent_t object have been set.
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_hasRequiredAttributes(const
  UserDefinedConstraintComponent_t * udcc)
{
  return (udcc != NULL) ? static_cast<int>(udcc->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


