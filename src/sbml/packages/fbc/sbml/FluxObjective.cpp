/**
 * @file   FluxObjective.cpp
 * @brief  Implementation of the FluxObjective class
 * @author SBMLTeam
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <iostream>
#include <limits>

#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/packages/fbc/sbml/FluxObjective.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>

#if defined(WIN32) && !defined(CYGWIN)
#define isnan _isnan
#endif


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

/*
 * Creates a new FluxObjective with the given level, version, and package version.
 */
FluxObjective::FluxObjective (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
//  , mId ("")
//  , mName ("")
  , mReaction ("")
  , mCoefficient (numeric_limits<double>::quiet_NaN())
  , mIsSetCoefficient (false)
  , mVariableType (FBC_VARIABLE_TYPE_INVALID)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new FluxObjective with the given FbcPkgNamespaces object.
 */
FluxObjective::FluxObjective (FbcPkgNamespaces* fbcns)
  : SBase(fbcns)
//  , mId ("")
//  , mName ("")
  , mReaction ("")
  , mCoefficient (numeric_limits<double>::quiet_NaN())
  , mIsSetCoefficient (false)
  , mVariableType (FBC_VARIABLE_TYPE_INVALID)
{
  // set the element namespace of this object
  setElementNamespace(fbcns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(fbcns);
}


/*
 * Copy constructor for FluxObjective.
 */
FluxObjective::FluxObjective(const FluxObjective& orig)
  : SBase( orig )
  , mReaction ( orig.mReaction )
  , mCoefficient ( orig.mCoefficient )
  , mIsSetCoefficient ( orig.mIsSetCoefficient )
  , mVariableType ( orig.mVariableType )
{
}


/*
 * Assignment for FluxObjective.
 */
FluxObjective&
FluxObjective::operator=(const FluxObjective& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    mReaction  = rhs.mReaction;
    mCoefficient  = rhs.mCoefficient;
    mIsSetCoefficient  = rhs.mIsSetCoefficient;
    mVariableType = rhs.mVariableType;
  }
  return *this;
}


/*
 * Clone for FluxObjective.
 */
FluxObjective*
FluxObjective::clone () const
{
  return new FluxObjective(*this);
}


/*
 * Destructor for FluxObjective.
 */
FluxObjective::~FluxObjective ()
{
}


/*
 * Returns the value of the "id" attribute of this FluxObjective.
 */
const std::string&
FluxObjective::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this FluxObjective.
 */
const std::string&
FluxObjective::getName() const
{
  return mName;
}


/*
 * Returns the value of the "reaction" attribute of this FluxObjective.
 */
const std::string&
FluxObjective::getReaction() const
{
  return mReaction;
}


/*
 * Returns the value of the "coefficient" attribute of this FluxObjective.
 */
double
FluxObjective::getCoefficient() const
{
  return mCoefficient;
}


/*
 * Returns the value of the "variableType" attribute of this FluxObjective.
 */
FbcVariableType_t
FluxObjective::getVariableType() const
{
  return mVariableType;
}


/*
 * Returns the value of the "variableType" attribute of this FluxObjective.
 */
std::string
FluxObjective::getVariableTypeAsString() const
{
  std::string code_str = FbcVariableType_toString(mVariableType);
  return code_str;
}


/*
 * Predicate returning @c true if this FluxObjective's "id" attribute is set.
 */
bool
FluxObjective::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this FluxObjective's "name" attribute is set.
 */
bool
FluxObjective::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this FluxObjective's "reaction" attribute is
 * set.
 */
bool
FluxObjective::isSetReaction() const
{
  return (mReaction.empty() == false);
}


/*
 * Predicate returning @c true if this FluxObjective's "coefficient" attribute
 * is set.
 */
bool
FluxObjective::isSetCoefficient() const
{
  return mIsSetCoefficient;
}


/*
 * Predicate returning @c true if this FluxObjective's "variableType" attribute
 * is set.
 */
bool
FluxObjective::isSetVariableType() const
{
  return (mVariableType != FBC_VARIABLE_TYPE_INVALID);
}


/*
 * Sets id and returns value indicating success.
 */
int
FluxObjective::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
FluxObjective::setName(const std::string& name)
{
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets reaction and returns value indicating success.
 */
int
FluxObjective::setReaction(const std::string& reaction)
{
  return SyntaxChecker::checkAndSetSId(reaction ,mReaction);
}


/*
 * Sets coefficient and returns value indicating success.
 */
int
FluxObjective::setCoefficient(double coefficient)
{
  mCoefficient = coefficient;
  mIsSetCoefficient = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "variableType" attribute of this FluxObjective.
 */
int
FluxObjective::setVariableType(const FbcVariableType_t variableType)
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
 * Sets the value of the "variableType" attribute of this FluxObjective.
 */
int
FluxObjective::setVariableType(const std::string& variableType)
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
 * Unsets id and returns value indicating success.
 */
int
FluxObjective::unsetId()
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
 * Unsets name and returns value indicating success.
 */
int
FluxObjective::unsetName()
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
 * Unsets reaction and returns value indicating success.
 */
int
FluxObjective::unsetReaction()
{
  mReaction.erase();

  if (mReaction.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets coefficient and returns value indicating success.
 */
int
FluxObjective::unsetCoefficient()
{
  mCoefficient = numeric_limits<double>::quiet_NaN();
  mIsSetCoefficient = false;

  if (isSetCoefficient() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "variableType" attribute of this FluxObjective.
 */
int
FluxObjective::unsetVariableType()
{
  mVariableType = FBC_VARIABLE_TYPE_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
FluxObjective::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetReaction() == true && mReaction == oldid)
  {
    setReaction(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
FluxObjective::getElementName () const
{
  static const string name = "fluxObjective";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
FluxObjective::getTypeCode () const
{
  return SBML_FBC_FLUXOBJECTIVE;
}


/*
 * check if all the required attributes are set
 */
bool
FluxObjective::hasRequiredAttributes () const
{
  bool allPresent = true;

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (isSetReaction() == false)
    allPresent = false;

  if (isSetCoefficient() == false)
    allPresent = false;

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
 * write contained elements
 */
void
FluxObjective::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
FluxObjective::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
FluxObjective::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
FluxObjective::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FluxObjective.
 */
int
FluxObjective::getAttribute(const std::string& attributeName,
                            bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FluxObjective.
 */
int
FluxObjective::getAttribute(const std::string& attributeName,
                            int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FluxObjective.
 */
int
FluxObjective::getAttribute(const std::string& attributeName,
                            double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "coefficient")
  {
    value = getCoefficient();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FluxObjective.
 */
int
FluxObjective::getAttribute(const std::string& attributeName,
                            unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FluxObjective.
 */
int
FluxObjective::getAttribute(const std::string& attributeName,
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
  else if (attributeName == "reaction")
  {
    value = getReaction();
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
 * Predicate returning @c true if this FluxObjective's attribute
 * "attributeName" is set.
 */
bool
FluxObjective::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "reaction")
  {
    value = isSetReaction();
  }
  else if (attributeName == "coefficient")
  {
    value = isSetCoefficient();
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
 * Sets the value of the "attributeName" attribute of this FluxObjective.
 */
int
FluxObjective::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FluxObjective.
 */
int
FluxObjective::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FluxObjective.
 */
int
FluxObjective::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "coefficient")
  {
    return_value = setCoefficient(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FluxObjective.
 */
int
FluxObjective::setAttribute(const std::string& attributeName,
                            unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FluxObjective.
 */
int
FluxObjective::setAttribute(const std::string& attributeName,
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
  else if (attributeName == "reaction")
  {
    return_value = setReaction(value);
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
 * Unsets the value of the "attributeName" attribute of this FluxObjective.
 */
int
FluxObjective::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "reaction")
  {
    value = unsetReaction();
  }
  else if (attributeName == "coefficient")
  {
    value = unsetCoefficient();
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
 * Get the list of expected attributes for this element.
 */
void
FluxObjective::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  attributes.add("id");
  attributes.add("name");
  attributes.add("reaction");
  attributes.add("coefficient");

  if (level == 3 && coreVersion == 1 && pkgVersion == 3)
  {
    attributes.add("variableType");
  }
}

/** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
FluxObjective::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel = getLevel();
  const unsigned int sbmlVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
  // look to see whether an unknown attribute error was logged
  // during the read of the listOfFluxBounds - which will have
  // happened immediately prior to this read
  if (log && getParentSBMLObject() &&
    static_cast<ListOfFluxObjectives*>(getParentSBMLObject())->size() < 2)
  {
    unsigned int numErrs = log->getNumErrors();
    for (int n = (int)numErrs - 1; n >= 0; n--)
    {
      if (log->getError((unsigned int)n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
          log->getError((unsigned int)n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("fbc", FbcObjectiveLOFluxObjAllowedAttribs,
          getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (log->getError((unsigned int)n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
          log->getError((unsigned int)n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("fbc", FbcObjectiveLOFluxObjAllowedAttribs,
          getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (log->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        log->remove(NotSchemaConformant);
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (log != NULL)
  {
    unsigned int numErrs = log->getNumErrors();
    for (int n = (int)numErrs - 1; n >= 0; n--)
    {
      if (log->getError((unsigned int)n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
          log->getError((unsigned int)n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("fbc", FbcFluxObjectRequiredAttributes,
          getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (log->getError((unsigned int)n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
          log->getError((unsigned int)n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("fbc", FbcFluxObjectAllowedL3Attributes,
          getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (log->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        log->remove(NotSchemaConformant);
      }
    }
  }


  //
  // Reads an attribute "id" (optional)
  //
  bool assigned = attributes.readInto("id", mId);

  if (assigned)
  {
    // "id" attribute is set to this fbc element

    if (mId.empty())
    {
      //
      // Logs an error if the "id" attribute is empty.
      //
      logEmptyString(mId, sbmlLevel, sbmlVersion, "<fbc>");
    }
    else if (!SyntaxChecker::isValidSBMLSId(mId))
    {
      //
      // Logs an error if the "id" attribute doesn't
      // conform to the SBML type SId.
      //
      log->logPackageError("fbc", FbcSBMLSIdSyntax,
        getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
    }
  }

  attributes.readInto("name", mName);

  assigned = attributes.readInto("reaction", mReaction);
  if (assigned == false)
  {
    std::string message = "Fbc attribute 'reaction' is missing.";
    log->logPackageError("fbc", FbcFluxObjectRequiredAttributes,
      getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }
  else
  {
    if (mReaction.empty())
    {
      //
      // Logs an error if the "id" attribute is empty.
      //
      logEmptyString(mReaction, sbmlLevel, sbmlVersion, "<fbc>");
    }
    else if (!SyntaxChecker::isValidSBMLSId(mReaction))
    {
      //
      // Logs an error if the "id" attribute doesn't
      // conform to the SBML type SId.
      //
      log->logPackageError("fbc", FbcFluxObjectReactionMustBeSIdRef,
        getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
    }
  }


  unsigned int numErrs = log->getNumErrors();
  mIsSetCoefficient = attributes.readInto("coefficient", mCoefficient, log);

  if (mIsSetCoefficient == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      log->logPackageError("fbc", FbcFluxObjectCoefficientMustBeDouble,
        getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
    }
    else
    {
      std::string message = "Fbc attribute 'coefficient' is missing.";
      log->logPackageError("fbc", FbcFluxObjectRequiredAttributes,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  // 
  // variableType enum (use = "required" )
  // 

  std::string variableType;
  if (pkgVersion == 3)
  {
    assigned = attributes.readInto("variableType", variableType);

    if (assigned == true)
    {
      if (variableType.empty() == true)
      {
        logEmptyString(variableType, sbmlLevel, sbmlVersion, "<FluxObjective>");
      }
      else
      {
        mVariableType = FbcVariableType_fromString(variableType.c_str());

        if (log && FbcVariableType_isValid(mVariableType) == 0)
        {
          std::string msg = "The variableType on the <FluxObjective> ";

          if (isSetId())
          {
            msg += "with id '" + getId() + "'";
          }

          msg += "is '" + variableType + "', which is not a valid option.";

          log->logPackageError("fbc",
            FbcFluxObjectiveVariableTypeMustBeFbcVariableTypeEnum, pkgVersion,
            sbmlLevel, sbmlVersion, msg, getLine(), getColumn());
        }
      }
    }
    else
    {
      if (log)
      {
        std::string message = "Fbc attribute 'variableType' is missing.";
        log->logPackageError("fbc", FbcFluxObjectAllowedL3Attributes,
          pkgVersion, sbmlLevel, sbmlVersion, message, getLine(), getColumn());
      }
    }
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
FluxObjective::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  if (isSetReaction() == true)
    stream.writeAttribute("reaction", getPrefix(), mReaction);

  if (isSetCoefficient() == true)
    stream.writeAttribute("coefficient", getPrefix(), mCoefficient);


  if (isSetVariableType() == true)
  {
    stream.writeAttribute("variableType", getPrefix(),
      FbcVariableType_toString(mVariableType));
  }

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}


  /** @endcond */


/*
 * Constructor 
 */
ListOfFluxObjectives::ListOfFluxObjectives(unsigned int level, 
                       unsigned int version, 
                       unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfFluxObjectives::ListOfFluxObjectives(FbcPkgNamespaces* fbcns)
  : ListOf(fbcns)
{
  setElementNamespace(fbcns->getURI());
}


/*
 * Returns a deep copy of this ListOfFluxObjectives 
 */
ListOfFluxObjectives* 
ListOfFluxObjectives::clone () const
 {
  return new ListOfFluxObjectives(*this);
}


/*
 * Get a FluxObjective from the ListOfFluxObjectives by index.
 */
FluxObjective*
ListOfFluxObjectives::get(unsigned int n)
{
  return static_cast<FluxObjective*>(ListOf::get(n));
}


/*
 * Get a FluxObjective from the ListOfFluxObjectives by index.
 */
const FluxObjective*
ListOfFluxObjectives::get(unsigned int n) const
{
  return static_cast<const FluxObjective*>(ListOf::get(n));
}


/*
 * Get a FluxObjective from the ListOfFluxObjectives by id.
 */
FluxObjective*
ListOfFluxObjectives::get(const std::string& sid)
{
  return const_cast<FluxObjective*>(
    static_cast<const ListOfFluxObjectives&>(*this).get(sid));
}


/*
 * Get a FluxObjective from the ListOfFluxObjectives by id.
 */
const FluxObjective*
ListOfFluxObjectives::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<FluxObjective>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <FluxObjective*> (*result);
}


/*
 * Adds a copy the given FluxObjective to this ListOfFluxObjectives.
 *
 * @param fo the FluxObjective object to add.
 *
 * @copydetails doc_returns_success_code
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfFluxObjectives::addFluxObjective(const FluxObjective* fo)
{
  if (fo == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (fo->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != fo->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != fo->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(fo)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(fo);
  }
}


/*
 * Get the number of FluxObjective objects in this ListOfFluxObjectives.
 *
 * @return the number of FluxObjective objects in this ListOfFluxObjectives
 */
unsigned int 
ListOfFluxObjectives::getNumFluxObjectives() const
{
  return size();
}


/*
 * Creates a new FluxObjective object, adds it to this ListOfFluxObjectives
 * and returns the FluxObjective object created. 
 *
 * @return a new FluxObjective object instance
 *
 * @see addFluxObjective(const FluxObjective* fo)
 */
FluxObjective* 
ListOfFluxObjectives::createFluxObjective()
{
  FluxObjective* fo = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    fo = new FluxObjective(fbcns);
    delete fbcns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(fo != NULL)
  {
    appendAndOwn(fo);
  }

  return fo;
}

/*
 * Removes the nth FluxObjective from this ListOfFluxObjectives
 */
FluxObjective*
ListOfFluxObjectives::remove(unsigned int n)
{
  return static_cast<FluxObjective*>(ListOf::remove(n));
}


/*
 * Removes the FluxObjective from this ListOfFluxObjectives with the given identifier
 */
FluxObjective*
ListOfFluxObjectives::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<FluxObjective>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <FluxObjective*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfFluxObjectives::getElementName () const
{
  static const string name = "listOfFluxObjectives";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfFluxObjectives::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfFluxObjectives::getItemTypeCode () const
{
  return SBML_FBC_FLUXOBJECTIVE;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new FluxObjective in this ListOfFluxObjectives
 */
SBase*
ListOfFluxObjectives::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "fluxObjective")
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    object = new FluxObjective(fbcns);
    appendAndOwn(object);
    delete fbcns;
  }

  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Fbc package.
 */
void
ListOfFluxObjectives::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(FbcExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(FbcExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


LIBSBML_EXTERN
FluxObjective_t *
FluxObjective_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion)
{
  return new FluxObjective(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
FluxObjective_free(FluxObjective_t * fo)
{
  if (fo != NULL)
    delete fo;
}


LIBSBML_EXTERN
FluxObjective_t *
FluxObjective_clone(FluxObjective_t * fo)
{
  if (fo != NULL)
  {
    return static_cast<FluxObjective_t*>(fo->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
FluxObjective_getId(const FluxObjective_t * fo)
{
  return (fo != NULL && fo->isSetId()) ? fo->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
FluxObjective_getName(const FluxObjective_t * fo)
{
  return (fo != NULL && fo->isSetName()) ? fo->getName().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
FluxObjective_getReaction(const FluxObjective_t * fo)
{
  return (fo != NULL && fo->isSetReaction()) ? fo->getReaction().c_str() : NULL;
}


LIBSBML_EXTERN
double
FluxObjective_getCoefficient(const FluxObjective_t * fo)
{
  return (fo != NULL) ? fo->getCoefficient() : numeric_limits<double>::quiet_NaN();
}


/*
 * Returns the value of the "variableType" attribute of this FluxObjective_t.
 */
LIBSBML_EXTERN
FbcVariableType_t
FluxObjective_getVariableType(const FluxObjective_t * fo)
{
  if (fo == NULL)
  {
    return FBC_VARIABLE_TYPE_INVALID;
  }

  return fo->getVariableType();
}


/*
 * Returns the value of the "variableType" attribute of this FluxObjective_t.
 */
LIBSBML_EXTERN
char *
FluxObjective_getVariableTypeAsString(const FluxObjective_t * fo)
{
  return (char*)(FbcVariableType_toString(fo->getVariableType()));
}


LIBSBML_EXTERN
int
FluxObjective_isSetId(const FluxObjective_t * fo)
{
  return (fo != NULL) ? static_cast<int>(fo->isSetId()) : 0;
}


LIBSBML_EXTERN
int
FluxObjective_isSetName(const FluxObjective_t * fo)
{
  return (fo != NULL) ? static_cast<int>(fo->isSetName()) : 0;
}


LIBSBML_EXTERN
int
FluxObjective_isSetReaction(const FluxObjective_t * fo)
{
  return (fo != NULL) ? static_cast<int>(fo->isSetReaction()) : 0;
}


LIBSBML_EXTERN
int
FluxObjective_isSetCoefficient(const FluxObjective_t * fo)
{
  return (fo != NULL) ? static_cast<int>(fo->isSetCoefficient()) : 0;
}


LIBSBML_EXTERN
int
FluxObjective_isSetVariableType(const FluxObjective_t * fo)
{
  return (fo != NULL) ? static_cast<int>(fo->isSetVariableType()) : 0;
}


/*
 * Sets the value of the "id" attribute of this FluxObjective_t.
 */
LIBSBML_EXTERN
int
FluxObjective_setId(FluxObjective_t * fo, const char * id)
{
  if (fo != NULL)
    return (id == NULL) ? fo->setId("") : fo->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FluxObjective_setName(FluxObjective_t * fo, const char * name)
{
  if (fo != NULL)
    return (name == NULL) ? fo->setName("") : fo->setName(name);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FluxObjective_setReaction(FluxObjective_t * fo, const char * reaction)
{
  if (fo != NULL)
    return (reaction == NULL) ? fo->setReaction("") : fo->setReaction(reaction);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FluxObjective_setCoefficient(FluxObjective_t * fo, double coefficient)
{
  if (fo != NULL)
    return fo->setCoefficient(coefficient);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FluxObjective_setVariableType(FluxObjective_t * fo,
                              FbcVariableType_t variableType)
{
  return (fo != NULL) ? fo->setVariableType(variableType) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "variableType" attribute of this FluxObjective_t.
 */
LIBSBML_EXTERN
int
FluxObjective_setVariableTypeAsString(FluxObjective_t * fo,
                                      const char * variableType)
{
  return (fo != NULL) ? fo->setVariableType(variableType):
    LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
int
FluxObjective_unsetId(FluxObjective_t * fo)
{
  return (fo != NULL) ? fo->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FluxObjective_unsetName(FluxObjective_t * fo)
{
  return (fo != NULL) ? fo->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FluxObjective_unsetReaction(FluxObjective_t * fo)
{
  return (fo != NULL) ? fo->unsetReaction() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FluxObjective_unsetCoefficient(FluxObjective_t * fo)
{
  return (fo != NULL) ? fo->unsetCoefficient() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "variableType" attribute of this FluxObjective_t.
 */
LIBSBML_EXTERN
int
FluxObjective_unsetVariableType(FluxObjective_t * fo)
{
  return (fo != NULL) ? fo->unsetVariableType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
FluxObjective_hasRequiredAttributes(const FluxObjective_t * fo)
{
  return (fo != NULL) ? static_cast<int>(fo->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
FluxObjective_t *
ListOfFluxObjectives_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfFluxObjectives *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
FluxObjective_t *
ListOfFluxObjectives_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfFluxObjectives *>(lo)->remove(sid) : NULL;
}




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */



