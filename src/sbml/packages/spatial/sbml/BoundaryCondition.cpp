/**
 * @file BoundaryCondition.cpp
 * @brief Implementation of the BoundaryCondition class.
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
#include <sbml/packages/spatial/sbml/BoundaryCondition.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new BoundaryCondition using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
BoundaryCondition::BoundaryCondition(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion)
  : SBase(level, version)
  , mVariable ("")
  , mType (SPATIAL_BOUNDARYKIND_INVALID)
  , mCoordinateBoundary ("")
  , mBoundaryDomainType ("")
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new BoundaryCondition using the given SpatialPkgNamespaces object.
 */
BoundaryCondition::BoundaryCondition(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mVariable ("")
  , mType (SPATIAL_BOUNDARYKIND_INVALID)
  , mCoordinateBoundary ("")
  , mBoundaryDomainType ("")
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for BoundaryCondition.
 */
BoundaryCondition::BoundaryCondition(const BoundaryCondition& orig)
  : SBase( orig )
  , mVariable ( orig.mVariable )
  , mType ( orig.mType )
  , mCoordinateBoundary ( orig.mCoordinateBoundary )
  , mBoundaryDomainType ( orig.mBoundaryDomainType )
{
}


/*
 * Assignment operator for BoundaryCondition.
 */
BoundaryCondition&
BoundaryCondition::operator=(const BoundaryCondition& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mVariable = rhs.mVariable;
    mType = rhs.mType;
    mCoordinateBoundary = rhs.mCoordinateBoundary;
    mBoundaryDomainType = rhs.mBoundaryDomainType;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this BoundaryCondition object.
 */
BoundaryCondition*
BoundaryCondition::clone() const
{
  return new BoundaryCondition(*this);
}


/*
 * Destructor for BoundaryCondition.
 */
BoundaryCondition::~BoundaryCondition()
{
}


/*
 * Returns the value of the "variable" attribute of this BoundaryCondition.
 */
const std::string&
BoundaryCondition::getVariable() const
{
  return mVariable;
}


/*
 * Returns the value of the "type" attribute of this BoundaryCondition.
 */
BoundaryKind_t
BoundaryCondition::getType() const
{
  return mType;
}


/*
 * Returns the value of the "type" attribute of this BoundaryCondition.
 */
std::string
BoundaryCondition::getTypeAsString() const
{
  return BoundaryKind_toString(mType);
}


/*
 * Returns the value of the "coordinateBoundary" attribute of this
 * BoundaryCondition.
 */
const std::string&
BoundaryCondition::getCoordinateBoundary() const
{
  return mCoordinateBoundary;
}


/*
 * Returns the value of the "boundaryDomainType" attribute of this
 * BoundaryCondition.
 */
const std::string&
BoundaryCondition::getBoundaryDomainType() const
{
  return mBoundaryDomainType;
}


/*
 * Predicate returning @c true if this BoundaryCondition's "variable" attribute
 * is set.
 */
bool
BoundaryCondition::isSetVariable() const
{
  return (mVariable.empty() == false);
}


/*
 * Predicate returning @c true if this BoundaryCondition's "type" attribute is
 * set.
 */
bool
BoundaryCondition::isSetType() const
{
  return (mType != SPATIAL_BOUNDARYKIND_INVALID);
}


/*
 * Predicate returning @c true if this BoundaryCondition's "coordinateBoundary"
 * attribute is set.
 */
bool
BoundaryCondition::isSetCoordinateBoundary() const
{
  return (mCoordinateBoundary.empty() == false);
}


/*
 * Predicate returning @c true if this BoundaryCondition's "boundaryDomainType"
 * attribute is set.
 */
bool
BoundaryCondition::isSetBoundaryDomainType() const
{
  return (mBoundaryDomainType.empty() == false);
}


/*
 * Sets the value of the "variable" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::setVariable(const std::string& variable)
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


/*
 * Sets the value of the "type" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::setType(const BoundaryKind_t type)
{
  if (BoundaryKind_isValid(type) == 0)
  {
    mType = SPATIAL_BOUNDARYKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mType = type;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "type" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::setType(const std::string& type)
{
  if (BoundaryKind_isValidString(type.c_str()) == 0)
  {
    mType = SPATIAL_BOUNDARYKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mType = BoundaryKind_fromString(type.c_str());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "coordinateBoundary" attribute of this
 * BoundaryCondition.
 */
int
BoundaryCondition::setCoordinateBoundary(const std::string& coordinateBoundary)
{
  if (!(SyntaxChecker::isValidInternalSId(coordinateBoundary)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCoordinateBoundary = coordinateBoundary;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "boundaryDomainType" attribute of this
 * BoundaryCondition.
 */
int
BoundaryCondition::setBoundaryDomainType(const std::string& boundaryDomainType)
{
  if (!(SyntaxChecker::isValidInternalSId(boundaryDomainType)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mBoundaryDomainType = boundaryDomainType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "variable" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::unsetVariable()
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
 * Unsets the value of the "type" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::unsetType()
{
  mType = SPATIAL_BOUNDARYKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "coordinateBoundary" attribute of this
 * BoundaryCondition.
 */
int
BoundaryCondition::unsetCoordinateBoundary()
{
  mCoordinateBoundary.erase();

  if (mCoordinateBoundary.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "boundaryDomainType" attribute of this
 * BoundaryCondition.
 */
int
BoundaryCondition::unsetBoundaryDomainType()
{
  mBoundaryDomainType.erase();

  if (mBoundaryDomainType.empty() == true)
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
BoundaryCondition::renameSIdRefs(const std::string& oldid,
                                 const std::string& newid)
{
  if (isSetVariable() && mVariable == oldid)
  {
    setVariable(newid);
  }

  if (isSetCoordinateBoundary() && mCoordinateBoundary == oldid)
  {
    setCoordinateBoundary(newid);
  }

  if (isSetBoundaryDomainType() && mBoundaryDomainType == oldid)
  {
    setBoundaryDomainType(newid);
  }
}


/*
 * Returns the XML element name of this BoundaryCondition object.
 */
const std::string&
BoundaryCondition::getElementName() const
{
  static const string name = "boundaryCondition";
  return name;
}


/*
 * Returns the libSBML type code for this BoundaryCondition object.
 */
int
BoundaryCondition::getTypeCode() const
{
  return SBML_SPATIAL_BOUNDARYCONDITION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * BoundaryCondition object have been set.
 */
bool
BoundaryCondition::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetVariable() == false)
  {
    allPresent = false;
  }

  if (isSetType() == false)
  {
    allPresent = false;
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
BoundaryCondition::writeElements(XMLOutputStream& stream) const
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
BoundaryCondition::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
BoundaryCondition::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
BoundaryCondition::enablePackageInternal(const std::string& pkgURI,
                                         const std::string& pkgPrefix,
                                         bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::getAttribute(const std::string& attributeName,
                                bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::getAttribute(const std::string& attributeName,
                                int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::getAttribute(const std::string& attributeName,
                                double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::getAttribute(const std::string& attributeName,
                                unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::getAttribute(const std::string& attributeName,
                                std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "variable")
  {
    value = getVariable();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "type")
  {
    value = getTypeAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "coordinateBoundary")
  {
    value = getCoordinateBoundary();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "boundaryDomainType")
  {
    value = getBoundaryDomainType();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this BoundaryCondition's attribute
 * "attributeName" is set.
 */
bool
BoundaryCondition::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "variable")
  {
    value = isSetVariable();
  }
  else if (attributeName == "type")
  {
    value = isSetType();
  }
  else if (attributeName == "coordinateBoundary")
  {
    value = isSetCoordinateBoundary();
  }
  else if (attributeName == "boundaryDomainType")
  {
    value = isSetBoundaryDomainType();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::setAttribute(const std::string& attributeName,
                                double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::setAttribute(const std::string& attributeName,
                                unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::setAttribute(const std::string& attributeName,
                                const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "variable")
  {
    return_value = setVariable(value);
  }
  else if (attributeName == "type")
  {
    return_value = setType(value);
  }
  else if (attributeName == "coordinateBoundary")
  {
    return_value = setCoordinateBoundary(value);
  }
  else if (attributeName == "boundaryDomainType")
  {
    return_value = setBoundaryDomainType(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this BoundaryCondition.
 */
int
BoundaryCondition::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "variable")
  {
    value = unsetVariable();
  }
  else if (attributeName == "type")
  {
    value = unsetType();
  }
  else if (attributeName == "coordinateBoundary")
  {
    value = unsetCoordinateBoundary();
  }
  else if (attributeName == "boundaryDomainType")
  {
    value = unsetBoundaryDomainType();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
BoundaryCondition::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("variable");

  attributes.add("type");

  attributes.add("coordinateBoundary");

  attributes.add("boundaryDomainType");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
BoundaryCondition::readAttributes(const XMLAttributes& attributes,
                                  const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

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
        log->logPackageError("spatial",
          SpatialBoundaryConditionAllowedAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialBoundaryConditionAllowedCoreAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
      }
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
      logEmptyString(mVariable, level, version, "<BoundaryCondition>");
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
      log->logPackageError("spatial",
        SpatialBoundaryConditionVariableMustBeSpecies, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'variable' is missing from the "
      "<BoundaryCondition> element.";
    log->logPackageError("spatial", SpatialBoundaryConditionAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }

  // 
  // type enum (use = "required" )
  // 

  std::string type;
  assigned = attributes.readInto("type", type);

  if (assigned == true)
  {
    if (type.empty() == true)
    {
      logEmptyString(type, level, version, "<BoundaryCondition>");
    }
    else
    {
      mType = BoundaryKind_fromString(type.c_str());

      if (BoundaryKind_isValid(mType) == 0)
      {
        std::string msg = "The type on the <BoundaryCondition> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + type + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialBoundaryConditionTypeMustBeBoundaryKindEnum, pkgVersion, level,
            version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
    std::string message = "Spatial attribute 'type' is missing.";
    log->logPackageError("spatial", SpatialBoundaryConditionAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }

  // 
  // coordinateBoundary SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("coordinateBoundary", mCoordinateBoundary);

  if (assigned == true)
  {
    if (mCoordinateBoundary.empty() == true)
    {
      logEmptyString(mCoordinateBoundary, level, version,
        "<BoundaryCondition>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mCoordinateBoundary) == false)
    {
      std::string msg = "The coordinateBoundary attribute on the <" +
        getElementName() + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mCoordinateBoundary + "', which does not conform to the "
        "syntax.";
      log->logPackageError("spatial",
        SpatialBoundaryConditionCoordinateBoundaryMustBeBoundary, pkgVersion,
          level, version, msg, getLine(), getColumn());
    }
  }

  // 
  // boundaryDomainType SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("boundaryDomainType", mBoundaryDomainType);

  if (assigned == true)
  {
    if (mBoundaryDomainType.empty() == true)
    {
      logEmptyString(mBoundaryDomainType, level, version,
        "<BoundaryCondition>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mBoundaryDomainType) == false)
    {
      std::string msg = "The boundaryDomainType attribute on the <" +
        getElementName() + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mBoundaryDomainType + "', which does not conform to the "
        "syntax.";
      log->logPackageError("spatial",
        SpatialBoundaryConditionBoundaryDomainTypeMustBeDomainType, pkgVersion,
          level, version, msg, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
BoundaryCondition::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetVariable() == true)
  {
    stream.writeAttribute("variable", getPrefix(), mVariable);
  }

  if (isSetType() == true)
  {
    stream.writeAttribute("type", getPrefix(), BoundaryKind_toString(mType));
  }

  if (isSetCoordinateBoundary() == true)
  {
    stream.writeAttribute("coordinateBoundary", getPrefix(),
      mCoordinateBoundary);
  }

  if (isSetBoundaryDomainType() == true)
  {
    stream.writeAttribute("boundaryDomainType", getPrefix(),
      mBoundaryDomainType);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new BoundaryCondition_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
BoundaryCondition_t *
BoundaryCondition_create(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
{
  return new BoundaryCondition(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this BoundaryCondition_t object.
 */
LIBSBML_EXTERN
BoundaryCondition_t*
BoundaryCondition_clone(const BoundaryCondition_t* bc)
{
  if (bc != NULL)
  {
    return static_cast<BoundaryCondition_t*>(bc->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this BoundaryCondition_t object.
 */
LIBSBML_EXTERN
void
BoundaryCondition_free(BoundaryCondition_t* bc)
{
  if (bc != NULL)
  {
    delete bc;
  }
}


/*
 * Returns the value of the "variable" attribute of this BoundaryCondition_t.
 */
LIBSBML_EXTERN
char *
BoundaryCondition_getVariable(const BoundaryCondition_t * bc)
{
  if (bc == NULL)
  {
    return NULL;
  }

  return bc->getVariable().empty() ? NULL :
    safe_strdup(bc->getVariable().c_str());
}


/*
 * Returns the value of the "type" attribute of this BoundaryCondition_t.
 */
LIBSBML_EXTERN
BoundaryKind_t
BoundaryCondition_getType(const BoundaryCondition_t * bc)
{
  if (bc == NULL)
  {
    return SPATIAL_BOUNDARYKIND_INVALID;
  }

  return bc->getType();
}


/*
 * Returns the value of the "type" attribute of this BoundaryCondition_t.
 */
LIBSBML_EXTERN
char *
BoundaryCondition_getTypeAsString(const BoundaryCondition_t * bc)
{
  return (char*)(BoundaryKind_toString(bc->getType()));
}


/*
 * Returns the value of the "coordinateBoundary" attribute of this
 * BoundaryCondition_t.
 */
LIBSBML_EXTERN
char *
BoundaryCondition_getCoordinateBoundary(const BoundaryCondition_t * bc)
{
  if (bc == NULL)
  {
    return NULL;
  }

  return bc->getCoordinateBoundary().empty() ? NULL :
    safe_strdup(bc->getCoordinateBoundary().c_str());
}


/*
 * Returns the value of the "boundaryDomainType" attribute of this
 * BoundaryCondition_t.
 */
LIBSBML_EXTERN
char *
BoundaryCondition_getBoundaryDomainType(const BoundaryCondition_t * bc)
{
  if (bc == NULL)
  {
    return NULL;
  }

  return bc->getBoundaryDomainType().empty() ? NULL :
    safe_strdup(bc->getBoundaryDomainType().c_str());
}


/*
 * Predicate returning @c 1 (true) if this BoundaryCondition_t's "variable"
 * attribute is set.
 */
LIBSBML_EXTERN
int
BoundaryCondition_isSetVariable(const BoundaryCondition_t * bc)
{
  return (bc != NULL) ? static_cast<int>(bc->isSetVariable()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this BoundaryCondition_t's "type"
 * attribute is set.
 */
LIBSBML_EXTERN
int
BoundaryCondition_isSetType(const BoundaryCondition_t * bc)
{
  return (bc != NULL) ? static_cast<int>(bc->isSetType()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this BoundaryCondition_t's
 * "coordinateBoundary" attribute is set.
 */
LIBSBML_EXTERN
int
BoundaryCondition_isSetCoordinateBoundary(const BoundaryCondition_t * bc)
{
  return (bc != NULL) ? static_cast<int>(bc->isSetCoordinateBoundary()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this BoundaryCondition_t's
 * "boundaryDomainType" attribute is set.
 */
LIBSBML_EXTERN
int
BoundaryCondition_isSetBoundaryDomainType(const BoundaryCondition_t * bc)
{
  return (bc != NULL) ? static_cast<int>(bc->isSetBoundaryDomainType()) : 0;
}


/*
 * Sets the value of the "variable" attribute of this BoundaryCondition_t.
 */
LIBSBML_EXTERN
int
BoundaryCondition_setVariable(BoundaryCondition_t * bc, const char * variable)
{
  return (bc != NULL) ? bc->setVariable(variable) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "type" attribute of this BoundaryCondition_t.
 */
LIBSBML_EXTERN
int
BoundaryCondition_setType(BoundaryCondition_t * bc, BoundaryKind_t type)
{
  return (bc != NULL) ? bc->setType(type) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "type" attribute of this BoundaryCondition_t.
 */
LIBSBML_EXTERN
int
BoundaryCondition_setTypeAsString(BoundaryCondition_t * bc, const char * type)
{
  return (bc != NULL) ? bc->setType(type): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coordinateBoundary" attribute of this
 * BoundaryCondition_t.
 */
LIBSBML_EXTERN
int
BoundaryCondition_setCoordinateBoundary(BoundaryCondition_t * bc,
                                        const char * coordinateBoundary)
{
  return (bc != NULL) ? bc->setCoordinateBoundary(coordinateBoundary) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "boundaryDomainType" attribute of this
 * BoundaryCondition_t.
 */
LIBSBML_EXTERN
int
BoundaryCondition_setBoundaryDomainType(BoundaryCondition_t * bc,
                                        const char * boundaryDomainType)
{
  return (bc != NULL) ? bc->setBoundaryDomainType(boundaryDomainType) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "variable" attribute of this BoundaryCondition_t.
 */
LIBSBML_EXTERN
int
BoundaryCondition_unsetVariable(BoundaryCondition_t * bc)
{
  return (bc != NULL) ? bc->unsetVariable() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "type" attribute of this BoundaryCondition_t.
 */
LIBSBML_EXTERN
int
BoundaryCondition_unsetType(BoundaryCondition_t * bc)
{
  return (bc != NULL) ? bc->unsetType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "coordinateBoundary" attribute of this
 * BoundaryCondition_t.
 */
LIBSBML_EXTERN
int
BoundaryCondition_unsetCoordinateBoundary(BoundaryCondition_t * bc)
{
  return (bc != NULL) ? bc->unsetCoordinateBoundary() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "boundaryDomainType" attribute of this
 * BoundaryCondition_t.
 */
LIBSBML_EXTERN
int
BoundaryCondition_unsetBoundaryDomainType(BoundaryCondition_t * bc)
{
  return (bc != NULL) ? bc->unsetBoundaryDomainType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * BoundaryCondition_t object have been set.
 */
LIBSBML_EXTERN
int
BoundaryCondition_hasRequiredAttributes(const BoundaryCondition_t * bc)
{
  return (bc != NULL) ? static_cast<int>(bc->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


