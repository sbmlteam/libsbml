/**
 * @file DiffusionCoefficient.cpp
 * @brief Implementation of the DiffusionCoefficient class.
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
#include <sbml/packages/spatial/sbml/DiffusionCoefficient.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DiffusionCoefficient using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
DiffusionCoefficient::DiffusionCoefficient(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion)
  : SBase(level, version)
  , mVariable ("")
  , mType (SPATIAL_DIFFUSIONKIND_INVALID)
  , mCoordinateReference1 (SPATIAL_COORDINATEKIND_INVALID)
  , mCoordinateReference2 (SPATIAL_COORDINATEKIND_INVALID)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new DiffusionCoefficient using the given SpatialPkgNamespaces
 * object.
 */
DiffusionCoefficient::DiffusionCoefficient(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mVariable ("")
  , mType (SPATIAL_DIFFUSIONKIND_INVALID)
  , mCoordinateReference1 (SPATIAL_COORDINATEKIND_INVALID)
  , mCoordinateReference2 (SPATIAL_COORDINATEKIND_INVALID)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for DiffusionCoefficient.
 */
DiffusionCoefficient::DiffusionCoefficient(const DiffusionCoefficient& orig)
  : SBase( orig )
  , mVariable ( orig.mVariable )
  , mType ( orig.mType )
  , mCoordinateReference1 ( orig.mCoordinateReference1 )
  , mCoordinateReference2 ( orig.mCoordinateReference2 )
{
}


/*
 * Assignment operator for DiffusionCoefficient.
 */
DiffusionCoefficient&
DiffusionCoefficient::operator=(const DiffusionCoefficient& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mVariable = rhs.mVariable;
    mType = rhs.mType;
    mCoordinateReference1 = rhs.mCoordinateReference1;
    mCoordinateReference2 = rhs.mCoordinateReference2;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DiffusionCoefficient object.
 */
DiffusionCoefficient*
DiffusionCoefficient::clone() const
{
  return new DiffusionCoefficient(*this);
}


/*
 * Destructor for DiffusionCoefficient.
 */
DiffusionCoefficient::~DiffusionCoefficient()
{
}


/*
 * Returns the value of the "variable" attribute of this DiffusionCoefficient.
 */
const std::string&
DiffusionCoefficient::getVariable() const
{
  return mVariable;
}


/*
 * Returns the value of the "type" attribute of this DiffusionCoefficient.
 */
DiffusionKind_t
DiffusionCoefficient::getType() const
{
  return mType;
}


/*
 * Returns the value of the "type" attribute of this DiffusionCoefficient.
 */
std::string
DiffusionCoefficient::getTypeAsString() const
{
  return DiffusionKind_toString(mType);
}


/*
 * Returns the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient.
 */
CoordinateKind_t
DiffusionCoefficient::getCoordinateReference1() const
{
  return mCoordinateReference1;
}


/*
 * Returns the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient.
 */
const std::string&
DiffusionCoefficient::getCoordinateReference1AsString() const
{
  static const std::string code_str =
    CoordinateKind_toString(mCoordinateReference1);
  return code_str;
}


/*
 * Returns the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient.
 */
CoordinateKind_t
DiffusionCoefficient::getCoordinateReference2() const
{
  return mCoordinateReference2;
}


/*
 * Returns the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient.
 */
std::string
DiffusionCoefficient::getCoordinateReference2AsString() const
{
  std::string code_str = CoordinateKind_toString(mCoordinateReference2);
  return code_str;
}


/*
 * Predicate returning @c true if this DiffusionCoefficient's "variable"
 * attribute is set.
 */
bool
DiffusionCoefficient::isSetVariable() const
{
  return (mVariable.empty() == false);
}


/*
 * Predicate returning @c true if this DiffusionCoefficient's "type" attribute
 * is set.
 */
bool
DiffusionCoefficient::isSetType() const
{
  return (mType != SPATIAL_DIFFUSIONKIND_INVALID);
}


/*
 * Predicate returning @c true if this DiffusionCoefficient's
 * "coordinateReference1" attribute is set.
 */
bool
DiffusionCoefficient::isSetCoordinateReference1() const
{
  return (mCoordinateReference1 != SPATIAL_COORDINATEKIND_INVALID);
}


/*
 * Predicate returning @c true if this DiffusionCoefficient's
 * "coordinateReference2" attribute is set.
 */
bool
DiffusionCoefficient::isSetCoordinateReference2() const
{
  return (mCoordinateReference2 != SPATIAL_COORDINATEKIND_INVALID);
}


/*
 * Sets the value of the "variable" attribute of this DiffusionCoefficient.
 */
int
DiffusionCoefficient::setVariable(const std::string& variable)
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
 * Sets the value of the "type" attribute of this DiffusionCoefficient.
 */
int
DiffusionCoefficient::setType(const DiffusionKind_t type)
{
  if (DiffusionKind_isValid(type) == 0)
  {
    mType = SPATIAL_DIFFUSIONKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mType = type;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "type" attribute of this DiffusionCoefficient.
 */
int
DiffusionCoefficient::setType(const std::string& type)
{
  mType = DiffusionKind_fromString(type.c_str());

  if (mType == SPATIAL_DIFFUSIONKIND_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::setCoordinateReference1(const CoordinateKind_t
  coordinateReference1)
{
  if (CoordinateKind_isValid(coordinateReference1) == 0)
  {
    mCoordinateReference1 = SPATIAL_COORDINATEKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCoordinateReference1 = coordinateReference1;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::setCoordinateReference1(const std::string&
  coordinateReference1)
{
  if (CoordinateKind_isValidString(coordinateReference1.c_str()) == 0)
  {
    mCoordinateReference1 = SPATIAL_COORDINATEKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCoordinateReference1 =
      CoordinateKind_fromString(coordinateReference1.c_str());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::setCoordinateReference2(const CoordinateKind_t
  coordinateReference2)
{
  if (CoordinateKind_isValid(coordinateReference2) == 0)
  {
    mCoordinateReference2 = SPATIAL_COORDINATEKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCoordinateReference2 = coordinateReference2;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::setCoordinateReference2(const std::string&
  coordinateReference2)
{
  mCoordinateReference2 =
    CoordinateKind_fromString(coordinateReference2.c_str());

  if (mCoordinateReference2 == SPATIAL_COORDINATEKIND_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "variable" attribute of this DiffusionCoefficient.
 */
int
DiffusionCoefficient::unsetVariable()
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
 * Unsets the value of the "type" attribute of this DiffusionCoefficient.
 */
int
DiffusionCoefficient::unsetType()
{
  mType = SPATIAL_DIFFUSIONKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::unsetCoordinateReference1()
{
  mCoordinateReference1 = SPATIAL_COORDINATEKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::unsetCoordinateReference2()
{
  mCoordinateReference2 = SPATIAL_COORDINATEKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * @copydoc doc_renamesidref_common
 */
void
DiffusionCoefficient::renameSIdRefs(const std::string& oldid,
                                    const std::string& newid)
{
  if (isSetVariable() && mVariable == oldid)
  {
    setVariable(newid);
  }
}


/*
 * Returns the XML element name of this DiffusionCoefficient object.
 */
const std::string&
DiffusionCoefficient::getElementName() const
{
  static const string name = "diffusionCoefficient";
  return name;
}


/*
 * Returns the libSBML type code for this DiffusionCoefficient object.
 */
int
DiffusionCoefficient::getTypeCode() const
{
  return SBML_SPATIAL_DIFFUSIONCOEFFICIENT;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DiffusionCoefficient object have been set.
 */
bool
DiffusionCoefficient::hasRequiredAttributes() const
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
DiffusionCoefficient::writeElements(XMLOutputStream& stream) const
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
DiffusionCoefficient::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DiffusionCoefficient::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DiffusionCoefficient::enablePackageInternal(const std::string& pkgURI,
                                            const std::string& pkgPrefix,
                                            bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::getAttribute(const std::string& attributeName,
                                   bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::getAttribute(const std::string& attributeName,
                                   int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::getAttribute(const std::string& attributeName,
                                   double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::getAttribute(const std::string& attributeName,
                                   unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::getAttribute(const std::string& attributeName,
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
  else if (attributeName == "coordinateReference1")
  {
    value = getCoordinateReference1AsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "coordinateReference2")
  {
    value = getCoordinateReference2AsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DiffusionCoefficient's attribute
 * "attributeName" is set.
 */
bool
DiffusionCoefficient::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "coordinateReference1")
  {
    value = isSetCoordinateReference1();
  }
  else if (attributeName == "coordinateReference2")
  {
    value = isSetCoordinateReference2();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::setAttribute(const std::string& attributeName,
                                   bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::setAttribute(const std::string& attributeName,
                                   int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::setAttribute(const std::string& attributeName,
                                   double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::setAttribute(const std::string& attributeName,
                                   unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::setAttribute(const std::string& attributeName,
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
  else if (attributeName == "coordinateReference1")
  {
    return_value = setCoordinateReference1(value);
  }
  else if (attributeName == "coordinateReference2")
  {
    return_value = setCoordinateReference2(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DiffusionCoefficient.
 */
int
DiffusionCoefficient::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "coordinateReference1")
  {
    value = unsetCoordinateReference1();
  }
  else if (attributeName == "coordinateReference2")
  {
    value = unsetCoordinateReference2();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DiffusionCoefficient::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("variable");

  attributes.add("type");

  attributes.add("coordinateReference1");

  attributes.add("coordinateReference2");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DiffusionCoefficient::readAttributes(const XMLAttributes& attributes,
                                     const ExpectedAttributes&
                                       expectedAttributes)
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
          SpatialDiffusionCoefficientAllowedAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialDiffusionCoefficientAllowedCoreAttributes, pkgVersion, level,
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
      logEmptyString(mVariable, level, version, "<DiffusionCoefficient>");
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
        SpatialDiffusionCoefficientVariableMustBeSpeciesOrParam, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'variable' is missing from the "
      "<DiffusionCoefficient> element.";
    log->logPackageError("spatial",
      SpatialDiffusionCoefficientAllowedAttributes, pkgVersion, level, version,
        message, getLine(), getColumn());
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
      logEmptyString(type, level, version, "<DiffusionCoefficient>");
    }
    else
    {
      mType = DiffusionKind_fromString(type.c_str());

      if (DiffusionKind_isValid(mType) == 0)
      {
        std::string msg = "The type on the <DiffusionCoefficient> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + type + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialDiffusionCoefficientTypeMustBeDiffusionKindEnum, pkgVersion,
            level, version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
    std::string message = "Spatial attribute 'type' is missing.";
    log->logPackageError("spatial",
      SpatialDiffusionCoefficientAllowedAttributes, pkgVersion, level, version,
        message, getLine(), getColumn());
  }

  // 
  // coordinateReference1 enum (use = "optional" )
  // 

  std::string coordinateReference1;
  assigned = attributes.readInto("coordinateReference1", coordinateReference1);

  if (assigned == true)
  {
    if (coordinateReference1.empty() == true)
    {
      logEmptyString(coordinateReference1, level, version,
        "<DiffusionCoefficient>");
    }
    else
    {
      mCoordinateReference1 =
        CoordinateKind_fromString(coordinateReference1.c_str());

      if (CoordinateKind_isValid(mCoordinateReference1) == 0)
      {
        std::string msg = "The coordinateReference1 on the "
          "<DiffusionCoefficient> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + coordinateReference1 + "', which is not a valid "
          "option.";

        log->logPackageError("spatial", SpatialDiffusionCoefficientCoordinateReference1MustBeCoordinateKindEnum,
          pkgVersion, level, version, msg, getLine(), getColumn());
      }
    }
  }

  // 
  // coordinateReference2 enum (use = "optional" )
  // 

  std::string coordinateReference2;
  assigned = attributes.readInto("coordinateReference2", coordinateReference2);

  if (assigned == true)
  {
    if (coordinateReference2.empty() == true)
    {
      logEmptyString(coordinateReference2, level, version,
        "<DiffusionCoefficient>");
    }
    else
    {
      mCoordinateReference2 =
        CoordinateKind_fromString(coordinateReference2.c_str());

      if (CoordinateKind_isValid(mCoordinateReference2) == 0)
      {
        std::string msg = "The coordinateReference2 on the "
          "<DiffusionCoefficient> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + coordinateReference2 + "', which is not a valid "
          "option.";

        log->logPackageError("spatial", SpatialDiffusionCoefficientCoordinateReference2MustBeCoordinateKindEnum,
          pkgVersion, level, version, msg, getLine(), getColumn());
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
DiffusionCoefficient::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetVariable() == true)
  {
    stream.writeAttribute("variable", getPrefix(), mVariable);
  }

  if (isSetType() == true)
  {
    stream.writeAttribute("type", getPrefix(), DiffusionKind_toString(mType));
  }

  if (isSetCoordinateReference1() == true)
  {
    stream.writeAttribute("coordinateReference1", getPrefix(),
      CoordinateKind_toString(mCoordinateReference1));
  }

  if (isSetCoordinateReference2() == true)
  {
    stream.writeAttribute("coordinateReference2", getPrefix(),
      CoordinateKind_toString(mCoordinateReference2));
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DiffusionCoefficient_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
DiffusionCoefficient_t *
DiffusionCoefficient_create(unsigned int level,
                            unsigned int version,
                            unsigned int pkgVersion)
{
  return new DiffusionCoefficient(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DiffusionCoefficient_t object.
 */
LIBSBML_EXTERN
DiffusionCoefficient_t*
DiffusionCoefficient_clone(const DiffusionCoefficient_t* dc)
{
  if (dc != NULL)
  {
    return static_cast<DiffusionCoefficient_t*>(dc->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DiffusionCoefficient_t object.
 */
LIBSBML_EXTERN
void
DiffusionCoefficient_free(DiffusionCoefficient_t* dc)
{
  if (dc != NULL)
  {
    delete dc;
  }
}


/*
 * Returns the value of the "variable" attribute of this
 * DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
char *
DiffusionCoefficient_getVariable(const DiffusionCoefficient_t * dc)
{
  if (dc == NULL)
  {
    return NULL;
  }

  return dc->getVariable().empty() ? NULL :
    safe_strdup(dc->getVariable().c_str());
}


/*
 * Returns the value of the "type" attribute of this DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
DiffusionKind_t
DiffusionCoefficient_getType(const DiffusionCoefficient_t * dc)
{
  if (dc == NULL)
  {
    return SPATIAL_DIFFUSIONKIND_INVALID;
  }

  return dc->getType();
}


/*
 * Returns the value of the "type" attribute of this DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
char *
DiffusionCoefficient_getTypeAsString(const DiffusionCoefficient_t * dc)
{
  return (char*)(DiffusionKind_toString(dc->getType()));
}


/*
 * Returns the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
CoordinateKind_t
DiffusionCoefficient_getCoordinateReference1(const DiffusionCoefficient_t * dc)
{
  if (dc == NULL)
  {
    return SPATIAL_COORDINATEKIND_INVALID;
  }

  return dc->getCoordinateReference1();
}


/*
 * Returns the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
char *
DiffusionCoefficient_getCoordinateReference1AsString(const
  DiffusionCoefficient_t * dc)
{
  return (char*)(CoordinateKind_toString(dc->getCoordinateReference1()));
}


/*
 * Returns the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
CoordinateKind_t
DiffusionCoefficient_getCoordinateReference2(const DiffusionCoefficient_t * dc)
{
  if (dc == NULL)
  {
    return SPATIAL_COORDINATEKIND_INVALID;
  }

  return dc->getCoordinateReference2();
}


/*
 * Returns the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
char *
DiffusionCoefficient_getCoordinateReference2AsString(const
  DiffusionCoefficient_t * dc)
{
  return (char*)(CoordinateKind_toString(dc->getCoordinateReference2()));
}


/*
 * Predicate returning @c 1 (true) if this DiffusionCoefficient_t's "variable"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_isSetVariable(const DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetVariable()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DiffusionCoefficient_t's "type"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_isSetType(const DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetType()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DiffusionCoefficient_t's
 * "coordinateReference1" attribute is set.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_isSetCoordinateReference1(const DiffusionCoefficient_t *
  dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetCoordinateReference1()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DiffusionCoefficient_t's
 * "coordinateReference2" attribute is set.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_isSetCoordinateReference2(const DiffusionCoefficient_t *
  dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetCoordinateReference2()) : 0;
}


/*
 * Sets the value of the "variable" attribute of this DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setVariable(DiffusionCoefficient_t * dc,
                                 const char * variable)
{
  return (dc != NULL) ? dc->setVariable(variable) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "type" attribute of this DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setType(DiffusionCoefficient_t * dc,
                             DiffusionKind_t type)
{
  return (dc != NULL) ? dc->setType(type) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "type" attribute of this DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setTypeAsString(DiffusionCoefficient_t * dc,
                                     const char * type)
{
  return (dc != NULL) ? dc->setType(type): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setCoordinateReference1(DiffusionCoefficient_t * dc,
                                             CoordinateKind_t
                                               coordinateReference1)
{
  return (dc != NULL) ? dc->setCoordinateReference1(coordinateReference1) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setCoordinateReference1AsString(
                                                     DiffusionCoefficient_t *
                                                       dc,
                                                     const char *
                                                       coordinateReference1)
{
  return (dc != NULL) ? dc->setCoordinateReference1(coordinateReference1):
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setCoordinateReference2(DiffusionCoefficient_t * dc,
                                             CoordinateKind_t
                                               coordinateReference2)
{
  return (dc != NULL) ? dc->setCoordinateReference2(coordinateReference2) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setCoordinateReference2AsString(
                                                     DiffusionCoefficient_t *
                                                       dc,
                                                     const char *
                                                       coordinateReference2)
{
  return (dc != NULL) ? dc->setCoordinateReference2(coordinateReference2):
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "variable" attribute of this DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_unsetVariable(DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? dc->unsetVariable() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "type" attribute of this DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_unsetType(DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? dc->unsetType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_unsetCoordinateReference1(DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? dc->unsetCoordinateReference1() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient_t.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_unsetCoordinateReference2(DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? dc->unsetCoordinateReference2() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DiffusionCoefficient_t object have been set.
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_hasRequiredAttributes(const DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


