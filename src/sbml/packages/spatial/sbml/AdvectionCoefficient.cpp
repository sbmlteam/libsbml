/**
 * @file AdvectionCoefficient.cpp
 * @brief Implementation of the AdvectionCoefficient class.
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
#include <sbml/packages/spatial/sbml/AdvectionCoefficient.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new AdvectionCoefficient using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
AdvectionCoefficient::AdvectionCoefficient(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion)
  : SBase(level, version)
  , mVariable ("")
  , mCoordinate (SPATIAL_COORDINATEKIND_INVALID)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new AdvectionCoefficient using the given SpatialPkgNamespaces
 * object.
 */
AdvectionCoefficient::AdvectionCoefficient(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mVariable ("")
  , mCoordinate (SPATIAL_COORDINATEKIND_INVALID)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for AdvectionCoefficient.
 */
AdvectionCoefficient::AdvectionCoefficient(const AdvectionCoefficient& orig)
  : SBase( orig )
  , mVariable ( orig.mVariable )
  , mCoordinate ( orig.mCoordinate )
{
}


/*
 * Assignment operator for AdvectionCoefficient.
 */
AdvectionCoefficient&
AdvectionCoefficient::operator=(const AdvectionCoefficient& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mVariable = rhs.mVariable;
    mCoordinate = rhs.mCoordinate;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this AdvectionCoefficient object.
 */
AdvectionCoefficient*
AdvectionCoefficient::clone() const
{
  return new AdvectionCoefficient(*this);
}


/*
 * Destructor for AdvectionCoefficient.
 */
AdvectionCoefficient::~AdvectionCoefficient()
{
}


/*
 * Returns the value of the "variable" attribute of this AdvectionCoefficient.
 */
const std::string&
AdvectionCoefficient::getVariable() const
{
  return mVariable;
}


/*
 * Returns the value of the "coordinate" attribute of this
 * AdvectionCoefficient.
 */
CoordinateKind_t
AdvectionCoefficient::getCoordinate() const
{
  return mCoordinate;
}


/*
 * Returns the value of the "coordinate" attribute of this
 * AdvectionCoefficient.
 */
const std::string&
AdvectionCoefficient::getCoordinateAsString() const
{
  static const std::string code_str = CoordinateKind_toString(mCoordinate);
  return code_str;
}


/*
 * Predicate returning @c true if this AdvectionCoefficient's "variable"
 * attribute is set.
 */
bool
AdvectionCoefficient::isSetVariable() const
{
  return (mVariable.empty() == false);
}


/*
 * Predicate returning @c true if this AdvectionCoefficient's "coordinate"
 * attribute is set.
 */
bool
AdvectionCoefficient::isSetCoordinate() const
{
  return (mCoordinate != SPATIAL_COORDINATEKIND_INVALID);
}


/*
 * Sets the value of the "variable" attribute of this AdvectionCoefficient.
 */
int
AdvectionCoefficient::setVariable(const std::string& variable)
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
 * Sets the value of the "coordinate" attribute of this AdvectionCoefficient.
 */
int
AdvectionCoefficient::setCoordinate(const CoordinateKind_t coordinate)
{
  if (CoordinateKind_isValid(coordinate) == 0)
  {
    mCoordinate = SPATIAL_COORDINATEKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCoordinate = coordinate;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "coordinate" attribute of this AdvectionCoefficient.
 */
int
AdvectionCoefficient::setCoordinate(const std::string& coordinate)
{
  if (CoordinateKind_isValidString(coordinate.c_str()) == 0)
  {
    mCoordinate = SPATIAL_COORDINATEKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCoordinate = CoordinateKind_fromString(coordinate.c_str());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "variable" attribute of this AdvectionCoefficient.
 */
int
AdvectionCoefficient::unsetVariable()
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
 * Unsets the value of the "coordinate" attribute of this AdvectionCoefficient.
 */
int
AdvectionCoefficient::unsetCoordinate()
{
  mCoordinate = SPATIAL_COORDINATEKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * @copydoc doc_renamesidref_common
 */
void
AdvectionCoefficient::renameSIdRefs(const std::string& oldid,
                                    const std::string& newid)
{
  if (isSetVariable() && mVariable == oldid)
  {
    setVariable(newid);
  }
}


/*
 * Returns the XML element name of this AdvectionCoefficient object.
 */
const std::string&
AdvectionCoefficient::getElementName() const
{
  static const string name = "advectionCoefficient";
  return name;
}


/*
 * Returns the libSBML type code for this AdvectionCoefficient object.
 */
int
AdvectionCoefficient::getTypeCode() const
{
  return SBML_SPATIAL_ADVECTIONCOEFFICIENT;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * AdvectionCoefficient object have been set.
 */
bool
AdvectionCoefficient::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetVariable() == false)
  {
    allPresent = false;
  }

  if (isSetCoordinate() == false)
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
AdvectionCoefficient::writeElements(XMLOutputStream& stream) const
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
AdvectionCoefficient::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
AdvectionCoefficient::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
AdvectionCoefficient::enablePackageInternal(const std::string& pkgURI,
                                            const std::string& pkgPrefix,
                                            bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * AdvectionCoefficient.
 */
int
AdvectionCoefficient::getAttribute(const std::string& attributeName,
                                   bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * AdvectionCoefficient.
 */
int
AdvectionCoefficient::getAttribute(const std::string& attributeName,
                                   int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * AdvectionCoefficient.
 */
int
AdvectionCoefficient::getAttribute(const std::string& attributeName,
                                   double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * AdvectionCoefficient.
 */
int
AdvectionCoefficient::getAttribute(const std::string& attributeName,
                                   unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * AdvectionCoefficient.
 */
int
AdvectionCoefficient::getAttribute(const std::string& attributeName,
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
  else if (attributeName == "coordinate")
  {
    value = getCoordinateAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this AdvectionCoefficient's attribute
 * "attributeName" is set.
 */
bool
AdvectionCoefficient::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "variable")
  {
    value = isSetVariable();
  }
  else if (attributeName == "coordinate")
  {
    value = isSetCoordinate();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * AdvectionCoefficient.
 */
int
AdvectionCoefficient::setAttribute(const std::string& attributeName,
                                   bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * AdvectionCoefficient.
 */
int
AdvectionCoefficient::setAttribute(const std::string& attributeName,
                                   int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * AdvectionCoefficient.
 */
int
AdvectionCoefficient::setAttribute(const std::string& attributeName,
                                   double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * AdvectionCoefficient.
 */
int
AdvectionCoefficient::setAttribute(const std::string& attributeName,
                                   unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * AdvectionCoefficient.
 */
int
AdvectionCoefficient::setAttribute(const std::string& attributeName,
                                   const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "variable")
  {
    return_value = setVariable(value);
  }
  else if (attributeName == "coordinate")
  {
    return_value = setCoordinate(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * AdvectionCoefficient.
 */
int
AdvectionCoefficient::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "variable")
  {
    value = unsetVariable();
  }
  else if (attributeName == "coordinate")
  {
    value = unsetCoordinate();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
AdvectionCoefficient::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("variable");

  attributes.add("coordinate");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
AdvectionCoefficient::readAttributes(const XMLAttributes& attributes,
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
          SpatialAdvectionCoefficientAllowedAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialAdvectionCoefficientAllowedCoreAttributes, pkgVersion, level,
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
      logEmptyString(mVariable, level, version, "<advectionCoefficient>");
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
        SpatialAdvectionCoefficientVariableMustBeSpeciesOrParam, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'variable' is missing from the "
      "<advectionCoefficient> element.";
    log->logPackageError("spatial",
      SpatialAdvectionCoefficientAllowedAttributes, pkgVersion, level, version,
        message, getLine(), getColumn());
  }

  // 
  // coordinate enum (use = "required" )
  // 

  std::string coordinate;
  assigned = attributes.readInto("coordinate", coordinate);

  if (assigned == true)
  {
    if (coordinate.empty() == true)
    {
      logEmptyString(coordinate, level, version, "<advectionCoefficient>");
    }
    else
    {
      mCoordinate = CoordinateKind_fromString(coordinate.c_str());

      if (CoordinateKind_isValid(mCoordinate) == 0)
      {
        std::string msg = "The coordinate on the <AdvectionCoefficient> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + coordinate + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialAdvectionCoefficientCoordinateMustBeCoordinateKindEnum,
            pkgVersion, level, version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
    std::string message = "Spatial attribute 'coordinate' is missing.";
    log->logPackageError("spatial",
      SpatialAdvectionCoefficientAllowedAttributes, pkgVersion, level, version,
        message, getLine(), getColumn());
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
AdvectionCoefficient::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetVariable() == true)
  {
    stream.writeAttribute("variable", getPrefix(), mVariable);
  }

  if (isSetCoordinate() == true)
  {
    stream.writeAttribute("coordinate", getPrefix(),
      CoordinateKind_toString(mCoordinate));
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new AdvectionCoefficient_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
AdvectionCoefficient_t *
AdvectionCoefficient_create(unsigned int level,
                            unsigned int version,
                            unsigned int pkgVersion)
{
  return new AdvectionCoefficient(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this AdvectionCoefficient_t object.
 */
LIBSBML_EXTERN
AdvectionCoefficient_t*
AdvectionCoefficient_clone(const AdvectionCoefficient_t* ac)
{
  if (ac != NULL)
  {
    return static_cast<AdvectionCoefficient_t*>(ac->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this AdvectionCoefficient_t object.
 */
LIBSBML_EXTERN
void
AdvectionCoefficient_free(AdvectionCoefficient_t* ac)
{
  if (ac != NULL)
  {
    delete ac;
  }
}


/*
 * Returns the value of the "variable" attribute of this
 * AdvectionCoefficient_t.
 */
LIBSBML_EXTERN
char *
AdvectionCoefficient_getVariable(const AdvectionCoefficient_t * ac)
{
  if (ac == NULL)
  {
    return NULL;
  }

  return ac->getVariable().empty() ? NULL :
    safe_strdup(ac->getVariable().c_str());
}


/*
 * Returns the value of the "coordinate" attribute of this
 * AdvectionCoefficient_t.
 */
LIBSBML_EXTERN
CoordinateKind_t
AdvectionCoefficient_getCoordinate(const AdvectionCoefficient_t * ac)
{
  if (ac == NULL)
  {
    return SPATIAL_COORDINATEKIND_INVALID;
  }

  return ac->getCoordinate();
}


/*
 * Returns the value of the "coordinate" attribute of this
 * AdvectionCoefficient_t.
 */
LIBSBML_EXTERN
char *
AdvectionCoefficient_getCoordinateAsString(const AdvectionCoefficient_t * ac)
{
  return (char*)(CoordinateKind_toString(ac->getCoordinate()));
}


/*
 * Predicate returning @c 1 (true) if this AdvectionCoefficient_t's "variable"
 * attribute is set.
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_isSetVariable(const AdvectionCoefficient_t * ac)
{
  return (ac != NULL) ? static_cast<int>(ac->isSetVariable()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this AdvectionCoefficient_t's
 * "coordinate" attribute is set.
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_isSetCoordinate(const AdvectionCoefficient_t * ac)
{
  return (ac != NULL) ? static_cast<int>(ac->isSetCoordinate()) : 0;
}


/*
 * Sets the value of the "variable" attribute of this AdvectionCoefficient_t.
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_setVariable(AdvectionCoefficient_t * ac,
                                 const char * variable)
{
  return (ac != NULL) ? ac->setVariable(variable) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coordinate" attribute of this AdvectionCoefficient_t.
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_setCoordinate(AdvectionCoefficient_t * ac,
                                   CoordinateKind_t coordinate)
{
  return (ac != NULL) ? ac->setCoordinate(coordinate) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coordinate" attribute of this AdvectionCoefficient_t.
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_setCoordinateAsString(AdvectionCoefficient_t * ac,
                                           const char * coordinate)
{
  return (ac != NULL) ? ac->setCoordinate(coordinate): LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "variable" attribute of this AdvectionCoefficient_t.
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_unsetVariable(AdvectionCoefficient_t * ac)
{
  return (ac != NULL) ? ac->unsetVariable() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "coordinate" attribute of this
 * AdvectionCoefficient_t.
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_unsetCoordinate(AdvectionCoefficient_t * ac)
{
  return (ac != NULL) ? ac->unsetCoordinate() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * AdvectionCoefficient_t object have been set.
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_hasRequiredAttributes(const AdvectionCoefficient_t * ac)
{
  return (ac != NULL) ? static_cast<int>(ac->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


