/**
 * @file CoordinateReference.cpp
 * @brief Implementation of the CoordinateReference class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
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
#include <sbml/packages/spatial/sbml/CoordinateReference.h>
#include <sbml/packages/spatial/sbml/ListOfCoordinateReferences.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new CoordinateReference using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
CoordinateReference::CoordinateReference(unsigned int level,
                                         unsigned int version,
                                         unsigned int pkgVersion)
  : SBase(level, version)
  , mCoordinate (COORDINATEKIND_UNKNOWN)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new CoordinateReference using the given SpatialPkgNamespaces
 * object.
 */
CoordinateReference::CoordinateReference(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mCoordinate (COORDINATEKIND_UNKNOWN)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CoordinateReference.
 */
CoordinateReference::CoordinateReference(const CoordinateReference& orig)
  : SBase( orig )
  , mCoordinate ( orig.mCoordinate )
{
}


/*
 * Assignment operator for CoordinateReference.
 */
CoordinateReference&
CoordinateReference::operator=(const CoordinateReference& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mCoordinate = rhs.mCoordinate;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this CoordinateReference object.
 */
CoordinateReference*
CoordinateReference::clone() const
{
  return new CoordinateReference(*this);
}


/*
 * Destructor for CoordinateReference.
 */
CoordinateReference::~CoordinateReference()
{
}


/*
 * Returns the value of the "coordinate" attribute of this CoordinateReference.
 */
CoordinateKind_t
CoordinateReference::getCoordinate() const
{
  return mCoordinate;
}


/*
 * Returns the value of the "coordinate" attribute of this CoordinateReference.
 */
const std::string&
CoordinateReference::getCoordinateAsString() const
{
  static const std::string code_str = CoordinateKind_toString(mCoordinate);
  return code_str;
}


/*
 * Predicate returning @c true if this CoordinateReference's "coordinate"
 * attribute is set.
 */
bool
CoordinateReference::isSetCoordinate() const
{
  return (mCoordinate != COORDINATEKIND_UNKNOWN);
}


/*
 * Sets the value of the "coordinate" attribute of this CoordinateReference.
 */
int
CoordinateReference::setCoordinate(const CoordinateKind_t coordinate)
{
  if (CoordinateKind_isValid(coordinate) == 0)
  {
    mCoordinate = COORDINATEKIND_UNKNOWN;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCoordinate = coordinate;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "coordinate" attribute of this CoordinateReference.
 */
int
CoordinateReference::setCoordinate(const std::string& coordinate)
{
  if (CoordinateKind_isValidString(coordinate.c_str()) == 0)
  {
    mCoordinate = COORDINATEKIND_UNKNOWN;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCoordinate = CoordinateKind_fromString(coordinate.c_str());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "coordinate" attribute of this CoordinateReference.
 */
int
CoordinateReference::unsetCoordinate()
{
  mCoordinate = COORDINATEKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this CoordinateReference object.
 */
const std::string&
CoordinateReference::getElementName() const
{
  static const string name = "coordinateReference";
  return name;
}


/*
 * Returns the libSBML type code for this CoordinateReference object.
 */
int
CoordinateReference::getTypeCode() const
{
  return SBML_SPATIAL_COORDINATEREFERENCE;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * CoordinateReference object have been set.
 */
bool
CoordinateReference::hasRequiredAttributes() const
{
  bool allPresent = true;

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
CoordinateReference::writeElements(XMLOutputStream& stream) const
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
CoordinateReference::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
CoordinateReference::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
CoordinateReference::enablePackageInternal(const std::string& pkgURI,
                                           const std::string& pkgPrefix,
                                           bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CoordinateReference.
 */
int
CoordinateReference::getAttribute(const std::string& attributeName,
                                  bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CoordinateReference.
 */
int
CoordinateReference::getAttribute(const std::string& attributeName,
                                  int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CoordinateReference.
 */
int
CoordinateReference::getAttribute(const std::string& attributeName,
                                  double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CoordinateReference.
 */
int
CoordinateReference::getAttribute(const std::string& attributeName,
                                  unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CoordinateReference.
 */
int
CoordinateReference::getAttribute(const std::string& attributeName,
                                  std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "coordinate")
  {
    value = getCoordinateAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CoordinateReference.
 */
int
CoordinateReference::getAttribute(const std::string& attributeName,
                                  const char* value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "coordinate")
  {
    value = getCoordinateAsString().c_str();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this CoordinateReference's attribute
 * "attributeName" is set.
 */
bool
CoordinateReference::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "coordinate")
  {
    value = isSetCoordinate();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CoordinateReference.
 */
int
CoordinateReference::setAttribute(const std::string& attributeName,
                                  bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CoordinateReference.
 */
int
CoordinateReference::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CoordinateReference.
 */
int
CoordinateReference::setAttribute(const std::string& attributeName,
                                  double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CoordinateReference.
 */
int
CoordinateReference::setAttribute(const std::string& attributeName,
                                  unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CoordinateReference.
 */
int
CoordinateReference::setAttribute(const std::string& attributeName,
                                  const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "coordinate")
  {
    return_value = setCoordinate(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CoordinateReference.
 */
int
CoordinateReference::setAttribute(const std::string& attributeName,
                                  const char* value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "coordinate")
  {
    return_value = setCoordinate(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * CoordinateReference.
 */
int
CoordinateReference::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "coordinate")
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
CoordinateReference::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("coordinate");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CoordinateReference::readAttributes(const XMLAttributes& attributes,
                                    const ExpectedAttributes&
                                      expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (static_cast<ListOfCoordinateReferences*>(getParentSBMLObject())->size() <
    2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial",
          SpatialCoordinateReferenceAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial", SpatialUnknown, pkgVersion, level,
          version, details);
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);
  numErrs = log->getNumErrors();

  for (int n = numErrs-1; n >= 0; n--)
  {
    if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
    {
      const std::string details = log->getError(n)->getMessage();
      log->remove(UnknownPackageAttribute);
      log->logPackageError("spatial",
        SpatialCoordinateReferenceAllowedAttributes, pkgVersion, level, version,
          details);
    }
    else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
    {
      const std::string details = log->getError(n)->getMessage();
      log->remove(UnknownCoreAttribute);
      log->logPackageError("spatial",
        SpatialCoordinateReferenceAllowedCoreAttributes, pkgVersion, level,
          version, details);
    }
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
      logEmptyString(coordinate, level, version, "<CoordinateReference>");
    }
    else
    {
      mCoordinate = CoordinateKind_fromString(coordinate.c_str());

      if (CoordinateKind_isValid(mCoordinate) == 0)
      {
        std::string msg = "The coordinate on the <CoordinateReference> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + coordinate + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialCoordinateReferenceCoordinateMustBeCoordinateKindEnum,
            pkgVersion, level, version, msg);
      }
    }
  }
  else
  {
    std::string message = "Spatial attribute 'coordinate' is missing.";
    log->logPackageError("spatial",
      SpatialCoordinateReferenceAllowedAttributes, pkgVersion, level, version,
        message);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
CoordinateReference::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

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
 * Creates a new CoordinateReference_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CoordinateReference_t *
CoordinateReference_create(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion)
{
  return new CoordinateReference(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CoordinateReference_t object.
 */
LIBSBML_EXTERN
CoordinateReference_t*
CoordinateReference_clone(const CoordinateReference_t* cr)
{
  if (cr != NULL)
  {
    return static_cast<CoordinateReference_t*>(cr->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this CoordinateReference_t object.
 */
LIBSBML_EXTERN
void
CoordinateReference_free(CoordinateReference_t* cr)
{
  if (cr != NULL)
  {
    delete cr;
  }
}


/*
 * Returns the value of the "coordinate" attribute of this
 * CoordinateReference_t.
 */
LIBSBML_EXTERN
CoordinateKind_t
CoordinateReference_getCoordinate(const CoordinateReference_t * cr)
{
  if (cr == NULL)
  {
    return COORDINATEKIND_UNKNOWN;
  }

  return cr->getCoordinate();
}


/*
 * Returns the value of the "coordinate" attribute of this
 * CoordinateReference_t.
 */
LIBSBML_EXTERN
const char *
CoordinateReference_getCoordinateAsString(const CoordinateReference_t * cr)
{
  return CoordinateKind_toString(cr->getCoordinate());
}


/*
 * Predicate returning @c 1 if this CoordinateReference_t's "coordinate"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CoordinateReference_isSetCoordinate(const CoordinateReference_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->isSetCoordinate()) : 0;
}


/*
 * Sets the value of the "coordinate" attribute of this CoordinateReference_t.
 */
LIBSBML_EXTERN
int
CoordinateReference_setCoordinate(CoordinateReference_t * cr,
                                  CoordinateKind_t coordinate)
{
  return (cr != NULL) ? cr->setCoordinate(coordinate) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coordinate" attribute of this CoordinateReference_t.
 */
LIBSBML_EXTERN
int
CoordinateReference_setCoordinateAsString(CoordinateReference_t * cr,
                                          const char * coordinate)
{
  return (cr != NULL) ? cr->setCoordinate(coordinate): LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "coordinate" attribute of this
 * CoordinateReference_t.
 */
LIBSBML_EXTERN
int
CoordinateReference_unsetCoordinate(CoordinateReference_t * cr)
{
  return (cr != NULL) ? cr->unsetCoordinate() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if all the required attributes for this
 * CoordinateReference_t object have been set.
 */
LIBSBML_EXTERN
int
CoordinateReference_hasRequiredAttributes(const CoordinateReference_t * cr)
{
  return (cr != NULL) ? static_cast<int>(cr->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


