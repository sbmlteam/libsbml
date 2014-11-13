/**
 * @file:   AdvectionCoefficient.cpp
 * @brief:  Implementation of the AdvectionCoefficient class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#include <sbml/packages/spatial/sbml/AdvectionCoefficient.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new AdvectionCoefficient with the given level, version, and package version.
 */
AdvectionCoefficient::AdvectionCoefficient (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mVariable ("")
  , mCoordinate (COORDINATEKIND_UNKNOWN)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new AdvectionCoefficient with the given SpatialPkgNamespaces object.
 */
AdvectionCoefficient::AdvectionCoefficient (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mVariable ("")
  , mCoordinate (COORDINATEKIND_UNKNOWN)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for AdvectionCoefficient.
 */
AdvectionCoefficient::AdvectionCoefficient (const AdvectionCoefficient& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mVariable  = orig.mVariable;
    mCoordinate  = orig.mCoordinate;
  }
}


/*
 * Assignment for AdvectionCoefficient.
 */
AdvectionCoefficient&
AdvectionCoefficient::operator=(const AdvectionCoefficient& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mVariable  = rhs.mVariable;
    mCoordinate  = rhs.mCoordinate;
  }
  return *this;
}


/*
 * Clone for AdvectionCoefficient.
 */
AdvectionCoefficient*
AdvectionCoefficient::clone () const
{
  return new AdvectionCoefficient(*this);
}


/*
 * Destructor for AdvectionCoefficient.
 */
AdvectionCoefficient::~AdvectionCoefficient ()
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
 * Returns the value of the "coordinate" attribute of this AdvectionCoefficient.
 */
CoordinateKind_t
AdvectionCoefficient::getCoordinate() const
{
  return mCoordinate;
}


/*
 * Returns true/false if variable is set.
 */
bool
AdvectionCoefficient::isSetVariable() const
{
  return (mVariable.empty() == false);
}


/*
 * Returns true/false if coordinate is set.
 */
bool
AdvectionCoefficient::isSetCoordinate() const
{
  return mCoordinate != COORDINATEKIND_UNKNOWN;
}


/*
 * Sets variable and returns value indicating success.
 */
int
AdvectionCoefficient::setVariable(const std::string& variable)
{
  if (&(variable) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(variable)))
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
 * Sets coordinate and returns value indicating success.
 */
int
AdvectionCoefficient::setCoordinate(CoordinateKind_t coordinate)
{
  mCoordinate = coordinate;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets coordinate and returns value indicating success.
 */
int
AdvectionCoefficient::setCoordinate(const std::string& coordinate)
{
  CoordinateKind_t parsed = CoordinateKind_parse(coordinate.c_str());
  if (parsed == COORDINATEKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mCoordinate = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets variable and returns value indicating success.
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
 * Unsets coordinate and returns value indicating success.
 */
int
AdvectionCoefficient::unsetCoordinate()
{
  mCoordinate = COORDINATEKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
AdvectionCoefficient::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetVariable() == true && mVariable == oldid)
  {
    setVariable(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
AdvectionCoefficient::getElementName () const
{
  static const string name = "advectionCoefficient";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
AdvectionCoefficient::getTypeCode () const
{
  return SBML_SPATIAL_ADVECTIONCOEFFICIENT;
}


/*
 * check if all the required attributes are set
 */
bool
AdvectionCoefficient::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetVariable() == false)
    allPresent = false;

  if (isSetCoordinate() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
AdvectionCoefficient::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
AdvectionCoefficient::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
AdvectionCoefficient::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
AdvectionCoefficient::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
AdvectionCoefficient::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("variable");
  attributes.add("coordinate");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
AdvectionCoefficient::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // variable SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("variable", mVariable);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mVariable.empty() == true)
    {
      logEmptyString(mVariable, getLevel(), getVersion(), "<AdvectionCoefficient>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVariable) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute variable='" + mVariable + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'variable' is missing from 'advectionCoefficient' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // coordinate enum  ( use = "required" )
  //
  mCoordinate = COORDINATEKIND_UNKNOWN;
  std::string stringValue;
  assigned = attributes.readInto("coordinate", stringValue);

  if (assigned == true)
  {
    // parse enum

    mCoordinate = CoordinateKind_parse(stringValue.c_str());
    if(mCoordinate == COORDINATEKIND_UNKNOWN) {
      std::string message = "Unknown value for spatial attribute 'coordinate' in 'advectionCoefficient' object: " + stringValue;
      getErrorLog()->logPackageError("spatial", SpatialUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  if(mCoordinate == COORDINATEKIND_UNKNOWN)
  {
    std::string message = "Spatial attribute 'coordinate' is missing from 'advectionCoefficient' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
AdvectionCoefficient::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetVariable() == true)
    stream.writeAttribute("variable", getPrefix(), mVariable);

  if (isSetCoordinate() == true)
    stream.writeAttribute("coordinate", getPrefix(), CoordinateKind_toString(mCoordinate));

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
AdvectionCoefficient_t *
AdvectionCoefficient_create(unsigned int level, unsigned int version,
                            unsigned int pkgVersion)
{
  return new AdvectionCoefficient(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
AdvectionCoefficient_free(AdvectionCoefficient_t * ac)
{
  if (ac != NULL)
    delete ac;
}


LIBSBML_EXTERN
AdvectionCoefficient_t *
AdvectionCoefficient_clone(AdvectionCoefficient_t * ac)
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


LIBSBML_EXTERN
const char *
AdvectionCoefficient_getVariable(const AdvectionCoefficient_t * ac)
{
	return (ac != NULL && ac->isSetVariable()) ? ac->getVariable().c_str() : NULL;
}


LIBSBML_EXTERN
CoordinateKind_t
AdvectionCoefficient_getCoordinate(const AdvectionCoefficient_t * ac)
{
	return (ac != NULL) ? ac->getCoordinate() : COORDINATEKIND_UNKNOWN;
}


LIBSBML_EXTERN
int
AdvectionCoefficient_isSetVariable(const AdvectionCoefficient_t * ac)
{
  return (ac != NULL) ? static_cast<int>(ac->isSetVariable()) : 0;
}


LIBSBML_EXTERN
int
AdvectionCoefficient_isSetCoordinate(const AdvectionCoefficient_t * ac)
{
  return (ac != NULL) ? static_cast<int>(ac->isSetCoordinate()) : 0;
}


LIBSBML_EXTERN
int
AdvectionCoefficient_setVariable(AdvectionCoefficient_t * ac, const char * variable)
{
  if (ac != NULL)
    return (variable == NULL) ? ac->setVariable("") : ac->setVariable(variable);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AdvectionCoefficient_setCoordinate(AdvectionCoefficient_t * ac, CoordinateKind_t coordinate)
{
  if (ac != NULL)
    return ac->setCoordinate(coordinate);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AdvectionCoefficient_unsetVariable(AdvectionCoefficient_t * ac)
{
  return (ac != NULL) ? ac->unsetVariable() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AdvectionCoefficient_unsetCoordinate(AdvectionCoefficient_t * ac)
{
  return (ac != NULL) ? ac->unsetCoordinate() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AdvectionCoefficient_hasRequiredAttributes(const AdvectionCoefficient_t * ac)
{
  return (ac != NULL) ? static_cast<int>(ac->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


