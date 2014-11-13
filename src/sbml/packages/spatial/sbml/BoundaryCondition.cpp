/**
 * @file:   BoundaryCondition.cpp
 * @brief:  Implementation of the BoundaryCondition class
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


#include <sbml/packages/spatial/sbml/BoundaryCondition.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new BoundaryCondition with the given level, version, and package version.
 */
BoundaryCondition::BoundaryCondition (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mVariable ("")
  , mType (BOUNDARYCONDITIONKIND_UNKNOWN)
  , mCoordinateBoundary ("")
  , mBoundaryDomainType ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new BoundaryCondition with the given SpatialPkgNamespaces object.
 */
BoundaryCondition::BoundaryCondition (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mVariable ("")
  , mType (BOUNDARYCONDITIONKIND_UNKNOWN)
  , mCoordinateBoundary ("")
  , mBoundaryDomainType ("")
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for BoundaryCondition.
 */
BoundaryCondition::BoundaryCondition (const BoundaryCondition& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mVariable  = orig.mVariable;
    mType  = orig.mType;
    mCoordinateBoundary  = orig.mCoordinateBoundary;
    mBoundaryDomainType  = orig.mBoundaryDomainType;
  }
}


/*
 * Assignment for BoundaryCondition.
 */
BoundaryCondition&
BoundaryCondition::operator=(const BoundaryCondition& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mVariable  = rhs.mVariable;
    mType  = rhs.mType;
    mCoordinateBoundary  = rhs.mCoordinateBoundary;
    mBoundaryDomainType  = rhs.mBoundaryDomainType;
  }
  return *this;
}


/*
 * Clone for BoundaryCondition.
 */
BoundaryCondition*
BoundaryCondition::clone () const
{
  return new BoundaryCondition(*this);
}


/*
 * Destructor for BoundaryCondition.
 */
BoundaryCondition::~BoundaryCondition ()
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
BoundaryConditionKind_t
BoundaryCondition::getType() const
{
  return mType;
}


/*
 * Returns the value of the "coordinateBoundary" attribute of this BoundaryCondition.
 */
const std::string&
BoundaryCondition::getCoordinateBoundary() const
{
  return mCoordinateBoundary;
}


/*
 * Returns the value of the "boundaryDomainType" attribute of this BoundaryCondition.
 */
const std::string&
BoundaryCondition::getBoundaryDomainType() const
{
  return mBoundaryDomainType;
}


/*
 * Returns true/false if variable is set.
 */
bool
BoundaryCondition::isSetVariable() const
{
  return (mVariable.empty() == false);
}


/*
 * Returns true/false if type is set.
 */
bool
BoundaryCondition::isSetType() const
{
  return mType != BOUNDARYCONDITIONKIND_UNKNOWN;
}


/*
 * Returns true/false if coordinateBoundary is set.
 */
bool
BoundaryCondition::isSetCoordinateBoundary() const
{
  return (mCoordinateBoundary.empty() == false);
}


/*
 * Returns true/false if boundaryDomainType is set.
 */
bool
BoundaryCondition::isSetBoundaryDomainType() const
{
  return (mBoundaryDomainType.empty() == false);
}


/*
 * Sets variable and returns value indicating success.
 */
int
BoundaryCondition::setVariable(const std::string& variable)
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
 * Sets type and returns value indicating success.
 */
int
BoundaryCondition::setType(BoundaryConditionKind_t type)
{
  mType = type;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets type and returns value indicating success.
 */
int
BoundaryCondition::setType(const std::string& type)
{
  BoundaryConditionKind_t parsed = BoundaryConditionKind_parse(type.c_str());
  if (parsed == BOUNDARYCONDITIONKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mType = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets coordinateBoundary and returns value indicating success.
 */
int
BoundaryCondition::setCoordinateBoundary(const std::string& coordinateBoundary)
{
  if (&(coordinateBoundary) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(coordinateBoundary)))
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
 * Sets boundaryDomainType and returns value indicating success.
 */
int
BoundaryCondition::setBoundaryDomainType(const std::string& boundaryDomainType)
{
  if (&(boundaryDomainType) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(boundaryDomainType)))
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
 * Unsets variable and returns value indicating success.
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
 * Unsets type and returns value indicating success.
 */
int
BoundaryCondition::unsetType()
{
  mType = BOUNDARYCONDITIONKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets coordinateBoundary and returns value indicating success.
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
 * Unsets boundaryDomainType and returns value indicating success.
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
 * rename attributes that are SIdRefs or instances in math
 */
void
BoundaryCondition::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetVariable() == true && mVariable == oldid)
  {
    setVariable(newid);
  }

  if (isSetCoordinateBoundary() == true && mCoordinateBoundary == oldid)
  {
    setCoordinateBoundary(newid);
  }

  if (isSetBoundaryDomainType() == true && mBoundaryDomainType == oldid)
  {
    setBoundaryDomainType(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
BoundaryCondition::getElementName () const
{
  static const string name = "boundaryCondition";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
BoundaryCondition::getTypeCode () const
{
  return SBML_SPATIAL_BOUNDARYCONDITION;
}


/*
 * check if all the required attributes are set
 */
bool
BoundaryCondition::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetVariable() == false)
    allPresent = false;

  if (isSetType() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
BoundaryCondition::writeElements (XMLOutputStream& stream) const
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
BoundaryCondition::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
BoundaryCondition::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
BoundaryCondition::enablePackageInternal(const std::string& pkgURI,
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
BoundaryCondition::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("variable");
  attributes.add("type");
  attributes.add("coordinateBoundary");
  attributes.add("boundaryDomainType");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
BoundaryCondition::readAttributes (const XMLAttributes& attributes,
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
      logEmptyString(mVariable, getLevel(), getVersion(), "<BoundaryCondition>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVariable) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute variable='" + mVariable + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'variable' is missing from 'boundaryCondition' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // type enum  ( use = "required" )
  //
  mType = BOUNDARYCONDITIONKIND_UNKNOWN;
  std::string stringValue;
  assigned = attributes.readInto("type", stringValue);

  if (assigned == true)
  {
    // parse enum

    mType = BoundaryConditionKind_parse(stringValue.c_str());
    if(mType == BOUNDARYCONDITIONKIND_UNKNOWN) {
      std::string message = "Unknown value for spatial attribute 'type' in 'boundaryCondition' object: " + stringValue;
      getErrorLog()->logPackageError("spatial", SpatialUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  if(mType == BOUNDARYCONDITIONKIND_UNKNOWN)
  {
    std::string message = "Spatial attribute 'type' is missing from 'boundaryCondition' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // coordinateBoundary SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("coordinateBoundary", mCoordinateBoundary);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mCoordinateBoundary.empty() == true)
    {
      logEmptyString(mCoordinateBoundary, getLevel(), getVersion(), "<BoundaryCondition>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mCoordinateBoundary) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute coordinateBoundary='" + mCoordinateBoundary + "' does not conform.");
    }
  }

  //
  // boundaryDomainType SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("boundaryDomainType", mBoundaryDomainType);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mBoundaryDomainType.empty() == true)
    {
      logEmptyString(mBoundaryDomainType, getLevel(), getVersion(), "<BoundaryCondition>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mBoundaryDomainType) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute boundaryDomainType='" + mBoundaryDomainType + "' does not conform.");
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
BoundaryCondition::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetVariable() == true)
    stream.writeAttribute("variable", getPrefix(), mVariable);

  if (isSetType() == true)
    stream.writeAttribute("type", getPrefix(), BoundaryConditionKind_toString(mType));

  if (isSetCoordinateBoundary() == true)
    stream.writeAttribute("coordinateBoundary", getPrefix(), mCoordinateBoundary);

  if (isSetBoundaryDomainType() == true)
    stream.writeAttribute("boundaryDomainType", getPrefix(), mBoundaryDomainType);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
BoundaryCondition_t *
BoundaryCondition_create(unsigned int level, unsigned int version,
                         unsigned int pkgVersion)
{
  return new BoundaryCondition(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
BoundaryCondition_free(BoundaryCondition_t * bc)
{
  if (bc != NULL)
    delete bc;
}


LIBSBML_EXTERN
BoundaryCondition_t *
BoundaryCondition_clone(BoundaryCondition_t * bc)
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


LIBSBML_EXTERN
const char *
BoundaryCondition_getVariable(const BoundaryCondition_t * bc)
{
	return (bc != NULL && bc->isSetVariable()) ? bc->getVariable().c_str() : NULL;
}


LIBSBML_EXTERN
BoundaryConditionKind_t
BoundaryCondition_getType(const BoundaryCondition_t * bc)
{
	return (bc != NULL) ? bc->getType() : BOUNDARYCONDITIONKIND_UNKNOWN;
}


LIBSBML_EXTERN
const char *
BoundaryCondition_getCoordinateBoundary(const BoundaryCondition_t * bc)
{
	return (bc != NULL && bc->isSetCoordinateBoundary()) ? bc->getCoordinateBoundary().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
BoundaryCondition_getBoundaryDomainType(const BoundaryCondition_t * bc)
{
	return (bc != NULL && bc->isSetBoundaryDomainType()) ? bc->getBoundaryDomainType().c_str() : NULL;
}


LIBSBML_EXTERN
int
BoundaryCondition_isSetVariable(const BoundaryCondition_t * bc)
{
  return (bc != NULL) ? static_cast<int>(bc->isSetVariable()) : 0;
}


LIBSBML_EXTERN
int
BoundaryCondition_isSetType(const BoundaryCondition_t * bc)
{
  return (bc != NULL) ? static_cast<int>(bc->isSetType()) : 0;
}


LIBSBML_EXTERN
int
BoundaryCondition_isSetCoordinateBoundary(const BoundaryCondition_t * bc)
{
  return (bc != NULL) ? static_cast<int>(bc->isSetCoordinateBoundary()) : 0;
}


LIBSBML_EXTERN
int
BoundaryCondition_isSetBoundaryDomainType(const BoundaryCondition_t * bc)
{
  return (bc != NULL) ? static_cast<int>(bc->isSetBoundaryDomainType()) : 0;
}


LIBSBML_EXTERN
int
BoundaryCondition_setVariable(BoundaryCondition_t * bc, const char * variable)
{
  if (bc != NULL)
    return (variable == NULL) ? bc->setVariable("") : bc->setVariable(variable);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
BoundaryCondition_setType(BoundaryCondition_t * bc, BoundaryConditionKind_t type)
{
  if (bc != NULL)
    return bc->setType(type);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
BoundaryCondition_setCoordinateBoundary(BoundaryCondition_t * bc, const char * coordinateBoundary)
{
  if (bc != NULL)
    return (coordinateBoundary == NULL) ? bc->setCoordinateBoundary("") : bc->setCoordinateBoundary(coordinateBoundary);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
BoundaryCondition_setBoundaryDomainType(BoundaryCondition_t * bc, const char * boundaryDomainType)
{
  if (bc != NULL)
    return (boundaryDomainType == NULL) ? bc->setBoundaryDomainType("") : bc->setBoundaryDomainType(boundaryDomainType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
BoundaryCondition_unsetVariable(BoundaryCondition_t * bc)
{
  return (bc != NULL) ? bc->unsetVariable() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
BoundaryCondition_unsetType(BoundaryCondition_t * bc)
{
  return (bc != NULL) ? bc->unsetType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
BoundaryCondition_unsetCoordinateBoundary(BoundaryCondition_t * bc)
{
  return (bc != NULL) ? bc->unsetCoordinateBoundary() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
BoundaryCondition_unsetBoundaryDomainType(BoundaryCondition_t * bc)
{
  return (bc != NULL) ? bc->unsetBoundaryDomainType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
BoundaryCondition_hasRequiredAttributes(const BoundaryCondition_t * bc)
{
  return (bc != NULL) ? static_cast<int>(bc->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


