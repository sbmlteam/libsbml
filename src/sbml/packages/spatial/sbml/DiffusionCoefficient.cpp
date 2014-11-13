/**
 * @file:   DiffusionCoefficient.cpp
 * @brief:  Implementation of the DiffusionCoefficient class
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


#include <sbml/packages/spatial/sbml/DiffusionCoefficient.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new DiffusionCoefficient with the given level, version, and package version.
 */
DiffusionCoefficient::DiffusionCoefficient (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mVariable ("")
  , mType (DIFFUSIONKIND_UNKNOWN)
  , mCoordinateReference1 (COORDINATEKIND_UNKNOWN)
  , mCoordinateReference2 (COORDINATEKIND_UNKNOWN)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new DiffusionCoefficient with the given SpatialPkgNamespaces object.
 */
DiffusionCoefficient::DiffusionCoefficient (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mVariable ("")
  , mType (DIFFUSIONKIND_UNKNOWN)
  , mCoordinateReference1 (COORDINATEKIND_UNKNOWN)
  , mCoordinateReference2 (COORDINATEKIND_UNKNOWN)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for DiffusionCoefficient.
 */
DiffusionCoefficient::DiffusionCoefficient (const DiffusionCoefficient& orig)
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
    mCoordinateReference1  = orig.mCoordinateReference1;
    mCoordinateReference2  = orig.mCoordinateReference2;

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for DiffusionCoefficient.
 */
DiffusionCoefficient&
DiffusionCoefficient::operator=(const DiffusionCoefficient& rhs)
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
    mCoordinateReference1  = rhs.mCoordinateReference1;
    mCoordinateReference2  = rhs.mCoordinateReference2;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for DiffusionCoefficient.
 */
DiffusionCoefficient*
DiffusionCoefficient::clone () const
{
  return new DiffusionCoefficient(*this);
}


/*
 * Destructor for DiffusionCoefficient.
 */
DiffusionCoefficient::~DiffusionCoefficient ()
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
 * Returns the value of the "coordinateReference1" attribute of this DiffusionCoefficient.
 */
CoordinateKind_t
DiffusionCoefficient::getCoordinateReference1() const
{
  return mCoordinateReference1;
}


/*
 * Returns the value of the "coordinateReference2" attribute of this DiffusionCoefficient.
 */
CoordinateKind_t
DiffusionCoefficient::getCoordinateReference2() const
{
  return mCoordinateReference2;
}


/*
 * Returns true/false if variable is set.
 */
bool
DiffusionCoefficient::isSetVariable() const
{
  return (mVariable.empty() == false);
}


/*
 * Returns true/false if type is set.
 */
bool
DiffusionCoefficient::isSetType() const
{
  return mType != DIFFUSIONKIND_UNKNOWN;
}


/*
 * Returns true/false if coordinateReference1 is set.
 */
bool
DiffusionCoefficient::isSetCoordinateReference1() const
{
  return mCoordinateReference1 != COORDINATEKIND_UNKNOWN;
}


/*
 * Returns true/false if coordinateReference2 is set.
 */
bool
DiffusionCoefficient::isSetCoordinateReference2() const
{
  return mCoordinateReference2 != COORDINATEKIND_UNKNOWN;
}


/*
 * Sets variable and returns value indicating success.
 */
int
DiffusionCoefficient::setVariable(const std::string& variable)
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
DiffusionCoefficient::setType(DiffusionKind_t type)
{
  mType = type;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets type and returns value indicating success.
 */
int
DiffusionCoefficient::setType(const std::string& type)
{
  DiffusionKind_t parsed = DiffusionKind_parse(type.c_str());
  if (parsed == DIFFUSIONKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mType = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets coordinateReference1 and returns value indicating success.
 */
int
DiffusionCoefficient::setCoordinateReference1(CoordinateKind_t coordinateReference1)
{
  mCoordinateReference1 = coordinateReference1;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets coordinateReference1 and returns value indicating success.
 */
int
DiffusionCoefficient::setCoordinateReference1(const std::string& coordinateReference1)
{
  CoordinateKind_t parsed = CoordinateKind_parse(coordinateReference1.c_str());
  if (parsed == COORDINATEKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mCoordinateReference1 = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets coordinateReference2 and returns value indicating success.
 */
int
DiffusionCoefficient::setCoordinateReference2(CoordinateKind_t coordinateReference2)
{
  mCoordinateReference2 = coordinateReference2;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets coordinateReference2 and returns value indicating success.
 */
int
DiffusionCoefficient::setCoordinateReference2(const std::string& coordinateReference2)
{
  CoordinateKind_t parsed = CoordinateKind_parse(coordinateReference2.c_str());
  if (parsed == COORDINATEKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mCoordinateReference2 = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets variable and returns value indicating success.
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
 * Unsets type and returns value indicating success.
 */
int
DiffusionCoefficient::unsetType()
{
  mType = DIFFUSIONKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets coordinateReference1 and returns value indicating success.
 */
int
DiffusionCoefficient::unsetCoordinateReference1()
{
  mCoordinateReference1 = COORDINATEKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets coordinateReference2 and returns value indicating success.
 */
int
DiffusionCoefficient::unsetCoordinateReference2()
{
  mCoordinateReference2 = COORDINATEKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
DiffusionCoefficient::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetVariable() == true && mVariable == oldid)
  {
    setVariable(newid);
  }

}


List*
DiffusionCoefficient::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
DiffusionCoefficient::getElementName () const
{
  static const string name = "diffusionCoefficient";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
DiffusionCoefficient::getTypeCode () const
{
  return SBML_SPATIAL_DIFFUSIONCOEFFICIENT;
}


/*
 * check if all the required attributes are set
 */
bool
DiffusionCoefficient::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetVariable() == false)
    allPresent = false;

  if (isSetType() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
DiffusionCoefficient::hasRequiredElements () const
{
  bool allPresent = true;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
DiffusionCoefficient::writeElements (XMLOutputStream& stream) const
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
DiffusionCoefficient::accept (SBMLVisitor& v) const
{
  v.visit(*this);

/* VISIT CHILDREN */

  v.leave(*this);

  return true;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
DiffusionCoefficient::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
DiffusionCoefficient::connectToChild()
{
  SBase::connectToChild();

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
DiffusionCoefficient::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
DiffusionCoefficient::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  //const string& name = stream.peek().getName();

  connectToChild();


  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
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


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
DiffusionCoefficient::readAttributes (const XMLAttributes& attributes,
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
      logEmptyString(mVariable, getLevel(), getVersion(), "<DiffusionCoefficient>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVariable) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute variable='" + mVariable + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'variable' is missing from 'diffusionCoefficient' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // type enum  ( use = "required" )
  //
  mType = DIFFUSIONKIND_UNKNOWN;
  std::string stringValue;
  assigned = attributes.readInto("type", stringValue);

  if (assigned == true)
  {
    // parse enum

    mType = DiffusionKind_parse(stringValue.c_str());
    if(mType == DIFFUSIONKIND_UNKNOWN) {
      std::string message = "Unknown value for spatial attribute 'type' in 'diffusionCoefficient' object: " + stringValue;
      getErrorLog()->logPackageError("spatial", SpatialUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  if(mType == DIFFUSIONKIND_UNKNOWN)
  {
    std::string message = "Spatial attribute 'type' is missing from 'diffusionCoefficient' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // coordinateReference1 enum  ( use = "optional" )
  //
  mCoordinateReference1 = COORDINATEKIND_UNKNOWN;
  assigned = attributes.readInto("coordinateReference1", stringValue);

  if (assigned == true)
  {
    // parse enum

    mCoordinateReference1 = CoordinateKind_parse(stringValue.c_str());
    if(mCoordinateReference1 == COORDINATEKIND_UNKNOWN) {
      std::string message = "Unknown value for spatial attribute 'coordinateReference1' in 'diffusionCoefficient' object: " + stringValue;
      getErrorLog()->logPackageError("spatial", SpatialUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  //
  // coordinateReference2 enum  ( use = "optional" )
  //
  mCoordinateReference2 = COORDINATEKIND_UNKNOWN;
  {
    std::string stringValue;
    assigned = attributes.readInto("coordinateReference2", stringValue);

    if (assigned == true)
    {
      // parse enum

      mCoordinateReference2 = CoordinateKind_parse(stringValue.c_str());
      if(mCoordinateReference2 == COORDINATEKIND_UNKNOWN) {
        std::string message = "Unknown value for spatial attribute 'coordinateReference2' in 'diffusionCoefficient' object: " + stringValue;
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
          getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
      }
    }
  }
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
DiffusionCoefficient::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetVariable() == true)
    stream.writeAttribute("variable", getPrefix(), mVariable);

  if (isSetType() == true)
    stream.writeAttribute("type", getPrefix(), DiffusionKind_toString(mType));

  if (isSetCoordinateReference1() == true)
    stream.writeAttribute("coordinateReference1", getPrefix(), CoordinateKind_toString(mCoordinateReference1));

  if (isSetCoordinateReference2() == true)
    stream.writeAttribute("coordinateReference2", getPrefix(), CoordinateKind_toString(mCoordinateReference2));

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
DiffusionCoefficient_t *
DiffusionCoefficient_create(unsigned int level, unsigned int version,
                            unsigned int pkgVersion)
{
  return new DiffusionCoefficient(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
DiffusionCoefficient_free(DiffusionCoefficient_t * dc)
{
  if (dc != NULL)
    delete dc;
}


LIBSBML_EXTERN
DiffusionCoefficient_t *
DiffusionCoefficient_clone(DiffusionCoefficient_t * dc)
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


LIBSBML_EXTERN
const char *
DiffusionCoefficient_getVariable(const DiffusionCoefficient_t * dc)
{
	return (dc != NULL && dc->isSetVariable()) ? dc->getVariable().c_str() : NULL;
}


LIBSBML_EXTERN
DiffusionKind_t
DiffusionCoefficient_getType(const DiffusionCoefficient_t * dc)
{
	return (dc != NULL) ? dc->getType() : DIFFUSIONKIND_UNKNOWN;
}


LIBSBML_EXTERN
CoordinateKind_t
DiffusionCoefficient_getCoordinateReference1(const DiffusionCoefficient_t * dc)
{
	return (dc != NULL) ? dc->getCoordinateReference1() : COORDINATEKIND_UNKNOWN;
}


LIBSBML_EXTERN
CoordinateKind_t
DiffusionCoefficient_getCoordinateReference2(const DiffusionCoefficient_t * dc)
{
	return (dc != NULL) ? dc->getCoordinateReference2() : COORDINATEKIND_UNKNOWN;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_isSetVariable(const DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetVariable()) : 0;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_isSetType(const DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetType()) : 0;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_isSetCoordinateReference1(const DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetCoordinateReference1()) : 0;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_isSetCoordinateReference2(const DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->isSetCoordinateReference2()) : 0;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_setVariable(DiffusionCoefficient_t * dc, const char * variable)
{
  if (dc != NULL)
    return (variable == NULL) ? dc->setVariable("") : dc->setVariable(variable);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_setType(DiffusionCoefficient_t * dc, DiffusionKind_t type)
{
  if (dc != NULL)
    return dc->setType(type);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_setCoordinateReference1(DiffusionCoefficient_t * dc, CoordinateKind_t coordinateReference1)
{
  if (dc != NULL)
    return dc->setCoordinateReference1(coordinateReference1);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_setCoordinateReference2(DiffusionCoefficient_t * dc, CoordinateKind_t coordinateReference2)
{
  if (dc != NULL)
    return dc->setCoordinateReference2(coordinateReference2);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_unsetVariable(DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? dc->unsetVariable() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_unsetType(DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? dc->unsetType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_unsetCoordinateReference1(DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? dc->unsetCoordinateReference1() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_unsetCoordinateReference2(DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? dc->unsetCoordinateReference2() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_hasRequiredAttributes(const DiffusionCoefficient_t * dc)
{
  return (dc != NULL) ? static_cast<int>(dc->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
DiffusionCoefficient_hasRequiredElements(const DiffusionCoefficient_t * dc)
{
	return (dc != NULL) ? static_cast<int>(dc->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


