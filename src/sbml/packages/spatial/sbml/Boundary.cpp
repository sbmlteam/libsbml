/**
 * @file:   Boundary.cpp
 * @brief:  Implementation of the Boundary class
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


#include <sbml/packages/spatial/sbml/Boundary.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new Boundary with the given level, version, and package version.
 */
Boundary::Boundary (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mValue (numeric_limits<double>::quiet_NaN())
  , mIsSetValue (false)
  , mElementName("boundary")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new Boundary with the given SpatialPkgNamespaces object.
 */
Boundary::Boundary (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mValue (numeric_limits<double>::quiet_NaN())
  , mIsSetValue (false)
  , mElementName("boundary")
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for Boundary.
 */
Boundary::Boundary (const Boundary& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mValue  = orig.mValue;
    mIsSetValue  = orig.mIsSetValue;
    mElementName = orig.mElementName;
  }
}


/*
 * Assignment for Boundary.
 */
Boundary&
Boundary::operator=(const Boundary& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mValue  = rhs.mValue;
    mIsSetValue  = rhs.mIsSetValue;
    mElementName = rhs.mElementName;
  }
  return *this;
}


/*
 * Clone for Boundary.
 */
Boundary*
Boundary::clone () const
{
  return new Boundary(*this);
}


/*
 * Destructor for Boundary.
 */
Boundary::~Boundary ()
{
}


/*
 * Returns the value of the "id" attribute of this Boundary.
 */
const std::string&
Boundary::getId() const
{
  return mId;
}


/*
 * Returns the value of the "value" attribute of this Boundary.
 */
double
Boundary::getValue() const
{
  return mValue;
}


/*
 * Returns true/false if id is set.
 */
bool
Boundary::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if value is set.
 */
bool
Boundary::isSetValue() const
{
  return mIsSetValue;
}


/*
 * Sets id and returns value indicating success.
 */
int
Boundary::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets value and returns value indicating success.
 */
int
Boundary::setValue(double value)
{
  mValue = value;
  mIsSetValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
 */
int
Boundary::unsetId()
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
 * Unsets value and returns value indicating success.
 */
int
Boundary::unsetValue()
{
  mValue = numeric_limits<double>::quiet_NaN();
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
 * Returns the XML element name of this object
 */
const std::string&
Boundary::getElementName () const
{
  return mElementName;
}


/*
 * Sets the element name for this object
 */
void
Boundary::setElementName(const std::string& name)
{
  mElementName = name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
Boundary::getTypeCode () const
{
  return SBML_SPATIAL_BOUNDARY;
}


/*
 * check if all the required attributes are set
 */
bool
Boundary::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetValue() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
Boundary::writeElements (XMLOutputStream& stream) const
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
Boundary::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
Boundary::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
Boundary::enablePackageInternal(const std::string& pkgURI,
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
Boundary::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("value");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
Boundary::readAttributes (const XMLAttributes& attributes,
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
  // id SId  ( use = "required" )
  //
  assigned = attributes.readInto("id", mId);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<Boundary>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'boundary' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // value double   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetValue = attributes.readInto("value", mValue);

  if (mIsSetValue == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
      else
      {
        std::string message = "Spatial attribute 'value' is missing from 'boundary' object.";
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
Boundary::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetValue() == true)
    stream.writeAttribute("value", getPrefix(), mValue);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
Boundary_t *
Boundary_create(unsigned int level, unsigned int version,
                unsigned int pkgVersion)
{
  return new Boundary(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
Boundary_free(Boundary_t * b)
{
  if (b != NULL)
    delete b;
}


LIBSBML_EXTERN
Boundary_t *
Boundary_clone(Boundary_t * b)
{
  if (b != NULL)
  {
    return static_cast<Boundary_t*>(b->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
Boundary_getId(const Boundary_t * b)
{
	return (b != NULL && b->isSetId()) ? b->getId().c_str() : NULL;
}


LIBSBML_EXTERN
double
Boundary_getValue(const Boundary_t * b)
{
	return (b != NULL) ? b->getValue() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
int
Boundary_isSetId(const Boundary_t * b)
{
  return (b != NULL) ? static_cast<int>(b->isSetId()) : 0;
}


LIBSBML_EXTERN
int
Boundary_isSetValue(const Boundary_t * b)
{
  return (b != NULL) ? static_cast<int>(b->isSetValue()) : 0;
}


LIBSBML_EXTERN
int
Boundary_setId(Boundary_t * b, const char * id)
{
  if (b != NULL)
    return (id == NULL) ? b->setId("") : b->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Boundary_setValue(Boundary_t * b, double value)
{
  if (b != NULL)
    return b->setValue(value);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Boundary_unsetId(Boundary_t * b)
{
  return (b != NULL) ? b->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Boundary_unsetValue(Boundary_t * b)
{
  return (b != NULL) ? b->unsetValue() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Boundary_hasRequiredAttributes(const Boundary_t * b)
{
  return (b != NULL) ? static_cast<int>(b->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


