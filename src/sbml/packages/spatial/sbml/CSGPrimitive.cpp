/**
 * @file:   CSGPrimitive.cpp
 * @brief:  Implementation of the CSGPrimitive class
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


#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CSGPrimitive with the given level, version, and package version.
 */
CSGPrimitive::CSGPrimitive (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : CSGNode(level, version)
  , mPrimitiveType (PRIMITIVEKIND_UNKNOWN)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new CSGPrimitive with the given SpatialPkgNamespaces object.
 */
CSGPrimitive::CSGPrimitive (SpatialPkgNamespaces* spatialns)
  : CSGNode(spatialns)
  , mPrimitiveType (PRIMITIVEKIND_UNKNOWN)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGPrimitive.
 */
CSGPrimitive::CSGPrimitive (const CSGPrimitive& orig)
  : CSGNode(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mPrimitiveType  = orig.mPrimitiveType;
  }
}


/*
 * Assignment for CSGPrimitive.
 */
CSGPrimitive&
CSGPrimitive::operator=(const CSGPrimitive& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    CSGNode::operator=(rhs);
    mPrimitiveType  = rhs.mPrimitiveType;
  }
  return *this;
}


/*
 * Clone for CSGPrimitive.
 */
CSGPrimitive*
CSGPrimitive::clone () const
{
  return new CSGPrimitive(*this);
}


/*
 * Destructor for CSGPrimitive.
 */
CSGPrimitive::~CSGPrimitive ()
{
}


/*
 * Returns the value of the "primitiveType" attribute of this CSGPrimitive.
 */
PrimitiveKind_t
CSGPrimitive::getPrimitiveType() const
{
  return mPrimitiveType;
}


/*
 * Returns true/false if primitiveType is set.
 */
bool
CSGPrimitive::isSetPrimitiveType() const
{
  return mPrimitiveType != PRIMITIVEKIND_UNKNOWN;
}


/*
 * Sets primitiveType and returns value indicating success.
 */
int
CSGPrimitive::setPrimitiveType(PrimitiveKind_t primitiveType)
{
  mPrimitiveType = primitiveType;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets primitiveType and returns value indicating success.
 */
int
CSGPrimitive::setPrimitiveType(const std::string& primitiveType)
{
  PrimitiveKind_t parsed = PrimitiveKind_parse(primitiveType.c_str());
  if (parsed == PRIMITIVEKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mPrimitiveType = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets primitiveType and returns value indicating success.
 */
int
CSGPrimitive::unsetPrimitiveType()
{
  mPrimitiveType = PRIMITIVEKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
CSGPrimitive::getElementName () const
{
  static const string name = "csgPrimitive";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CSGPrimitive::getTypeCode () const
{
  return SBML_SPATIAL_CSGPRIMITIVE;
}


/*
 * check if all the required attributes are set
 */
bool
CSGPrimitive::hasRequiredAttributes () const
{
  bool allPresent = CSGNode::hasRequiredAttributes();

  if (isSetPrimitiveType() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CSGPrimitive::writeElements (XMLOutputStream& stream) const
{
  CSGNode::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGPrimitive::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
CSGPrimitive::setSBMLDocument (SBMLDocument* d)
{
  CSGNode::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CSGPrimitive::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  CSGNode::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
CSGPrimitive::createObject(XMLInputStream& stream)
{
  SBase* object = CSGNode::createObject(stream);

  connectToChild();


  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
CSGPrimitive::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGNode::addExpectedAttributes(attributes);

  attributes.add("primitiveType");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CSGPrimitive::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  CSGNode::readAttributes(attributes, expectedAttributes);

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
  // primitiveType enum  ( use = "required" )
  //
  mPrimitiveType = PRIMITIVEKIND_UNKNOWN;
  std::string stringValue;
  assigned = attributes.readInto("primitiveType", stringValue);

  if (assigned == true)
  {
    // parse enum

    mPrimitiveType = PrimitiveKind_parse(stringValue.c_str());
    if(mPrimitiveType == PRIMITIVEKIND_UNKNOWN) {
      std::string message = "Unknown value for spatial attribute 'primitiveType' in 'csgPrimitive' object: " + stringValue;
      getErrorLog()->logPackageError("spatial", SpatialUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  if(mPrimitiveType == PRIMITIVEKIND_UNKNOWN)
  {
    std::string message = "Spatial attribute 'primitiveType' is missing from 'csgPrimitive' object.";
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
CSGPrimitive::writeAttributes (XMLOutputStream& stream) const
{
  CSGNode::writeAttributes(stream);

  if (isSetPrimitiveType() == true)
    stream.writeAttribute("primitiveType", getPrefix(), PrimitiveKind_toString(mPrimitiveType));

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
CSGPrimitive_t *
CSGPrimitive_create(unsigned int level, unsigned int version,
                    unsigned int pkgVersion)
{
  return new CSGPrimitive(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CSGPrimitive_free(CSGPrimitive_t * csgp)
{
  if (csgp != NULL)
    delete csgp;
}


LIBSBML_EXTERN
CSGPrimitive_t *
CSGPrimitive_clone(CSGPrimitive_t * csgp)
{
  if (csgp != NULL)
  {
    return static_cast<CSGPrimitive_t*>(csgp->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
PrimitiveKind_t
CSGPrimitive_getPrimitiveType(const CSGPrimitive_t * csgp)
{
	return (csgp != NULL) ? csgp->getPrimitiveType() : PRIMITIVEKIND_UNKNOWN;
}


LIBSBML_EXTERN
int
CSGPrimitive_isSetPrimitiveType(const CSGPrimitive_t * csgp)
{
  return (csgp != NULL) ? static_cast<int>(csgp->isSetPrimitiveType()) : 0;
}


LIBSBML_EXTERN
int
CSGPrimitive_setPrimitiveType(CSGPrimitive_t * csgp, PrimitiveKind_t primitiveType)
{
  if (csgp != NULL)
    return csgp->setPrimitiveType(primitiveType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGPrimitive_unsetPrimitiveType(CSGPrimitive_t * csgp)
{
  return (csgp != NULL) ? csgp->unsetPrimitiveType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGPrimitive_hasRequiredAttributes(const CSGPrimitive_t * csgp)
{
  return (csgp != NULL) ? static_cast<int>(csgp->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


