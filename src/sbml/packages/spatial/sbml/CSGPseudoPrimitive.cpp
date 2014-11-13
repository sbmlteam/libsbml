/**
 * @file:   CSGPseudoPrimitive.cpp
 * @brief:  Implementation of the CSGPseudoPrimitive class
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


#include <sbml/packages/spatial/sbml/CSGPseudoPrimitive.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CSGPseudoPrimitive with the given level, version, and package version.
 */
CSGPseudoPrimitive::CSGPseudoPrimitive (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : CSGNode(level, version)
  , mCsgObjectRef ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new CSGPseudoPrimitive with the given SpatialPkgNamespaces object.
 */
CSGPseudoPrimitive::CSGPseudoPrimitive (SpatialPkgNamespaces* spatialns)
  : CSGNode(spatialns)
  , mCsgObjectRef ("")
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGPseudoPrimitive.
 */
CSGPseudoPrimitive::CSGPseudoPrimitive (const CSGPseudoPrimitive& orig)
  : CSGNode(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mCsgObjectRef  = orig.mCsgObjectRef;
  }
}


/*
 * Assignment for CSGPseudoPrimitive.
 */
CSGPseudoPrimitive&
CSGPseudoPrimitive::operator=(const CSGPseudoPrimitive& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    CSGNode::operator=(rhs);
    mCsgObjectRef  = rhs.mCsgObjectRef;
  }
  return *this;
}


/*
 * Clone for CSGPseudoPrimitive.
 */
CSGPseudoPrimitive*
CSGPseudoPrimitive::clone () const
{
  return new CSGPseudoPrimitive(*this);
}


/*
 * Destructor for CSGPseudoPrimitive.
 */
CSGPseudoPrimitive::~CSGPseudoPrimitive ()
{
}


/*
 * Returns the value of the "csgObjectRef" attribute of this CSGPseudoPrimitive.
 */
const std::string&
CSGPseudoPrimitive::getCsgObjectRef() const
{
  return mCsgObjectRef;
}


/*
 * Returns true/false if csgObjectRef is set.
 */
bool
CSGPseudoPrimitive::isSetCsgObjectRef() const
{
  return (mCsgObjectRef.empty() == false);
}


/*
 * Sets csgObjectRef and returns value indicating success.
 */
int
CSGPseudoPrimitive::setCsgObjectRef(const std::string& csgObjectRef)
{
  if (&(csgObjectRef) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(csgObjectRef)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCsgObjectRef = csgObjectRef;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets csgObjectRef and returns value indicating success.
 */
int
CSGPseudoPrimitive::unsetCsgObjectRef()
{
  mCsgObjectRef.erase();

  if (mCsgObjectRef.empty() == true)
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
CSGPseudoPrimitive::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetCsgObjectRef() == true && mCsgObjectRef == oldid)
  {
    setCsgObjectRef(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
CSGPseudoPrimitive::getElementName () const
{
  static const string name = "csgPseudoPrimitive";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CSGPseudoPrimitive::getTypeCode () const
{
  return SBML_SPATIAL_CSGPSEUDOPRIMITIVE;
}


/*
 * check if all the required attributes are set
 */
bool
CSGPseudoPrimitive::hasRequiredAttributes () const
{
  bool allPresent = CSGNode::hasRequiredAttributes();

  if (isSetCsgObjectRef() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CSGPseudoPrimitive::writeElements (XMLOutputStream& stream) const
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
CSGPseudoPrimitive::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
CSGPseudoPrimitive::setSBMLDocument (SBMLDocument* d)
{
  CSGNode::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CSGPseudoPrimitive::enablePackageInternal(const std::string& pkgURI,
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
CSGPseudoPrimitive::createObject(XMLInputStream& stream)
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
CSGPseudoPrimitive::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGNode::addExpectedAttributes(attributes);

  attributes.add("csgObjectRef");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CSGPseudoPrimitive::readAttributes (const XMLAttributes& attributes,
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
  // csgObjectRef SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("csgObjectRef", mCsgObjectRef);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mCsgObjectRef.empty() == true)
    {
      logEmptyString(mCsgObjectRef, getLevel(), getVersion(), "<CSGPseudoPrimitive>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mCsgObjectRef) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute csgObjectRef='" + mCsgObjectRef + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'csgObjectRef' is missing from 'csgPseudoPrimitive' object.";
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
CSGPseudoPrimitive::writeAttributes (XMLOutputStream& stream) const
{
  CSGNode::writeAttributes(stream);

  if (isSetCsgObjectRef() == true)
    stream.writeAttribute("csgObjectRef", getPrefix(), mCsgObjectRef);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGPseudoPrimitive_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion)
{
  return new CSGPseudoPrimitive(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CSGPseudoPrimitive_free(CSGPseudoPrimitive_t * csgpp)
{
  if (csgpp != NULL)
    delete csgpp;
}


LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGPseudoPrimitive_clone(CSGPseudoPrimitive_t * csgpp)
{
  if (csgpp != NULL)
  {
    return static_cast<CSGPseudoPrimitive_t*>(csgpp->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
CSGPseudoPrimitive_getCsgObjectRef(const CSGPseudoPrimitive_t * csgpp)
{
	return (csgpp != NULL && csgpp->isSetCsgObjectRef()) ? csgpp->getCsgObjectRef().c_str() : NULL;
}


LIBSBML_EXTERN
int
CSGPseudoPrimitive_isSetCsgObjectRef(const CSGPseudoPrimitive_t * csgpp)
{
  return (csgpp != NULL) ? static_cast<int>(csgpp->isSetCsgObjectRef()) : 0;
}


LIBSBML_EXTERN
int
CSGPseudoPrimitive_setCsgObjectRef(CSGPseudoPrimitive_t * csgpp, const char * csgObjectRef)
{
  if (csgpp != NULL)
    return (csgObjectRef == NULL) ? csgpp->setCsgObjectRef("") : csgpp->setCsgObjectRef(csgObjectRef);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGPseudoPrimitive_unsetCsgObjectRef(CSGPseudoPrimitive_t * csgpp)
{
  return (csgpp != NULL) ? csgpp->unsetCsgObjectRef() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGPseudoPrimitive_hasRequiredAttributes(const CSGPseudoPrimitive_t * csgpp)
{
  return (csgpp != NULL) ? static_cast<int>(csgpp->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


