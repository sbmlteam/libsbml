/**
 * @file:   SpatialSymbolReference.cpp
 * @brief:  Implementation of the SpatialSymbolReference class
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


#include <sbml/packages/spatial/sbml/SpatialSymbolReference.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpatialSymbolReference with the given level, version, and package version.
 */
SpatialSymbolReference::SpatialSymbolReference (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mSpatialRef ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new SpatialSymbolReference with the given SpatialPkgNamespaces object.
 */
SpatialSymbolReference::SpatialSymbolReference (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mSpatialRef ("")
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for SpatialSymbolReference.
 */
SpatialSymbolReference::SpatialSymbolReference (const SpatialSymbolReference& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mSpatialRef  = orig.mSpatialRef;
  }
}


/*
 * Assignment for SpatialSymbolReference.
 */
SpatialSymbolReference&
SpatialSymbolReference::operator=(const SpatialSymbolReference& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mSpatialRef  = rhs.mSpatialRef;
  }
  return *this;
}


/*
 * Clone for SpatialSymbolReference.
 */
SpatialSymbolReference*
SpatialSymbolReference::clone () const
{
  return new SpatialSymbolReference(*this);
}


/*
 * Destructor for SpatialSymbolReference.
 */
SpatialSymbolReference::~SpatialSymbolReference ()
{
}


/*
 * Returns the value of the "spatialRef" attribute of this SpatialSymbolReference.
 */
const std::string&
SpatialSymbolReference::getSpatialRef() const
{
  return mSpatialRef;
}


/*
 * Returns true/false if spatialRef is set.
 */
bool
SpatialSymbolReference::isSetSpatialRef() const
{
  return (mSpatialRef.empty() == false);
}


/*
 * Sets spatialRef and returns value indicating success.
 */
int
SpatialSymbolReference::setSpatialRef(const std::string& spatialRef)
{
  if (&(spatialRef) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(spatialRef)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSpatialRef = spatialRef;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets spatialRef and returns value indicating success.
 */
int
SpatialSymbolReference::unsetSpatialRef()
{
  mSpatialRef.erase();

  if (mSpatialRef.empty() == true)
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
SpatialSymbolReference::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetSpatialRef() == true && mSpatialRef == oldid)
  {
    setSpatialRef(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
SpatialSymbolReference::getElementName () const
{
  static const string name = "spatialSymbolReference";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SpatialSymbolReference::getTypeCode () const
{
  return SBML_SPATIAL_SPATIALSYMBOLREFERENCE;
}


/*
 * check if all the required attributes are set
 */
bool
SpatialSymbolReference::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetSpatialRef() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
SpatialSymbolReference::writeElements (XMLOutputStream& stream) const
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
SpatialSymbolReference::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
SpatialSymbolReference::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
SpatialSymbolReference::enablePackageInternal(const std::string& pkgURI,
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
SpatialSymbolReference::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("spatialRef");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpatialSymbolReference::readAttributes (const XMLAttributes& attributes,
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
  // spatialRef SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("spatialRef", mSpatialRef);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mSpatialRef.empty() == true)
    {
      logEmptyString(mSpatialRef, getLevel(), getVersion(), "<SpatialSymbolReference>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mSpatialRef) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute spatialRef='" + mSpatialRef + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'spatialRef' is missing from 'spatialSymbolReference' object.";
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
SpatialSymbolReference::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetSpatialRef() == true)
    stream.writeAttribute("spatialRef", getPrefix(), mSpatialRef);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
SpatialSymbolReference_t *
SpatialSymbolReference_create(unsigned int level, unsigned int version,
                              unsigned int pkgVersion)
{
  return new SpatialSymbolReference(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
SpatialSymbolReference_free(SpatialSymbolReference_t * ssr)
{
  if (ssr != NULL)
    delete ssr;
}


LIBSBML_EXTERN
SpatialSymbolReference_t *
SpatialSymbolReference_clone(SpatialSymbolReference_t * ssr)
{
  if (ssr != NULL)
  {
    return static_cast<SpatialSymbolReference_t*>(ssr->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
SpatialSymbolReference_getSpatialRef(const SpatialSymbolReference_t * ssr)
{
	return (ssr != NULL && ssr->isSetSpatialRef()) ? ssr->getSpatialRef().c_str() : NULL;
}


LIBSBML_EXTERN
int
SpatialSymbolReference_isSetSpatialRef(const SpatialSymbolReference_t * ssr)
{
  return (ssr != NULL) ? static_cast<int>(ssr->isSetSpatialRef()) : 0;
}


LIBSBML_EXTERN
int
SpatialSymbolReference_setSpatialRef(SpatialSymbolReference_t * ssr, const char * spatialRef)
{
  if (ssr != NULL)
    return (spatialRef == NULL) ? ssr->setSpatialRef("") : ssr->setSpatialRef(spatialRef);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialSymbolReference_unsetSpatialRef(SpatialSymbolReference_t * ssr)
{
  return (ssr != NULL) ? ssr->unsetSpatialRef() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialSymbolReference_hasRequiredAttributes(const SpatialSymbolReference_t * ssr)
{
  return (ssr != NULL) ? static_cast<int>(ssr->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


