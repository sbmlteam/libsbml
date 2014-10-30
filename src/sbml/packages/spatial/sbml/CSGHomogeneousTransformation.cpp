/**
 * @file:   CSGHomogeneousTransformation.cpp
 * @brief:  Implementation of the CSGHomogeneousTransformation class
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


#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CSGHomogeneousTransformation with the given level, version, and package version.
 */
CSGHomogeneousTransformation::CSGHomogeneousTransformation (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : CSGTransformation(level, version)
  , mForwardTransformation (NULL)
  , mReverseTransformation (NULL)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new CSGHomogeneousTransformation with the given SpatialPkgNamespaces object.
 */
CSGHomogeneousTransformation::CSGHomogeneousTransformation (SpatialPkgNamespaces* spatialns)
  : CSGTransformation(spatialns)
  , mForwardTransformation (NULL)
  , mReverseTransformation (NULL)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGHomogeneousTransformation.
 */
CSGHomogeneousTransformation::CSGHomogeneousTransformation (const CSGHomogeneousTransformation& orig)
  : CSGTransformation(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    if (orig.mForwardTransformation != NULL)
    {
      mForwardTransformation = orig.mForwardTransformation->clone();
    }
    else
    {
      mForwardTransformation = NULL;
    }
    if (orig.mReverseTransformation != NULL)
    {
      mReverseTransformation = orig.mReverseTransformation->clone();
    }
    else
    {
      mReverseTransformation = NULL;
    }

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for CSGHomogeneousTransformation.
 */
CSGHomogeneousTransformation&
CSGHomogeneousTransformation::operator=(const CSGHomogeneousTransformation& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    CSGTransformation::operator=(rhs);
    if (rhs.mForwardTransformation != NULL)
    {
      mForwardTransformation = rhs.mForwardTransformation->clone();
    }
    else
    {
      mForwardTransformation = NULL;
    }
    if (rhs.mReverseTransformation != NULL)
    {
      mReverseTransformation = rhs.mReverseTransformation->clone();
    }
    else
    {
      mReverseTransformation = NULL;
    }

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for CSGHomogeneousTransformation.
 */
CSGHomogeneousTransformation*
CSGHomogeneousTransformation::clone () const
{
  return new CSGHomogeneousTransformation(*this);
}


/*
 * Destructor for CSGHomogeneousTransformation.
 */
CSGHomogeneousTransformation::~CSGHomogeneousTransformation ()
{
  delete mForwardTransformation;
  mForwardTransformation = NULL;
  delete mReverseTransformation;
  mReverseTransformation = NULL;
}


/*
 * Returns the value of the "forwardTransformation" attribute of this CSGHomogeneousTransformation.
 */
const TransformationComponents*
CSGHomogeneousTransformation::getForwardTransformation() const
{
  return mForwardTransformation;
}


/*
 * Returns the value of the "forwardTransformation" attribute of this CSGHomogeneousTransformation.
 */
TransformationComponents*
CSGHomogeneousTransformation::getForwardTransformation()
{
  return mForwardTransformation;
}


/*
 * Creates a new "forwardTransformation" element of this CSGHomogeneousTransformation and returns it.
 */
TransformationComponents*
CSGHomogeneousTransformation::createForwardTransformation()
{
  if (mForwardTransformation != NULL) delete mForwardTransformation;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mForwardTransformation = new TransformationComponents(spatialns);
  mForwardTransformation->setElementName("forwardTransformation");
  delete spatialns;
  connectToChild();
  return mForwardTransformation;
}


/*
 * Returns the value of the "reverseTransformation" attribute of this CSGHomogeneousTransformation.
 */
const TransformationComponents*
CSGHomogeneousTransformation::getReverseTransformation() const
{
  return mReverseTransformation;
}


/*
 * Returns the value of the "reverseTransformation" attribute of this CSGHomogeneousTransformation.
 */
TransformationComponents*
CSGHomogeneousTransformation::getReverseTransformation()
{
  return mReverseTransformation;
}


/*
 * Creates a new "reverseTransformation" element of this CSGHomogeneousTransformation and returns it.
 */
TransformationComponents*
CSGHomogeneousTransformation::createReverseTransformation()
{
  if (mReverseTransformation != NULL) delete mReverseTransformation;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mReverseTransformation = new TransformationComponents(spatialns);
  mReverseTransformation->setElementName("reverseTransformation");
  delete spatialns;
  connectToChild();
  return mReverseTransformation;
}


/*
 * Returns true/false if forwardTransformation is set.
 */
bool
CSGHomogeneousTransformation::isSetForwardTransformation() const
{
  return (mForwardTransformation != NULL);
}


/*
 * Returns true/false if reverseTransformation is set.
 */
bool
CSGHomogeneousTransformation::isSetReverseTransformation() const
{
  return (mReverseTransformation != NULL);
}


/*
 * Sets forwardTransformation and returns value indicating success.
 */
int
CSGHomogeneousTransformation::setForwardTransformation(TransformationComponents* forwardTransformation)
{
  if (mForwardTransformation == forwardTransformation)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (forwardTransformation == NULL)
  {
    delete mForwardTransformation;
    mForwardTransformation = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mForwardTransformation;
    mForwardTransformation = (forwardTransformation != NULL) ?
      static_cast<TransformationComponents*>(forwardTransformation->clone()) : NULL;
    if (mForwardTransformation != NULL)
    {
      mForwardTransformation->setElementName("forwardTransformation");
      mForwardTransformation->connectToParent(this);
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets reverseTransformation and returns value indicating success.
 */
int
CSGHomogeneousTransformation::setReverseTransformation(TransformationComponents* reverseTransformation)
{
  if (mReverseTransformation == reverseTransformation)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (reverseTransformation == NULL)
  {
    delete mReverseTransformation;
    mReverseTransformation = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mReverseTransformation;
    mReverseTransformation = (reverseTransformation != NULL) ?
      static_cast<TransformationComponents*>(reverseTransformation->clone()) : NULL;
    if (mReverseTransformation != NULL)
    {
      mReverseTransformation->setElementName("reverseTransformation");
      mReverseTransformation->connectToParent(this);
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets forwardTransformation and returns value indicating success.
 */
int
CSGHomogeneousTransformation::unsetForwardTransformation()
{
  delete mForwardTransformation;
  mForwardTransformation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets reverseTransformation and returns value indicating success.
 */
int
CSGHomogeneousTransformation::unsetReverseTransformation()
{
  delete mReverseTransformation;
  mReverseTransformation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


List*
CSGHomogeneousTransformation::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mForwardTransformation, filter);
  ADD_FILTERED_POINTER(ret, sublist, mReverseTransformation, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
CSGHomogeneousTransformation::getElementName () const
{
  static const string name = "csgHomogeneousTransformation";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CSGHomogeneousTransformation::getTypeCode () const
{
  return SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION;
}


/*
 * check if all the required attributes are set
 */
bool
CSGHomogeneousTransformation::hasRequiredAttributes () const
{
  bool allPresent = CSGTransformation::hasRequiredAttributes();

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
CSGHomogeneousTransformation::hasRequiredElements () const
{
  bool allPresent = CSGTransformation::hasRequiredElements();

  if (isSetForwardTransformation() == false)
    allPresent = false;

  if (isSetReverseTransformation() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CSGHomogeneousTransformation::writeElements (XMLOutputStream& stream) const
{
  CSGTransformation::writeElements(stream);
  if (isSetForwardTransformation() == true)
  {
    mForwardTransformation->write(stream);
  }
  if (isSetReverseTransformation() == true)
  {
    mReverseTransformation->write(stream);
  }
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGHomogeneousTransformation::accept (SBMLVisitor& v) const
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
CSGHomogeneousTransformation::setSBMLDocument (SBMLDocument* d)
{
  CSGTransformation::setSBMLDocument(d);
  if ( mForwardTransformation != NULL)
    mForwardTransformation->setSBMLDocument(d);
  if ( mReverseTransformation != NULL)
    mReverseTransformation->setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
CSGHomogeneousTransformation::connectToChild()
{
  CSGTransformation::connectToChild();

  if (mForwardTransformation != NULL)
    mForwardTransformation->connectToParent(this);
  if (mReverseTransformation != NULL)
    mReverseTransformation->connectToParent(this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CSGHomogeneousTransformation::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  CSGTransformation::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
CSGHomogeneousTransformation::createObject(XMLInputStream& stream)
{
  SBase* object = CSGTransformation::createObject(stream);

  const string& name = stream.peek().getName();

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "forwardTransformation")
  {
    mForwardTransformation = new TransformationComponents(spatialns);
    mForwardTransformation->setElementName(name);
    object = mForwardTransformation;
  }
  else if (name == "reverseTransformation")
  {
    mReverseTransformation = new TransformationComponents(spatialns);
    mReverseTransformation->setElementName(name);
    object = mReverseTransformation;
  }

  delete spatialns;

  connectToChild();


  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
CSGHomogeneousTransformation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGTransformation::addExpectedAttributes(attributes);

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CSGHomogeneousTransformation::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  CSGTransformation::readAttributes(attributes, expectedAttributes);

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

  //bool assigned = false;

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
CSGHomogeneousTransformation::writeAttributes (XMLOutputStream& stream) const
{
  CSGTransformation::writeAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGHomogeneousTransformation_create(unsigned int level, unsigned int version,
                                    unsigned int pkgVersion)
{
  return new CSGHomogeneousTransformation(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CSGHomogeneousTransformation_free(CSGHomogeneousTransformation_t * csght)
{
  if (csght != NULL)
    delete csght;
}


LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGHomogeneousTransformation_clone(CSGHomogeneousTransformation_t * csght)
{
  if (csght != NULL)
  {
    return static_cast<CSGHomogeneousTransformation_t*>(csght->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
TransformationComponents_t*
CSGHomogeneousTransformation_getForwardTransformation(CSGHomogeneousTransformation_t * csght)
{
	if (csght == NULL)
		return NULL;

	return (TransformationComponents_t*)csght->getForwardTransformation();
}


LIBSBML_EXTERN
TransformationComponents_t*
CSGHomogeneousTransformation_createForwardTransformation(CSGHomogeneousTransformation_t * csght)
{
	if (csght == NULL)
		return NULL;

	return (TransformationComponents_t*)csght->createForwardTransformation();
}


LIBSBML_EXTERN
TransformationComponents_t*
CSGHomogeneousTransformation_getReverseTransformation(CSGHomogeneousTransformation_t * csght)
{
	if (csght == NULL)
		return NULL;

	return (TransformationComponents_t*)csght->getReverseTransformation();
}


LIBSBML_EXTERN
TransformationComponents_t*
CSGHomogeneousTransformation_createReverseTransformation(CSGHomogeneousTransformation_t * csght)
{
	if (csght == NULL)
		return NULL;

	return (TransformationComponents_t*)csght->createReverseTransformation();
}


LIBSBML_EXTERN
int
CSGHomogeneousTransformation_isSetForwardTransformation(const CSGHomogeneousTransformation_t * csght)
{
  return (csght != NULL) ? static_cast<int>(csght->isSetForwardTransformation()) : 0;
}


LIBSBML_EXTERN
int
CSGHomogeneousTransformation_isSetReverseTransformation(const CSGHomogeneousTransformation_t * csght)
{
  return (csght != NULL) ? static_cast<int>(csght->isSetReverseTransformation()) : 0;
}


LIBSBML_EXTERN
int
CSGHomogeneousTransformation_setForwardTransformation(CSGHomogeneousTransformation_t * csght, TransformationComponents_t* forwardTransformation)
{
	return (csght != NULL) ? csght->setForwardTransformation(forwardTransformation) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGHomogeneousTransformation_setReverseTransformation(CSGHomogeneousTransformation_t * csght, TransformationComponents_t* reverseTransformation)
{
	return (csght != NULL) ? csght->setReverseTransformation(reverseTransformation) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGHomogeneousTransformation_hasRequiredAttributes(const CSGHomogeneousTransformation_t * csght)
{
  return (csght != NULL) ? static_cast<int>(csght->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
CSGHomogeneousTransformation_hasRequiredElements(const CSGHomogeneousTransformation_t * csght)
{
	return (csght != NULL) ? static_cast<int>(csght->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


