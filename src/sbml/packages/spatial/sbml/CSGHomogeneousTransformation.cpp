/**
 * @file CSGHomogeneousTransformation.cpp
 * @brief Implementation of the CSGHomogeneousTransformation class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <util/ElementFilter.h>



using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new CSGHomogeneousTransformation using the given SBML Level,
 * Version and &ldquo;spatial&rdquo; package version.
 */
CSGHomogeneousTransformation::CSGHomogeneousTransformation(unsigned int level,
                                                           unsigned int
                                                             version,
                                                           unsigned int
                                                             pkgVersion)
  : CSGTransformation(level, version)
  , mForwardTransformation (NULL)
  , mReverseTransformation (NULL)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new CSGHomogeneousTransformation using the given
 * SpatialPkgNamespaces object.
 */
CSGHomogeneousTransformation::CSGHomogeneousTransformation(SpatialPkgNamespaces
  *spatialns)
  : CSGTransformation(spatialns)
  , mForwardTransformation (NULL)
  , mReverseTransformation (NULL)
{
  setElementNamespace(spatialns->getURI());
  connectToChild();
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGHomogeneousTransformation.
 */
CSGHomogeneousTransformation::CSGHomogeneousTransformation(const
  CSGHomogeneousTransformation& orig)
  : CSGTransformation( orig )
  , mForwardTransformation ( NULL )
  , mReverseTransformation ( NULL )
{
  if (orig.mForwardTransformation != NULL)
  {
    mForwardTransformation = orig.mForwardTransformation->clone();
  }

  if (orig.mReverseTransformation != NULL)
  {
    mReverseTransformation = orig.mReverseTransformation->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for CSGHomogeneousTransformation.
 */
CSGHomogeneousTransformation&
CSGHomogeneousTransformation::operator=(const CSGHomogeneousTransformation&
  rhs)
{
  if (&rhs != this)
  {
    CSGTransformation::operator=(rhs);
    delete mForwardTransformation;
    if (rhs.mForwardTransformation != NULL)
    {
      mForwardTransformation = rhs.mForwardTransformation->clone();
    }
    else
    {
      mForwardTransformation = NULL;
    }

    delete mReverseTransformation;
    if (rhs.mReverseTransformation != NULL)
    {
      mReverseTransformation = rhs.mReverseTransformation->clone();
    }
    else
    {
      mReverseTransformation = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this CSGHomogeneousTransformation object.
 */
CSGHomogeneousTransformation*
CSGHomogeneousTransformation::clone() const
{
  return new CSGHomogeneousTransformation(*this);
}


/*
 * Destructor for CSGHomogeneousTransformation.
 */
CSGHomogeneousTransformation::~CSGHomogeneousTransformation()
{
  delete mForwardTransformation;
  mForwardTransformation = NULL;
  delete mReverseTransformation;
  mReverseTransformation = NULL;
}


/*
 * Returns the value of the "forwardTransformation" element of this
 * CSGHomogeneousTransformation.
 */
const TransformationComponent*
CSGHomogeneousTransformation::getForwardTransformation() const
{
  return mForwardTransformation;
}


/*
 * Returns the value of the "forwardTransformation" element of this
 * CSGHomogeneousTransformation.
 */
TransformationComponent*
CSGHomogeneousTransformation::getForwardTransformation()
{
  return mForwardTransformation;
}


/*
 * Returns the value of the "reverseTransformation" element of this
 * CSGHomogeneousTransformation.
 */
const TransformationComponent*
CSGHomogeneousTransformation::getReverseTransformation() const
{
  return mReverseTransformation;
}


/*
 * Returns the value of the "reverseTransformation" element of this
 * CSGHomogeneousTransformation.
 */
TransformationComponent*
CSGHomogeneousTransformation::getReverseTransformation()
{
  return mReverseTransformation;
}


/*
 * Predicate returning @c true if this CSGHomogeneousTransformation's
 * "forwardTransformation" element is set.
 */
bool
CSGHomogeneousTransformation::isSetForwardTransformation() const
{
  return (mForwardTransformation != NULL);
}


/*
 * Predicate returning @c true if this CSGHomogeneousTransformation's
 * "reverseTransformation" element is set.
 */
bool
CSGHomogeneousTransformation::isSetReverseTransformation() const
{
  return (mReverseTransformation != NULL);
}


/*
 * Sets the value of the "forwardTransformation" element of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::setForwardTransformation(const
  TransformationComponent* forwardTransformation)
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
      forwardTransformation->clone() : NULL;
    if (mForwardTransformation != NULL)
    {
      mForwardTransformation->setElementName("forwardTransformation");
      mForwardTransformation->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "reverseTransformation" element of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::setReverseTransformation(const
  TransformationComponent* reverseTransformation)
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
      reverseTransformation->clone() : NULL;
    if (mReverseTransformation != NULL)
    {
      mReverseTransformation->setElementName("reverseTransformation");
      mReverseTransformation->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new TransformationComponent object, adds it to this
 * CSGHomogeneousTransformation object and returns the TransformationComponent
 * object created.
 */
TransformationComponent*
CSGHomogeneousTransformation::createForwardTransformation()
{
  if (mForwardTransformation != NULL)
  {
    delete mForwardTransformation;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mForwardTransformation = new TransformationComponent(spatialns);

  mForwardTransformation->setElementName("forwardTransformation");

  delete spatialns;

  connectToChild();

  return mForwardTransformation;
}


/*
 * Creates a new TransformationComponent object, adds it to this
 * CSGHomogeneousTransformation object and returns the TransformationComponent
 * object created.
 */
TransformationComponent*
CSGHomogeneousTransformation::createReverseTransformation()
{
  if (mReverseTransformation != NULL)
  {
    delete mReverseTransformation;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mReverseTransformation = new TransformationComponent(spatialns);

  mReverseTransformation->setElementName("reverseTransformation");

  delete spatialns;

  connectToChild();

  return mReverseTransformation;
}


/*
 * Unsets the value of the "forwardTransformation" element of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::unsetForwardTransformation()
{
  delete mForwardTransformation;
  mForwardTransformation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "reverseTransformation" element of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::unsetReverseTransformation()
{
  delete mReverseTransformation;
  mReverseTransformation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this CSGHomogeneousTransformation object.
 */
const std::string&
CSGHomogeneousTransformation::getElementName() const
{
  static const string name = "csgHomogeneousTransformation";
  return name;
}


/*
 * Returns the libSBML type code for this CSGHomogeneousTransformation object.
 */
int
CSGHomogeneousTransformation::getTypeCode() const
{
  return SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * CSGHomogeneousTransformation object have been set.
 */
bool
CSGHomogeneousTransformation::hasRequiredAttributes() const
{
  bool allPresent = CSGTransformation::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * CSGHomogeneousTransformation object have been set.
 */
bool
CSGHomogeneousTransformation::hasRequiredElements() const
{
  bool allPresent = CSGTransformation::hasRequiredElements();

  if (isSetForwardTransformation() == false)
  {
    allPresent = false;
  }

  if (isSetReverseTransformation() == false)
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
CSGHomogeneousTransformation::writeElements(XMLOutputStream& stream) const
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

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
CSGHomogeneousTransformation::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mForwardTransformation != NULL)
  {
    mForwardTransformation->accept(v);
  }

  if (mReverseTransformation != NULL)
  {
    mReverseTransformation->accept(v);
  }

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
CSGHomogeneousTransformation::setSBMLDocument(SBMLDocument* d)
{
  CSGTransformation::setSBMLDocument(d);

  if (mForwardTransformation != NULL)
  {
    mForwardTransformation->setSBMLDocument(d);
  }

  if (mReverseTransformation != NULL)
  {
    mReverseTransformation->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
CSGHomogeneousTransformation::connectToChild()
{
  CSGTransformation::connectToChild();

  if (mForwardTransformation != NULL)
  {
    mForwardTransformation->connectToParent(this);
  }

  if (mReverseTransformation != NULL)
  {
    mReverseTransformation->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
CSGHomogeneousTransformation::enablePackageInternal(const std::string& pkgURI,
                                                    const std::string&
                                                      pkgPrefix,
                                                    bool flag)
{
  CSGTransformation::enablePackageInternal(pkgURI, pkgPrefix, flag);

  if (isSetForwardTransformation())
  {
    mForwardTransformation->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetReverseTransformation())
  {
    mReverseTransformation->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::getAttribute(const std::string& attributeName,
                                           bool& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::getAttribute(const std::string& attributeName,
                                           int& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::getAttribute(const std::string& attributeName,
                                           double& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::getAttribute(const std::string& attributeName,
                                           unsigned int& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::getAttribute(const std::string& attributeName,
                                           std::string& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this CSGHomogeneousTransformation's attribute
 * "attributeName" is set.
 */
bool
CSGHomogeneousTransformation::isSetAttribute(const std::string& attributeName)
  const
{
  bool value = CSGTransformation::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::setAttribute(const std::string& attributeName,
                                           bool value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::setAttribute(const std::string& attributeName,
                                           int value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::setAttribute(const std::string& attributeName,
                                           double value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::setAttribute(const std::string& attributeName,
                                           unsigned int value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::setAttribute(const std::string& attributeName,
                                           const std::string& value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * CSGHomogeneousTransformation.
 */
int
CSGHomogeneousTransformation::unsetAttribute(const std::string& attributeName)
{
  int value = CSGTransformation::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * CSGHomogeneousTransformation.
 */
SBase*
CSGHomogeneousTransformation::createObject(const std::string& elementName)
{
  CSGTransformation* obj = NULL;

  if (elementName == "forwardTransformation")
  {
    return createForwardTransformation();
  }
  else if (elementName == "reverseTransformation")
  {
    return createReverseTransformation();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this CSGHomogeneousTransformation.
 */
unsigned int
CSGHomogeneousTransformation::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "forwardTransformation")
  {
    if (isSetForwardTransformation())
    {
      return 1;
    }
  }
  else if (elementName == "reverseTransformation")
  {
    if (isSetReverseTransformation())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this CSGHomogeneousTransformation.
 */
SBase*
CSGHomogeneousTransformation::getObject(const std::string& elementName,
                                        unsigned int index)
{
  CSGTransformation* obj = NULL;

  if (elementName == "forwardTransformation")
  {
    return getForwardTransformation();
  }
  else if (elementName == "reverseTransformation")
  {
    return getReverseTransformation();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
CSGHomogeneousTransformation::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mForwardTransformation != NULL)
  {
    if (mForwardTransformation->getId() == id)
    {
      return mForwardTransformation;
    }

    obj = mForwardTransformation->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mReverseTransformation != NULL)
  {
    if (mReverseTransformation->getId() == id)
    {
      return mReverseTransformation;
    }

    obj = mReverseTransformation->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
CSGHomogeneousTransformation::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mForwardTransformation != NULL)
  {
    if (mForwardTransformation->getMetaId() == metaid)
    {
      return mForwardTransformation;
    }

    obj = mForwardTransformation->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mReverseTransformation != NULL)
  {
    if (mReverseTransformation->getMetaId() == metaid)
    {
      return mReverseTransformation;
    }

    obj = mReverseTransformation->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
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



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
CSGHomogeneousTransformation::createObject(XMLInputStream& stream)
{
  SBase* obj = CSGTransformation::createObject(stream);

  const std::string& name = stream.peek().getName();

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "forwardTransformation")
  {
    if (mForwardTransformation != NULL)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialCSGHomogeneousTransformationAllowedElements, getPackageVersion(),
          getLevel(), getVersion());

      delete mForwardTransformation;
      mForwardTransformation = NULL;
    }

    mForwardTransformation = new TransformationComponent(spatialns);
    mForwardTransformation->setElementName(name);
    obj = mForwardTransformation;
  }
  else if (name == "reverseTransformation")
  {
    if (mReverseTransformation != NULL)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialCSGHomogeneousTransformationAllowedElements, getPackageVersion(),
          getLevel(), getVersion());

      delete mReverseTransformation;
      mReverseTransformation = NULL;
    }

    mReverseTransformation = new TransformationComponent(spatialns);
    mReverseTransformation->setElementName(name);
    obj = mReverseTransformation;
  }

  delete spatialns;

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
CSGHomogeneousTransformation::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  CSGTransformation::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CSGHomogeneousTransformation::readAttributes(const XMLAttributes& attributes,
                                             const ExpectedAttributes&
                                               expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  CSGTransformation::readAttributes(attributes, expectedAttributes);
  numErrs = log->getNumErrors();

  for (int n = numErrs-1; n >= 0; n--)
  {
    if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
    {
      const std::string details = log->getError(n)->getMessage();
      log->remove(UnknownPackageAttribute);
      log->logPackageError("spatial", SpatialUnknown, pkgVersion, level,
        version, details);
    }
    else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
    {
      const std::string details = log->getError(n)->getMessage();
      log->remove(UnknownCoreAttribute);
      log->logPackageError("spatial",
        SpatialCSGHomogeneousTransformationAllowedCoreAttributes, pkgVersion,
          level, version, details);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
CSGHomogeneousTransformation::writeAttributes(XMLOutputStream& stream) const
{
  CSGTransformation::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new CSGHomogeneousTransformation_t using the given SBML Level,
 * Version and &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGHomogeneousTransformation_create(unsigned int level,
                                    unsigned int version,
                                    unsigned int pkgVersion)
{
  return new CSGHomogeneousTransformation(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CSGHomogeneousTransformation_t
 * object.
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t*
CSGHomogeneousTransformation_clone(const CSGHomogeneousTransformation_t* csght)
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


/*
 * Frees this CSGHomogeneousTransformation_t object.
 */
LIBSBML_EXTERN
void
CSGHomogeneousTransformation_free(CSGHomogeneousTransformation_t* csght)
{
  if (csght != NULL)
  {
    delete csght;
  }
}


/*
 * Returns the value of the "forwardTransformation" element of this
 * CSGHomogeneousTransformation_t.
 */
LIBSBML_EXTERN
const TransformationComponent_t*
CSGHomogeneousTransformation_getForwardTransformation(const
  CSGHomogeneousTransformation_t * csght)
{
  if (csght == NULL)
  {
    return NULL;
  }

  return (TransformationComponent_t*)(csght->getForwardTransformation());
}


/*
 * Returns the value of the "reverseTransformation" element of this
 * CSGHomogeneousTransformation_t.
 */
LIBSBML_EXTERN
const TransformationComponent_t*
CSGHomogeneousTransformation_getReverseTransformation(const
  CSGHomogeneousTransformation_t * csght)
{
  if (csght == NULL)
  {
    return NULL;
  }

  return (TransformationComponent_t*)(csght->getReverseTransformation());
}


/*
 * Predicate returning @c 1 if this CSGHomogeneousTransformation_t's
 * "forwardTransformation" element is set.
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_isSetForwardTransformation(const
  CSGHomogeneousTransformation_t * csght)
{
  return (csght != NULL) ?
    static_cast<int>(csght->isSetForwardTransformation()) : 0;
}


/*
 * Predicate returning @c 1 if this CSGHomogeneousTransformation_t's
 * "reverseTransformation" element is set.
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_isSetReverseTransformation(const
  CSGHomogeneousTransformation_t * csght)
{
  return (csght != NULL) ?
    static_cast<int>(csght->isSetReverseTransformation()) : 0;
}


/*
 * Sets the value of the "forwardTransformation" element of this
 * CSGHomogeneousTransformation_t.
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_setForwardTransformation(
                                                      CSGHomogeneousTransformation_t
                                                        * csght,
                                                      const
                                                        TransformationComponent_t*
                                                          forwardTransformation)
{
  return (csght != NULL) ?
    csght->setForwardTransformation(forwardTransformation) :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "reverseTransformation" element of this
 * CSGHomogeneousTransformation_t.
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_setReverseTransformation(
                                                      CSGHomogeneousTransformation_t
                                                        * csght,
                                                      const
                                                        TransformationComponent_t*
                                                          reverseTransformation)
{
  return (csght != NULL) ?
    csght->setReverseTransformation(reverseTransformation) :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new TransformationComponent_t object, adds it to this
 * CSGHomogeneousTransformation_t object and returns the
 * TransformationComponent_t object created.
 */
LIBSBML_EXTERN
TransformationComponent_t*
CSGHomogeneousTransformation_createForwardTransformation(CSGHomogeneousTransformation_t*
  csght)
{
  if (csght == NULL)
  {
    return NULL;
  }

  return (TransformationComponent_t*)(csght->createForwardTransformation());
}


/*
 * Creates a new TransformationComponent_t object, adds it to this
 * CSGHomogeneousTransformation_t object and returns the
 * TransformationComponent_t object created.
 */
LIBSBML_EXTERN
TransformationComponent_t*
CSGHomogeneousTransformation_createReverseTransformation(CSGHomogeneousTransformation_t*
  csght)
{
  if (csght == NULL)
  {
    return NULL;
  }

  return (TransformationComponent_t*)(csght->createReverseTransformation());
}


/*
 * Unsets the value of the "forwardTransformation" element of this
 * CSGHomogeneousTransformation_t.
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_unsetForwardTransformation(CSGHomogeneousTransformation_t
  * csght)
{
  return (csght != NULL) ? csght->unsetForwardTransformation() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "reverseTransformation" element of this
 * CSGHomogeneousTransformation_t.
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_unsetReverseTransformation(CSGHomogeneousTransformation_t
  * csght)
{
  return (csght != NULL) ? csght->unsetReverseTransformation() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if all the required attributes for this
 * CSGHomogeneousTransformation_t object have been set.
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_hasRequiredAttributes(const
  CSGHomogeneousTransformation_t * csght)
{
  return (csght != NULL) ? static_cast<int>(csght->hasRequiredAttributes()) :
    0;
}


/*
 * Predicate returning @c 1 if all the required elements for this
 * CSGHomogeneousTransformation_t object have been set.
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_hasRequiredElements(const
  CSGHomogeneousTransformation_t * csght)
{
  return (csght != NULL) ? static_cast<int>(csght->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


