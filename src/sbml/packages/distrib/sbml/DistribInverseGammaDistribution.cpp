/**
 * @file DistribInverseGammaDistribution.cpp
 * @brief Implementation of the DistribInverseGammaDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribInverseGammaDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribInverseGammaDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribInverseGammaDistribution::DistribInverseGammaDistribution(
                                                                 unsigned int
                                                                   level,
                                                                 unsigned int
                                                                   version,
                                                                 unsigned int
                                                                   pkgVersion)
  : DistribContinuousUnivariateDistribution(level, version)
  , mShape (NULL)
  , mScale (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribInverseGammaDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribInverseGammaDistribution::DistribInverseGammaDistribution(DistribPkgNamespaces
  *distribns)
  : DistribContinuousUnivariateDistribution(distribns)
  , mShape (NULL)
  , mScale (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribInverseGammaDistribution.
 */
DistribInverseGammaDistribution::DistribInverseGammaDistribution(const
  DistribInverseGammaDistribution& orig)
  : DistribContinuousUnivariateDistribution( orig )
  , mShape ( NULL )
  , mScale ( NULL )
{
  if (orig.mShape != NULL)
  {
    mShape = orig.mShape->clone();
  }

  if (orig.mScale != NULL)
  {
    mScale = orig.mScale->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribInverseGammaDistribution.
 */
DistribInverseGammaDistribution&
DistribInverseGammaDistribution::operator=(const
  DistribInverseGammaDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribContinuousUnivariateDistribution::operator=(rhs);
    delete mShape;
    if (rhs.mShape != NULL)
    {
      mShape = rhs.mShape->clone();
    }
    else
    {
      mShape = NULL;
    }

    delete mScale;
    if (rhs.mScale != NULL)
    {
      mScale = rhs.mScale->clone();
    }
    else
    {
      mScale = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribInverseGammaDistribution
 * object.
 */
DistribInverseGammaDistribution*
DistribInverseGammaDistribution::clone() const
{
  return new DistribInverseGammaDistribution(*this);
}


/*
 * Destructor for DistribInverseGammaDistribution.
 */
DistribInverseGammaDistribution::~DistribInverseGammaDistribution()
{
  delete mShape;
  mShape = NULL;
  delete mScale;
  mScale = NULL;
}


/*
 * Returns the value of the "id" attribute of this
 * DistribInverseGammaDistribution.
 */
const std::string&
DistribInverseGammaDistribution::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * DistribInverseGammaDistribution.
 */
const std::string&
DistribInverseGammaDistribution::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribInverseGammaDistribution's "id"
 * attribute is set.
 */
bool
DistribInverseGammaDistribution::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribInverseGammaDistribution's "name"
 * attribute is set.
 */
bool
DistribInverseGammaDistribution::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::unsetId()
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
 * Unsets the value of the "name" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::unsetName()
{
  mName.erase();

  if (mName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the value of the "shape" element of this
 * DistribInverseGammaDistribution.
 */
const DistribUncertValue*
DistribInverseGammaDistribution::getShape() const
{
  return mShape;
}


/*
 * Returns the value of the "shape" element of this
 * DistribInverseGammaDistribution.
 */
DistribUncertValue*
DistribInverseGammaDistribution::getShape()
{
  return mShape;
}


/*
 * Returns the value of the "scale" element of this
 * DistribInverseGammaDistribution.
 */
const DistribUncertValue*
DistribInverseGammaDistribution::getScale() const
{
  return mScale;
}


/*
 * Returns the value of the "scale" element of this
 * DistribInverseGammaDistribution.
 */
DistribUncertValue*
DistribInverseGammaDistribution::getScale()
{
  return mScale;
}


/*
 * Predicate returning @c true if this DistribInverseGammaDistribution's
 * "shape" element is set.
 */
bool
DistribInverseGammaDistribution::isSetShape() const
{
  return (mShape != NULL);
}


/*
 * Predicate returning @c true if this DistribInverseGammaDistribution's
 * "scale" element is set.
 */
bool
DistribInverseGammaDistribution::isSetScale() const
{
  return (mScale != NULL);
}


/*
 * Sets the value of the "shape" element of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::setShape(const DistribUncertValue* shape)
{
  if (shape == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (shape->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != shape->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != shape->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != shape->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mShape;
    mShape = (shape != NULL) ? static_cast<DistribUncertValue*>(shape->clone())
      : NULL;
    if (mShape != NULL) mShape->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "scale" element of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::setScale(const DistribUncertValue* scale)
{
  if (scale == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (scale->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != scale->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != scale->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != scale->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mScale;
    mScale = (scale != NULL) ? static_cast<DistribUncertValue*>(scale->clone())
      : NULL;
    if (mScale != NULL) mScale->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribInverseGammaDistribution object and returns the DistribUncertValue
 * object created.
 */
DistribUncertValue*
DistribInverseGammaDistribution::createShape()
{
  if (mShape != NULL)
  {
    delete mShape;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mShape = new DistribUncertValue(distribns);

  mShape->setElementName("shape");

  delete distribns;

  connectToChild();

  return mShape;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribInverseGammaDistribution object and returns the DistribUncertValue
 * object created.
 */
DistribUncertValue*
DistribInverseGammaDistribution::createScale()
{
  if (mScale != NULL)
  {
    delete mScale;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mScale = new DistribUncertValue(distribns);

  mScale->setElementName("scale");

  delete distribns;

  connectToChild();

  return mScale;
}


/*
 * Unsets the value of the "shape" element of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::unsetShape()
{
  delete mShape;
  mShape = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "scale" element of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::unsetScale()
{
  delete mScale;
  mScale = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribInverseGammaDistribution object.
 */
const std::string&
DistribInverseGammaDistribution::getElementName() const
{
  static const string name = "inverseGammaDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribInverseGammaDistribution
 * object.
 */
int
DistribInverseGammaDistribution::getTypeCode() const
{
  return SBML_DISTRIB_INVERSEGAMMADISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribInverseGammaDistribution object have been set.
 */
bool
DistribInverseGammaDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribInverseGammaDistribution object have been set.
 */
bool
DistribInverseGammaDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredElements();

  if (isSetShape() == false)
  {
    allPresent = false;
  }

  if (isSetScale() == false)
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
DistribInverseGammaDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeElements(stream);

  if (isSetShape() == true)
  {
    mShape->write(stream);
  }

  if (isSetScale() == true)
  {
    mScale->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribInverseGammaDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mShape != NULL)
  {
    mShape->accept(v);
  }

  if (mScale != NULL)
  {
    mScale->accept(v);
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
DistribInverseGammaDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribContinuousUnivariateDistribution::setSBMLDocument(d);

  if (mShape != NULL)
  {
    mShape->setSBMLDocument(d);
  }

  if (mScale != NULL)
  {
    mScale->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribInverseGammaDistribution::connectToChild()
{
  DistribContinuousUnivariateDistribution::connectToChild();

  if (mShape != NULL)
  {
    mShape->connectToParent(this);
  }

  if (mScale != NULL)
  {
    mScale->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribInverseGammaDistribution::enablePackageInternal(
                                                       const std::string&
                                                         pkgURI,
                                                       const std::string&
                                                         pkgPrefix,
                                                       bool flag)
{
  DistribContinuousUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetShape())
  {
    mShape->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetScale())
  {
    mScale->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribInverseGammaDistribution::updateSBMLNamespace(
                                                     const std::string&
                                                       package,
                                                     unsigned int level,
                                                     unsigned int version)
{
  DistribContinuousUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mShape != NULL)
  {
    mShape->updateSBMLNamespace(package, level, version);
  }

  if (mScale != NULL)
  {
    mScale->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::getAttribute(const std::string& attributeName,
                                              bool& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::getAttribute(const std::string& attributeName,
                                              int& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::getAttribute(const std::string& attributeName,
                                              double& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::getAttribute(const std::string& attributeName,
                                              unsigned int& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::getAttribute(const std::string& attributeName,
                                              std::string& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribInverseGammaDistribution's
 * attribute "attributeName" is set.
 */
bool
DistribInverseGammaDistribution::isSetAttribute(const std::string&
  attributeName) const
{
  bool value =
    DistribContinuousUnivariateDistribution::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::setAttribute(const std::string& attributeName,
                                              bool value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::setAttribute(const std::string& attributeName,
                                              int value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::setAttribute(const std::string& attributeName,
                                              double value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::setAttribute(const std::string& attributeName,
                                              unsigned int value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::setAttribute(const std::string& attributeName,
                                              const std::string& value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::unsetAttribute(const std::string&
  attributeName)
{
  int value =
    DistribContinuousUnivariateDistribution::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribInverseGammaDistribution.
 */
SBase*
DistribInverseGammaDistribution::createChildObject(const std::string&
  elementName)
{
  DistribContinuousUnivariateDistribution* obj = NULL;

  if (elementName == "shape")
  {
    return createShape();
  }
  else if (elementName == "scale")
  {
    return createScale();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribInverseGammaDistribution.
 */
int
DistribInverseGammaDistribution::addChildObject(const std::string& elementName,
                                                const SBase* element)
{
  if (elementName == "shape" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setShape((const DistribUncertValue*)(element));
  }
  else if (elementName == "scale" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setScale((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribInverseGammaDistribution.
 */
SBase*
DistribInverseGammaDistribution::removeChildObject(
                                                   const std::string&
                                                     elementName,
                                                   const std::string& id)
{
  if (elementName == "shape")
  {
    DistribUncertValue * obj = getShape();
    if (unsetShape() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "scale")
  {
    DistribUncertValue * obj = getScale();
    if (unsetScale() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribInverseGammaDistribution.
 */
unsigned int
DistribInverseGammaDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "shape")
  {
    if (isSetShape())
    {
      return 1;
    }
  }
  else if (elementName == "scale")
  {
    if (isSetScale())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this
 * DistribInverseGammaDistribution.
 */
SBase*
DistribInverseGammaDistribution::getObject(const std::string& elementName,
                                           unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "shape")
  {
    return getShape();
  }
  else if (elementName == "scale")
  {
    return getScale();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribInverseGammaDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mShape != NULL)
  {
    if (mShape->getId() == id)
    {
      return mShape;
    }

    obj = mShape->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mScale != NULL)
  {
    if (mScale->getId() == id)
    {
      return mScale;
    }

    obj = mScale->getElementBySId(id);
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
DistribInverseGammaDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mShape != NULL)
  {
    if (mShape->getMetaId() == metaid)
    {
      return mShape;
    }

    obj = mShape->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mScale != NULL)
  {
    if (mScale->getMetaId() == metaid)
    {
      return mScale;
    }

    obj = mScale->getElementByMetaId(metaid);
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
DistribInverseGammaDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mShape, filter);
  ADD_FILTERED_POINTER(ret, sublist, mScale, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribInverseGammaDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "shape")
  {
    if (isSetShape())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribInverseGammaDistributionAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
    }

    delete mShape;
    mShape = new DistribUncertValue(distribns);
    mShape->setElementName(name);
    obj = mShape;
  }
  else if (name == "scale")
  {
    if (isSetScale())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribInverseGammaDistributionAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
    }

    delete mScale;
    mScale = new DistribUncertValue(distribns);
    mScale->setElementName(name);
    obj = mScale;
  }

  delete distribns;

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DistribInverseGammaDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribContinuousUnivariateDistribution::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
    attributes.add("id");
    attributes.add("name");
  }

  if (level == 3 && coreVersion == 2 && pkgVersion == 1)
  {
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribInverseGammaDistribution::readAttributes(
                                                const XMLAttributes&
                                                  attributes,
                                                const ExpectedAttributes&
                                                  expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribContinuousUnivariateDistribution::readAttributes(attributes,
    expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib",
          DistribDistribInverseGammaDistributionAllowedAttributes, pkgVersion,
            level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribInverseGammaDistributionAllowedCoreAttributes,
            pkgVersion, level, version, details);
      }
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribInverseGammaDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeAttributes(stream);
  SBase::writeExtensionAttributes(stream);
}

/** @endcond */


#endif /* __cplusplus */


/*
 * Creates a new DistribInverseGammaDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribInverseGammaDistribution_t *
DistribInverseGammaDistribution_create(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion)
{
  return new DistribInverseGammaDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribInverseGammaDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribInverseGammaDistribution_t*
DistribInverseGammaDistribution_clone(const DistribInverseGammaDistribution_t*
  digd)
{
  if (digd != NULL)
  {
    return static_cast<DistribInverseGammaDistribution_t*>(digd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribInverseGammaDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribInverseGammaDistribution_free(DistribInverseGammaDistribution_t* digd)
{
  if (digd != NULL)
  {
    delete digd;
  }
}


/*
 * Returns the value of the "id" attribute of this
 * DistribInverseGammaDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribInverseGammaDistribution_getId(const DistribInverseGammaDistribution_t *
  digd)
{
  if (digd == NULL)
  {
    return NULL;
  }

  return digd->getId().empty() ? NULL : safe_strdup(digd->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * DistribInverseGammaDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribInverseGammaDistribution_getName(const DistribInverseGammaDistribution_t
  * digd)
{
  if (digd == NULL)
  {
    return NULL;
  }

  return digd->getName().empty() ? NULL : safe_strdup(digd->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribInverseGammaDistribution_t's
 * "id" attribute is set.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_isSetId(const DistribInverseGammaDistribution_t
  * digd)
{
  return (digd != NULL) ? static_cast<int>(digd->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribInverseGammaDistribution_t's
 * "name" attribute is set.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_isSetName(const
  DistribInverseGammaDistribution_t * digd)
{
  return (digd != NULL) ? static_cast<int>(digd->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this
 * DistribInverseGammaDistribution_t.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_setId(DistribInverseGammaDistribution_t * digd,
                                      const char * id)
{
  return (digd != NULL) ? digd->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this
 * DistribInverseGammaDistribution_t.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_setName(
                                        DistribInverseGammaDistribution_t *
                                          digd,
                                        const char * name)
{
  return (digd != NULL) ? digd->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribInverseGammaDistribution_t.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_unsetId(DistribInverseGammaDistribution_t *
  digd)
{
  return (digd != NULL) ? digd->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this
 * DistribInverseGammaDistribution_t.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_unsetName(DistribInverseGammaDistribution_t *
  digd)
{
  return (digd != NULL) ? digd->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "shape" element of this
 * DistribInverseGammaDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribInverseGammaDistribution_getShape(const
  DistribInverseGammaDistribution_t * digd)
{
  if (digd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(digd->getShape());
}


/*
 * Returns the value of the "scale" element of this
 * DistribInverseGammaDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribInverseGammaDistribution_getScale(const
  DistribInverseGammaDistribution_t * digd)
{
  if (digd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(digd->getScale());
}


/*
 * Predicate returning @c 1 (true) if this DistribInverseGammaDistribution_t's
 * "shape" element is set.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_isSetShape(const
  DistribInverseGammaDistribution_t * digd)
{
  return (digd != NULL) ? static_cast<int>(digd->isSetShape()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribInverseGammaDistribution_t's
 * "scale" element is set.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_isSetScale(const
  DistribInverseGammaDistribution_t * digd)
{
  return (digd != NULL) ? static_cast<int>(digd->isSetScale()) : 0;
}


/*
 * Sets the value of the "shape" element of this
 * DistribInverseGammaDistribution_t.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_setShape(
                                         DistribInverseGammaDistribution_t *
                                           digd,
                                         const DistribUncertValue_t* shape)
{
  return (digd != NULL) ? digd->setShape(shape) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "scale" element of this
 * DistribInverseGammaDistribution_t.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_setScale(
                                         DistribInverseGammaDistribution_t *
                                           digd,
                                         const DistribUncertValue_t* scale)
{
  return (digd != NULL) ? digd->setScale(scale) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribInverseGammaDistribution_t object and returns the
 * DistribUncertValue_t object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribInverseGammaDistribution_createShape(DistribInverseGammaDistribution_t*
  digd)
{
  if (digd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(digd->createShape());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribInverseGammaDistribution_t object and returns the
 * DistribUncertValue_t object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribInverseGammaDistribution_createScale(DistribInverseGammaDistribution_t*
  digd)
{
  if (digd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(digd->createScale());
}


/*
 * Unsets the value of the "shape" element of this
 * DistribInverseGammaDistribution_t.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_unsetShape(DistribInverseGammaDistribution_t *
  digd)
{
  return (digd != NULL) ? digd->unsetShape() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "scale" element of this
 * DistribInverseGammaDistribution_t.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_unsetScale(DistribInverseGammaDistribution_t *
  digd)
{
  return (digd != NULL) ? digd->unsetScale() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribInverseGammaDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_hasRequiredAttributes(const
  DistribInverseGammaDistribution_t * digd)
{
  return (digd != NULL) ? static_cast<int>(digd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribInverseGammaDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribInverseGammaDistribution_hasRequiredElements(const
  DistribInverseGammaDistribution_t * digd)
{
  return (digd != NULL) ? static_cast<int>(digd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


