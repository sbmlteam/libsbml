/**
 * @file DistribWeibullDistribution.cpp
 * @brief Implementation of the DistribWeibullDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribWeibullDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribWeibullDistribution using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
DistribWeibullDistribution::DistribWeibullDistribution(unsigned int level,
                                                       unsigned int version,
                                                       unsigned int pkgVersion)
  : DistribContinuousUnivariateDistribution(level, version)
  , mScale (NULL)
  , mShape (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribWeibullDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribWeibullDistribution::DistribWeibullDistribution(DistribPkgNamespaces
  *distribns)
  : DistribContinuousUnivariateDistribution(distribns)
  , mScale (NULL)
  , mShape (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribWeibullDistribution.
 */
DistribWeibullDistribution::DistribWeibullDistribution(const
  DistribWeibullDistribution& orig)
  : DistribContinuousUnivariateDistribution( orig )
  , mScale ( NULL )
  , mShape ( NULL )
{
  if (orig.mScale != NULL)
  {
    mScale = orig.mScale->clone();
  }

  if (orig.mShape != NULL)
  {
    mShape = orig.mShape->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribWeibullDistribution.
 */
DistribWeibullDistribution&
DistribWeibullDistribution::operator=(const DistribWeibullDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribContinuousUnivariateDistribution::operator=(rhs);
    delete mScale;
    if (rhs.mScale != NULL)
    {
      mScale = rhs.mScale->clone();
    }
    else
    {
      mScale = NULL;
    }

    delete mShape;
    if (rhs.mShape != NULL)
    {
      mShape = rhs.mShape->clone();
    }
    else
    {
      mShape = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribWeibullDistribution object.
 */
DistribWeibullDistribution*
DistribWeibullDistribution::clone() const
{
  return new DistribWeibullDistribution(*this);
}


/*
 * Destructor for DistribWeibullDistribution.
 */
DistribWeibullDistribution::~DistribWeibullDistribution()
{
  delete mScale;
  mScale = NULL;
  delete mShape;
  mShape = NULL;
}


/*
 * Returns the value of the "id" attribute of this DistribWeibullDistribution.
 */
const std::string&
DistribWeibullDistribution::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * DistribWeibullDistribution.
 */
const std::string&
DistribWeibullDistribution::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribWeibullDistribution's "id"
 * attribute is set.
 */
bool
DistribWeibullDistribution::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribWeibullDistribution's "name"
 * attribute is set.
 */
bool
DistribWeibullDistribution::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::unsetId()
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
 * Unsets the value of the "name" attribute of this DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::unsetName()
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
 * Returns the value of the "scale" element of this DistribWeibullDistribution.
 */
const DistribUncertValue*
DistribWeibullDistribution::getScale() const
{
  return mScale;
}


/*
 * Returns the value of the "scale" element of this DistribWeibullDistribution.
 */
DistribUncertValue*
DistribWeibullDistribution::getScale()
{
  return mScale;
}


/*
 * Returns the value of the "shape" element of this DistribWeibullDistribution.
 */
const DistribUncertValue*
DistribWeibullDistribution::getShape() const
{
  return mShape;
}


/*
 * Returns the value of the "shape" element of this DistribWeibullDistribution.
 */
DistribUncertValue*
DistribWeibullDistribution::getShape()
{
  return mShape;
}


/*
 * Predicate returning @c true if this DistribWeibullDistribution's "scale"
 * element is set.
 */
bool
DistribWeibullDistribution::isSetScale() const
{
  return (mScale != NULL);
}


/*
 * Predicate returning @c true if this DistribWeibullDistribution's "shape"
 * element is set.
 */
bool
DistribWeibullDistribution::isSetShape() const
{
  return (mShape != NULL);
}


/*
 * Sets the value of the "scale" element of this DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::setScale(const DistribUncertValue* scale)
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
 * Sets the value of the "shape" element of this DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::setShape(const DistribUncertValue* shape)
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
 * Creates a new DistribUncertValue object, adds it to this
 * DistribWeibullDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribWeibullDistribution::createScale()
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
 * Creates a new DistribUncertValue object, adds it to this
 * DistribWeibullDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribWeibullDistribution::createShape()
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
 * Unsets the value of the "scale" element of this DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::unsetScale()
{
  delete mScale;
  mScale = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "shape" element of this DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::unsetShape()
{
  delete mShape;
  mShape = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribWeibullDistribution object.
 */
const std::string&
DistribWeibullDistribution::getElementName() const
{
  static const string name = "weibullDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribWeibullDistribution object.
 */
int
DistribWeibullDistribution::getTypeCode() const
{
  return SBML_DISTRIB_WEIBULLDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribWeibullDistribution object have been set.
 */
bool
DistribWeibullDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribWeibullDistribution object have been set.
 */
bool
DistribWeibullDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredElements();

  if (isSetScale() == false)
  {
    allPresent = false;
  }

  if (isSetShape() == false)
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
DistribWeibullDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeElements(stream);

  if (isSetScale() == true)
  {
    mScale->write(stream);
  }

  if (isSetShape() == true)
  {
    mShape->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribWeibullDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mScale != NULL)
  {
    mScale->accept(v);
  }

  if (mShape != NULL)
  {
    mShape->accept(v);
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
DistribWeibullDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribContinuousUnivariateDistribution::setSBMLDocument(d);

  if (mScale != NULL)
  {
    mScale->setSBMLDocument(d);
  }

  if (mShape != NULL)
  {
    mShape->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribWeibullDistribution::connectToChild()
{
  DistribContinuousUnivariateDistribution::connectToChild();

  if (mScale != NULL)
  {
    mScale->connectToParent(this);
  }

  if (mShape != NULL)
  {
    mShape->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribWeibullDistribution::enablePackageInternal(const std::string& pkgURI,
                                                  const std::string& pkgPrefix,
                                                  bool flag)
{
  DistribContinuousUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetScale())
  {
    mScale->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetShape())
  {
    mShape->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribWeibullDistribution::updateSBMLNamespace(const std::string& package,
                                                unsigned int level,
                                                unsigned int version)
{
  DistribContinuousUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mScale != NULL)
  {
    mScale->updateSBMLNamespace(package, level, version);
  }

  if (mShape != NULL)
  {
    mShape->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::getAttribute(const std::string& attributeName,
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
 * DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::getAttribute(const std::string& attributeName,
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
 * DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::getAttribute(const std::string& attributeName,
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
 * DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::getAttribute(const std::string& attributeName,
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
 * DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::getAttribute(const std::string& attributeName,
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
 * Predicate returning @c true if this DistribWeibullDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribWeibullDistribution::isSetAttribute(const std::string& attributeName)
  const
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
 * DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::setAttribute(const std::string& attributeName,
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
 * DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::setAttribute(const std::string& attributeName,
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
 * DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::setAttribute(const std::string& attributeName,
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
 * DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::setAttribute(const std::string& attributeName,
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
 * DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::setAttribute(const std::string& attributeName,
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
 * DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::unsetAttribute(const std::string& attributeName)
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
 * DistribWeibullDistribution.
 */
SBase*
DistribWeibullDistribution::createChildObject(const std::string& elementName)
{
  DistribContinuousUnivariateDistribution* obj = NULL;

  if (elementName == "scale")
  {
    return createScale();
  }
  else if (elementName == "shape")
  {
    return createShape();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribWeibullDistribution.
 */
int
DistribWeibullDistribution::addChildObject(const std::string& elementName,
                                           const SBase* element)
{
  if (elementName == "scale" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setScale((const DistribUncertValue*)(element));
  }
  else if (elementName == "shape" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setShape((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribWeibullDistribution.
 */
SBase*
DistribWeibullDistribution::removeChildObject(const std::string& elementName,
                                              const std::string& id)
{
  if (elementName == "scale")
  {
    DistribUncertValue * obj = getScale();
    if (unsetScale() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "shape")
  {
    DistribUncertValue * obj = getShape();
    if (unsetShape() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribWeibullDistribution.
 */
unsigned int
DistribWeibullDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "scale")
  {
    if (isSetScale())
    {
      return 1;
    }
  }
  else if (elementName == "shape")
  {
    if (isSetShape())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribWeibullDistribution.
 */
SBase*
DistribWeibullDistribution::getObject(const std::string& elementName,
                                      unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "scale")
  {
    return getScale();
  }
  else if (elementName == "shape")
  {
    return getShape();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribWeibullDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

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

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
DistribWeibullDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

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

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
DistribWeibullDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mScale, filter);
  ADD_FILTERED_POINTER(ret, sublist, mShape, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribWeibullDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "scale")
  {
    if (isSetScale())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribWeibullDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mScale;
    mScale = new DistribUncertValue(distribns);
    mScale->setElementName(name);
    obj = mScale;
  }
  else if (name == "shape")
  {
    if (isSetShape())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribWeibullDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mShape;
    mShape = new DistribUncertValue(distribns);
    mShape->setElementName(name);
    obj = mShape;
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
DistribWeibullDistribution::addExpectedAttributes(ExpectedAttributes&
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
DistribWeibullDistribution::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribWeibullDistributionAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribWeibullDistributionAllowedCoreAttributes, pkgVersion,
            level, version, details);
      }
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    readL3V1V1Attributes(attributes);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    readL3V2V1Attributes(attributes);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribWeibullDistribution::readL3V1V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // id SId (use = "optional" )
  // 

  XMLTriple tripleID("id", mURI, getPrefix());
  assigned = attributes.readInto(tripleID, mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribWeibullDistribution>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("distrib", DistribIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  XMLTriple tripleNAME("name", mURI, getPrefix());
  assigned = attributes.readInto(tripleNAME, mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<DistribWeibullDistribution>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribWeibullDistribution::readL3V2V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribWeibullDistribution>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("distrib", DistribIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  // read by SBase;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribWeibullDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    writeL3V1V1Attributes(stream);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    writeL3V2V1Attributes(stream);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribWeibullDistribution::writeL3V1V1Attributes(XMLOutputStream& stream)
  const
{
  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribWeibullDistribution::writeL3V2V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribWeibullDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribWeibullDistribution_t *
DistribWeibullDistribution_create(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion)
{
  return new DistribWeibullDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribWeibullDistribution_t object.
 */
LIBSBML_EXTERN
DistribWeibullDistribution_t*
DistribWeibullDistribution_clone(const DistribWeibullDistribution_t* dwd)
{
  if (dwd != NULL)
  {
    return static_cast<DistribWeibullDistribution_t*>(dwd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribWeibullDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribWeibullDistribution_free(DistribWeibullDistribution_t* dwd)
{
  if (dwd != NULL)
  {
    delete dwd;
  }
}


/*
 * Returns the value of the "id" attribute of this
 * DistribWeibullDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribWeibullDistribution_getId(const DistribWeibullDistribution_t * dwd)
{
  if (dwd == NULL)
  {
    return NULL;
  }

  return dwd->getId().empty() ? NULL : safe_strdup(dwd->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * DistribWeibullDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribWeibullDistribution_getName(const DistribWeibullDistribution_t * dwd)
{
  if (dwd == NULL)
  {
    return NULL;
  }

  return dwd->getName().empty() ? NULL : safe_strdup(dwd->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribWeibullDistribution_t's "id"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_isSetId(const DistribWeibullDistribution_t * dwd)
{
  return (dwd != NULL) ? static_cast<int>(dwd->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribWeibullDistribution_t's
 * "name" attribute is set.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_isSetName(const DistribWeibullDistribution_t * dwd)
{
  return (dwd != NULL) ? static_cast<int>(dwd->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DistribWeibullDistribution_t.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_setId(DistribWeibullDistribution_t * dwd,
                                 const char * id)
{
  return (dwd != NULL) ? dwd->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this DistribWeibullDistribution_t.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_setName(DistribWeibullDistribution_t * dwd,
                                   const char * name)
{
  return (dwd != NULL) ? dwd->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this DistribWeibullDistribution_t.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_unsetId(DistribWeibullDistribution_t * dwd)
{
  return (dwd != NULL) ? dwd->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this
 * DistribWeibullDistribution_t.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_unsetName(DistribWeibullDistribution_t * dwd)
{
  return (dwd != NULL) ? dwd->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "scale" element of this
 * DistribWeibullDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribWeibullDistribution_getScale(const DistribWeibullDistribution_t * dwd)
{
  if (dwd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dwd->getScale());
}


/*
 * Returns the value of the "shape" element of this
 * DistribWeibullDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribWeibullDistribution_getShape(const DistribWeibullDistribution_t * dwd)
{
  if (dwd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dwd->getShape());
}


/*
 * Predicate returning @c 1 (true) if this DistribWeibullDistribution_t's
 * "scale" element is set.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_isSetScale(const DistribWeibullDistribution_t * dwd)
{
  return (dwd != NULL) ? static_cast<int>(dwd->isSetScale()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribWeibullDistribution_t's
 * "shape" element is set.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_isSetShape(const DistribWeibullDistribution_t * dwd)
{
  return (dwd != NULL) ? static_cast<int>(dwd->isSetShape()) : 0;
}


/*
 * Sets the value of the "scale" element of this DistribWeibullDistribution_t.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_setScale(DistribWeibullDistribution_t * dwd,
                                    const DistribUncertValue_t* scale)
{
  return (dwd != NULL) ? dwd->setScale(scale) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "shape" element of this DistribWeibullDistribution_t.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_setShape(DistribWeibullDistribution_t * dwd,
                                    const DistribUncertValue_t* shape)
{
  return (dwd != NULL) ? dwd->setShape(shape) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribWeibullDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribWeibullDistribution_createScale(DistribWeibullDistribution_t* dwd)
{
  if (dwd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dwd->createScale());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribWeibullDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribWeibullDistribution_createShape(DistribWeibullDistribution_t* dwd)
{
  if (dwd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dwd->createShape());
}


/*
 * Unsets the value of the "scale" element of this
 * DistribWeibullDistribution_t.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_unsetScale(DistribWeibullDistribution_t * dwd)
{
  return (dwd != NULL) ? dwd->unsetScale() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "shape" element of this
 * DistribWeibullDistribution_t.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_unsetShape(DistribWeibullDistribution_t * dwd)
{
  return (dwd != NULL) ? dwd->unsetShape() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribWeibullDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_hasRequiredAttributes(const
  DistribWeibullDistribution_t * dwd)
{
  return (dwd != NULL) ? static_cast<int>(dwd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribWeibullDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribWeibullDistribution_hasRequiredElements(const
  DistribWeibullDistribution_t * dwd)
{
  return (dwd != NULL) ? static_cast<int>(dwd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


