/**
 * @file CSGTransformation.cpp
 * @brief Implementation of the CSGTransformation class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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
#include <sbml/packages/spatial/sbml/CSGTransformation.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>
#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGSetOperator.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new CSGTransformation using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
CSGTransformation::CSGTransformation(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion)
  : CSGNode(level, version, pkgVersion)
  , mCSGNode (NULL)
  , mElementName("csgTransformation")
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new CSGTransformation using the given SpatialPkgNamespaces object.
 */
CSGTransformation::CSGTransformation(SpatialPkgNamespaces *spatialns)
  : CSGNode(spatialns)
  , mCSGNode (NULL)
  , mElementName("csgTransformation")
{
  setElementNamespace(spatialns->getURI());
  connectToChild();
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGTransformation.
 */
CSGTransformation::CSGTransformation(const CSGTransformation& orig)
  : CSGNode( orig )
  , mCSGNode ( NULL )
  , mElementName ( orig.mElementName )
{
  if (orig.mCSGNode != NULL)
  {
    mCSGNode = orig.mCSGNode->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for CSGTransformation.
 */
CSGTransformation&
CSGTransformation::operator=(const CSGTransformation& rhs)
{
  if (&rhs != this)
  {
    CSGNode::operator=(rhs);
    mElementName = rhs.mElementName;
    delete mCSGNode;
    if (rhs.mCSGNode != NULL)
    {
      mCSGNode = rhs.mCSGNode->clone();
    }
    else
    {
      mCSGNode = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this CSGTransformation object.
 */
CSGTransformation*
CSGTransformation::clone() const
{
  return new CSGTransformation(*this);
}


/*
 * Destructor for CSGTransformation.
 */
CSGTransformation::~CSGTransformation()
{
  delete mCSGNode;
  mCSGNode = NULL;
}


/*
 * Returns the value of the "csgNode" element of this CSGTransformation.
 */
const CSGNode*
CSGTransformation::getCSGNode() const
{
  return mCSGNode;
}


/*
 * Returns the value of the "csgNode" element of this CSGTransformation.
 */
CSGNode*
CSGTransformation::getCSGNode()
{
  return mCSGNode;
}


/*
 * Predicate returning @c true if this CSGTransformation's "csgNode" element is
 * set.
 */
bool
CSGTransformation::isSetCSGNode() const
{
  return (mCSGNode != NULL);
}


/*
 * Sets the value of the "csgNode" element of this CSGTransformation.
 */
int
CSGTransformation::setCSGNode(const CSGNode* csgNode)
{
  if (mCSGNode == csgNode)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (csgNode == NULL)
  {
    delete mCSGNode;
    mCSGNode = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mCSGNode;
    mCSGNode = (csgNode != NULL) ? csgNode->clone() : NULL;
    if (mCSGNode != NULL)
    {
      mCSGNode->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new CSGPrimitive object, adds it to this CSGTransformation object
 * and returns the CSGPrimitive object created.
 */
CSGPrimitive*
CSGTransformation::createCSGPrimitive()
{
  if (mCSGNode != NULL)
  {
    delete mCSGNode;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCSGNode = new CSGPrimitive(spatialns);

  delete spatialns;

  connectToChild();

  return static_cast<CSGPrimitive*>(mCSGNode);
}


/*
 * Creates a new CSGTranslation object, adds it to this CSGTransformation
 * object and returns the CSGTranslation object created.
 */
CSGTranslation*
CSGTransformation::createCSGTranslation()
{
  if (mCSGNode != NULL)
  {
    delete mCSGNode;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCSGNode = new CSGTranslation(spatialns);

  delete spatialns;

  connectToChild();

  return static_cast<CSGTranslation*>(mCSGNode);
}


/*
 * Creates a new CSGRotation object, adds it to this CSGTransformation object
 * and returns the CSGRotation object created.
 */
CSGRotation*
CSGTransformation::createCSGRotation()
{
  if (mCSGNode != NULL)
  {
    delete mCSGNode;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCSGNode = new CSGRotation(spatialns);

  delete spatialns;

  connectToChild();

  return static_cast<CSGRotation*>(mCSGNode);
}


/*
 * Creates a new CSGScale object, adds it to this CSGTransformation object and
 * returns the CSGScale object created.
 */
CSGScale*
CSGTransformation::createCSGScale()
{
  if (mCSGNode != NULL)
  {
    delete mCSGNode;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCSGNode = new CSGScale(spatialns);

  delete spatialns;

  connectToChild();

  return static_cast<CSGScale*>(mCSGNode);
}


/*
 * Creates a new CSGHomogeneousTransformation object, adds it to this
 * CSGTransformation object and returns the CSGHomogeneousTransformation object
 * created.
 */
CSGHomogeneousTransformation*
CSGTransformation::createCSGHomogeneousTransformation()
{
  if (mCSGNode != NULL)
  {
    delete mCSGNode;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCSGNode = new CSGHomogeneousTransformation(spatialns);

  delete spatialns;

  connectToChild();

  return static_cast<CSGHomogeneousTransformation*>(mCSGNode);
}


/*
 * Creates a new CSGSetOperator object, adds it to this CSGTransformation
 * object and returns the CSGSetOperator object created.
 */
CSGSetOperator*
CSGTransformation::createCSGSetOperator()
{
  if (mCSGNode != NULL)
  {
    delete mCSGNode;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCSGNode = new CSGSetOperator(spatialns);

  delete spatialns;

  connectToChild();

  return static_cast<CSGSetOperator*>(mCSGNode);
}


/*
 * Unsets the value of the "csgNode" element of this CSGTransformation.
 */
int
CSGTransformation::unsetCSGNode()
{
  delete mCSGNode;
  mCSGNode = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Predicate returning @c true if this abstract "CSGTransformation" is of type
 * CSGTranslation
 */
bool
CSGTransformation::isCSGTranslation() const
{
  return dynamic_cast<const CSGTranslation*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "CSGTransformation" is of type
 * CSGRotation
 */
bool
CSGTransformation::isCSGRotation() const
{
  return dynamic_cast<const CSGRotation*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "CSGTransformation" is of type
 * CSGScale
 */
bool
CSGTransformation::isCSGScale() const
{
  return dynamic_cast<const CSGScale*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract "CSGTransformation" is of type
 * CSGHomogeneousTransformation
 */
bool
CSGTransformation::isCSGHomogeneousTransformation() const
{
  return dynamic_cast<const CSGHomogeneousTransformation*>(this) != NULL;
}


/*
 * Returns the XML element name of this CSGTransformation object.
 */
const std::string&
CSGTransformation::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this CSGTransformation object.
 */
void
CSGTransformation::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this CSGTransformation object.
 */
int
CSGTransformation::getTypeCode() const
{
  return SBML_SPATIAL_CSGTRANSFORMATION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * CSGTransformation object have been set.
 */
bool
CSGTransformation::hasRequiredAttributes() const
{
  bool allPresent = CSGNode::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * CSGTransformation object have been set.
 */
bool
CSGTransformation::hasRequiredElements() const
{
  bool allPresent = CSGNode::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
CSGTransformation::writeElements(XMLOutputStream& stream) const
{
  CSGNode::writeElements(stream);

  if (isSetCSGNode() == true)
  {
    mCSGNode->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
CSGTransformation::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mCSGNode != NULL)
  {
    mCSGNode->accept(v);
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
CSGTransformation::setSBMLDocument(SBMLDocument* d)
{
  CSGNode::setSBMLDocument(d);

  if (mCSGNode != NULL)
  {
    mCSGNode->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
CSGTransformation::connectToChild()
{
  CSGNode::connectToChild();

  if (mCSGNode != NULL)
  {
    mCSGNode->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
CSGTransformation::enablePackageInternal(const std::string& pkgURI,
                                         const std::string& pkgPrefix,
                                         bool flag)
{
  CSGNode::enablePackageInternal(pkgURI, pkgPrefix, flag);

  if (isSetCSGNode())
  {
    mCSGNode->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
CSGTransformation::updateSBMLNamespace(const std::string& package,
                                       unsigned int level,
                                       unsigned int version)
{
  CSGNode::updateSBMLNamespace(package, level, version);

  if (mCSGNode != NULL)
  {
    mCSGNode->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGTransformation.
 */
int
CSGTransformation::getAttribute(const std::string& attributeName,
                                bool& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGTransformation.
 */
int
CSGTransformation::getAttribute(const std::string& attributeName,
                                int& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGTransformation.
 */
int
CSGTransformation::getAttribute(const std::string& attributeName,
                                double& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGTransformation.
 */
int
CSGTransformation::getAttribute(const std::string& attributeName,
                                unsigned int& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGTransformation.
 */
int
CSGTransformation::getAttribute(const std::string& attributeName,
                                std::string& value) const
{
  int return_value = CSGNode::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this CSGTransformation's attribute
 * "attributeName" is set.
 */
bool
CSGTransformation::isSetAttribute(const std::string& attributeName) const
{
  bool value = CSGNode::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGTransformation.
 */
int
CSGTransformation::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGTransformation.
 */
int
CSGTransformation::setAttribute(const std::string& attributeName, int value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGTransformation.
 */
int
CSGTransformation::setAttribute(const std::string& attributeName,
                                double value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGTransformation.
 */
int
CSGTransformation::setAttribute(const std::string& attributeName,
                                unsigned int value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGTransformation.
 */
int
CSGTransformation::setAttribute(const std::string& attributeName,
                                const std::string& value)
{
  int return_value = CSGNode::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this CSGTransformation.
 */
int
CSGTransformation::unsetAttribute(const std::string& attributeName)
{
  int value = CSGNode::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this CSGTransformation.
 */
SBase*
CSGTransformation::createChildObject(const std::string& elementName)
{
  CSGNode* obj = NULL;

  if (elementName == "csgPrimitive")
  {
    return createCSGPrimitive();
  }
  else if (elementName == "csgTranslation")
  {
    return createCSGTranslation();
  }
  else if (elementName == "csgRotation")
  {
    return createCSGRotation();
  }
  else if (elementName == "csgScale")
  {
    return createCSGScale();
  }
  else if (elementName == "csgHomogeneousTransformation")
  {
    return createCSGHomogeneousTransformation();
  }
  else if (elementName == "csgSetOperator")
  {
    return createCSGSetOperator();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this CSGTransformation.
 */
int
CSGTransformation::addChildObject(const std::string& elementName,
                                  const SBase* element)
{
  if (elementName == "csgPrimitive" && element->getTypeCode() ==
    SBML_SPATIAL_CSGPRIMITIVE)
  {
    return setCSGNode((const CSGNode*)(element));
  }
  else if (elementName == "csgTranslation" && element->getTypeCode() ==
    SBML_SPATIAL_CSGTRANSLATION)
  {
    return setCSGNode((const CSGNode*)(element));
  }
  else if (elementName == "csgRotation" && element->getTypeCode() ==
    SBML_SPATIAL_CSGROTATION)
  {
    return setCSGNode((const CSGNode*)(element));
  }
  else if (elementName == "csgScale" && element->getTypeCode() ==
    SBML_SPATIAL_CSGSCALE)
  {
    return setCSGNode((const CSGNode*)(element));
  }
  else if (elementName == "csgHomogeneousTransformation" &&
    element->getTypeCode() == SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION)
  {
    return setCSGNode((const CSGNode*)(element));
  }
  else if (elementName == "csgSetOperator" && element->getTypeCode() ==
    SBML_SPATIAL_CSGSETOPERATOR)
  {
    return setCSGNode((const CSGNode*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * CSGTransformation.
 */
SBase*
CSGTransformation::removeChildObject(const std::string& elementName,
                                     const std::string& id)
{
  if (elementName == "csgPrimitive")
  {
    CSGNode * obj = getCSGNode();
    if (unsetCSGNode() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "csgTranslation")
  {
    CSGNode * obj = getCSGNode();
    if (unsetCSGNode() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "csgRotation")
  {
    CSGNode * obj = getCSGNode();
    if (unsetCSGNode() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "csgScale")
  {
    CSGNode * obj = getCSGNode();
    if (unsetCSGNode() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "csgHomogeneousTransformation")
  {
    CSGNode * obj = getCSGNode();
    if (unsetCSGNode() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "csgSetOperator")
  {
    CSGNode * obj = getCSGNode();
    if (unsetCSGNode() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this CSGTransformation.
 */
unsigned int
CSGTransformation::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "csgNode")
  {
    if (isSetCSGNode())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this CSGTransformation.
 */
SBase*
CSGTransformation::getObject(const std::string& elementName,
                             unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "csgNode")
  {
    return getCSGNode();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
CSGTransformation::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mCSGNode != NULL)
  {
    if (mCSGNode->getId() == id)
    {
      return mCSGNode;
    }

    obj = mCSGNode->getElementBySId(id);
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
CSGTransformation::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mCSGNode != NULL)
  {
    if (mCSGNode->getMetaId() == metaid)
    {
      return mCSGNode;
    }

    obj = mCSGNode->getElementByMetaId(metaid);
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
CSGTransformation::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mCSGNode, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
CSGTransformation::createObject(XMLInputStream& stream)
{
  SBase* obj = CSGNode::createObject(stream);

  const std::string& name = stream.peek().getName();

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  SpatialSBMLErrorCode_t err = SpatialCSGTranslationAllowedElements;

  if (getTypeCode() == SBML_SPATIAL_CSGTRANSLATION)
  {
   err = SpatialCSGTranslationAllowedElements;
  }
  else if (getTypeCode() == SBML_SPATIAL_CSGROTATION)
  {
    err = SpatialCSGRotationAllowedElements;
  }
  else if (getTypeCode() == SBML_SPATIAL_CSGSCALE)
  {
    err = SpatialCSGScaleAllowedElements;
  }
  else if (getTypeCode() == SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION)
  {
    err = SpatialCSGHomogeneousTransformationAllowedElements;
  }

  if (name == "csgPrimitive")
  {
    if (isSetCSGNode())
    {
      getErrorLog()->logPackageError("spatial", err, getPackageVersion(), getLevel(),
        getVersion(), "", getLine(), getColumn());
      delete obj;
      return NULL;
    }
    mCSGNode = new CSGPrimitive(spatialns);
    obj = mCSGNode;
  }
  else if (name == "csgTranslation")
  {
    if (isSetCSGNode())
    {
      getErrorLog()->logPackageError("spatial", err, getPackageVersion(), getLevel(),
        getVersion(), "", getLine(), getColumn());
      delete obj;
      return NULL;
    }
    mCSGNode = new CSGTranslation(spatialns);
    obj = mCSGNode;
  }
  else if (name == "csgRotation")
  {
    if (isSetCSGNode())
    {
      getErrorLog()->logPackageError("spatial", err, getPackageVersion(), getLevel(),
        getVersion(), "", getLine(), getColumn());
      delete obj;
      return NULL;
    }
    mCSGNode = new CSGRotation(spatialns);
    obj = mCSGNode;
  }
  else if (name == "csgScale")
  {
    if (isSetCSGNode())
    {
      getErrorLog()->logPackageError("spatial", err, getPackageVersion(), getLevel(),
        getVersion(), "", getLine(), getColumn());
      delete obj;
      return NULL;
    }
    mCSGNode = new CSGScale(spatialns);
    obj = mCSGNode;
  }
  else if (name == "csgHomogeneousTransformation")
  {
    if (isSetCSGNode())
    {
      getErrorLog()->logPackageError("spatial", err, getPackageVersion(), getLevel(),
        getVersion(), "", getLine(), getColumn());
      delete obj;
      return NULL;
    }
    mCSGNode = new CSGHomogeneousTransformation(spatialns);
    obj = mCSGNode;
  }
  else if (name == "csgSetOperator")
  {
    if (isSetCSGNode())
    {
      getErrorLog()->logPackageError("spatial", err, getPackageVersion(), getLevel(),
        getVersion(), "", getLine(), getColumn());
      delete obj;
      return NULL;
    }
    mCSGNode = new CSGSetOperator(spatialns);
    obj = mCSGNode;
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
CSGTransformation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGNode::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CSGTransformation::readAttributes(const XMLAttributes& attributes,
                                  const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  CSGNode::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();
    SpatialSBMLErrorCode_t errcode = SpatialCSGTranslationAllowedAttributes;
    switch (getTypeCode()) {
    case SBML_SPATIAL_CSGTRANSLATION:
      errcode = SpatialCSGTranslationAllowedCoreAttributes;
      break;
    case SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION:
      errcode = SpatialCSGHomogeneousTransformationAllowedCoreAttributes;
      break;
    case SBML_SPATIAL_CSGSCALE:
      errcode = SpatialCSGScaleAllowedCoreAttributes;
      break;
    case SBML_SPATIAL_CSGROTATION:
      errcode = SpatialCSGRotationAllowedCoreAttributes;
      break;
    }

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialUnknown, pkgVersion, level,
          version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          errcode, pkgVersion, level,
            version, details, getLine(), getColumn());
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
CSGTransformation::writeAttributes(XMLOutputStream& stream) const
{
  CSGNode::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new CSGTransformation_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGTransformation_t *
CSGTransformation_create(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
{
  return new CSGTransformation(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CSGTransformation_t object.
 */
LIBSBML_EXTERN
CSGTransformation_t*
CSGTransformation_clone(const CSGTransformation_t* csgt)
{
  if (csgt != NULL)
  {
    return static_cast<CSGTransformation_t*>(csgt->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this CSGTransformation_t object.
 */
LIBSBML_EXTERN
void
CSGTransformation_free(CSGTransformation_t* csgt)
{
  if (csgt != NULL)
  {
    delete csgt;
  }
}


/*
 * Returns the value of the "csgNode" element of this CSGTransformation_t.
 */
LIBSBML_EXTERN
const CSGNode_t*
CSGTransformation_getCSGNode(const CSGTransformation_t * csgt)
{
  if (csgt == NULL)
  {
    return NULL;
  }

  return (CSGNode_t*)(csgt->getCSGNode());
}


/*
 * Predicate returning @c 1 (true) if this CSGTransformation_t's "csgNode"
 * element is set.
 */
LIBSBML_EXTERN
int
CSGTransformation_isSetCSGNode(const CSGTransformation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->isSetCSGNode()) : 0;
}


/*
 * Sets the value of the "csgNode" element of this CSGTransformation_t.
 */
LIBSBML_EXTERN
int
CSGTransformation_setCSGNode(CSGTransformation_t * csgt,
                             const CSGNode_t* csgNode)
{
  return (csgt != NULL) ? csgt->setCSGNode(csgNode) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new CSGPrimitive_t object, adds it to this CSGTransformation_t
 * object and returns the CSGPrimitive_t object created.
 */
LIBSBML_EXTERN
CSGPrimitive_t*
CSGTransformation_createCSGPrimitive(CSGTransformation_t* csgt)
{
  return (csgt != NULL) ? csgt->createCSGPrimitive() : NULL;
}


/*
 * Creates a new CSGTranslation_t object, adds it to this CSGTransformation_t
 * object and returns the CSGTranslation_t object created.
 */
LIBSBML_EXTERN
CSGTranslation_t*
CSGTransformation_createCSGTranslation(CSGTransformation_t* csgt)
{
  return (csgt != NULL) ? csgt->createCSGTranslation() : NULL;
}


/*
 * Creates a new CSGRotation_t object, adds it to this CSGTransformation_t
 * object and returns the CSGRotation_t object created.
 */
LIBSBML_EXTERN
CSGRotation_t*
CSGTransformation_createCSGRotation(CSGTransformation_t* csgt)
{
  return (csgt != NULL) ? csgt->createCSGRotation() : NULL;
}


/*
 * Creates a new CSGScale_t object, adds it to this CSGTransformation_t object
 * and returns the CSGScale_t object created.
 */
LIBSBML_EXTERN
CSGScale_t*
CSGTransformation_createCSGScale(CSGTransformation_t* csgt)
{
  return (csgt != NULL) ? csgt->createCSGScale() : NULL;
}


/*
 * Creates a new CSGHomogeneousTransformation_t object, adds it to this
 * CSGTransformation_t object and returns the CSGHomogeneousTransformation_t
 * object created.
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t*
CSGTransformation_createCSGHomogeneousTransformation(CSGTransformation_t* csgt)
{
  return (csgt != NULL) ? csgt->createCSGHomogeneousTransformation() : NULL;
}


/*
 * Creates a new CSGSetOperator_t object, adds it to this CSGTransformation_t
 * object and returns the CSGSetOperator_t object created.
 */
LIBSBML_EXTERN
CSGSetOperator_t*
CSGTransformation_createCSGSetOperator(CSGTransformation_t* csgt)
{
  return (csgt != NULL) ? csgt->createCSGSetOperator() : NULL;
}


/*
 * Unsets the value of the "csgNode" element of this CSGTransformation_t.
 */
LIBSBML_EXTERN
int
CSGTransformation_unsetCSGNode(CSGTransformation_t * csgt)
{
  return (csgt != NULL) ? csgt->unsetCSGNode() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this CSGTransformation_t is of type
 * CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_isCSGTranslation(const CSGTransformation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->isCSGTranslation()) : 0;
}


/*
 * Predicate returning @c 1 if this CSGTransformation_t is of type
 * CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_isCSGRotation(const CSGTransformation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->isCSGRotation()) : 0;
}


/*
 * Predicate returning @c 1 if this CSGTransformation_t is of type CSGScale_t
 */
LIBSBML_EXTERN
int
CSGTransformation_isCSGScale(const CSGTransformation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->isCSGScale()) : 0;
}


/*
 * Predicate returning @c 1 if this CSGTransformation_t is of type
 * CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_isCSGHomogeneousTransformation(const CSGTransformation_t *
  csgt)
{
  return (csgt != NULL) ?
    static_cast<int>(csgt->isCSGHomogeneousTransformation()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGTransformation_t object have been set.
 */
LIBSBML_EXTERN
int
CSGTransformation_hasRequiredAttributes(const CSGTransformation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * CSGTransformation_t object have been set.
 */
LIBSBML_EXTERN
int
CSGTransformation_hasRequiredElements(const CSGTransformation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


