/**
 * @file CSGObject.cpp
 * @brief Implementation of the CSGObject class.
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
#include <sbml/packages/spatial/sbml/CSGObject.h>
#include <sbml/packages/spatial/sbml/ListOfCSGObjects.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>
#include <sbml/packages/spatial/sbml/CSGSetOperator.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new CSGObject using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
CSGObject::CSGObject(unsigned int level,
                     unsigned int version,
                     unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mDomainType ("")
  , mOrdinal (SBML_INT_MAX)
  , mIsSetOrdinal (false)
  , mCSGNode (NULL)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new CSGObject using the given SpatialPkgNamespaces object.
 */
CSGObject::CSGObject(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mId ("")
  , mDomainType ("")
  , mOrdinal (SBML_INT_MAX)
  , mIsSetOrdinal (false)
  , mCSGNode (NULL)
{
  setElementNamespace(spatialns->getURI());
  connectToChild();
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGObject.
 */
CSGObject::CSGObject(const CSGObject& orig)
  : SBase( orig )
  , mId ( orig.mId )
  , mDomainType ( orig.mDomainType )
  , mOrdinal ( orig.mOrdinal )
  , mIsSetOrdinal ( orig.mIsSetOrdinal )
  , mCSGNode ( NULL )
{
  if (orig.mCSGNode != NULL)
  {
    mCSGNode = orig.mCSGNode->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for CSGObject.
 */
CSGObject&
CSGObject::operator=(const CSGObject& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId = rhs.mId;
    mDomainType = rhs.mDomainType;
    mOrdinal = rhs.mOrdinal;
    mIsSetOrdinal = rhs.mIsSetOrdinal;
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
 * Creates and returns a deep copy of this CSGObject object.
 */
CSGObject*
CSGObject::clone() const
{
  return new CSGObject(*this);
}


/*
 * Destructor for CSGObject.
 */
CSGObject::~CSGObject()
{
  delete mCSGNode;
  mCSGNode = NULL;
}


/*
 * Returns the value of the "id" attribute of this CSGObject.
 */
const std::string&
CSGObject::getId() const
{
  return mId;
}


/*
 * Returns the value of the "domainType" attribute of this CSGObject.
 */
const std::string&
CSGObject::getDomainType() const
{
  return mDomainType;
}


/*
 * Returns the value of the "ordinal" attribute of this CSGObject.
 */
int
CSGObject::getOrdinal() const
{
  return mOrdinal;
}


/*
 * Predicate returning @c true if this CSGObject's "id" attribute is set.
 */
bool
CSGObject::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this CSGObject's "domainType" attribute is
 * set.
 */
bool
CSGObject::isSetDomainType() const
{
  return (mDomainType.empty() == false);
}


/*
 * Predicate returning @c true if this CSGObject's "ordinal" attribute is set.
 */
bool
CSGObject::isSetOrdinal() const
{
  return mIsSetOrdinal;
}


/*
 * Sets the value of the "id" attribute of this CSGObject.
 */
int
CSGObject::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "domainType" attribute of this CSGObject.
 */
int
CSGObject::setDomainType(const std::string& domainType)
{
  if (!(SyntaxChecker::isValidInternalSId(domainType)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDomainType = domainType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "ordinal" attribute of this CSGObject.
 */
int
CSGObject::setOrdinal(int ordinal)
{
  mOrdinal = ordinal;
  mIsSetOrdinal = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this CSGObject.
 */
int
CSGObject::unsetId()
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
 * Unsets the value of the "domainType" attribute of this CSGObject.
 */
int
CSGObject::unsetDomainType()
{
  mDomainType.erase();

  if (mDomainType.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "ordinal" attribute of this CSGObject.
 */
int
CSGObject::unsetOrdinal()
{
  mOrdinal = SBML_INT_MAX;
  mIsSetOrdinal = false;

  if (isSetOrdinal() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the value of the "csgNode" element of this CSGObject.
 */
const CSGNode*
CSGObject::getCSGNode() const
{
  return mCSGNode;
}


/*
 * Returns the value of the "csgNode" element of this CSGObject.
 */
CSGNode*
CSGObject::getCSGNode()
{
  return mCSGNode;
}


/*
 * Predicate returning @c true if this CSGObject's "csgNode" element is set.
 */
bool
CSGObject::isSetCSGNode() const
{
  return (mCSGNode != NULL);
}


/*
 * Sets the value of the "csgNode" element of this CSGObject.
 */
int
CSGObject::setCSGNode(const CSGNode* csgNode)
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
 * Creates a new CSGPrimitive object, adds it to this CSGObject object and
 * returns the CSGPrimitive object created.
 */
CSGPrimitive*
CSGObject::createCSGPrimitive()
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
 * Creates a new CSGTranslation object, adds it to this CSGObject object and
 * returns the CSGTranslation object created.
 */
CSGTranslation*
CSGObject::createCSGTranslation()
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
 * Creates a new CSGRotation object, adds it to this CSGObject object and
 * returns the CSGRotation object created.
 */
CSGRotation*
CSGObject::createCSGRotation()
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
 * Creates a new CSGScale object, adds it to this CSGObject object and returns
 * the CSGScale object created.
 */
CSGScale*
CSGObject::createCSGScale()
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
 * Creates a new CSGHomogeneousTransformation object, adds it to this CSGObject
 * object and returns the CSGHomogeneousTransformation object created.
 */
CSGHomogeneousTransformation*
CSGObject::createCSGHomogeneousTransformation()
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
 * Creates a new CSGSetOperator object, adds it to this CSGObject object and
 * returns the CSGSetOperator object created.
 */
CSGSetOperator*
CSGObject::createCSGSetOperator()
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
 * Unsets the value of the "csgNode" element of this CSGObject.
 */
int
CSGObject::unsetCSGNode()
{
  delete mCSGNode;
  mCSGNode = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * @copydoc doc_renamesidref_common
 */
void
CSGObject::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetDomainType() && mDomainType == oldid)
  {
    setDomainType(newid);
  }
}


/*
 * Returns the XML element name of this CSGObject object.
 */
const std::string&
CSGObject::getElementName() const
{
  static const string name = "csgObject";
  return name;
}


/*
 * Returns the libSBML type code for this CSGObject object.
 */
int
CSGObject::getTypeCode() const
{
  return SBML_SPATIAL_CSGOBJECT;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * CSGObject object have been set.
 */
bool
CSGObject::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetId() == false)
  {
    allPresent = false;
  }

  if (isSetDomainType() == false)
  {
    allPresent = false;
  }

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this CSGObject
 * object have been set.
 */
bool
CSGObject::hasRequiredElements() const
{
  bool allPresent = true;

  if (isSetCSGNode() == false)
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
CSGObject::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

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
CSGObject::accept(SBMLVisitor& v) const
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
CSGObject::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

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
CSGObject::connectToChild()
{
  SBase::connectToChild();

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
CSGObject::enablePackageInternal(const std::string& pkgURI,
                                 const std::string& pkgPrefix,
                                 bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  if (isSetCSGNode())
  {
    mCSGNode->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGObject.
 */
int
CSGObject::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGObject.
 */
int
CSGObject::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "ordinal")
  {
    value = getOrdinal();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGObject.
 */
int
CSGObject::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGObject.
 */
int
CSGObject::getAttribute(const std::string& attributeName,
                        unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGObject.
 */
int
CSGObject::getAttribute(const std::string& attributeName,
                        std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "domainType")
  {
    value = getDomainType();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this CSGObject's attribute "attributeName" is
 * set.
 */
bool
CSGObject::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "domainType")
  {
    value = isSetDomainType();
  }
  else if (attributeName == "ordinal")
  {
    value = isSetOrdinal();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGObject.
 */
int
CSGObject::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGObject.
 */
int
CSGObject::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "ordinal")
  {
    return_value = setOrdinal(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGObject.
 */
int
CSGObject::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGObject.
 */
int
CSGObject::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGObject.
 */
int
CSGObject::setAttribute(const std::string& attributeName,
                        const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "domainType")
  {
    return_value = setDomainType(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this CSGObject.
 */
int
CSGObject::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "domainType")
  {
    value = unsetDomainType();
  }
  else if (attributeName == "ordinal")
  {
    value = unsetOrdinal();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this CSGObject.
 */
SBase*
CSGObject::createObject(const std::string& elementName)
{
  SBase* obj = NULL;

  //if (elementName == "csgNode")
  //{
  //  return createCSGNode();
  //}

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this CSGObject.
 */
unsigned int
CSGObject::getNumObjects(const std::string& elementName)
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
 * Returns the nth object of "objectName" in this CSGObject.
 */
SBase*
CSGObject::getObject(const std::string& elementName, unsigned int index)
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
CSGObject::getElementBySId(const std::string& id)
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
CSGObject::getElementByMetaId(const std::string& metaid)
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
CSGObject::getAllElements(ElementFilter* filter)
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
CSGObject::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "csgPrimitive")
  {
    if (mCSGNode != NULL)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialCSGObjectAllowedElements, getPackageVersion(),
        getLevel(), getVersion());

      delete mCSGNode;
      mCSGNode = NULL;
    }
    
    mCSGNode = new CSGPrimitive(spatialns);
    obj = mCSGNode;
  }
  else if (name == "csgTranslation")
  {
    if (mCSGNode != NULL)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialCSGObjectAllowedElements, getPackageVersion(),
        getLevel(), getVersion());

      delete mCSGNode;
      mCSGNode = NULL;
    }

    mCSGNode = new CSGTranslation(spatialns);
    obj = mCSGNode;
  }
  else if (name == "csgRotation")
  {
    if (mCSGNode != NULL)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialCSGObjectAllowedElements, getPackageVersion(),
        getLevel(), getVersion());

      delete mCSGNode;
      mCSGNode = NULL;
    }

    mCSGNode = new CSGRotation(spatialns);
    obj = mCSGNode;
  }
  else if (name == "csgScale")
  {
    if (mCSGNode != NULL)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialCSGObjectAllowedElements, getPackageVersion(),
        getLevel(), getVersion());

      delete mCSGNode;
      mCSGNode = NULL;
    }

    mCSGNode = new CSGScale(spatialns);
    obj = mCSGNode;
  }
  else if (name == "csgHomogeneousTransformation")
  {
    if (mCSGNode != NULL)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialCSGObjectAllowedElements, getPackageVersion(),
        getLevel(), getVersion());

      delete mCSGNode;
      mCSGNode = NULL;
    }

    mCSGNode = new CSGHomogeneousTransformation(spatialns);
    obj = mCSGNode;
  }
  else if (name == "csgSetOperator")
  {
    if (mCSGNode != NULL)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialCSGObjectAllowedElements, getPackageVersion(),
        getLevel(), getVersion());

      delete mCSGNode;
      mCSGNode = NULL;
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
CSGObject::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("domainType");

  attributes.add("ordinal");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CSGObject::readAttributes(const XMLAttributes& attributes,
                          const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (static_cast<ListOfCSGObjects*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialCSGObjectAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialCSGeometryLOCSGObjectsAllowedCoreAttributes, pkgVersion, level,
            version, details);
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);
  numErrs = log->getNumErrors();

  for (int n = numErrs-1; n >= 0; n--)
  {
    if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
    {
      const std::string details = log->getError(n)->getMessage();
      log->remove(UnknownPackageAttribute);
      log->logPackageError("spatial", SpatialCSGObjectAllowedAttributes,
        pkgVersion, level, version, details);
    }
    else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
    {
      const std::string details = log->getError(n)->getMessage();
      log->remove(UnknownCoreAttribute);
      log->logPackageError("spatial", SpatialCSGObjectAllowedCoreAttributes,
        pkgVersion, level, version, details);
    }
  }

  // 
  // id SId (use = "required" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<CSGObject>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      logError(SpatialIdSyntaxRule, level, version, "The id '" + mId + "' does "
        "not conform to the syntax.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from the "
      "<CSGObject> element.";
    log->logPackageError("spatial", SpatialCSGObjectAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // domainType SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("domainType", mDomainType);

  if (assigned == true)
  {
    if (mDomainType.empty() == true)
    {
      logEmptyString(mDomainType, level, version, "<CSGObject>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomainType) == false)
    {
      logError(SpatialCSGObjectDomainTypeMustBeDomainType, level, version, "The "
        "attribute domainType='" + mDomainType + "' does not conform to the "
          "syntax.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domainType' is missing from the "
      "<CSGObject> element.";
    log->logPackageError("spatial", SpatialCSGObjectAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // ordinal int (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetOrdinal = attributes.readInto("ordinal", mOrdinal);

  if ( mIsSetOrdinal == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'ordinal' from the <CSGObject> "
        "element must be an integer.";
      log->logPackageError("spatial", SpatialCSGObjectOrdinalMustBeInteger,
        pkgVersion, level, version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
CSGObject::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetDomainType() == true)
  {
    stream.writeAttribute("domainType", getPrefix(), mDomainType);
  }

  if (isSetOrdinal() == true)
  {
    stream.writeAttribute("ordinal", getPrefix(), mOrdinal);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new CSGObject_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGObject_t *
CSGObject_create(unsigned int level,
                 unsigned int version,
                 unsigned int pkgVersion)
{
  return new CSGObject(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CSGObject_t object.
 */
LIBSBML_EXTERN
CSGObject_t*
CSGObject_clone(const CSGObject_t* csgo)
{
  if (csgo != NULL)
  {
    return static_cast<CSGObject_t*>(csgo->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this CSGObject_t object.
 */
LIBSBML_EXTERN
void
CSGObject_free(CSGObject_t* csgo)
{
  if (csgo != NULL)
  {
    delete csgo;
  }
}


/*
 * Returns the value of the "id" attribute of this CSGObject_t.
 */
LIBSBML_EXTERN
const char *
CSGObject_getId(const CSGObject_t * csgo)
{
  if (csgo == NULL)
  {
    return NULL;
  }

  return csgo->getId().empty() ? NULL : safe_strdup(csgo->getId().c_str());
}


/*
 * Returns the value of the "domainType" attribute of this CSGObject_t.
 */
LIBSBML_EXTERN
const char *
CSGObject_getDomainType(const CSGObject_t * csgo)
{
  if (csgo == NULL)
  {
    return NULL;
  }

  return csgo->getDomainType().empty() ? NULL :
    safe_strdup(csgo->getDomainType().c_str());
}


/*
 * Returns the value of the "ordinal" attribute of this CSGObject_t.
 */
LIBSBML_EXTERN
int
CSGObject_getOrdinal(const CSGObject_t * csgo)
{
  return (csgo != NULL) ? csgo->getOrdinal() : SBML_INT_MAX;
}


/*
 * Predicate returning @c 1 if this CSGObject_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
CSGObject_isSetId(const CSGObject_t * csgo)
{
  return (csgo != NULL) ? static_cast<int>(csgo->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 if this CSGObject_t's "domainType" attribute is
 * set.
 */
LIBSBML_EXTERN
int
CSGObject_isSetDomainType(const CSGObject_t * csgo)
{
  return (csgo != NULL) ? static_cast<int>(csgo->isSetDomainType()) : 0;
}


/*
 * Predicate returning @c 1 if this CSGObject_t's "ordinal" attribute is set.
 */
LIBSBML_EXTERN
int
CSGObject_isSetOrdinal(const CSGObject_t * csgo)
{
  return (csgo != NULL) ? static_cast<int>(csgo->isSetOrdinal()) : 0;
}


/*
 * Sets the value of the "id" attribute of this CSGObject_t.
 */
LIBSBML_EXTERN
int
CSGObject_setId(CSGObject_t * csgo, const char * id)
{
  return (csgo != NULL) ? csgo->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "domainType" attribute of this CSGObject_t.
 */
LIBSBML_EXTERN
int
CSGObject_setDomainType(CSGObject_t * csgo, const char * domainType)
{
  return (csgo != NULL) ? csgo->setDomainType(domainType) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "ordinal" attribute of this CSGObject_t.
 */
LIBSBML_EXTERN
int
CSGObject_setOrdinal(CSGObject_t * csgo, int ordinal)
{
  return (csgo != NULL) ? csgo->setOrdinal(ordinal) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this CSGObject_t.
 */
LIBSBML_EXTERN
int
CSGObject_unsetId(CSGObject_t * csgo)
{
  return (csgo != NULL) ? csgo->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "domainType" attribute of this CSGObject_t.
 */
LIBSBML_EXTERN
int
CSGObject_unsetDomainType(CSGObject_t * csgo)
{
  return (csgo != NULL) ? csgo->unsetDomainType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "ordinal" attribute of this CSGObject_t.
 */
LIBSBML_EXTERN
int
CSGObject_unsetOrdinal(CSGObject_t * csgo)
{
  return (csgo != NULL) ? csgo->unsetOrdinal() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "csgNode" element of this CSGObject_t.
 */
LIBSBML_EXTERN
const CSGNode_t*
CSGObject_getCSGNode(const CSGObject_t * csgo)
{
  if (csgo == NULL)
  {
    return NULL;
  }

  return (CSGNode_t*)(csgo->getCSGNode());
}


/*
 * Predicate returning @c 1 if this CSGObject_t's "csgNode" element is set.
 */
LIBSBML_EXTERN
int
CSGObject_isSetCSGNode(const CSGObject_t * csgo)
{
  return (csgo != NULL) ? static_cast<int>(csgo->isSetCSGNode()) : 0;
}


/*
 * Sets the value of the "csgNode" element of this CSGObject_t.
 */
LIBSBML_EXTERN
int
CSGObject_setCSGNode(CSGObject_t * csgo, const CSGNode_t* csgNode)
{
  return (csgo != NULL) ? csgo->setCSGNode(csgNode) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new CSGPrimitive_t object, adds it to this CSGObject_t object and
 * returns the CSGPrimitive_t object created.
 */
LIBSBML_EXTERN
CSGPrimitive_t*
CSGObject_createCSGPrimitive(CSGObject_t* csgo)
{
  return (csgo != NULL) ? csgo->createCSGPrimitive() : NULL;
}


/*
 * Creates a new CSGTranslation_t object, adds it to this CSGObject_t object
 * and returns the CSGTranslation_t object created.
 */
LIBSBML_EXTERN
CSGTranslation_t*
CSGObject_createCSGTranslation(CSGObject_t* csgo)
{
  return (csgo != NULL) ? csgo->createCSGTranslation() : NULL;
}


/*
 * Creates a new CSGRotation_t object, adds it to this CSGObject_t object and
 * returns the CSGRotation_t object created.
 */
LIBSBML_EXTERN
CSGRotation_t*
CSGObject_createCSGRotation(CSGObject_t* csgo)
{
  return (csgo != NULL) ? csgo->createCSGRotation() : NULL;
}


/*
 * Creates a new CSGScale_t object, adds it to this CSGObject_t object and
 * returns the CSGScale_t object created.
 */
LIBSBML_EXTERN
CSGScale_t*
CSGObject_createCSGScale(CSGObject_t* csgo)
{
  return (csgo != NULL) ? csgo->createCSGScale() : NULL;
}


/*
 * Creates a new CSGHomogeneousTransformation_t object, adds it to this
 * CSGObject_t object and returns the CSGHomogeneousTransformation_t object
 * created.
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t*
CSGObject_createCSGHomogeneousTransformation(CSGObject_t* csgo)
{
  return (csgo != NULL) ? csgo->createCSGHomogeneousTransformation() : NULL;
}


/*
 * Creates a new CSGSetOperator_t object, adds it to this CSGObject_t object
 * and returns the CSGSetOperator_t object created.
 */
LIBSBML_EXTERN
CSGSetOperator_t*
CSGObject_createCSGSetOperator(CSGObject_t* csgo)
{
  return (csgo != NULL) ? csgo->createCSGSetOperator() : NULL;
}


/*
 * Unsets the value of the "csgNode" element of this CSGObject_t.
 */
LIBSBML_EXTERN
int
CSGObject_unsetCSGNode(CSGObject_t * csgo)
{
  return (csgo != NULL) ? csgo->unsetCSGNode() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if all the required attributes for this CSGObject_t
 * object have been set.
 */
LIBSBML_EXTERN
int
CSGObject_hasRequiredAttributes(const CSGObject_t * csgo)
{
  return (csgo != NULL) ? static_cast<int>(csgo->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 if all the required elements for this CSGObject_t
 * object have been set.
 */
LIBSBML_EXTERN
int
CSGObject_hasRequiredElements(const CSGObject_t * csgo)
{
  return (csgo != NULL) ? static_cast<int>(csgo->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


