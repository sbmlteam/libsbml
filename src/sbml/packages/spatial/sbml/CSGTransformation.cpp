/**
 * @file:   CSGTransformation.cpp
 * @brief:  Implementation of the CSGTransformation class
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


#include <sbml/packages/spatial/sbml/CSGTransformation.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>


#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>
#include <sbml/packages/spatial/sbml/CSGPseudoPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGSetOperator.h>



using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CSGTransformation with the given level, version, and package version.
 */
CSGTransformation::CSGTransformation (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : CSGNode(level, version)
  , mCsgNode (NULL)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new CSGTransformation with the given SpatialPkgNamespaces object.
 */
CSGTransformation::CSGTransformation (SpatialPkgNamespaces* spatialns)
  : CSGNode(spatialns)
  , mCsgNode (NULL)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGTransformation.
 */
CSGTransformation::CSGTransformation (const CSGTransformation& orig)
  : CSGNode(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    if (orig.mCsgNode != NULL)
    {
      mCsgNode = orig.mCsgNode->clone();
    }
    else
    {
      mCsgNode = NULL;
    }

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for CSGTransformation.
 */
CSGTransformation&
CSGTransformation::operator=(const CSGTransformation& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    CSGNode::operator=(rhs);
    if (rhs.mCsgNode != NULL)
    {
      mCsgNode = rhs.mCsgNode->clone();
    }
    else
    {
      mCsgNode = NULL;
    }

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for CSGTransformation.
 */
CSGTransformation*
CSGTransformation::clone () const
{
  return new CSGTransformation(*this);
}


/*
 * Destructor for CSGTransformation.
 */
CSGTransformation::~CSGTransformation ()
{
  delete mCsgNode;
  mCsgNode = NULL;
}


/*
 * Returns the value of the "csgNode" attribute of this CSGTransformation.
 */
const CSGNode*
CSGTransformation::getCsgNode() const
{
  return mCsgNode;
}


/*
 * Returns the value of the "csgNode" attribute of this CSGTransformation.
 */
CSGNode*
CSGTransformation::getCsgNode()
{
  return mCsgNode;
}


/*
 * Creates a new "csgNode" element of this CSGTransformation and returns it.
 */
CSGPrimitive*
CSGTransformation::createCsgPrimitive()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGPrimitive(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGPrimitive*>(mCsgNode);
}


/*
 * Creates a new "csgNode" element of this CSGTransformation and returns it.
 */
CSGTranslation*
CSGTransformation::createCsgTranslation()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGTranslation(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGTranslation*>(mCsgNode);
}


/*
 * Creates a new "csgNode" element of this CSGTransformation and returns it.
 */
CSGRotation*
CSGTransformation::createCsgRotation()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGRotation(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGRotation*>(mCsgNode);
}


/*
 * Creates a new "csgNode" element of this CSGTransformation and returns it.
 */
CSGScale*
CSGTransformation::createCsgScale()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGScale(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGScale*>(mCsgNode);
}


/*
 * Creates a new "csgNode" element of this CSGTransformation and returns it.
 */
CSGHomogeneousTransformation*
CSGTransformation::createCsgHomogeneousTransformation()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGHomogeneousTransformation(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGHomogeneousTransformation*>(mCsgNode);
}


/*
 * Creates a new "csgNode" element of this CSGTransformation and returns it.
 */
CSGPseudoPrimitive*
CSGTransformation::createCsgPseudoPrimitive()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGPseudoPrimitive(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGPseudoPrimitive*>(mCsgNode);
}


/*
 * Creates a new "csgNode" element of this CSGTransformation and returns it.
 */
CSGSetOperator*
CSGTransformation::createCsgSetOperator()
{
  if (mCsgNode != NULL) delete mCsgNode;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCsgNode = new CSGSetOperator(spatialns);
  delete spatialns;
  connectToChild();
  return static_cast<CSGSetOperator*>(mCsgNode);
}


/*
 * Returns true/false if csgNode is set.
 */
bool
CSGTransformation::isSetCsgNode() const
{
  return (mCsgNode != NULL);
}


/*
 * Sets csgNode and returns value indicating success.
 */
int
CSGTransformation::setCsgNode(CSGNode* csgNode)
{
  if (mCsgNode == csgNode)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (csgNode == NULL)
  {
    delete mCsgNode;
    mCsgNode = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mCsgNode;
    mCsgNode = (csgNode != NULL) ?
      static_cast<CSGNode*>(csgNode->clone()) : NULL;
    if (mCsgNode != NULL)
    {
      mCsgNode->connectToParent(this);
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets csgNode and returns value indicating success.
 */
int
CSGTransformation::unsetCsgNode()
{
  delete mCsgNode;
  mCsgNode = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Return @c true if of type CSGTranslation.
 */
bool
CSGTransformation::isCSGTranslation() const
{
  return dynamic_cast<const CSGTranslation*>(this) != NULL;
}


/*
 * Return @c true if of type CSGRotation.
 */
bool
CSGTransformation::isCSGRotation() const
{
  return dynamic_cast<const CSGRotation*>(this) != NULL;
}


/*
 * Return @c true if of type CSGScale.
 */
bool
CSGTransformation::isCSGScale() const
{
  return dynamic_cast<const CSGScale*>(this) != NULL;
}


/*
 * Return @c true if of type CSGHomogeneousTransformation.
 */
bool
CSGTransformation::isCSGHomogeneousTransformation() const
{
  return dynamic_cast<const CSGHomogeneousTransformation*>(this) != NULL;
}


List*
CSGTransformation::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mCsgNode, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
CSGTransformation::getElementName () const
{
  static const string name = "csgTransformation";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CSGTransformation::getTypeCode () const
{
  return SBML_SPATIAL_CSGTRANSFORMATION;
}


/*
 * check if all the required attributes are set
 */
bool
CSGTransformation::hasRequiredAttributes () const
{
  bool allPresent = CSGNode::hasRequiredAttributes();

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
CSGTransformation::hasRequiredElements () const
{
  bool allPresent = CSGNode::hasRequiredElements();

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CSGTransformation::writeElements (XMLOutputStream& stream) const
{
  CSGNode::writeElements(stream);
  if (isSetCsgNode() == true)
  {
    mCsgNode->write(stream);
  }
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGTransformation::accept (SBMLVisitor& v) const
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
CSGTransformation::setSBMLDocument (SBMLDocument* d)
{
  CSGNode::setSBMLDocument(d);
  if (mCsgNode != NULL)
    mCsgNode->setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
CSGTransformation::connectToChild()
{
  CSGNode::connectToChild();

  if (mCsgNode != NULL)
    mCsgNode->connectToParent(this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CSGTransformation::enablePackageInternal(const std::string& pkgURI,
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
CSGTransformation::createObject(XMLInputStream& stream)
{
  SBase* object = CSGNode::createObject(stream);

  const string& name = stream.peek().getName();

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "csgPrimitive")
  {
    mCsgNode = new CSGPrimitive(spatialns);
    object = mCsgNode;
  }
  else if (name == "csgTranslation")
  {
    mCsgNode = new CSGTranslation(spatialns);
    object = mCsgNode;
  }
  else if (name == "csgRotation")
  {
    mCsgNode = new CSGRotation(spatialns);
    object = mCsgNode;
  }
  else if (name == "csgScale")
  {
    mCsgNode = new CSGScale(spatialns);
    object = mCsgNode;
  }
  else if (name == "csgHomogeneousTransformation")
  {
    mCsgNode = new CSGHomogeneousTransformation(spatialns);
    object = mCsgNode;
  }
  else if (name == "csgPseudoPrimitive")
  {
    mCsgNode = new CSGPseudoPrimitive(spatialns);
    object = mCsgNode;
  }
  else if (name == "csgSetOperator")
  {
    mCsgNode = new CSGSetOperator(spatialns);
    object = mCsgNode;
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
CSGTransformation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGNode::addExpectedAttributes(attributes);

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CSGTransformation::readAttributes (const XMLAttributes& attributes,
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

  //bool assigned = false;

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
CSGTransformation::writeAttributes (XMLOutputStream& stream) const
{
  CSGNode::writeAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
CSGTransformation_t *
CSGTransformation_create(unsigned int level, unsigned int version,
                         unsigned int pkgVersion)
{
  return new CSGTransformation(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CSGTransformation_free(CSGTransformation_t * csgt)
{
  if (csgt != NULL)
    delete csgt;
}


LIBSBML_EXTERN
CSGTransformation_t *
CSGTransformation_clone(CSGTransformation_t * csgt)
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


LIBSBML_EXTERN
CSGNode_t*
CSGTransformation_getCsgNode(CSGTransformation_t * csgt)
{
	if (csgt == NULL)
		return NULL;

	return (CSGNode_t*)csgt->getCsgNode();
}


LIBSBML_EXTERN
CSGPrimitive_t *
CSGTransformation_createCsgPrimitive(CSGTransformation_t * csgt)
{
	return  (csgt != NULL) ? csgt->createCsgPrimitive() : NULL;
}

LIBSBML_EXTERN
CSGTranslation_t *
CSGTransformation_createCsgTranslation(CSGTransformation_t * csgt)
{
	return  (csgt != NULL) ? csgt->createCsgTranslation() : NULL;
}

LIBSBML_EXTERN
CSGRotation_t *
CSGTransformation_createCsgRotation(CSGTransformation_t * csgt)
{
	return  (csgt != NULL) ? csgt->createCsgRotation() : NULL;
}

LIBSBML_EXTERN
CSGScale_t *
CSGTransformation_createCsgScale(CSGTransformation_t * csgt)
{
	return  (csgt != NULL) ? csgt->createCsgScale() : NULL;
}

LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGTransformation_createCsgHomogeneousTransformation(CSGTransformation_t * csgt)
{
	return  (csgt != NULL) ? csgt->createCsgHomogeneousTransformation() : NULL;
}

LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGTransformation_createCsgPseudoPrimitive(CSGTransformation_t * csgt)
{
	return  (csgt != NULL) ? csgt->createCsgPseudoPrimitive() : NULL;
}

LIBSBML_EXTERN
CSGSetOperator_t *
CSGTransformation_createCsgSetOperator(CSGTransformation_t * csgt)
{
	return  (csgt != NULL) ? csgt->createCsgSetOperator() : NULL;
}

LIBSBML_EXTERN
int
CSGTransformation_isSetCsgNode(const CSGTransformation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->isSetCsgNode()) : 0;
}


LIBSBML_EXTERN
int
CSGTransformation_setCsgNode(CSGTransformation_t * csgt, CSGNode_t* csgNode)
{
	return (csgt != NULL) ? csgt->setCsgNode(csgNode) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGTransformation_hasRequiredAttributes(const CSGTransformation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
CSGTransformation_hasRequiredElements(const CSGTransformation_t * csgt)
{
	return (csgt != NULL) ? static_cast<int>(csgt->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


