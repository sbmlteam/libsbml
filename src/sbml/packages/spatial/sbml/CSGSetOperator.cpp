/**
 * @file    CSGSetOperator.cpp
 * @brief   Implementation of CSGSetOperator, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGSetOperator.cpp  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGSetOperator.cpp $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <iostream>
#include <limits>

#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/packages/spatial/sbml/CSGSetOperator.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGPseudoPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGSetOperator.h>
#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>


#include <sbml/SBase.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new CSGSetOperator with the given level, version, and package version.
 */
CSGSetOperator::CSGSetOperator (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : CSGNode (SBML_SPATIAL_CSGSETOPERATOR, level,version)
   , mOperationType("")
   , mCSGNodeChildren (level,version, pkgVersion)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion)); 

  if (!hasValidLevelVersionNamespaceCombination())
	throw SBMLConstructorException();

  connectToChild();
}


/*
 * Creates a new CSGSetOperator with the given SpatialPkgNamespaces object.
 */
CSGSetOperator::CSGSetOperator(SpatialPkgNamespaces* spatialns)
 : CSGNode (SBML_SPATIAL_CSGSETOPERATOR, spatialns)
  , mOperationType("")
  , mCSGNodeChildren (spatialns)
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(spatialns->getURI());

  if (!hasValidLevelVersionNamespaceCombination())
  {
    std::string err(getElementName());
    XMLNamespaces* xmlns = spatialns->getNamespaces();
    if (xmlns)
    {
      std::ostringstream oss;
      XMLOutputStream xos(oss);
      xos << *xmlns;
      err.append(oss.str());
    }
    throw SBMLConstructorException(err);
  }

  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor.
 */
CSGSetOperator::CSGSetOperator(const CSGSetOperator& source) : CSGNode(source)
{
  this->mOperationType=source.mOperationType;
  this->mCSGNodeChildren = source.mCSGNodeChildren;

  connectToChild();
}

/*
 * Assignment operator.
 */
CSGSetOperator& CSGSetOperator::operator=(const CSGSetOperator& source)
{
  if(&source!=this)
  {
    this->CSGNode::operator=(source);
	this->mOperationType = source.mOperationType;
	this->mCSGNodeChildren = source.mCSGNodeChildren;
  }

  connectToChild();
  
  return *this;
}

/*
 * Destructor.
 */ 
CSGSetOperator::~CSGSetOperator ()
{
}

/*
  * Returns the value of the "operationType" attribute of this CSGSetOperator.
  */
const std::string& 
CSGSetOperator::getOperationType () const
{
  return mOperationType;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CSGSetOperator's "operationType" attribute has been set.
  */
bool 
CSGSetOperator::isSetOperationType () const
{
  return (mOperationType.empty() == false);
}

/*
  * Sets the value of the "operationType" attribute of this CSGSetOperator.
  */
int 
CSGSetOperator::setOperationType (const std::string& operationType)
{
  return SyntaxChecker::checkAndSetSId(operationType ,mOperationType);
}

 /*
  * Unsets the value of the "operationType" attribute of this CSGSetOperator.
  */
int 
CSGSetOperator::unsetOperationType ()
{
  mOperationType.erase();
  if (mOperationType.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Adds a copy of the given CSGNode to this CSGSetOperator.
 */
int
CSGSetOperator::addCSGNodeChild (const CSGNode* n)
{
  if (n == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(n->hasRequiredAttributes()) || !(n->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getCSGNodeChild(n->getId()) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    mCSGNodeChildren.append(n); 
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Creates a new CSGPrimitive for this CSGSetOperator and returns it.  
 */
CSGPrimitive*
CSGSetOperator::createCSGPrimitive ()
{
  //new CSGPrimitive(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  CSGPrimitive* n = CSGNode::create_CSGPrimitive();
  this->mCSGNodeChildren.appendAndOwn(n);
  return n;
}

/*
 * Creates a new CSGPseudoPrimitive for this CSGSetOperator and returns it.  
 */
CSGPseudoPrimitive*
CSGSetOperator::createCSGPseudoPrimitive ()
{
  //CSGPseudoPrimitive* n = new CSGPseudoPrimitive(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  CSGPseudoPrimitive* n = CSGNode::create_CSGPseudoPrimitive();
  this->mCSGNodeChildren.appendAndOwn(n);
  return n;
}

/*
 * Creates a new CSGSetOperator for this CSGSetOperator and returns it.  
 */
CSGSetOperator*
CSGSetOperator::createCSGSetOperator ()
{
  //* n = new CSGSetOperator(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  CSGSetOperator* n = CSGNode::create_CSGSetOperator();
  this->mCSGNodeChildren.appendAndOwn(n);
  return n;
}

/*
 * Creates a new CSGTranslation for this CSGSetOperator and returns it.  
 */
CSGTranslation*
CSGSetOperator::createCSGTranslation ()
{
  //* n = new CSGTranslation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  CSGTranslation* n = CSGNode::create_CSGTranslation();
  this->mCSGNodeChildren.appendAndOwn(n);
  return n;
}

/*
 * Creates a new CSGRotation for this CSGSetOperator and returns it.  
 */
CSGRotation*
CSGSetOperator::createCSGRotation ()
{
  //* n = new CSGRotation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  CSGRotation* n = CSGNode::create_CSGRotation();
  this->mCSGNodeChildren.appendAndOwn(n);
  return n;
}

/*
 * Creates a new CSGScale for this CSGSetOperator and returns it.  
 */
CSGScale*
CSGSetOperator::createCSGScale ()
{
  //* n = new CSGScale(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  CSGScale* n = CSGNode::create_CSGScale();
  this->mCSGNodeChildren.appendAndOwn(n);
  return n;
}

/*
 * Creates a new CSGHomogeneousTransformation for this CSGSetOperator and returns it.  
 */
CSGHomogeneousTransformation*
CSGSetOperator::createCSGHomogeneousTransformation ()
{
  //* n = new CSGHomogeneousTransformation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  CSGHomogeneousTransformation* n = CSGNode::create_CSGHomogeneousTransformation();
  this->mCSGNodeChildren.appendAndOwn(n);
  return n;
}

/*
 * @return the list of CSGNodes for this CSGSetOperator.
 */
const ListOfCSGNodes*
CSGSetOperator::getListOfCSGNodeChildren () const
{
	return &this->mCSGNodeChildren;
}

/*
 * @return the list of CSGNodes for this CSGSetOperator.
 */
ListOfCSGNodes*
CSGSetOperator::getListOfCSGNodeChildren ()
{
	return &this->mCSGNodeChildren;
}

/*
 * @return the nth CSGNode object of this CSGSetOperator.
 */
const CSGNode*
CSGSetOperator::getCSGNodeChild (unsigned int n) const
{
  return mCSGNodeChildren.get(n);
}


/*
 * @return the nth CSGNode of this CSGSetOperator.
 */
CSGNode*
CSGSetOperator::getCSGNodeChild (unsigned int n)
{
  return mCSGNodeChildren.get(n);
}

/*
 * @return the CSGNode object in this CSGSetOperator with the given id or NULL
 * if no such CSGNode exists.
 */
const CSGNode*
CSGSetOperator::getCSGNodeChild (const std::string& sid) const
{
  return mCSGNodeChildren.get(sid);
}

/*
 * @return the CSGNode object in this CSGSetOperator with the given id or NULL
 * if no such CSGNode exists.
 */
CSGNode*
CSGSetOperator::getCSGNodeChild (const std::string& sid)
{
  return mCSGNodeChildren.get(sid);
}

/*
 * @return the number of CSGNode in this CSGSetOperator.
 */
unsigned int
CSGSetOperator::getNumCSGNodeChildren () const
{
  return this->mCSGNodeChildren.size();
}

/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
  */
void
CSGSetOperator::connectToChild()
{	
	mCSGNodeChildren.connectToParent(this);
}

/**
 * Removes the nth CSGNode object from this CSGSetOperator object and
 * returns a pointer to it.
 */
CSGNode* 
CSGSetOperator::removeCSGNodeChild (unsigned int n)
{
  return mCSGNodeChildren.remove(n);
}


/**
 * Removes the CSGNode object with the given identifier from this CSGSetOperator
 * object and returns a pointer to it.
 */
CSGNode* 
CSGSetOperator::removeCSGNodeChild (const std::string& sid)
{
  return mCSGNodeChildren.remove(sid);
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 
const std::string&
CSGSetOperator::getElementName () const
{
  static const std::string name = "csgOperator";
  return name;
}
*/

/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
CSGSetOperator::createObject (XMLInputStream& stream)
{
  // return 0;
  
  const string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "listOfCSGNodes")
  {
    if (mCSGNodeChildren.size() != 0)
    {
      logError(NotSchemaConformant);
    }
    object = &mCSGNodeChildren;
  }
  
  return object;

}

/*
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
CSGSetOperator::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGNode::addExpectedAttributes(attributes);
  
  attributes.add("operationType");
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CSGSetOperator::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  CSGNode::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("operationType", mOperationType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mOperationType.empty())
  {
    logEmptyString(mOperationType, sbmlLevel, sbmlVersion, "<CSGSetOperator>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mOperationType)) logError(InvalidIdSyntax);

}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CSGSetOperator::writeAttributes (XMLOutputStream& stream) const
{
  CSGNode::writeAttributes(stream);

  stream.writeAttribute("operationType",   getPrefix(), mOperationType);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}


/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
CSGSetOperator::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getNumCSGNodeChildren() > 0 ) mCSGNodeChildren.write(stream);
  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}


CSGSetOperator*
CSGSetOperator::clone() const
{
    return new CSGSetOperator(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGSetOperator::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
  
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
CSGSetOperator::setSBMLDocument (SBMLDocument* d)
{
  CSGNode::setSBMLDocument(d);
  
  mCSGNodeChildren.setSBMLDocument(d);

}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
CSGSetOperator::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  CSGNode::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mCSGNodeChildren.enablePackageInternal(pkgURI,pkgPrefix,flag);
}
 

LIBSBML_CPP_NAMESPACE_END

