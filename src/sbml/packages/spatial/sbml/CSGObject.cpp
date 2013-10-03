/**
 * @file    CSGObject.cpp
 * @brief   Implementation of CSGObject, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGObject.cpp 10670 2010-01-16 12:10:06Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGObject.cpp $
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

#include <sbml/packages/spatial/sbml/CSGObject.h>
#include <sbml/packages/spatial/sbml/CSGPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGPseudoPrimitive.h>
#include <sbml/packages/spatial/sbml/CSGSetOperator.h>
#include <sbml/packages/spatial/sbml/CSGTransformation.h>
#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/sbml/CSGHomogeneousTransformation.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

CSGObject::CSGObject (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId("")
   , mDomainType("")
   , mOrdinal(0)
   , mCSGNodeRoot (0)
   , mIsSetOrdinal(false)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion)); 

  if (!hasValidLevelVersionNamespaceCombination())
	throw SBMLConstructorException();

  connectToChild();
}


CSGObject::CSGObject(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mDomainType("")
  , mOrdinal(0)
  , mCSGNodeRoot (0)
  , mIsSetOrdinal(false)
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
CSGObject::CSGObject(const CSGObject& source) : SBase(source)
{
	this->mSpatialId=source.mSpatialId;
	this->mDomainType=source.mDomainType;
	this->mOrdinal=source.mOrdinal;
    this->mIsSetOrdinal=source.mIsSetOrdinal;

	if (source.mCSGNodeRoot)
	{
		mCSGNodeRoot = static_cast<CSGNode*>( source.mCSGNodeRoot->clone() );
	}

	connectToChild();
}

/*
 * Assignment operator.
 */
CSGObject& CSGObject::operator=(const CSGObject& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
	this->mSpatialId = source.mSpatialId;
	this->mDomainType = source.mDomainType;
	this->mOrdinal=source.mOrdinal;
	this->mIsSetOrdinal=source.mIsSetOrdinal;

  }

  delete mCSGNodeRoot;
  if (source.mCSGNodeRoot) 
  {
	  mCSGNodeRoot = static_cast<CSGNode*>( source.mCSGNodeRoot->clone() );
  }
  else
  {
	  mCSGNodeRoot = 0;
  }

  connectToChild();
  
  return *this;
}

/*
 * Destructor.
 */ 
CSGObject::~CSGObject ()
{
	delete mCSGNodeRoot;
}

/*
  * Returns the value of the "spatialId" attribute of this CSGObject.
  */
const std::string& 
CSGObject::getSpatialId () const
{
  return mSpatialId;
}

/*
  * Returns the value of the "domainType" attribute of this CSGObject.
  */
const std::string& 
CSGObject::getDomainType () const
{
  return mDomainType;
}

/*
 * @return the ordinal of this Domaintype.
 */
unsigned int
CSGObject::getOrdinal () const
{
	return mOrdinal;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CSGObject's "spatialId" attribute has been set.
  */
bool 
CSGObject::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CSGObject's "domainType" attribute has been set.
  */
bool 
CSGObject::isSetDomainType () const
{
  return (mDomainType.empty() == false);
}

/*
 * @return true if the spatialDimenions of this CSGObject has been set, false
 * otherwise.
 */
bool
CSGObject::isSetOrdinal () const
{
  return mIsSetOrdinal;
}

/*
  * Sets the value of the "spatialId" attribute of this CSGObject.
  */
int 
CSGObject::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
  * Sets the value of the "domain" attribute of this CSGObject.
  */
int 
CSGObject::setDomainType (const std::string& domainType)
{
  return SyntaxChecker::checkAndSetSId(domainType ,mDomainType);
}

/*
 * Sets the ordinal of this CSGObject to value.
 *
 * If value is not one of [0, 1, 2, 3] the function will have no effect
 * (i.e. ordinal will not be set).
 */
int
CSGObject::setOrdinal (unsigned int value)
{
    mOrdinal = value;
    mIsSetOrdinal  = true;
    return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Unsets the value of the "spatialId" attribute of this CSGObject.
 */
int 
CSGObject::unsetSpatialId ()
{
  mSpatialId.erase();
  if (mSpatialId.empty())
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
CSGObject::unsetDomainType ()
{
  mDomainType.erase();
  if (mDomainType.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Unsets the ordinal of this CSGObject.
 */
int
CSGObject::unsetOrdinal ()
{
  mIsSetOrdinal = false;
  
  if (!isSetOrdinal())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * @return the CSGNodeRoot of this CSGObject.
 */
const CSGNode*
CSGObject::getCSGNodeRoot () const
{
  return mCSGNodeRoot;
}

/*
 * @return true if the CSGNodeRoot of this CSGObject has been set, false
 * otherwise.
 */
bool
CSGObject::isSetCSGNodeRoot () const
{
  return (mCSGNodeRoot != 0);
}

/*
 * Sets the CSGNodeRoot of this CSGObject to a copy of the given CSGObject.
 */
int
CSGObject::setCSGNodeRoot (const CSGNode* csgn)
{
  if (mCSGNodeRoot == csgn)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (csgn == NULL)
  {
    delete mCSGNodeRoot;
    mCSGNodeRoot = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mCSGNodeRoot;
    mCSGNodeRoot = static_cast<CSGNode*>( csgn->clone() );

    if (mCSGNodeRoot) mCSGNodeRoot->connectToParent(this);
    
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Unsets the CSGNodeRoot of this CSGObject.
 */
int
CSGObject::unsetCSGNodeRoot ()
{
  delete mCSGNodeRoot;
  mCSGNodeRoot = 0;

  if (mCSGNodeRoot == NULL) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Creates a new CSGPrimitive for this CSGObject and returns it.  
 */
CSGPrimitive*
CSGObject::createCSGPrimitive ()
{
    CSGPrimitive* n = new CSGPrimitive(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	setCSGNodeRoot(n);
    return static_cast<CSGPrimitive*>(mCSGNodeRoot);
}

/*
 * Creates a new CSGPseudoPrimitive for this CSGObject and returns it.  
 */
CSGPseudoPrimitive*
CSGObject::createCSGPseudoPrimitive ()
{
  CSGPseudoPrimitive* n = new CSGPseudoPrimitive(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  setCSGNodeRoot(n);
  return static_cast<CSGPseudoPrimitive*>(mCSGNodeRoot);

}

/*
 * Creates a new CSGSetOperator for this CSGObject and returns it.  
 */
CSGSetOperator*
CSGObject::createCSGSetOperator ()
{
  CSGSetOperator* n = new CSGSetOperator(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  setCSGNodeRoot(n);
  return static_cast<CSGSetOperator*>(mCSGNodeRoot);

}

/*
 * Creates a new CSGTranslation for this CSGObject and returns it.  
 */
CSGTranslation*
CSGObject::createCSGTranslation ()
{
  CSGTranslation* n = new CSGTranslation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  setCSGNodeRoot(n);
  return static_cast<CSGTranslation*>(mCSGNodeRoot);
}

/*
 * Creates a new CSGRotation for this CSGObject and returns it.  
 */
CSGRotation*
CSGObject::createCSGRotation ()
{
  CSGRotation* n = new CSGRotation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  setCSGNodeRoot(n);
  return static_cast<CSGRotation*>(mCSGNodeRoot);
}

/*
 * Creates a new CSGScale for this CSGObject and returns it.  
 */
CSGScale*
CSGObject::createCSGScale ()
{
  CSGScale* n = new CSGScale(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  setCSGNodeRoot(n);
  return static_cast<CSGScale*>(mCSGNodeRoot);
}

/*
 * Creates a new CSGHomogeneousTransformation for this CSGObject and returns it.  
 */
CSGHomogeneousTransformation*
CSGObject::createCSGHomogeneousTransformation ()
{
  CSGHomogeneousTransformation* n = new CSGHomogeneousTransformation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  setCSGNodeRoot(n);
  return static_cast<CSGHomogeneousTransformation*>(mCSGNodeRoot);
}

/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
  */
void
CSGObject::connectToChild()
{
  SBase::connectToChild();
	if (mCSGNodeRoot) mCSGNodeRoot->connectToParent(this);
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
CSGObject::getElementName () const
{
  static const std::string name = "csgObject";
  return name;
}


/*
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
CSGObject::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);
  
  attributes.add("spatialId");
  attributes.add("domainType");
  attributes.add("ordinal");
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CSGObject::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<CSGObject>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("domainType", mDomainType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mDomainType.empty())
  {
    logEmptyString(mDomainType, sbmlLevel, sbmlVersion, "<CSGObject>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mDomainType)) logError(InvalidIdSyntax);

  attributes.readInto("ordinal", mOrdinal, getErrorLog(), false, getLine(), getColumn());
  /*if (mOrdinal < 0)
  {
    std::string message = "The ordinal attribute on ";
    message += "an <CSGObject> may only have values > 0.";
    getErrorLog()->logError(NotSchemaConformant, sbmlLevel, sbmlVersion, message);
  }*/

}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CSGObject::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);
  stream.writeAttribute("domainType",   getPrefix(), mDomainType);
  stream.writeAttribute("ordinal", getPrefix(), mOrdinal);

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
CSGObject::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (mCSGNodeRoot) mCSGNodeRoot->write(stream);
  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}


/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
CSGObject::getTypeCode () const
{
	return SBML_SPATIAL_CSGOBJECT;
}

CSGObject*
CSGObject::clone() const
{
    return new CSGObject(*this);
}

/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
CSGObject::createObject (XMLInputStream& stream)
{
  // return 0;
  const string& name   = stream.peek().getName();
  SBase*        object = 0;
  //object=SBase::createObject(stream);


  if (name == "csgTranslation")
  {
	if (mCSGNodeRoot) 
	{
	  logError(NotSchemaConformant);
	}
	delete mCSGNodeRoot;

	try
	{
		mCSGNodeRoot = new CSGTranslation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
	  mCSGNodeRoot = new CSGTranslation(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mCSGNodeRoot;
  } 

  if (name == "csgRotation")
  {
	if (mCSGNodeRoot) 
	{
	  logError(NotSchemaConformant);
	}
	delete mCSGNodeRoot;

	try
	{
		mCSGNodeRoot = new CSGRotation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
	  mCSGNodeRoot = new CSGRotation(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mCSGNodeRoot;
  } 

  if (name == "csgScale")
  {
	if (mCSGNodeRoot) 
	{
	  logError(NotSchemaConformant);
	}
	delete mCSGNodeRoot;

	try
	{
		mCSGNodeRoot = new CSGScale(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
	  mCSGNodeRoot = new CSGScale(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mCSGNodeRoot;
  } 

  if (name == "csgHomogeneousTransformation")
  {
	if (mCSGNodeRoot) 
	{
	  logError(NotSchemaConformant);
	}
	delete mCSGNodeRoot;

	try
	{
		mCSGNodeRoot = new CSGHomogeneousTransformation(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
	  mCSGNodeRoot = new CSGHomogeneousTransformation(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mCSGNodeRoot;
  } 

  if (name == "csgPrimitive")
  {
	if (mCSGNodeRoot) 
	{
	  logError(NotSchemaConformant);
	}
	delete mCSGNodeRoot;

	try
	{
		mCSGNodeRoot = new CSGPrimitive(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
		mCSGNodeRoot = new CSGPrimitive(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mCSGNodeRoot;
  }

  if (name == "csgPseudoPrimitive")
  {
	if (mCSGNodeRoot) 
	{
	  logError(NotSchemaConformant);
	}
	delete mCSGNodeRoot;

	try
	{
		mCSGNodeRoot = new CSGPseudoPrimitive(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
		mCSGNodeRoot = new CSGPseudoPrimitive(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mCSGNodeRoot;
  }

  if (name == "csgSetOperator") {
	if (mCSGNodeRoot) 
	{
	  logError(NotSchemaConformant);
	}
	delete mCSGNodeRoot;

	try
	{
		mCSGNodeRoot = new CSGSetOperator(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	}
	catch ( ... )
	{
		mCSGNodeRoot = new CSGSetOperator(SBMLDocument::getDefaultLevel(), SBMLDocument::getDefaultVersion());
	}
	object = mCSGNodeRoot;
  }

  return object;
}

/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGObject::accept (SBMLVisitor& v) const
{
    // return v.visit(*this);
  bool result = v.visit(*this);

  if (mCSGNodeRoot) mCSGNodeRoot->accept(v);

  return result;

}


/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
CSGObject::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  if (mCSGNodeRoot) mCSGNodeRoot->setSBMLDocument(d);
}


/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
CSGObject::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI,pkgPrefix,flag);

  if (mCSGNodeRoot) mCSGNodeRoot->enablePackageInternal(pkgURI,pkgPrefix,flag);

}


/** @cond doxygenCOnly */

/**
 * Creates and returns a deep copy of a given CSGObject_t structure.
 *
 * @param g the CSGObject_t structure to copy
 * 
 * @return a (deep) copy of this CSGObject_t structure.
 */
LIBSBML_EXTERN
CSGObject_t *
CSGObject_clone (const CSGObject_t *csg)
{
  return static_cast<CSGObject*>( csg->clone() );
}


/*
 * Ctor.
 */
ListOfCSGObjects::ListOfCSGObjects(SpatialPkgNamespaces* spatialns)
 : ListOf(spatialns)
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(spatialns->getURI());
}


/*
 * Ctor.
 */
ListOfCSGObjects::ListOfCSGObjects(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfCSGObjects.
 */
ListOfCSGObjects*
ListOfCSGObjects::clone () const
{
  return new ListOfCSGObjects(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfCSGObjects::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "csgObject")
  {
    SPATIAL_CREATE_NS(spatialns, this->getSBMLNamespaces());
	object = new CSGObject(spatialns);
	appendAndOwn(object);
  }

  return object;
}


/* return nth item in list */
CSGObject *
ListOfCSGObjects::get(unsigned int n)
{
  return static_cast<CSGObject*>(ListOf::get(n));
}


/* return nth item in list */
const CSGObject *
ListOfCSGObjects::get(unsigned int n) const
{
  return static_cast<const CSGObject*>(ListOf::get(n));
}


/* return item by spatialId */
CSGObject*
ListOfCSGObjects::get (const std::string& spatialId)
{
  return const_cast<CSGObject*>( 
    static_cast<const ListOfCSGObjects&>(*this).get(spatialId) );
}


/* return item by spatialId */
const CSGObject*
ListOfCSGObjects::get (const std::string& spatialId) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CSGObject>(spatialId) );
  return (result == mItems.end()) ? 0 : static_cast <CSGObject*> (*result);
}


/* Removes the nth item from this list */
CSGObject*
ListOfCSGObjects::remove (unsigned int n)
{
   return static_cast<CSGObject*>(ListOf::remove(n));
}


/* Removes item in this list by spatialId */
CSGObject*
ListOfCSGObjects::remove (const std::string& spatialId)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CSGObject>(spatialId) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <CSGObject*> (item);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfCSGObjects::getItemTypeCode () const
{
	return SBML_SPATIAL_CSGOBJECT;
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfCSGObjects::getElementName () const
{
  static const std::string name = "listOfCSGObjects";
  return name;
}


LIBSBML_CPP_NAMESPACE_END

