/**
 * @file    CoordinateComponent.cpp
 * @brief   Implementation of CoordinateComponent, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CoordinateComponent.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CoordinateComponent.cpp $
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

#include <sbml/packages/spatial/sbml/Boundary.h>

#include <sbml/packages/spatial/sbml/CoordinateComponent.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new CoordinateComponent with the given level, version, and package version.
 */
CoordinateComponent::CoordinateComponent (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId("")
   , mComponentType("")
   , mSbmlUnit("")
   , mIndex(0)
   , mBoundaryMin(NULL)
   , mBoundaryMax(NULL)
   , mIsSetIndex (false)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  

  if (!hasValidLevelVersionNamespaceCombination())
	throw SBMLConstructorException();

  connectToChild();

}


/*
 * Creates a new CoordinateComponent with the given SpatialPkgNamespaces object.
 */
CoordinateComponent::CoordinateComponent(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mComponentType("")
  , mSbmlUnit("")
  , mIndex(0)
  , mBoundaryMin(NULL)
  , mBoundaryMax(NULL)
  , mIsSetIndex (false)
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(spatialns->getURI());

  if (!hasValidLevelVersionNamespaceCombination())
	throw SBMLConstructorException();

  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor.
 */
CoordinateComponent::CoordinateComponent(const CoordinateComponent& source) 
  : SBase(source)
  , mBoundaryMin(NULL)
  , mBoundaryMax(NULL)
{
  this->mSpatialId=source.mSpatialId;
  this->mComponentType=source.mComponentType;
  this->mSbmlUnit=source.mSbmlUnit;
  this->mIndex=source.mIndex;
  this->mIsSetIndex=source.mIsSetIndex;

  if (source.isSetBoundaryMin())
  this->mBoundaryMin = source.mBoundaryMin->clone();
  if (source.isSetBoundaryMax())
  this->mBoundaryMax = source.mBoundaryMax->clone();

  connectToChild();
}

/*
 * Assignment operator.
 */
CoordinateComponent& CoordinateComponent::operator=(const CoordinateComponent& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
	  this->mSpatialId = source.mSpatialId;
    this->mComponentType = source.mComponentType;
	  this->mSbmlUnit = source.mSbmlUnit;
	  this->mIndex = source.mIndex;
	  this->mIsSetIndex=source.mIsSetIndex;
    
    if (source.isSetBoundaryMin())
    this->mBoundaryMin = source.mBoundaryMin->clone();
    else
      this->mBoundaryMin = NULL;
    if (source.isSetBoundaryMax())
    this->mBoundaryMax = source.mBoundaryMax->clone();
    else this->mBoundaryMax = NULL;
  }
  
  connectToChild();

  return *this;
}


/*
 * Destructor.
 */ 
CoordinateComponent::~CoordinateComponent ()
{
	// destroy 'BoundaryMin'
	if (isSetBoundaryMin())
  {
    delete mBoundaryMin;
    mBoundaryMin = NULL;
  }

	// destroy 'BoundaryMax'
	if (isSetBoundaryMax())
  {
    delete mBoundaryMax;
    mBoundaryMax = NULL;
  }

}

/*
  * Returns the value of the "spatialId" attribute of this CoordinateComponent.
  */
const std::string& 
CoordinateComponent::getSpatialId () const
{
  return mSpatialId;
}

/*
  * Returns the value of the "componentType" attribute of this CoordinateComponent.
  */
const std::string& 
CoordinateComponent::getComponentType () const
{
  return mComponentType;
}

/*
  * Returns the value of the "sbmlUnit" attribute of this CoordinateComponent.
  */
const std::string& 
CoordinateComponent::getSbmlUnit () const
{
  return mSbmlUnit;
}

/*
  * Returns the value of the "index" attribute of this CoordinateComponent.
  */
unsigned int 
CoordinateComponent::getIndex () const
{
  return mIndex;
}

/*
  * Returns the "boundaryMin" of this CoordinateComponent.
  */
BoundaryMin* 
CoordinateComponent::getBoundaryMin () const
{
  return mBoundaryMin;
}

/*
  * Returns the "boundaryMax" of this CoordinateComponent.
  */
BoundaryMax* 
CoordinateComponent::getBoundaryMax () const
{
  return mBoundaryMax;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CoordinateComponent's "spatialId" attribute has been set.
  */
bool 
CoordinateComponent::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CoordinateComponent's "componentType" attribute has been set.
  */
bool 
CoordinateComponent::isSetComponentType () const
{
  return (mComponentType.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CoordinateComponent's "sbmlUnit" attribute has been set.
  */
bool 
CoordinateComponent::isSetSbmlUnit () const
{
  return (mSbmlUnit.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CoordinateComponent's "index" attribute has been set.
  */
bool 
CoordinateComponent::isSetIndex () const
{
  return mIsSetIndex;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CoordinateComponent's "boundaryMin" has been set.
  */
bool 
CoordinateComponent::isSetBoundaryMin () const
{
  return (mBoundaryMin != NULL);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CoordinateComponent's "boundaryMax" has been set.
  */
bool 
CoordinateComponent::isSetBoundaryMax () const
{
  return (mBoundaryMax != NULL);
}

/*
  * Sets the value of the "spatialId" attribute of this CoordinateComponent.
  */
int 
CoordinateComponent::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
  * Sets the value of the "componentType" attribute of this CoordinateComponent.
  */
int 
CoordinateComponent::setComponentType (const std::string& componentType)
{
  return SyntaxChecker::checkAndSetSId(componentType ,mComponentType);
}

/*
  * Sets the value of the "sbmlUnit" attribute of this CoordinateComponent.
  */
int 
CoordinateComponent::setSbmlUnit (const std::string& sbmlUnit)
{
  return SyntaxChecker::checkAndSetSId(sbmlUnit ,mSbmlUnit);
}

/*
  * Sets the value of the "index" attribute of this CoordinateComponent.
  */
int 
CoordinateComponent::setIndex (unsigned int index)
{
    mIndex = index;
    mIsSetIndex  = true;
    return LIBSBML_OPERATION_SUCCESS;
}

/*
  * Sets the value of the "boundaryMin" of this CoordinateComponent.
  */
int 
CoordinateComponent::setBoundaryMin (const BoundaryMin* boundaryMin)
{
  if (mBoundaryMin == boundaryMin)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (boundaryMin == NULL)
  {
    if (isSetBoundaryMin())
	  delete mBoundaryMin;
	  mBoundaryMin = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    if (isSetBoundaryMin())
    delete mBoundaryMin;
    mBoundaryMin = static_cast<BoundaryMin*>(boundaryMin->clone());
	  if (isSetBoundaryMin())
      mBoundaryMin->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
  * Sets the value of the "boundaryMax" of this CoordinateComponent.
  */
int 
CoordinateComponent::setBoundaryMax (const BoundaryMax* boundaryMax)
{
  if (mBoundaryMax == boundaryMax)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (boundaryMax == NULL)
  {
    if (isSetBoundaryMax())
	  delete mBoundaryMax;
	  mBoundaryMax = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    if (isSetBoundaryMax())
    delete mBoundaryMax;
    mBoundaryMax = static_cast<BoundaryMax*>(boundaryMax->clone());
	  if (isSetBoundaryMax()) mBoundaryMax->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}

 /*
  * Unsets the value of the "spatialId" attribute of this CoordinateComponent.
  */
int 
CoordinateComponent::unsetSpatialId ()
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
  * Unsets the value of the "componentType" attribute of this CoordinateComponent.
  */
int 
CoordinateComponent::unsetComponentType ()
{
  mComponentType.erase();
  if (mComponentType.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "sbmlUnit" attribute of this CoordinateComponent.
  */
int 
CoordinateComponent::unsetSbmlUnit ()
{
  mSbmlUnit.erase();
  if (mSbmlUnit.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "index" attribute of this CoordinateComponent.
  */
int 
CoordinateComponent::unsetIndex ()
{
  mIsSetIndex = false;
  
  if (!isSetIndex())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Creates a new BoundaryMin for this CoordinateComponent and returns it.  If this
 * CoordinateComponent had a previous BoundaryMin, it will be destroyed.
 */
BoundaryMin*
CoordinateComponent::createBoundaryMin ()
{
  if (isSetBoundaryMin())
  delete mBoundaryMin;
  mBoundaryMin = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    mBoundaryMin = new BoundaryMin(spatialns);
  }
  catch (...)
  {
    /* here we do not create a default object 
     *
     * so do nothing
     */
  }

  if (isSetBoundaryMin())
  {
    mBoundaryMin->connectToParent(this);
  }

  return mBoundaryMin;
}

/*
 * Creates a new BoundaryMax for this CoordinateComponent and returns it.  If this
 * CoordinateComponent had a previous BoundaryMax, it will be destroyed.
 */
BoundaryMax*
CoordinateComponent::createBoundaryMax ()
{
  if (isSetBoundaryMax())
  delete mBoundaryMax;
  mBoundaryMax = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    mBoundaryMax = new BoundaryMax(spatialns);
  }
  catch (...)
  {
    /* here we do not create a default object 
     *
     * so do nothing
     */
  }

  if (isSetBoundaryMax())
  {
    mBoundaryMax->connectToParent(this);
  }

  return mBoundaryMax;
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
CoordinateComponent::getElementName () const
{
  static const std::string name = "coordinateComponent";
  return name;
}

/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
  */
void
CoordinateComponent::connectToChild()
{
  SBase::connectToChild();
	if (isSetBoundaryMin()) 
    mBoundaryMin->connectToParent(this);
	if (isSetBoundaryMax()) 
    mBoundaryMax->connectToParent(this);
}

/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
CoordinateComponent::createObject (XMLInputStream& stream)
{
  // return 0;

//   const unsigned int level  = getLevel();
//   const unsigned int level  = getLevel();
	const unsigned int pkgVersion  = getPackageVersion();

  const string& name   = stream.peek().getName();
  SBase*        object = NULL;
      SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "boundaryMin") {
	  if (isSetBoundaryMin()) 
	  {
		logError(NotSchemaConformant);
    delete mBoundaryMin;
	  }
	  

	  try
	  {
		mBoundaryMin = new BoundaryMin(spatialns);
	  }
	  catch ( ... )
	  {
		mBoundaryMin = new BoundaryMin(SBMLDocument::getDefaultLevel(),
			SBMLDocument::getDefaultVersion(), pkgVersion);
	  }

	  object = mBoundaryMin;
  }
  
  if (name == "boundaryMax") {
	  if (isSetBoundaryMax()) 
	  {
		logError(NotSchemaConformant);
	  delete mBoundaryMax;
	  }

	  try
	  {
		mBoundaryMax = new BoundaryMax(spatialns);
	  }
	  catch ( ... )
	  {
		mBoundaryMax = new BoundaryMax(SBMLDocument::getDefaultLevel(),
 			SBMLDocument::getDefaultVersion(), pkgVersion);
	  }

	  object = mBoundaryMax;
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
CoordinateComponent::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);
  
  attributes.add("spatialId");
  attributes.add("componentType");
  attributes.add("sbmlUnit");
  attributes.add("index");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CoordinateComponent::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<CoordinateComponent>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("componentType", mComponentType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mComponentType.empty())
  {
    logEmptyString(mComponentType, sbmlLevel, sbmlVersion, "<CoordinateComponent>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mComponentType)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("sbmlUnit", mSbmlUnit, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSbmlUnit.empty())
  {
    logEmptyString(mSbmlUnit, sbmlLevel, sbmlVersion, "<CoordinateComponent>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSbmlUnit)) logError(InvalidIdSyntax);
  
  attributes.readInto("index", mIndex, getErrorLog(), true, getLine(), getColumn());
  /*if (mIndex < 0)
  {
    std::string message = "The index attribute on ";
    message += "a <CoordinateComponent> may only have values > 0.";
    getErrorLog()->logError(NotSchemaConformant, sbmlLevel, sbmlVersion, message);
  }*/

}


/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CoordinateComponent::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);
  stream.writeAttribute("componentType",   getPrefix(), mComponentType);
  stream.writeAttribute("sbmlUnit",   getPrefix(), mSbmlUnit);  
  stream.writeAttribute("index",   getPrefix(), mIndex);

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
CoordinateComponent::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (mBoundaryMin != NULL) mBoundaryMin->write(stream);
  if (mBoundaryMax != NULL) mBoundaryMax->write(stream);

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
CoordinateComponent::getTypeCode () const
{
	return SBML_SPATIAL_COORDINATECOMPONENT;
}

CoordinateComponent*
CoordinateComponent::clone() const
{
    return new CoordinateComponent(*this);
}

/*
 * Accepts the given SBMLVisitor.
 */
bool
CoordinateComponent::accept (SBMLVisitor& v) const
{
  // return false;
  // return v.visit(*this);

  bool result = v.visit(*this);

  if (mBoundaryMin) mBoundaryMin->accept(v);
  if (mBoundaryMax) mBoundaryMax->accept(v);

//  v.leave(*this);

  return result;

}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
CoordinateComponent::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  if (mBoundaryMin) mBoundaryMin->setSBMLDocument(d);
  if (mBoundaryMax) mBoundaryMax->setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
CoordinateComponent::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI,pkgPrefix,flag);

  if (mBoundaryMin) mBoundaryMin->enablePackageInternal(pkgURI,pkgPrefix,flag);
  if (mBoundaryMax) mBoundaryMax->enablePackageInternal(pkgURI,pkgPrefix,flag);
}

/*
 * Ctor.
 */
ListOfCoordinateComponents::ListOfCoordinateComponents(SpatialPkgNamespaces* spatialns)
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
ListOfCoordinateComponents::ListOfCoordinateComponents(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfCoordinateComponents.
 */
ListOfCoordinateComponents*
ListOfCoordinateComponents::clone () const
{
  return new ListOfCoordinateComponents(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfCoordinateComponents::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = NULL;


  if (name == "coordinateComponent")
  {
      SPATIAL_CREATE_NS(spatialns, this->getSBMLNamespaces());
	  object = new CoordinateComponent(spatialns);
	  appendAndOwn(object);
      //mItems.push_back(object);
  }

  return object;
}


/* return nth item in list */
CoordinateComponent *
ListOfCoordinateComponents::get(unsigned int n)
{
  return static_cast<CoordinateComponent*>(ListOf::get(n));
}


/* return nth item in list */
const CoordinateComponent *
ListOfCoordinateComponents::get(unsigned int n) const
{
  return static_cast<const CoordinateComponent*>(ListOf::get(n));
}


/* return item by spatialId */
CoordinateComponent*
ListOfCoordinateComponents::get (const std::string& spatialId)
{
  return const_cast<CoordinateComponent*>( 
    static_cast<const ListOfCoordinateComponents&>(*this).get(spatialId) );
}


/* return item by spatialId */
const CoordinateComponent*
ListOfCoordinateComponents::get (const std::string& spatialId) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CoordinateComponent>(spatialId) );
  return (result == mItems.end()) ? 0 : static_cast <CoordinateComponent*> (*result);
}


/* Removes the nth item from this list */
CoordinateComponent*
ListOfCoordinateComponents::remove (unsigned int n)
{
   return static_cast<CoordinateComponent*>(ListOf::remove(n));
}


/* Removes item in this list by spatialId */
CoordinateComponent*
ListOfCoordinateComponents::remove (const std::string& spatialId)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CoordinateComponent>(spatialId) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <CoordinateComponent*> (item);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfCoordinateComponents::getItemTypeCode () const
{
	return SBML_SPATIAL_COORDINATECOMPONENT;
}


/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfCoordinateComponents::getElementName () const
{
  static const std::string name = "listOfCoordinateComponents";
  return name;
}


LIBSBML_CPP_NAMESPACE_END
