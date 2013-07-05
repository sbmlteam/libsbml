/**
 * @file    Domain.cpp
 * @brief   Implementation of Domain, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: Domain.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/Domain.cpp $
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

#include <sbml/packages/spatial/sbml/Domain.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new Domain with the given level, version, and package version.
 */
Domain::Domain (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId("")
   , mDomainType("")
   , mInteriorPoints(level,version, pkgVersion)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  

  if (!hasValidLevelVersionNamespaceCombination())
	throw SBMLConstructorException();

  connectToChild();

}


/*
 * Creates a new Domain with the given SpatialPkgNamespaces object.
 */
Domain::Domain(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mDomainType("")
  , mInteriorPoints(spatialns)
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
Domain::Domain(const Domain& source) : SBase(source)
{
  this->mSpatialId=source.mSpatialId;
  this->mDomainType=source.mDomainType;
  this->mInteriorPoints=source.mInteriorPoints;

  connectToChild();
}

/*
 * Assignment operator.
 */
Domain& Domain::operator=(const Domain& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mSpatialId = source.mSpatialId;
	this->mDomainType = source.mDomainType;
	this->mInteriorPoints = source.mInteriorPoints;
  }
  
  connectToChild();

  return *this;
}

/*
 * Destructor.
 */ 
Domain::~Domain ()
{
}

/*
  * Returns the value of the "spatialId" attribute of this Domain.
  */
const std::string& 
Domain::getSpatialId () const
{
  return mSpatialId;
}

/*
  * Returns the value of the "domainType" attribute of this Domain.
  */
const std::string& 
Domain::getDomainType () const
{
  return mDomainType;
}


/*
  * Predicate returning @c true or @c false depending on whether this
  * Domain's "spatialId" attribute has been set.
  */
bool 
Domain::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * Domain's "domainType" attribute has been set.
  */
bool 
Domain::isSetDomainType () const
{
  return (mDomainType.empty() == false);
}


/*
  * Sets the value of the "spatialId" attribute of this Domain.
  */
int 
Domain::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
  * Sets the value of the "domainType" attribute of this Domain.
  */
int 
Domain::setDomainType (const std::string& domainType)
{
  return SyntaxChecker::checkAndSetSId(domainType ,mDomainType);
}


/*
  * Unsets the value of the "spatialId" attribute of this Domain.
  */
int 
Domain::unsetSpatialId ()
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
  * Unsets the value of the "domainType" attribute of this Domain.
  */
int 
Domain::unsetDomainType ()
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
 * Adds a copy of the given interiorPoint to this Domain.
 */
int
Domain::addInteriorPoint (const InteriorPoint* p)
{
  if (p == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(p->hasRequiredAttributes()) || !(p->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
//  else if (getInteriorPoint(p->getId()) != NULL)
//  {
//    // an object with this id already exists
//    return LIBSBML_DUPLICATE_OBJECT_ID;
//  }
  else
  {
	  mInteriorPoints.append(p);
      return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Creates a new InteriorPoint inside this Domain and returns it.
 */
InteriorPoint*
Domain::createInteriorPoint ()
{

  InteriorPoint*p = new InteriorPoint(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  this->mInteriorPoints.appendAndOwn(p);
  return p;
}

/*
 * @return the list of InteriorPoints for this Domain.
 */
const ListOfInteriorPoints*
Domain::getListOfInteriorPoints () const
{
	return &this->mInteriorPoints;
}

/*
 * @return the list of InteriorPoints for this Domain.
 */
ListOfInteriorPoints*
Domain::getListOfInteriorPoints ()
{
	return &this->mInteriorPoints;
}


/*
 * @return the nth interiorPoint of this Domain.
 */
const InteriorPoint*
Domain::getInteriorPoint (unsigned int n) const
{
  return mInteriorPoints.get(n);
}


/*
 * @return the nth interiorPoint of this Domain.
 */
InteriorPoint*
Domain::getInteriorPoint (unsigned int n)
{
  return mInteriorPoints.get(n);
}

/*
 * @return the number of interiorPoints in this Domain.
 */
unsigned int
Domain::getNumInteriorPoints () const
{
	return this->mInteriorPoints.size();
}

/**
 * Removes the nth interiorPoint object from this Domain object and
 * returns a pointer to it.
 */
InteriorPoint* 
Domain::removeInteriorPoint (unsigned int n)
{
	return mInteriorPoints.remove(n);
}


/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
  */
void
Domain::connectToChild()
{	
	mInteriorPoints.connectToParent(this);
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
Domain::getElementName () const
{
  static const std::string name = "domain";
  return name;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
Domain::createObject (XMLInputStream& stream)
{
  // return 0;
  const string& name   = stream.peek().getName();
  SBase*        object = 0;

//  object=SBase::createObject(stream);
  if (name == "listOfInteriorPoints")
  {
	if (mInteriorPoints.size() != 0)
    {
      logError(NotSchemaConformant);
    }
	  object = &mInteriorPoints;
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
Domain::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("spatialId");
  attributes.add("domainType");
  // attributes.add("goemetricObject");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Domain::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<Domain>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("domainType", mDomainType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mDomainType.empty())
  {
    logEmptyString(mDomainType, sbmlLevel, sbmlVersion, "<Domain>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mDomainType)) logError(InvalidIdSyntax);
  
}


/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Domain::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);
  stream.writeAttribute("domainType",   getPrefix(), mDomainType);  

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
Domain::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getNumInteriorPoints() > 0 ) mInteriorPoints.write(stream);

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
Domain::getTypeCode () const
{
	return SBML_SPATIAL_DOMAIN;
}

Domain*
Domain::clone() const
{
    return new Domain(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
Domain::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Domain::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  mInteriorPoints.setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
Domain::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mInteriorPoints.enablePackageInternal(pkgURI,pkgPrefix,flag);
}


/*
 * Ctor.
 */
ListOfDomains::ListOfDomains(SpatialPkgNamespaces* spatialns)
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
ListOfDomains::ListOfDomains(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
ListOfDomains*
ListOfDomains::clone () const
{
  return new ListOfDomains(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfDomains::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "domain")
  {
      SPATIAL_CREATE_NS(spatialns, this->getSBMLNamespaces());
	  object = new Domain(spatialns);
	  appendAndOwn(object);
      //mItems.push_back(object);
  }

  return object;
}


/* return nth item in list */
Domain *
ListOfDomains::get(unsigned int n)
{
  return static_cast<Domain*>(ListOf::get(n));
}


/* return nth item in list */
const Domain *
ListOfDomains::get(unsigned int n) const
{
  return static_cast<const Domain*>(ListOf::get(n));
}


/* return item by spatialId */
Domain*
ListOfDomains::get (const std::string& spatialId)
{
  return const_cast<Domain*>( 
    static_cast<const ListOfDomains&>(*this).get(spatialId) );
}


/* return item by spatialId */
const Domain*
ListOfDomains::get (const std::string& spatialId) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Domain>(spatialId) );
  return (result == mItems.end()) ? 0 : static_cast <Domain*> (*result);
}


/* Removes the nth item from this list */
Domain*
ListOfDomains::remove (unsigned int n)
{
   return static_cast<Domain*>(ListOf::remove(n));
}


/* Removes item in this list by spatialId */
Domain*
ListOfDomains::remove (const std::string& spatialId)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Domain>(spatialId) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Domain*> (item);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfDomains::getItemTypeCode () const
{
	return SBML_SPATIAL_DOMAIN;
}


/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfDomains::getElementName () const
{
  static const std::string name = "listOfDomains";
  return name;
}


LIBSBML_CPP_NAMESPACE_END

