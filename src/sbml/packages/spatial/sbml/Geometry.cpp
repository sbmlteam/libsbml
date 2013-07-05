/**
 * @file    Geometry.cpp
 * @brief   Implementation of Geometry, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: Geometry.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/Geometry.cpp $
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

#include <sbml/packages/spatial/sbml/Geometry.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/sbml/AnalyticGeometry.h>
#include <sbml/packages/spatial/sbml/SampledFieldGeometry.h>
#include <sbml/packages/spatial/sbml/ParametricGeometry.h>
#include <sbml/packages/spatial/sbml/CSGeometry.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new Geometry with the given level, version, and package version.
 */
Geometry::Geometry (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mCoordinateSystem("")
   , mCoordinateComponents (level,version, pkgVersion)
   , mDomainTypes (level,version, pkgVersion)
   , mDomains (level,version, pkgVersion)
   , mAdjacentDomains (level,version, pkgVersion)
   , mGeometryDefinitions (level,version, pkgVersion)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  

  if (!hasValidLevelVersionNamespaceCombination())
	throw SBMLConstructorException();

  connectToChild();
}


/*
 * Creates a new Geometry with the given SpatialPkgNamespaces object.
 */
Geometry::Geometry(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mCoordinateSystem("")
  , mCoordinateComponents (spatialns)
  , mDomainTypes (spatialns)
  , mDomains (spatialns)
  , mAdjacentDomains (spatialns)
  , mGeometryDefinitions (spatialns)
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
Geometry::Geometry(const Geometry& source) : SBase(source)
{
  this->mCoordinateSystem = source.mCoordinateSystem;
  this->mCoordinateComponents = source.mCoordinateComponents;
  this->mDomainTypes = source.mDomainTypes;
  this->mDomains = source.mDomains;
  this->mAdjacentDomains = source.mAdjacentDomains;
  this->mGeometryDefinitions = source.mGeometryDefinitions;

  connectToChild();
}

/*
 * Assignment operator.
 */
Geometry& Geometry::operator=(const Geometry& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mCoordinateSystem = source.mCoordinateSystem;
    this->mCoordinateComponents = source.mCoordinateComponents;
    this->mDomainTypes = source.mDomainTypes;
    this->mDomains = source.mDomains;
	this->mAdjacentDomains = source.mAdjacentDomains;
	this->mGeometryDefinitions = source.mGeometryDefinitions;
  }

  connectToChild();

  return *this;
}


/*
 * Destructor.
 */ 
Geometry::~Geometry ()
{
	// delete mGeometryDefinition;
}


/*
  * Returns the value of the "coordinateSystem" attribute of this Geometry.
  */
const std::string& 
Geometry::getCoordinateSystem () const
{
  return mCoordinateSystem;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * Geometry's "coordinateSystem" attribute has been set.
  */
bool 
Geometry::isSetCoordinateSystem () const
{
  return (mCoordinateSystem.empty() == false);
}

/*
  * Sets the value of the "coordinateSystem" attribute of this Geometry.
  */
int 
Geometry::setCoordinateSystem (const std::string& coordinateSystem)
{
  return SyntaxChecker::checkAndSetSId(coordinateSystem ,mCoordinateSystem);
}

 /*
  * Unsets the value of the "coordinateSystem" attribute of this Geometry.
  */
int 
Geometry::unsetCoordinateSystem ()
{
  mCoordinateSystem.erase();
  if (mCoordinateSystem.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Adds a copy of the given CoordinateComponent to this Geometry.
 */
int
Geometry::addCoordinateComponent (const CoordinateComponent* cc)
{
  if (cc == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(cc->hasRequiredAttributes()) || !(cc->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getCoordinateComponent(cc->getId()) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
	  mCoordinateComponents.append(cc);
      return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Adds a copy of the given DomainType to this Geometry.
 */
int
Geometry::addDomainType (const DomainType* dt)
{
  if (dt == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(dt->hasRequiredAttributes()) || !(dt->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getDomainType(dt->getId()) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    mDomainTypes.append(dt);
   
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Adds a copy of the given Domain to this Geometry.
 */
int
Geometry::addDomain (const Domain* d)
{
  if (d == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(d->hasRequiredAttributes()) || !(d->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getDomain(d->getId()) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    mDomains.append(d); 
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Adds a copy of the given AdjacentDomains to this Geometry.
 */
int
Geometry::addAdjacentDomains (const AdjacentDomains* ad)
{
  if (ad == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(ad->hasRequiredAttributes()) || !(ad->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getAdjacentDomains(ad->getId()) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    mAdjacentDomains.append(ad); 
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Adds a copy of the given GeometryDefinition to this Geometry.
 */
int
Geometry::addGeometryDefinition (const GeometryDefinition* gd)
{
  if (gd == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(gd->hasRequiredAttributes()) || !(gd->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getGeometryDefinition(gd->getId()) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    mGeometryDefinitions.append(gd); 
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Creates a new CoordinateComponent inside this Geometry and returns it.
 */
CoordinateComponent*
Geometry::createCoordinateComponent ()
{

  CoordinateComponent*cc = new CoordinateComponent(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  this->mCoordinateComponents.appendAndOwn(cc);
  return cc;
}

/*
 * Creates a new DomainType inside this Geometry and returns it.
 */
DomainType*
Geometry::createDomainType ()
{

  DomainType*dt = new DomainType(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  this->mDomainTypes.appendAndOwn(dt);
  return dt;
}

/*
 * Creates a new Domain inside this Geometry and returns it.
 */
Domain*
Geometry::createDomain ()
{

  Domain*d = new Domain(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  this->mDomains.appendAndOwn(d);
  return d;
}

/*
 * Creates a new AdjacentDomains object inside this Geometry and returns it.
 */
AdjacentDomains*
Geometry::createAdjacentDomains ()
{

  AdjacentDomains*ad = new AdjacentDomains(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  this->mAdjacentDomains.appendAndOwn(ad);
  return ad;
}

/*
 * Creates a new AnalyticGeometry for this Geometry and returns it.  If this
 * Geometry had a previous AnalyticGeometry, it will be destroyed.
 */
AnalyticGeometry*
Geometry::createAnalyticGeometry ()
{
  AnalyticGeometry* ag = 0;

  try
  {
    ag = new AnalyticGeometry(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
  }
  
  if (ag) this->mGeometryDefinitions.appendAndOwn(ag);

  return ag;
}

/*
 * Creates a new SampledFieldGeometry for this Geometry and returns it.  If this
 * Geometry had a previous SampledFieldGeometry, it will be destroyed.
 */
SampledFieldGeometry*
Geometry::createSampledFieldGeometry ()
{
  SampledFieldGeometry* sfg = 0;
  try
  {
	 sfg = new SampledFieldGeometry(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
  }

  if (sfg)
  {
	mGeometryDefinitions.appendAndOwn(sfg);
  }

  return sfg;
}

/*
 * Creates a new ParametricGeometry for this Geometry and returns it.  If this
 * Geometry had a previous ParametricGeometry, it will be destroyed.
 */
ParametricGeometry*
Geometry::createParametricGeometry ()
{
  ParametricGeometry* pg = 0;

  try
  {
    pg = new ParametricGeometry(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
  }
  
  if (pg) this->mGeometryDefinitions.appendAndOwn(pg);

  return pg;
}

/*
 * Creates a new CSGeometry for this Geometry and returns it.  If this
 * Geometry had a previous CSGeometry, it will be destroyed.
 */
CSGeometry*
Geometry::createCSGeometry ()
{
  CSGeometry* csg = 0;

  try
  {
    csg = new CSGeometry(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
  }
  
  if (csg) this->mGeometryDefinitions.appendAndOwn(csg);

  return csg;
}


/*
 * @return the list of CoordinateComponents for this Geometry.
 */
const ListOfCoordinateComponents*
Geometry::getListOfCoordinateComponents () const
{
	return &this->mCoordinateComponents;
}


/*
 * @return the list of CoordinateComponents for this Geometry.
 */
ListOfCoordinateComponents*
Geometry::getListOfCoordinateComponents ()
{
	return &this->mCoordinateComponents;
}

/*
 * @return the list of DomainTypes for this Geometry.
 */
const ListOfDomainTypes*
Geometry::getListOfDomainTypes () const
{
	return &this->mDomainTypes;
}

/*
 * @return the list of DomainTypes for this Geometry.
 */
ListOfDomainTypes*
Geometry::getListOfDomainTypes ()
{
	return &this->mDomainTypes;
}

/*
 * @return the list of Domains for this Geometry.
 */
const ListOfDomains*
Geometry::getListOfDomains () const
{
	return &this->mDomains;
}

/*
 * @return the list of Domains for this Geometry.
 */
ListOfDomains*
Geometry::getListOfDomains ()
{
	return &this->mDomains;
}

/*
 * @return the list of AdjacentDomains for this Geometry.
 */
const ListOfAdjacentDomains*
Geometry::getListOfAdjacentDomains () const
{
	return &this->mAdjacentDomains;
}

/*
 * @return the list of AdjacentDomains for this Geometry.
 */
ListOfAdjacentDomains*
Geometry::getListOfAdjacentDomains ()
{
	return &this->mAdjacentDomains;
}

/*
 * @return the list of GeometryDefinitions for this Geometry.
 */
const ListOfGeometryDefinitions*
Geometry::getListOfGeometryDefinitions () const
{
	return &this->mGeometryDefinitions;
}


/*
 * @return the list of GeometryDefinitions for this Geometry.
 */
ListOfGeometryDefinitions*
Geometry::getListOfGeometryDefinitions ()
{
	return &this->mGeometryDefinitions;
}


/*
 * @return the nth CoordinateComponent of this Geometry.
 */
const CoordinateComponent*
Geometry::getCoordinateComponent (unsigned int n) const
{
  return mCoordinateComponents.get(n);
}


/*
 * @return the nth CoordinateComponent of this Geometry.
 */
CoordinateComponent*
Geometry::getCoordinateComponent (unsigned int n)
{
  return mCoordinateComponents.get(n);
}


/*
 * @return the CoordinateComponent in this Geometry with the given id or NULL
 * if no such CoordinateComponent exists.
 */
const CoordinateComponent*
Geometry::getCoordinateComponent (const std::string& sid) const
{
  return mCoordinateComponents.get(sid);
}


/*
 * @return the CoordinateComponent in this Geometry with the given id or NULL
 * if no such CoordinateComponent exists.
 */
CoordinateComponent*
Geometry::getCoordinateComponent (const std::string& sid)
{
  return mCoordinateComponents.get(sid);
}

/*
 * @return the nth DomainType of this Geometry.
 */
const DomainType*
Geometry::getDomainType (unsigned int n) const
{
  return mDomainTypes.get(n);
}


/*
 * @return the nth DomainType of this Geometry.
 */
DomainType*
Geometry::getDomainType (unsigned int n)
{
  return mDomainTypes.get(n);
}


/*
 * @return the DomainType in this Geometry with the given id or NULL
 * if no such DomainType exists.
 */
const DomainType*
Geometry::getDomainType (const std::string& sid) const
{
  return mDomainTypes.get(sid);
}


/*
 * @return the DomainType in this Geometry with the given id or NULL
 * if no such DomainType exists.
 */
DomainType*
Geometry::getDomainType (const std::string& sid)
{
  return mDomainTypes.get(sid);
}

/*
 * @return the nth Domain of this Geometry.
 */
const Domain*
Geometry::getDomain (unsigned int n) const
{
  return mDomains.get(n);
}

/*
 * @return the nth Domain of this Geometry.
 */
Domain*
Geometry::getDomain (unsigned int n)
{
  return mDomains.get(n);
}


/*
 * @return the Domain in this Geometry with the given id or NULL
 * if no such Domain exists.
 */
const Domain*
Geometry::getDomain (const std::string& sid) const
{
  return mDomains.get(sid);
}

/*
 * @return the Domain in this Geometry with the given id or NULL
 * if no such Domain exists.
 */
Domain*
Geometry::getDomain (const std::string& sid)
{
  return mDomains.get(sid);
}

/*
 * @return the nth AdjacentDomains object of this Geometry.
 */
const AdjacentDomains*
Geometry::getAdjacentDomains (unsigned int n) const
{
  return mAdjacentDomains.get(n);
}


/*
 * @return the nth AdjacentDomains of this Geometry.
 */
AdjacentDomains*
Geometry::getAdjacentDomains (unsigned int n)
{
  return mAdjacentDomains.get(n);
}

/*
 * @return the AdjacentDomains object in this Geometry with the given id or NULL
 * if no such AdjacentDomains exists.
 */
const AdjacentDomains*
Geometry::getAdjacentDomains (const std::string& sid) const
{
  return mAdjacentDomains.get(sid);
}

/*
 * @return the AdjacentDomains object in this Geometry with the given id or NULL
 * if no such AdjacentDomains exists.
 */
AdjacentDomains*
Geometry::getAdjacentDomains (const std::string& sid)
{
  return mAdjacentDomains.get(sid);
}

/*
 * @return the nth GeometryDefinition of this Geometry.
 */
const GeometryDefinition*
Geometry::getGeometryDefinition (unsigned int n) const
{
  return mGeometryDefinitions.get(n);
}

/*
 * @return the nth GeometryDefinition of this Geometry.
 */
GeometryDefinition*
Geometry::getGeometryDefinition (unsigned int n)
{
  return mGeometryDefinitions.get(n);
}

/*
 * @return the GeometryDefinition in this Geometry with the given id or NULL
 * if no such GeometryDefinition exists.
 */
const GeometryDefinition*
Geometry::getGeometryDefinition (const std::string& sid) const
{
  return mGeometryDefinitions.get(sid);
}

/*
 * @return the GeometryDefinition in this Geometry with the given id or NULL
 * if no such GeometryDefinition exists.
 */
GeometryDefinition*
Geometry::getGeometryDefinition (const std::string& sid)
{
  return mGeometryDefinitions.get(sid);
}

/*
 * @return the number of CoordinateComponents in this Geometry.
 */
unsigned int
Geometry::getNumCoordinateComponents () const
{
	return this->mCoordinateComponents.size();
}

/*
 * @return the number of DomainTypes in this Geometry.
 */
unsigned int
Geometry::getNumDomainTypes () const
{
  return this->mDomainTypes.size();
}

/*
 * @return the number of Domains in this Geometry.
 */
unsigned int
Geometry::getNumDomains () const
{
  return this->mDomains.size();
}

/*
 * @return the number of AdjacentDomains in this Geometry.
 */
unsigned int
Geometry::getNumAdjacentDomains () const
{
  return this->mAdjacentDomains.size();
}

/*
 * @return the number of GeometryDefinitions in this Geometry.
 */
unsigned int
Geometry::getNumGeometryDefinitions () const
{
  return this->mGeometryDefinitions.size();
}

/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
  */
void
Geometry::connectToChild()
{	
	mCoordinateComponents.connectToParent(this);
	mDomainTypes.connectToParent(this);
	mDomains.connectToParent(this);
	mAdjacentDomains.connectToParent(this);
	mGeometryDefinitions.connectToParent(this);

	// if (mGeometryDefinition) mGeometryDefinition->connectToParent(this);
}


/**
 * Removes the nth CoordinateComponent object from this Geometry object and
 * returns a pointer to it.
 */
CoordinateComponent* 
Geometry::removeCoordinateComponent (unsigned int n)
{
	return mCoordinateComponents.remove(n);
}


/**
 * Removes the CoordinateComponent object with the given identifier from this Geometry
 * object and returns a pointer to it.
 */
CoordinateComponent* 
Geometry::removeCoordinateComponent (const std::string& sid)
{
  return mCoordinateComponents.remove(sid);
}

/**
 * Removes the nth DomainType object from this Geometry object and
 * returns a pointer to it.
 */
DomainType* 
Geometry::removeDomainType (unsigned int n)
{
  return mDomainTypes.remove(n);
}


/**
 * Removes the DomainType object with the given identifier from this Geometry
 * object and returns a pointer to it.
 */
DomainType* 
Geometry::removeDomainType (const std::string& sid)
{
  return mDomainTypes.remove(sid);
}

/**
 * Removes the nth Domain object from this Geometry object and
 * returns a pointer to it.
 */
Domain* 
Geometry::removeDomain (unsigned int n)
{
  return mDomains.remove(n);
}

/**
 * Removes the Domain object with the given identifier from this Geometry
 * object and returns a pointer to it.
 */
Domain* 
Geometry::removeDomain (const std::string& sid)
{
  return mDomains.remove(sid);
}

/**
 * Removes the nth AdjacentDomains object from this Geometry object and
 * returns a pointer to it.
 */
AdjacentDomains* 
Geometry::removeAdjacentDomains (unsigned int n)
{
  return mAdjacentDomains.remove(n);
}


/**
 * Removes the AdjacentDomains object with the given identifier from this Geometry
 * object and returns a pointer to it.
 */
AdjacentDomains* 
Geometry::removeAdjacentDomains (const std::string& sid)
{
  return mAdjacentDomains.remove(sid);
}

/**
 * Removes the nth GeometryDefinition object from this Geometry object and
 * returns a pointer to it.
 */
GeometryDefinition* 
Geometry::removeGeometryDefinition (unsigned int n)
{
  return mGeometryDefinitions.remove(n);
}

/**
 * Removes the GeometryDefinition object with the given identifier from this Geometry
 * object and returns a pointer to it.
 */
GeometryDefinition* 
Geometry::removeGeometryDefinition (const std::string& sid)
{
  return mGeometryDefinitions.remove(sid);
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
Geometry::getElementName () const
{
  static const std::string name = "geometry";
  return name;
}



/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
Geometry::getElementPosition () const
{
  return 3;
}
/** @endcond doxygen-libsbml-internal */


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
Geometry::createObject (XMLInputStream& stream)
{
  // return 0;

  const string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "listOfCoordinateComponents")
  {
	if (mCoordinateComponents.size() != 0)
    {
      logError(NotSchemaConformant);
    }
	  object = &mCoordinateComponents;
  }

  if (name == "listOfDomainTypes")
  {
    if (mDomainTypes.size() != 0)
    {
      logError(NotSchemaConformant);
    }
    object = &mDomainTypes;
  }

  if (name == "listOfDomains")
  {
    if (mDomains.size() != 0)
    {
      logError(NotSchemaConformant);
    }
    object = &mDomains;
  }

  if (name == "listOfAdjacentDomains")
  {
    if (mAdjacentDomains.size() != 0)
    {
      logError(NotSchemaConformant);
    }
    object = &mAdjacentDomains;
  }
  
  if (name == "listOfGeometryDefinitions")
  {
    if (mGeometryDefinitions.size() != 0)
    {
      logError(NotSchemaConformant);
    }
    object = &mGeometryDefinitions;
  }

  /*
  if ((name == "analyticGeometry") || (name == "sampledFieldGeometry") ||
	  (name == "parametricGeometry") || (name == "csGeometry"))
  {
    if (mGeometryDefinition)
    {
      logError(NotSchemaConformant);
    }
    delete mGeometryDefinition;

	if (name == "analyticGeometry") {
		try
		{
			mGeometryDefinition = new AnalyticGeometry(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
		}
		catch ( ... )
		{
		  mGeometryDefinition = new AnalyticGeometry(SBMLDocument::getDefaultLevel(),
			SBMLDocument::getDefaultVersion());
		}
	}

	if (name == "sampledFieldGeometry") {
		try
		{
			mGeometryDefinition = new SampledFieldGeometry(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
		}
		catch ( ... )
		{
			mGeometryDefinition = new SampledFieldGeometry(SBMLDocument::getDefaultLevel(),
				SBMLDocument::getDefaultVersion());
		}
	}

	if (name == "parametricGeometry") {
	}

	if (name == "csGeometry") {
	}

	object = mGeometryDefinition;
  }
  */

  return object;
}

/*
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
Geometry::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("coordinateSystem");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Geometry::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("coordinateSystem", mCoordinateSystem, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mCoordinateSystem.empty())
  {
    logEmptyString(mCoordinateSystem, sbmlLevel, sbmlVersion, "<geometry>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mCoordinateSystem)) logError(InvalidIdSyntax);

}


/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Geometry::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("coordinateSystem",   getPrefix(), mCoordinateSystem);

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
Geometry::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getNumCoordinateComponents() > 0 ) mCoordinateComponents.write(stream);
  if ( getNumDomainTypes() > 0 ) mDomainTypes.write(stream);
  if ( getNumDomains() > 0 ) mDomains.write(stream);
  if ( getNumAdjacentDomains() > 0 ) mAdjacentDomains.write(stream);
  if ( getNumGeometryDefinitions() > 0 ) mGeometryDefinitions.write(stream);

  // if (mGeometryDefinition) mGeometryDefinition->write(stream);

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
Geometry::getTypeCode () const
{
  return SBML_SPATIAL_GEOMETRY;
}

Geometry*
Geometry::clone() const
{
    return new Geometry(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
Geometry::accept (SBMLVisitor& v) const
{
 // return v.visit(*this);
  bool result = v.visit(*this);

  mCoordinateComponents.accept(v);
  mDomainTypes .accept(v);
  mDomains.accept(v);
  mAdjacentDomains.accept(v);
  mGeometryDefinitions.accept(v);

  // if (mGeometryDefinition) mGeometryDefinition->accept(v);

//  v.leave(*this);

  return result;
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Geometry::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  mCoordinateComponents.setSBMLDocument(d);
  mDomainTypes.setSBMLDocument(d);
  mDomains.setSBMLDocument(d);
  mAdjacentDomains.setSBMLDocument(d);
  mGeometryDefinitions.setSBMLDocument(d);

  // if (mGeometryDefinition) mGeometryDefinition->setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
Geometry::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mCoordinateComponents.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mDomainTypes.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mDomains.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mAdjacentDomains.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mGeometryDefinitions.enablePackageInternal(pkgURI,pkgPrefix,flag);

  // if (mGeometryDefinition) mGeometryDefinition->enablePackageInternal(pkgURI,pkgPrefix,flag);
}



/** @cond doxygen-c-only */

/**
 * Creates and returns a deep copy of a given Geometry_t structure.
 *
 * @param g the Geometry_t structure to copy
 * 
 * @return a (deep) copy of this Geometry_t structure.
 */
LIBSBML_EXTERN
Geometry_t *
Geometry_clone (const Geometry_t *g)
{
  return static_cast<Geometry*>( g->clone() );
}



LIBSBML_CPP_NAMESPACE_END
