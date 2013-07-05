/**
 * @file    ParametricGeometry.cpp
 * @brief   Implementation of ParametricGeometry, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: ParametricGeometry.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.pon.sourceforge.net/ponroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/ParametricGeometry.cpp $
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

#include <sbml/packages/spatial/sbml/ParametricGeometry.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/sbml/ParametricObject.h>
#include <sbml/packages/spatial/sbml/SpatialPoint.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new ParametricGeometry with the given level, version, and package version.
 */
ParametricGeometry::ParametricGeometry (unsigned int level, unsigned int version, unsigned int pkgVersion) 
: GeometryDefinition (SBML_SPATIAL_PARAMETRICGEOMETRY,level,version)
   , mParametricObjects (level,version, pkgVersion)
   , mSpatialPoints (level,version, pkgVersion)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  

  if (!hasValidLevelVersionNamespaceCombination())
	throw SBMLConstructorException();

  connectToChild();
}


/*
 * Creates a new ParametricGeometry with the given SpatialPkgNamespaces object.
 */
ParametricGeometry::ParametricGeometry(SpatialPkgNamespaces* spatialns)
: GeometryDefinition(SBML_SPATIAL_PARAMETRICGEOMETRY,spatialns)
  , mParametricObjects (spatialns)
  , mSpatialPoints (spatialns)
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
ParametricGeometry::ParametricGeometry(const ParametricGeometry& source) : GeometryDefinition(source)
{
  this->setSpatialId(source.getSpatialId());
  this->mParametricObjects = source.mParametricObjects;
  this->mSpatialPoints = source.mSpatialPoints;

  connectToChild();
}

/*
 * Assignment operator.
 */
ParametricGeometry& ParametricGeometry::operator=(const ParametricGeometry& source)
{
  if(&source!=this)
  {
	this->GeometryDefinition::operator=(source);
	this->mParametricObjects = source.mParametricObjects;
	this->mSpatialPoints = source.mSpatialPoints;
  }
  
  connectToChild();

  return *this;
}


/*
 * Destructor.
 */ 
ParametricGeometry::~ParametricGeometry ()
{
}

/*
 * Adds a copy of the given ParametricObject to this ParametricGeometry.
 */
int
ParametricGeometry::addParametricObject (const ParametricObject* po)
{
  if (po == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(po->hasRequiredAttributes()) || !(po->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getParametricObject(po->getId()) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    mParametricObjects.append(po); 
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Adds a copy of the given SpatialPoint to this ParametricGeometry.
 */
int
ParametricGeometry::addSpatialPoint (const SpatialPoint* sp)
{
  if (sp == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(sp->hasRequiredAttributes()) || !(sp->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getSpatialPoint(sp->getId()) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    mSpatialPoints.append(sp); 
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Creates a new ParametricObject object inside this ParametricGeometry and returns it.
 */
ParametricObject*
ParametricGeometry::createParametricObject ()
{

  ParametricObject*po = new ParametricObject(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  this->mParametricObjects.appendAndOwn(po);
  return po;
}


/*
 * Creates a new SpatialPoint object inside this ParametricGeometry and returns it.
 */
SpatialPoint*
ParametricGeometry::createSpatialPoint ()
{

  SpatialPoint* sp = new SpatialPoint(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  this->mSpatialPoints.appendAndOwn(sp);
  return sp;
}


/*
 * @return the list of ParametricObject for this ParametricGeometry.
 */
const ListOfParametricObjects*
ParametricGeometry::getListOfParametricObjects () const
{
	return &this->mParametricObjects;
}

/*
 * @return the list of ParametricObject for this ParametricGeometry.
 */
ListOfParametricObjects*
ParametricGeometry::getListOfParametricObjects () 
{
	return &this->mParametricObjects;
}

/*
 * @return the list of SpatialPoint for this ParametricGeometry.
 */
const ListOfSpatialPoints*
ParametricGeometry::getListOfSpatialPoints () const
{
	return &this->mSpatialPoints;
}

/*
 * @return the list of SpatialPoint for this ParametricGeometry.
 */
ListOfSpatialPoints*
ParametricGeometry::getListOfSpatialPoints () 
{
	return &this->mSpatialPoints;
}


/*
 * @return the nth ParametricObject object of this ParametricGeometry.
 */
const ParametricObject*
ParametricGeometry::getParametricObject (unsigned int n) const
{
  return mParametricObjects.get(n);
}

/*
 * @return the nth ParametricObject of this ParametricGeometry.
 */
ParametricObject*
ParametricGeometry::getParametricObject (unsigned int n)
{
  return mParametricObjects.get(n);
}

/*
 * @return the ParametricObject object in this ParametricGeometry with the given id or NULL
 * if no such ParametricObject exists.
 */
const ParametricObject*
ParametricGeometry::getParametricObject (const std::string& sid) const
{
  return mParametricObjects.get(sid);
}

/*
 * @return the ParametricObject object in this ParametricGeometry with the given id or NULL
 * if no such ParametricObject exists.
 */
ParametricObject*
ParametricGeometry::getParametricObject (const std::string& sid)
{
  return mParametricObjects.get(sid);
}

/*
 * @return the nth SpatialPoint object of this ParametricGeometry.
 */
const SpatialPoint*
ParametricGeometry::getSpatialPoint (unsigned int n) const
{
  return mSpatialPoints.get(n);
}

/*
 * @return the nth SpatialPoint of this ParametricGeometry.
 */
SpatialPoint*
ParametricGeometry::getSpatialPoint (unsigned int n)
{
  return mSpatialPoints.get(n);
}

/*
 * @return the SpatialPoint object in this ParametricGeometry with the given id or NULL
 * if no such SpatialPoint exists.
 */
const SpatialPoint*
ParametricGeometry::getSpatialPoint (const std::string& sid) const
{
  return mSpatialPoints.get(sid);
}

/*
 * @return the SpatialPoint object in this ParametricGeometry with the given id or NULL
 * if no such SpatialPoint exists.
 */
SpatialPoint*
ParametricGeometry::getSpatialPoint (const std::string& sid)
{
  return mSpatialPoints.get(sid);
}


/*
 * @return the number of ParametricObject in this ParametricGeometry.
 */
unsigned int
ParametricGeometry::getNumParametricObjects () const
{
  return this->mParametricObjects.size();
}

/*
 * @return the number of SpatialPoints in this ParametricGeometry.
 */
unsigned int
ParametricGeometry::getNumSpatialPoints () const
{
  return this->mSpatialPoints.size();
}


/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
  */
void
ParametricGeometry::connectToChild()
{	
	mParametricObjects.connectToParent(this);
	mSpatialPoints.connectToParent(this);
}

/**
 * Removes the nth ParametricObject object from this ParametricGeometry object and
 * returns a pointer to it.
 */
ParametricObject* 
ParametricGeometry::removeParametricObject (unsigned int n)
{
  return mParametricObjects.remove(n);
}


/**
 * Removes the ParametricObject object with the given identifier from this ParametricGeometry
 * object and returns a pointer to it.
 */
ParametricObject* 
ParametricGeometry::removeParametricObject (const std::string& sid)
{
  return mParametricObjects.remove(sid);
}

/**
 * Removes the nth SpatialPoint object from this ParametricGeometry object and
 * returns a pointer to it.
 */
SpatialPoint* 
ParametricGeometry::removeSpatialPoint (unsigned int n)
{
  return mSpatialPoints.remove(n);
}


/**
 * Removes the SpatialPoint object with the given identifier from this ParametricGeometry
 * object and returns a pointer to it.
 */
SpatialPoint* 
ParametricGeometry::removeSpatialPoint (const std::string& sid)
{
  return mSpatialPoints.remove(sid);
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.

const std::string&
ParametricGeometry::getElementName () const
{
  static const std::string name = "ParametricGeometry";
  return name;
}
*/

/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ParametricGeometry::createObject (XMLInputStream& stream)
{
  // return 0;

  const string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "listOfParametricObjects")
  {
    if (mParametricObjects.size() != 0)
    {
      logError(NotSchemaConformant);
    }
    object = &mParametricObjects;
  }

  if (name == "listOfSpatialPoints")
  {
    if (mSpatialPoints.size() != 0)
    {
      logError(NotSchemaConformant);
    }
    object = &mSpatialPoints;
  }

  return object;
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
ParametricGeometry::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  GeometryDefinition::readAttributes(attributes,expectedAttributes);

  // nothing else to be done, since ParametricGeometry doesn't have other attributes to be read.
}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
ParametricGeometry::writeAttributes (XMLOutputStream& stream) const
{
  GeometryDefinition::writeAttributes(stream);

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
ParametricGeometry::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getNumParametricObjects() > 0 ) mParametricObjects.write(stream);
  if ( getNumSpatialPoints() > 0 ) mSpatialPoints.write(stream);
  

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}

ParametricGeometry*
ParametricGeometry::clone() const
{
    return new ParametricGeometry(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
ParametricGeometry::accept (SBMLVisitor& v) const
{
 // return v.visit(*this);
  bool result = v.visit(*this);

  mParametricObjects.accept(v);
  mSpatialPoints.accept(v);

//  v.leave(*this);

  return result;

}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
ParametricGeometry::setSBMLDocument (SBMLDocument* d)
{
  GeometryDefinition::setSBMLDocument(d);

  mParametricObjects.setSBMLDocument(d);
  mSpatialPoints.setSBMLDocument(d);

}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
ParametricGeometry::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  GeometryDefinition::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mParametricObjects.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mSpatialPoints.enablePackageInternal(pkgURI,pkgPrefix,flag);

}



/** @cond doxygen-c-only */

/**
 * Creates and returns a deep copy of a given ParametricGeometry_t structure.
 *
 * @param g the ParametricGeometry_t structure to copy
 * 
 * @return a (deep) copy of this ParametricGeometry_t structure.
 */
LIBSBML_EXTERN
ParametricGeometry_t *
ParametricGeometry_clone (const ParametricGeometry_t *pg)
{
  return static_cast<ParametricGeometry*>( pg->clone() );
}



LIBSBML_CPP_NAMESPACE_END

