/**
 * @file    AnalyticGeometry.cpp
 * @brief   Implementation of AnalyticAnalyticGeometry, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: AnalyticGeometry.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/AnalyticGeometry.cpp $
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

#include <sbml/packages/spatial/sbml/AnalyticGeometry.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/sbml/AnalyticVolume.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new AnalyticGeometry with the given level, version, and package version.
 */
AnalyticGeometry::AnalyticGeometry (unsigned int level, unsigned int version, unsigned int pkgVersion) 
: GeometryDefinition (SBML_SPATIAL_ANALYTICGEOMETRY,level,version)
   , mAnalyticVolumes (level,version, pkgVersion)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  

  if (!hasValidLevelVersionNamespaceCombination())
	throw SBMLConstructorException();

  connectToChild();
}


/*
 * Creates a new AnalyticGeometry with the given SpatialPkgNamespaces object.
 */
AnalyticGeometry::AnalyticGeometry(SpatialPkgNamespaces* spatialns)
 : GeometryDefinition(SBML_SPATIAL_ANALYTICGEOMETRY,spatialns)
  , mAnalyticVolumes (spatialns)
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
AnalyticGeometry::AnalyticGeometry(const AnalyticGeometry& source) : GeometryDefinition(source)
{
  this->setSpatialId(source.getSpatialId());
  this->mAnalyticVolumes = source.mAnalyticVolumes;

  connectToChild();
}

/*
 * Assignment operator.
 */
AnalyticGeometry& AnalyticGeometry::operator=(const AnalyticGeometry& source)
{
  if(&source!=this)
  {
	this->GeometryDefinition::operator=(source);
	this->mAnalyticVolumes = source.mAnalyticVolumes;
  }
  
  connectToChild();

  return *this;
}


/*
 * Destructor.
 */ 
AnalyticGeometry::~AnalyticGeometry ()
{
}

/*
 * Adds a copy of the given AnalyticVolume to this AnalyticGeometry.
 */
int
AnalyticGeometry::addAnalyticVolume (const AnalyticVolume* av)
{
  if (av == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(av->hasRequiredAttributes()) || !(av->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getAnalyticVolume(av->getId()) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    mAnalyticVolumes.append(av); 
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Creates a new AnalyticVolume object inside this AnalyticGeometry and returns it.
 */
AnalyticVolume*
AnalyticGeometry::createAnalyticVolume ()
{

  AnalyticVolume*av = new AnalyticVolume(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  this->mAnalyticVolumes.appendAndOwn(av);
  return av;
}

/*
 * @return the list of AnalyticVolume for this AnalyticGeometry.
 */
const ListOfAnalyticVolumes*
AnalyticGeometry::getListOfAnalyticVolumes () const
{
	return &this->mAnalyticVolumes;
}

/*
 * @return the list of AnalyticVolume for this AnalyticGeometry.
 */
ListOfAnalyticVolumes*
AnalyticGeometry::getListOfAnalyticVolumes ()
{
	return &this->mAnalyticVolumes;
}

/*
 * @return the nth AnalyticVolume object of this AnalyticGeometry.
 */
const AnalyticVolume*
AnalyticGeometry::getAnalyticVolume (unsigned int n) const
{
  return mAnalyticVolumes.get(n);
}


/*
 * @return the nth AnalyticVolume of this AnalyticGeometry.
 */
AnalyticVolume*
AnalyticGeometry::getAnalyticVolume (unsigned int n)
{
  return mAnalyticVolumes.get(n);
}

/*
 * @return the AnalyticVolume object in this AnalyticGeometry with the given id or NULL
 * if no such AnalyticVolume exists.
 */
const AnalyticVolume*
AnalyticGeometry::getAnalyticVolume (const std::string& sid) const
{
  return mAnalyticVolumes.get(sid);
}

/*
 * @return the AnalyticVolume object in this AnalyticGeometry with the given id or NULL
 * if no such AnalyticVolume exists.
 */
AnalyticVolume*
AnalyticGeometry::getAnalyticVolume (const std::string& sid)
{
  return mAnalyticVolumes.get(sid);
}

/*
 * @return the number of AnalyticVolume in this AnalyticGeometry.
 */
unsigned int
AnalyticGeometry::getNumAnalyticVolumes () const
{
  return this->mAnalyticVolumes.size();
}

/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
  */
void
AnalyticGeometry::connectToChild()
{	
	mAnalyticVolumes.connectToParent(this);
}

/**
 * Removes the nth AnalyticVolume object from this AnalyticGeometry object and
 * returns a pointer to it.
 */
AnalyticVolume* 
AnalyticGeometry::removeAnalyticVolume (unsigned int n)
{
  return mAnalyticVolumes.remove(n);
}


/**
 * Removes the AnalyticVolume object with the given identifier from this AnalyticGeometry
 * object and returns a pointer to it.
 */
AnalyticVolume* 
AnalyticGeometry::removeAnalyticVolume (const std::string& sid)
{
  return mAnalyticVolumes.remove(sid);
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 *
const std::string&
AnalyticGeometry::getElementName () const
{
  static const std::string name = "analyticGeometry";
  return name;
}
*/

/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
AnalyticGeometry::createObject (XMLInputStream& stream)
{
  // return 0;

  const string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "listOfAnalyticVolumes")
  {
    if (mAnalyticVolumes.size() != 0)
    {
      logError(NotSchemaConformant);
    }
    object = &mAnalyticVolumes;
  }

  return object;
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
AnalyticGeometry::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  GeometryDefinition::readAttributes(attributes,expectedAttributes);

  // nothing else to be done, since AnalyticGeometry doesn't have other attributes to be read.
}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
AnalyticGeometry::writeAttributes (XMLOutputStream& stream) const
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
AnalyticGeometry::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getNumAnalyticVolumes() > 0 ) mAnalyticVolumes.write(stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}

AnalyticGeometry*
AnalyticGeometry::clone() const
{
    return new AnalyticGeometry(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
AnalyticGeometry::accept (SBMLVisitor& v) const
{
 return v.visit(*this);
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
AnalyticGeometry::setSBMLDocument (SBMLDocument* d)
{
  GeometryDefinition::setSBMLDocument(d);

  mAnalyticVolumes.setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
AnalyticGeometry::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  GeometryDefinition::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mAnalyticVolumes.enablePackageInternal(pkgURI,pkgPrefix,flag);
}



/** @cond doxygen-c-only */

/**
 * Creates and returns a deep copy of a given AnalyticGeometry_t structure.
 *
 * @param g the AnalyticGeometry_t structure to copy
 * 
 * @return a (deep) copy of this AnalyticGeometry_t structure.
 */
LIBSBML_EXTERN
AnalyticGeometry_t *
AnalyticGeometry_clone (const AnalyticGeometry_t *g)
{
  return static_cast<AnalyticGeometry*>( g->clone() );
}



LIBSBML_CPP_NAMESPACE_END

