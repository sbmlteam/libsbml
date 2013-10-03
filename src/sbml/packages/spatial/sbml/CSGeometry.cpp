/**
 * @file    CSGeometry.cpp
 * @brief   Implementation of CSGeometry, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGeometry.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.pon.sourceforge.net/ponroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGeometry.cpp $
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

#include <sbml/packages/spatial/sbml/CSGeometry.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/sbml/CSGObject.h>
#include <sbml/packages/spatial/sbml/CSGNode.h>


using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new CSGeometry with the given level, version, and package version.
 */
CSGeometry::CSGeometry (unsigned int level, unsigned int version, unsigned int pkgVersion) 
: GeometryDefinition (SBML_SPATIAL_CSGGEOMETRY,level,version)
   , mCSGObjects (level,version, pkgVersion)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  

  if (!hasValidLevelVersionNamespaceCombination())
	throw SBMLConstructorException();

  connectToChild();
}


/*
 * Creates a new CSGeometry with the given SpatialPkgNamespaces object.
 */
CSGeometry::CSGeometry(SpatialPkgNamespaces* spatialns)
	: GeometryDefinition(SBML_SPATIAL_CSGGEOMETRY,spatialns)
  , mCSGObjects (spatialns)
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
CSGeometry::CSGeometry(const CSGeometry& source) : GeometryDefinition(source)
{
  this->setSpatialId(source.getSpatialId());
  this->mCSGObjects = source.mCSGObjects;

  connectToChild();
}

/*
 * Assignment operator.
 */
CSGeometry& CSGeometry::operator=(const CSGeometry& source)
{
  if(&source!=this)
  {
	this->GeometryDefinition::operator=(source);
	this->mCSGObjects = source.mCSGObjects;
  }
  
  connectToChild();

  return *this;
}


/*
 * Destructor.
 */ 
CSGeometry::~CSGeometry ()
{
}

/*
 * Adds a copy of the given CSGObject to this CSGeometry.
 */
int
CSGeometry::addCSGObject (const CSGObject* csgo)
{
  if (csgo == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(csgo->hasRequiredAttributes()) || !(csgo->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getCSGObject(csgo->getId()) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    mCSGObjects.append(csgo); 
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Creates a new CSGObject inside this CSGeometry and returns it.
 */
CSGObject*
CSGeometry::createCSGObject ()
{

  CSGObject*csgo = new CSGObject(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  this->mCSGObjects.appendAndOwn(csgo);
  return csgo;
}


/*
 * @return the list of CSGObject for this CSGeometry.
 */
const ListOfCSGObjects*
CSGeometry::getListOfCSGObjects () const
{
	return &this->mCSGObjects;
}

/*
 * @return the list of CSGObject for this CSGeometry.
 */
ListOfCSGObjects*
CSGeometry::getListOfCSGObjects () 
{
	return &this->mCSGObjects;
}

/*
 * @return the nth CSGObject object of this CSGeometry.
 */
const CSGObject*
CSGeometry::getCSGObject (unsigned int n) const
{
  return mCSGObjects.get(n);
}

/*
 * @return the nth CSGObject of this CSGeometry.
 */
CSGObject*
CSGeometry::getCSGObject (unsigned int n)
{
  return mCSGObjects.get(n);
}

/*
 * @return the CSGObject object in this CSGeometry with the given id or NULL
 * if no such CSGObject exists.
 */
const CSGObject*
CSGeometry::getCSGObject (const std::string& sid) const
{
  return mCSGObjects.get(sid);
}

/*
 * @return the CSGObject object in this CSGeometry with the given id or NULL
 * if no such CSGObject exists.
 */
CSGObject*
CSGeometry::getCSGObject (const std::string& sid)
{
  return mCSGObjects.get(sid);
}

/*
 * @return the number of CSGObject in this CSGeometry.
 */
unsigned int
CSGeometry::getNumCSGObjects () const
{
  return this->mCSGObjects.size();
}

/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
  */
void
CSGeometry::connectToChild()
{
  GeometryDefinition::connectToChild();
	mCSGObjects.connectToParent(this);
}

/**
 * Removes the nth CSGObject object from this CSGeometry object and
 * returns a pointer to it.
 */
CSGObject* 
CSGeometry::removeCSGObject (unsigned int n)
{
  return mCSGObjects.remove(n);
}


/**
 * Removes the CSGObject object with the given identifier from this CSGeometry
 * object and returns a pointer to it.
 */
CSGObject* 
CSGeometry::removeCSGObject (const std::string& sid)
{
  return mCSGObjects.remove(sid);
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.

const std::string&
CSGeometry::getElementName () const
{
  static const std::string name = "CSGeometry";
  return name;
}
*/

/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
CSGeometry::createObject (XMLInputStream& stream)
{
  // return 0;

  const string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "listOfCSGObjects")
  {
    if (mCSGObjects.size() != 0)
    {
      logError(NotSchemaConformant);
    }
    object = &mCSGObjects;
  }

  return object;
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CSGeometry::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  GeometryDefinition::readAttributes(attributes,expectedAttributes);

  // nothing else to be done, since CSGeometry doesn't have other attributes to be read.
}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CSGeometry::writeAttributes (XMLOutputStream& stream) const
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
CSGeometry::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getNumCSGObjects() > 0 ) mCSGObjects.write(stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}

CSGeometry*
CSGeometry::clone() const
{
    return new CSGeometry(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGeometry::accept (SBMLVisitor& v) const
{
 // return v.visit(*this);
  bool result = v.visit(*this);

  mCSGObjects.accept(v);

//  v.leave(*this);

  return result;

}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
CSGeometry::setSBMLDocument (SBMLDocument* d)
{
  GeometryDefinition::setSBMLDocument(d);

  mCSGObjects.setSBMLDocument(d);

}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
CSGeometry::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  GeometryDefinition::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mCSGObjects.enablePackageInternal(pkgURI,pkgPrefix,flag);

}



/** @cond doxygenCOnly */

/**
 * Creates and returns a deep copy of a given CSGeometry_t structure.
 *
 * @param g the CSGeometry_t structure to copy
 * 
 * @return a (deep) copy of this CSGeometry_t structure.
 */
LIBSBML_EXTERN
CSGeometry_t *
CSGeometry_clone (const CSGeometry_t *csg)
{
  return static_cast<CSGeometry*>( csg->clone() );
}



LIBSBML_CPP_NAMESPACE_END

