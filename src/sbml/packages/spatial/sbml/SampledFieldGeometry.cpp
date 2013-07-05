/**
 * @file    SampledFieldGeometry.cpp
 * @brief   Implementation of AnalyticSampledFieldGeometry, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: SampledFieldGeometry.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/SampledFieldGeometry.cpp $
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

#include <sbml/packages/spatial/sbml/SampledFieldGeometry.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/sbml/SampledVolume.h>
#include <sbml/packages/spatial/sbml/SampledField.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new SampledFieldGeometry with the given level, version, and package version.
 */
SampledFieldGeometry::SampledFieldGeometry (unsigned int level, unsigned int version, unsigned int pkgVersion) 
: GeometryDefinition (SBML_SPATIAL_SAMPLEDFIELDGEOMETRY,level,version)
   , mSampledField(0)
   , mSampledVolumes (level,version, pkgVersion)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  

  if (!hasValidLevelVersionNamespaceCombination())
	throw SBMLConstructorException();

  connectToChild();
}


/*
 * Creates a new SampledFieldGeometry with the given SpatialPkgNamespaces object.
 */
SampledFieldGeometry::SampledFieldGeometry(SpatialPkgNamespaces* spatialns)
: GeometryDefinition(SBML_SPATIAL_SAMPLEDFIELDGEOMETRY,spatialns)
  , mSampledField(0)
  , mSampledVolumes (spatialns)
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
SampledFieldGeometry::SampledFieldGeometry(const SampledFieldGeometry& source) : GeometryDefinition(source)
{
  this->setSpatialId(source.getSpatialId());
  this->mSampledVolumes = source.mSampledVolumes;

  if (source.mSampledField)
  {
	  mSampledField = static_cast<SampledField*>( source.mSampledField->clone() );
  }

  connectToChild();
}

/*
 * Assignment operator.
 */
SampledFieldGeometry& SampledFieldGeometry::operator=(const SampledFieldGeometry& source)
{
  if(&source!=this)
  {
	this->GeometryDefinition::operator=(source);
	this->mSampledVolumes = source.mSampledVolumes;
  }
  
  delete mSampledField;
  if (source.mSampledField)
  {
	mSampledField = static_cast<SampledField*>( source.mSampledField->clone() );
  }
  else
  {
    mSampledField = 0;
  }

  connectToChild();

  return *this;
}


/*
 * Destructor.
 */ 
SampledFieldGeometry::~SampledFieldGeometry ()
{
	delete mSampledField;
}

/*
 * @return the SampledField of this SampledFieldGeometry.
 */
const SampledField*
SampledFieldGeometry::getSampledField () const
{
  return mSampledField;
}

/*
 * @return the SampledField of this SampledFieldGeometry.
 */
SampledField*
SampledFieldGeometry::getSampledField ()
{
  return mSampledField;
}

/*
 * @return true if the SampledField of this SampledFieldGeometry has been set, false
 * otherwise.
 */
bool
SampledFieldGeometry::isSetSampledField () const
{
  return (mSampledField != 0);
}

/*
 * Sets the SampledField of this SampledFieldGeometry to a copy of the given SampledField.
 */
int
SampledFieldGeometry::setSampledField (const SampledField* sf)
{
  if (mSampledField == sf)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (sf == NULL)
  {
    delete mSampledField;
    mSampledField = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mSampledField;
    mSampledField = static_cast<SampledField*>( sf->clone() );

    if (mSampledField) mSampledField->connectToParent(this);
    
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Unsets the SampledField of this SampledFieldGeometry.
 */
int
SampledFieldGeometry::unsetSampledField ()
{
  delete mSampledField;
  mSampledField = 0;

  if (mSampledField == NULL) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Adds a copy of the given SampledVolume to this SampledFieldGeometry.
 */
int
SampledFieldGeometry::addSampledVolume (const SampledVolume* sv)
{
  if (sv == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(sv->hasRequiredAttributes()) || !(sv->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getSampledVolume(sv->getId()) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    mSampledVolumes.append(sv); 
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Creates a new SampledVolume object inside this SampledFieldGeometry and returns it.
 */
SampledVolume*
SampledFieldGeometry::createSampledVolume ()
{

  SampledVolume*sv = new SampledVolume(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  this->mSampledVolumes.appendAndOwn(sv);
  return sv;
}

/*
 * Creates a new SampledField for this SampledFieldGeometry and returns it.  If this
 * SampledFieldGeometry had a previous SampledField, it will be destroyed.
 */
SampledField*
SampledFieldGeometry::createSampledField ()
{
  delete mSampledField;
  mSampledField = 0;

  try
  {
    mSampledField = new SampledField(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
  }

  if (mSampledField)
  {
    mSampledField->connectToParent(this);
  }

  return mSampledField;
}


/*
 * @return the list of SampledVolume for this SampledFieldGeometry.
 */
const ListOfSampledVolumes*
SampledFieldGeometry::getListOfSampledVolumes () const
{
	return &this->mSampledVolumes;
}

/*
 * @return the list of SampledVolume for this SampledFieldGeometry.
 */
ListOfSampledVolumes*
SampledFieldGeometry::getListOfSampledVolumes () 
{
	return &this->mSampledVolumes;
}

/*
 * @return the nth SampledVolume object of this SampledFieldGeometry.
 */
const SampledVolume*
SampledFieldGeometry::getSampledVolume (unsigned int n) const
{
  return mSampledVolumes.get(n);
}

/*
 * @return the nth SampledVolume of this SampledFieldGeometry.
 */
SampledVolume*
SampledFieldGeometry::getSampledVolume (unsigned int n)
{
  return mSampledVolumes.get(n);
}

/*
 * @return the SampledVolume object in this SampledFieldGeometry with the given id or NULL
 * if no such SampledVolume exists.
 */
const SampledVolume*
SampledFieldGeometry::getSampledVolume (const std::string& sid) const
{
  return mSampledVolumes.get(sid);
}

/*
 * @return the SampledVolume object in this SampledFieldGeometry with the given id or NULL
 * if no such SampledVolume exists.
 */
SampledVolume*
SampledFieldGeometry::getSampledVolume (const std::string& sid)
{
  return mSampledVolumes.get(sid);
}

/*
 * @return the number of SampledVolume in this SampledFieldGeometry.
 */
unsigned int
SampledFieldGeometry::getNumSampledVolumes () const
{
  return this->mSampledVolumes.size();
}

/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
  */
void
SampledFieldGeometry::connectToChild()
{	
	mSampledVolumes.connectToParent(this);

	if (mSampledField) mSampledField->connectToParent(this);
}

/**
 * Removes the nth SampledVolume object from this SampledFieldGeometry object and
 * returns a pointer to it.
 */
SampledVolume* 
SampledFieldGeometry::removeSampledVolume (unsigned int n)
{
  return mSampledVolumes.remove(n);
}


/**
 * Removes the SampledVolume object with the given identifier from this SampledFieldGeometry
 * object and returns a pointer to it.
 */
SampledVolume* 
SampledFieldGeometry::removeSampledVolume (const std::string& sid)
{
  return mSampledVolumes.remove(sid);
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
SampledFieldGeometry::createObject (XMLInputStream& stream)
{
  // return 0;

  const string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "listOfSampledVolumes")
  {
    if (mSampledVolumes.size() != 0)
    {
      logError(NotSchemaConformant);
    }
    object = &mSampledVolumes;
  }

  if (name == "sampledField") {
	  if (mSampledField) 
	  {
		logError(NotSchemaConformant);
	  }
	  delete mSampledField;

	  try
	  {
		mSampledField = new SampledField(static_cast<SpatialPkgNamespaces*>(mSBMLNamespaces));
	  }
	  catch ( ... )
	  {
		mSampledField = new SampledField(SBMLDocument::getDefaultLevel(),
 			SBMLDocument::getDefaultVersion());
	  }

	  object = mSampledField;
  }

  return object;
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SampledFieldGeometry::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  GeometryDefinition::readAttributes(attributes,expectedAttributes);

  // nothing else to be done, since SampledFieldGeometry doesn't have other attributes to be read.
}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SampledFieldGeometry::writeAttributes (XMLOutputStream& stream) const
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
SampledFieldGeometry::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getNumSampledVolumes() > 0 ) mSampledVolumes.write(stream);

  if (mSampledField) mSampledField->write(stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}

SampledFieldGeometry*
SampledFieldGeometry::clone() const
{
    return new SampledFieldGeometry(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
SampledFieldGeometry::accept (SBMLVisitor& v) const
{
 // return v.visit(*this);
  bool result = v.visit(*this);

  mSampledVolumes.accept(v);

  if (mSampledField) mSampledField->accept(v);

//  v.leave(*this);

  return result;

}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
SampledFieldGeometry::setSBMLDocument (SBMLDocument* d)
{
  GeometryDefinition::setSBMLDocument(d);

  mSampledVolumes.setSBMLDocument(d);

  if (mSampledField) mSampledField->setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
SampledFieldGeometry::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  GeometryDefinition::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mSampledVolumes.enablePackageInternal(pkgURI,pkgPrefix,flag);

  if (mSampledField) mSampledField->enablePackageInternal(pkgURI,pkgPrefix,flag);
}



/** @cond doxygen-c-only */

/**
 * Creates and returns a deep copy of a given SampledFieldGeometry_t structure.
 *
 * @param g the SampledFieldGeometry_t structure to copy
 * 
 * @return a (deep) copy of this SampledFieldGeometry_t structure.
 */
LIBSBML_EXTERN
SampledFieldGeometry_t *
SampledFieldGeometry_clone (const SampledFieldGeometry_t *sfg)
{
  return static_cast<SampledFieldGeometry*>( sfg->clone() );
}



LIBSBML_CPP_NAMESPACE_END

