/**
 * @file    SpatialSpeciesRxnPlugin.cpp
 * @brief   Implementation of SpatialSpeciesRxnPlugin, the plugin class of
 *          spatial package for the SBase element.
 * @author  
 *
 * $Id: SpatialSpeciesRxnPlugin.cpp 10673 2010-01-17 07:18:20Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/extension/SpatialSpeciesRxnPlugin.cpp $
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

#include <sbml/packages/spatial/extension/SpatialSpeciesRxnPlugin.h>

#include <iostream>

#ifdef __cplusplus

//using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Constructor
 */
SpatialSpeciesRxnPlugin::SpatialSpeciesRxnPlugin (const std::string &uri, const std::string &prefix, SpatialPkgNamespaces *spatialns)
  : SBasePlugin(uri,prefix, spatialns)
  , mIsSpatial(false)
  , mIsSetSpatial(false)
  , mIsLocal(false)
  , mIsSetLocal(false)
{
}


/*
 * Copy constructor. Creates a copy of this SBase object.
 */
SpatialSpeciesRxnPlugin::SpatialSpeciesRxnPlugin(const SpatialSpeciesRxnPlugin& orig)
  : SBasePlugin(orig)
  , mIsSpatial(orig.mIsSpatial)
  , mIsSetSpatial(orig.mIsSetSpatial)
  , mIsLocal(orig.mIsLocal)
  , mIsSetLocal(orig.mIsSetLocal)
{
}


/*
 * Destroy this object.
 */
SpatialSpeciesRxnPlugin::~SpatialSpeciesRxnPlugin () {}

/*
 * Assignment operator for SpatialSpeciesRxnPlugin.
 */
SpatialSpeciesRxnPlugin& 
SpatialSpeciesRxnPlugin::operator=(const SpatialSpeciesRxnPlugin& orig)
{
  if(&orig!=this)
  {
    this->SBasePlugin::operator =(orig);
    this->mIsSpatial = orig.mIsSpatial;
    this->mIsLocal = orig.mIsLocal;
  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialSpeciesRxnPlugin object.
 * 
 * @return a (deep) copy of this SBase object
 */
SpatialSpeciesRxnPlugin* 
SpatialSpeciesRxnPlugin::clone () const
{
  return new SpatialSpeciesRxnPlugin(*this);  
}

// -----------------------------------------------
//
// virtual functions for attributes
//
// ------------------------------------------------


/** @cond doxygenLibsbmlInternal */

/**
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
SpatialSpeciesRxnPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  attributes.add("isLocal");
  attributes.add("isSpatial");
}

/**
 *
 */
void 
SpatialSpeciesRxnPlugin::readAttributes (const XMLAttributes& attributes,
                                    const ExpectedAttributes& expectedAttributes)
{
  SBasePlugin::readAttributes(attributes, expectedAttributes);
  
  mIsSetSpatial = attributes.readInto("isSpatial", mIsSpatial, getErrorLog(), 
    mParent != NULL && mParent->getTypeCode() == SBML_SPECIES, 
    getLine(), getColumn());
  mIsSetLocal = attributes.readInto("isLocal", mIsLocal, getErrorLog(), 
    mParent != NULL && mParent->getTypeCode() == SBML_REACTION, 
    getLine(), getColumn());
}


/**
 *
 */
void 
SpatialSpeciesRxnPlugin::writeAttributes (XMLOutputStream& stream) const
{
  if ( isSetIsSpatial() ) 
  {
	  XMLTriple tripleIsSpatial("isSpatial", mURI, mPrefix);
	  stream.writeAttribute(tripleIsSpatial, mIsSpatial);
  }
  if ( isSetIsLocal() )
  {
	  XMLTriple tripleIsLocal("isLocal", mURI, mPrefix);
	  stream.writeAttribute(tripleIsLocal, mIsLocal);
  }
}

/** @endcond doxygenLibsbmlInternal */

/*
 *
 *  (EXTENSION) Additional public functions
 *
 */  

  
bool 
SpatialSpeciesRxnPlugin::isSetIsSpatial() const
{
  return mIsSetSpatial;
}

void 
SpatialSpeciesRxnPlugin::unsetIsSpatial()
{
  mIsSetSpatial = false;
}
void 
SpatialSpeciesRxnPlugin::unsetIsLocal()
{
  mIsSetLocal = false;
}

bool 
SpatialSpeciesRxnPlugin::isSetIsLocal() const
{
  return mIsSetLocal;
}


bool
SpatialSpeciesRxnPlugin::getIsSpatial() const
{
	return mIsSpatial;
}
  
int 
SpatialSpeciesRxnPlugin::setIsSpatial(bool value) 
{
  mIsSpatial = value;
  mIsSetSpatial = true;
  return LIBSBML_OPERATION_SUCCESS;
}


bool
SpatialSpeciesRxnPlugin::getIsLocal() const
{
	return mIsLocal;
}
  
int 
SpatialSpeciesRxnPlugin::setIsLocal(bool value) 
{
  mIsLocal = value;
  mIsSetLocal = true;
  return LIBSBML_OPERATION_SUCCESS;
}



LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
