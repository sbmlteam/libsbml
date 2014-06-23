/**
 * @file    SpatialSpeciesPlugin.cpp
 * @brief   Implementation of SpatialSpeciesPlugin, the plugin class of
 *          spatial package for the SBase element.
 * @author  
 *
 * $Id: SpatialSpeciesPlugin.cpp 10673 2010-01-17 07:18:20Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/extension/SpatialSpeciesPlugin.cpp $
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

#include <sbml/packages/spatial/extension/SpatialSpeciesPlugin.h>

#include <iostream>

#ifdef __cplusplus

//using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Constructor
 */
SpatialSpeciesPlugin::SpatialSpeciesPlugin (const std::string &uri, const std::string &prefix, SpatialPkgNamespaces *spatialns)
  : SBasePlugin(uri,prefix, spatialns)
  , mIsSpatial(false)
  , mIsSetSpatial(false)
{
}


/*
 * Copy constructor. Creates a copy of this SBase object.
 */
SpatialSpeciesPlugin::SpatialSpeciesPlugin(const SpatialSpeciesPlugin& orig)
  : SBasePlugin(orig)
  , mIsSpatial(orig.mIsSpatial)
  , mIsSetSpatial(orig.mIsSetSpatial)
{
}


/*
 * Destroy this object.
 */
SpatialSpeciesPlugin::~SpatialSpeciesPlugin () {}

/*
 * Assignment operator for SpatialSpeciesPlugin.
 */
SpatialSpeciesPlugin& 
SpatialSpeciesPlugin::operator=(const SpatialSpeciesPlugin& orig)
{
  if(&orig!=this)
  {
    this->SBasePlugin::operator =(orig);
    this->mIsSpatial = orig.mIsSpatial;
  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialSpeciesPlugin object.
 * 
 * @return a (deep) copy of this SBase object
 */
SpatialSpeciesPlugin* 
SpatialSpeciesPlugin::clone () const
{
  return new SpatialSpeciesPlugin(*this);  
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
SpatialSpeciesPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  attributes.add("isSpatial");
}

/**
 *
 */
void 
SpatialSpeciesPlugin::readAttributes (const XMLAttributes& attributes,
                                    const ExpectedAttributes& expectedAttributes)
{
  SBasePlugin::readAttributes(attributes, expectedAttributes);
  
  mIsSetSpatial = attributes.readInto("isSpatial", mIsSpatial, getErrorLog(), 
    mParent != NULL && mParent->getTypeCode() == SBML_SPECIES, 
    getLine(), getColumn());
}


/**
 *
 */
void 
SpatialSpeciesPlugin::writeAttributes (XMLOutputStream& stream) const
{
  if ( isSetIsSpatial() ) 
  {
	  XMLTriple tripleIsSpatial("isSpatial", mURI, mPrefix);
	  stream.writeAttribute(tripleIsSpatial, mIsSpatial);
  }
}

/** @endcond doxygenLibsbmlInternal */

/*
 *
 *  (EXTENSION) Additional public functions
 *
 */  

  
bool 
SpatialSpeciesPlugin::isSetIsSpatial() const
{
  return mIsSetSpatial;
}

void 
SpatialSpeciesPlugin::unsetIsSpatial()
{
  mIsSetSpatial = false;
}

bool
SpatialSpeciesPlugin::getIsSpatial() const
{
	return mIsSpatial;
}
  
int 
SpatialSpeciesPlugin::setIsSpatial(bool value) 
{
  mIsSpatial = value;
  mIsSetSpatial = true;
  return LIBSBML_OPERATION_SUCCESS;
}


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
