/**
 * @file    SpatialReactionPlugin.cpp
 * @brief   Implementation of SpatialReactionPlugin, the plugin class of
 *          spatial package for the SBase element.
 * @author  
 *
 * $Id: SpatialReactionPlugin.cpp 10673 2010-01-17 07:18:20Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/extension/SpatialReactionPlugin.cpp $
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

#include <sbml/packages/spatial/extension/SpatialReactionPlugin.h>

#include <iostream>

#ifdef __cplusplus

//using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Constructor
 */
SpatialReactionPlugin::SpatialReactionPlugin (const std::string &uri, const std::string &prefix, SpatialPkgNamespaces *spatialns)
  : SBasePlugin(uri,prefix, spatialns)
  , mIsLocal(false)
  , mIsSetLocal(false)
{
}


/*
 * Copy constructor. Creates a copy of this SBase object.
 */
SpatialReactionPlugin::SpatialReactionPlugin(const SpatialReactionPlugin& orig)
  : SBasePlugin(orig)
  , mIsLocal(orig.mIsLocal)
  , mIsSetLocal(orig.mIsSetLocal)
{
}


/*
 * Destroy this object.
 */
SpatialReactionPlugin::~SpatialReactionPlugin () {}

/*
 * Assignment operator for SpatialReactionPlugin.
 */
SpatialReactionPlugin& 
SpatialReactionPlugin::operator=(const SpatialReactionPlugin& orig)
{
  if(&orig!=this)
  {
    this->SBasePlugin::operator =(orig);
    this->mIsLocal = orig.mIsLocal;
  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialReactionPlugin object.
 * 
 * @return a (deep) copy of this SBase object
 */
SpatialReactionPlugin* 
SpatialReactionPlugin::clone () const
{
  return new SpatialReactionPlugin(*this);  
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
SpatialReactionPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  attributes.add("isLocal");
}

/**
 *
 */
void 
SpatialReactionPlugin::readAttributes (const XMLAttributes& attributes,
                                    const ExpectedAttributes& expectedAttributes)
{
  SBasePlugin::readAttributes(attributes, expectedAttributes);
  
  mIsSetLocal = attributes.readInto("isLocal", mIsLocal, getErrorLog(), 
    mParent != NULL && mParent->getTypeCode() == SBML_REACTION, 
    getLine(), getColumn());
}


/**
 *
 */
void 
SpatialReactionPlugin::writeAttributes (XMLOutputStream& stream) const
{
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

  
void 
SpatialReactionPlugin::unsetIsLocal()
{
  mIsSetLocal = false;
}

bool 
SpatialReactionPlugin::isSetIsLocal() const
{
  return mIsSetLocal;
}


bool
SpatialReactionPlugin::getIsLocal() const
{
	return mIsLocal;
}
  
int 
SpatialReactionPlugin::setIsLocal(bool value) 
{
  mIsLocal = value;
  mIsSetLocal = true;
  return LIBSBML_OPERATION_SUCCESS;
}



LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
