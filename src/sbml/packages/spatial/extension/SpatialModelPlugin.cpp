/**
 * @file    SpatialModelPlugin.cpp
 * @brief   Implementation of SpatialModelPlugin, the plugin class of
 *          spatial package for the Model element.
 * @author  
 *
 * $Id: SpatialModelPlugin.cpp 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/extension/SpatialModelPlugin.cpp $
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

#include <sbml/packages/spatial/extension/SpatialModelPlugin.h>

#include <iostream>
using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Constructor
 */
SpatialModelPlugin::SpatialModelPlugin (const std::string &uri, 
                                      const std::string &prefix,
                                      SpatialPkgNamespaces *spatialns)
  : SBasePlugin(uri,prefix, spatialns)
   ,mGeometry(spatialns)
{
}


/*
 * Copy constructor. Creates a copy of this SBase object.
 */
SpatialModelPlugin::SpatialModelPlugin(const SpatialModelPlugin& orig)
  : SBasePlugin(orig)
  , mGeometry(orig.mGeometry)
{
}


/*
 * Destroy this object.
 */
SpatialModelPlugin::~SpatialModelPlugin () {}

/*
 * Assignment operator for SpatialModelPlugin.
 */
SpatialModelPlugin& 
SpatialModelPlugin::operator=(const SpatialModelPlugin& orig)
{
  if(&orig!=this)
  {
    this->SBasePlugin::operator =(orig);
    mGeometry    = orig.mGeometry;
  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialModelPlugin object.
 * 
 * @return a (deep) copy of this SBase object
 */
SpatialModelPlugin* 
SpatialModelPlugin::clone () const
{
  return new SpatialModelPlugin(*this);  
}


/*
 *
 */
SBase*
SpatialModelPlugin::createObject(XMLInputStream& stream)
{
  SBase*        object = 0;

  const std::string&   name   = stream.peek().getName();
  const XMLNamespaces& xmlns  = stream.peek().getNamespaces();
  const std::string&   prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;
  
  if (prefix == targetPrefix)
  {
    if ( name == "geometry" ) 
    {
      //cout << "[DEBUG] SpatialModelPlugin::createObject create "geometry" << endl;
      object = &mGeometry;
    
      if (targetPrefix.empty())
      {
        //
        // (NOTE)
        //
        // A top-level element (geometry) of the spatial extension is located 
        // in a default namespace, and thus xmlns=".." attribute must be added to 
        // the element.
        // This is done by invoking SBMLDocument::enableDefaultNS() function with 
        // the two arguments (the uri of this package and true value).
        //
        mGeometry.getSBMLDocument()->enableDefaultNS(mURI,true);
      }
    }          
  }    

  return object;
}


bool 
SpatialModelPlugin::readOtherXML (SBase* parentObject, XMLInputStream& stream)
{
	return false;
}


/*
 *
 */
void
SpatialModelPlugin::writeElements (XMLOutputStream& stream) const
{
	if (getGeometry() != NULL)
  {
    mGeometry.write(stream);
  }    
}


/* default for components that have no required elements */
bool
SpatialModelPlugin::hasRequiredElements() const
{
  bool allPresent = true;

  return allPresent;
}



/*
 *
 *  (EXTENSION) Additional public functions
 *
 */  


/*
 * Returns the Geometry for this Model.
 */
const Geometry*
SpatialModelPlugin::getGeometry () const
{
  return &this->mGeometry;
}


/*
 * Returns the Geometry for this Model.
 */
Geometry*
SpatialModelPlugin::getGeometry ()
{
  return &this->mGeometry;
}


/*
 * Sets the parent SBMLDocument of this SBML object.
 *
 * @param d the SBMLDocument object to use
 */
void 
SpatialModelPlugin::setSBMLDocument (SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  mGeometry.setSBMLDocument(d);  
}


/*
 * Sets the parent SBML object of this plugin object to
 * this object and child elements (if any).
 * (Creates a child-parent relationship by this plugin object)
 */
void
SpatialModelPlugin::connectToParent (SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  mGeometry.connectToParent(sbase);
}


/*
 * Enables/Disables the given package with child elements in this plugin
 * object (if any).
 */
void
SpatialModelPlugin::enablePackageInternal(const std::string& pkgURI,
                                        const std::string& pkgPrefix, bool flag)
{
  mGeometry.enablePackageInternal(pkgURI,pkgPrefix,flag);
}

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
