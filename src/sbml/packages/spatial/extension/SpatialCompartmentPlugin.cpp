/**
 * @file    SpatialCompartmentPlugin.cpp
 * @brief   Implementation of SpatialCompartmentPlugin, the plugin class of
 *          spatial package for the Parameter element.
 * @author  
 *
 * $Id: SpatialCompartmentPlugin.cpp 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/extension/SpatialCompartmentPlugin.cpp $
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

#include <sbml/packages/spatial/extension/SpatialCompartmentPlugin.h>

#include <iostream>
using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Constructor
 */
SpatialCompartmentPlugin::SpatialCompartmentPlugin (const std::string &uri, 
                                      const std::string &prefix,
                                      SpatialPkgNamespaces *spatialns)
  : SBasePlugin(uri,prefix, spatialns)
   , mCompartmentMapping(spatialns)
{
}


/*
 * Copy constructor. Creates a copy of this SBase object.
 */
SpatialCompartmentPlugin::SpatialCompartmentPlugin(const SpatialCompartmentPlugin& orig)
  : SBasePlugin(orig)
  , mCompartmentMapping(orig.mCompartmentMapping)
{
}


/*
 * Destroy this object.
 */
SpatialCompartmentPlugin::~SpatialCompartmentPlugin () {}

/*
 * Assignment operator for SpatialCompartmentPlugin.
 */
SpatialCompartmentPlugin& 
SpatialCompartmentPlugin::operator=(const SpatialCompartmentPlugin& orig)
{
  if(&orig!=this)
  {
    this->SBasePlugin::operator =(orig);
    mCompartmentMapping    = orig.mCompartmentMapping;
  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialCompartmentPlugin object.
 * 
 * @return a (deep) copy of this SBase object
 */
SpatialCompartmentPlugin* 
SpatialCompartmentPlugin::clone () const
{
  return new SpatialCompartmentPlugin(*this);  
}


/*
 *
 */
SBase*
SpatialCompartmentPlugin::createObject(XMLInputStream& stream)
{
  SBase*        object = 0;

  const std::string&   name   = stream.peek().getName();
  const XMLNamespaces& xmlns  = stream.peek().getNamespaces();
  const std::string&   prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;
  
  if (prefix == targetPrefix)
  {
    if ( name == "compartmentMapping" ) 
    {
      //cout << "[DEBUG] SpatialCompartmentPlugin::createObject create "compartmentMapping" << endl;
      object = &mCompartmentMapping;
    
      if (targetPrefix.empty())
      {
        //
        // (NOTE)
        //
        // A top-level element (compartmentMapping) of the spatial extension is located 
        // in a default namespace, and thus xmlns=".." attribute must be added to 
        // the element.
        // This is done by invoking SBMLDocument::enableDefaultNS() function with 
        // the two arguments (the uri of this package and true value).
        //
        mCompartmentMapping.getSBMLDocument()->enableDefaultNS(mURI,true);
      }
    }          
  }    

  return object;
}

bool 
SpatialCompartmentPlugin::readOtherXML (SBase* parentObject, XMLInputStream& stream)
{
	return false;
}

/*
 *
 */
void
SpatialCompartmentPlugin::writeElements (XMLOutputStream& stream) const
{
  if (getCompartmentMapping()->isSetSpatialId())
  {
    mCompartmentMapping.write(stream);
  }
}


/* default for components that have no required elements */
bool
SpatialCompartmentPlugin::hasRequiredElements() const
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
 * Returns the CompartmentMapping for this Parameter.
 */
const CompartmentMapping*
SpatialCompartmentPlugin::getCompartmentMapping () const
{
  return &this->mCompartmentMapping;
}


/*
 * Returns the CompartmentMapping for this Parameter.
 */
CompartmentMapping*
SpatialCompartmentPlugin::getCompartmentMapping ()
{
  return &this->mCompartmentMapping;
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 *
 * @param d the SBMLDocument object to use
 */
void 
SpatialCompartmentPlugin::setSBMLDocument (SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  mCompartmentMapping.setSBMLDocument(d);
}


/*
 * Sets the parent SBML object of this plugin object to
 * this object and child elements (if any).
 * (Creates a child-parent relationship by this plugin object)
 */
void
SpatialCompartmentPlugin::connectToParent (SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  mCompartmentMapping.connectToParent(sbase);
}


/*
 * Enables/Disables the given package with child elements in this plugin
 * object (if any).
 */
void
SpatialCompartmentPlugin::enablePackageInternal(const std::string& pkgURI,
                                        const std::string& pkgPrefix, bool flag)
{
  mCompartmentMapping.enablePackageInternal(pkgURI,pkgPrefix,flag);
}

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
