/**
 * @file    SpatialParameterPlugin.cpp
 * @brief   Implementation of SpatialParameterPlugin, the plugin class of
 *          spatial package for the Parameter element.
 * @author  
 *
 * $Id: SpatialParameterPlugin.cpp 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/extension/SpatialParameterPlugin.cpp $
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

#include <sbml/packages/spatial/extension/SpatialParameterPlugin.h>

#include <iostream>
using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Constructor
 */
SpatialParameterPlugin::SpatialParameterPlugin (const std::string &uri, 
                                      const std::string &prefix,
                                      SpatialPkgNamespaces *spatialns)
  : SBasePlugin(uri,prefix, spatialns)
   , mSpatialSymbolReference(spatialns)
   , mDiffusionCoefficient(spatialns)
   , mAdvectionCoefficient(spatialns)
   , mBoundaryCondition(spatialns)
{
}


/*
 * Copy constructor. Creates a copy of this SBase object.
 */
SpatialParameterPlugin::SpatialParameterPlugin(const SpatialParameterPlugin& orig)
  : SBasePlugin(orig)
  , mSpatialSymbolReference(orig.mSpatialSymbolReference)
  , mDiffusionCoefficient(orig.mDiffusionCoefficient)
  , mAdvectionCoefficient(orig.mAdvectionCoefficient)
  , mBoundaryCondition(orig.mBoundaryCondition)

{
}


/*
 * Destroy this object.
 */
SpatialParameterPlugin::~SpatialParameterPlugin () {}

/*
 * Assignment operator for SpatialParameterPlugin.
 */
SpatialParameterPlugin& 
SpatialParameterPlugin::operator=(const SpatialParameterPlugin& orig)
{
  if(&orig!=this)
  {
    this->SBasePlugin::operator =(orig);
    mSpatialSymbolReference    = orig.mSpatialSymbolReference;
	mDiffusionCoefficient      = orig.mDiffusionCoefficient;
	mAdvectionCoefficient      = orig.mAdvectionCoefficient;
	mBoundaryCondition		   = orig.mBoundaryCondition;
  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialParameterPlugin object.
 * 
 * @return a (deep) copy of this SBase object
 */
SpatialParameterPlugin* 
SpatialParameterPlugin::clone () const
{
  return new SpatialParameterPlugin(*this);  
}


/*
 *
 */
SBase*
SpatialParameterPlugin::createObject(XMLInputStream& stream)
{
  SBase*        object = 0;

  const std::string&   name   = stream.peek().getName();
  const XMLNamespaces& xmlns  = stream.peek().getNamespaces();
  const std::string&   prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;
  
  if (prefix == targetPrefix)
  {
    if ( name == "spatialSymbolReference" ) 
    {
      //cout << "[DEBUG] SpatialParameterPlugin::createObject create "spatialSymbolReference" << endl;
      object = &mSpatialSymbolReference;
    
      if (targetPrefix.empty())
      {
        //
        // (NOTE)
        //
        // A top-level element (spatialSymbolReference) of the spatial extension is located 
        // in a default namespace, and thus xmlns=".." attribute must be added to 
        // the element.
        // This is done by invoking SBMLDocument::enableDefaultNS() function with 
        // the two arguments (the uri of this package and true value).
        //
        mSpatialSymbolReference.getSBMLDocument()->enableDefaultNS(mURI,true);
      }
    }          
    if ( name == "diffusionCoefficient" ) 
    {
      //cout << "[DEBUG] SpatialParameterPlugin::createObject create "diffusionCoefficient" << endl;
      object = &mDiffusionCoefficient;
    
      if (targetPrefix.empty())
      {
        //
        // (NOTE)
        //
        // A top-level element (diffusionCoefficient) of the spatial extension is located 
        // in a default namespace, and thus xmlns=".." attribute must be added to 
        // the element.
        // This is done by invoking SBMLDocument::enableDefaultNS() function with 
        // the two arguments (the uri of this package and true value).
        //
        mDiffusionCoefficient.getSBMLDocument()->enableDefaultNS(mURI,true);
      }
    }          
    if ( name == "advectionCoefficient" ) 
    {
      //cout << "[DEBUG] SpatialParameterPlugin::createObject create "advectionCoefficient" << endl;
      object = &mAdvectionCoefficient;
    
      if (targetPrefix.empty())
      {
        //
        // (NOTE)
        //
        // A top-level element (advectionCoefficient) of the spatial extension is located 
        // in a default namespace, and thus xmlns=".." attribute must be added to 
        // the element.
        // This is done by invoking SBMLDocument::enableDefaultNS() function with 
        // the two arguments (the uri of this package and true value).
        //
        mAdvectionCoefficient.getSBMLDocument()->enableDefaultNS(mURI,true);
      }
    }          

    if ( name == "boundaryCondition" ) 
    {
      //cout << "[DEBUG] SpatialParameterPlugin::createObject create "boundaryCondition" << endl;
      object = &mBoundaryCondition;
    
      if (targetPrefix.empty())
      {
        //
        // (NOTE)
        //
        // A top-level element (boundaryCondition) of the spatial extension is located 
        // in a default namespace, and thus xmlns=".." attribute must be added to 
        // the element.
        // This is done by invoking SBMLDocument::enableDefaultNS() function with 
        // the two arguments (the uri of this package and true value).
        //
        mBoundaryCondition.getSBMLDocument()->enableDefaultNS(mURI,true);
      }
    }          
  }    

  return object;
}

bool 
SpatialParameterPlugin::readOtherXML (SBase* parentObject, XMLInputStream& stream)
{
	return false;
}

/*
 *
 */
void
SpatialParameterPlugin::writeElements (XMLOutputStream& stream) const
{
  if (getSpatialSymbolReference()->isSetSpatialId())
  {
    mSpatialSymbolReference.write(stream);
  }
  if (getDiffusionCoefficient()->isSetVariable())
  {
    mDiffusionCoefficient.write(stream);
  }    
  if (getAdvectionCoefficient()->isSetVariable())
  {
    mAdvectionCoefficient.write(stream);
  }    
  if (getBoundaryCondition()->isSetVariable())
  {
    mBoundaryCondition.write(stream);
  }    
}


/* default for components that have no required elements */
bool
SpatialParameterPlugin::hasRequiredElements() const
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
 * Returns the SpatialSymbolReference for this Parameter.
 */
const SpatialSymbolReference*
SpatialParameterPlugin::getSpatialSymbolReference () const
{
  return &this->mSpatialSymbolReference;
}


/*
 * Returns the SpatialSymbolReference for this Parameter.
 */
SpatialSymbolReference*
SpatialParameterPlugin::getSpatialSymbolReference ()
{
  return &this->mSpatialSymbolReference;
}

/*
 * Returns the DiffusionCoefficient for this Parameter.
 */
const DiffusionCoefficient*
SpatialParameterPlugin::getDiffusionCoefficient () const
{
  return &this->mDiffusionCoefficient;
}


/*
 * Returns the DiffusionCoefficient for this Parameter.
 */
DiffusionCoefficient*
SpatialParameterPlugin::getDiffusionCoefficient ()
{
  return &this->mDiffusionCoefficient;
}


/*
 * Returns the AdvectionCoefficient for this Parameter.
 */
const AdvectionCoefficient*
SpatialParameterPlugin::getAdvectionCoefficient () const
{
  return &this->mAdvectionCoefficient;
}


/*
 * Returns the AdvectionCoefficient for this Parameter.
 */
AdvectionCoefficient*
SpatialParameterPlugin::getAdvectionCoefficient ()
{
  return &this->mAdvectionCoefficient;
}


/*
 * Returns the BoundaryCondition for this Parameter.
 */
const BoundaryCondition*
SpatialParameterPlugin::getBoundaryCondition () const
{
  return &this->mBoundaryCondition;
}


/*
 * Returns the BoundaryCondition for this Parameter.
 */
BoundaryCondition*
SpatialParameterPlugin::getBoundaryCondition ()
{
  return &this->mBoundaryCondition;
}


/*
 * Sets the parent SBMLDocument of this SBML object.
 *
 * @param d the SBMLDocument object to use
 */
void 
SpatialParameterPlugin::setSBMLDocument (SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  mSpatialSymbolReference.setSBMLDocument(d);
  mDiffusionCoefficient.setSBMLDocument(d);
  mAdvectionCoefficient.setSBMLDocument(d);
  mBoundaryCondition.setSBMLDocument(d);

}

/** 
 * @return true, if either the spatial symbol reference, diffusion coefficient, 
 *   advection coefficient or boundary is set. Otherwise the return value is false.
 */ 
bool 
SpatialParameterPlugin::isSpatialParameter() const
{
  return getType() != -1;
}

/** 
 * Determines the type of the spatial parameter, that is one of: 
 * 
 * SBML_SPATIAL_SPATIALSYMBOLREFERENCE
 * SBML_SPATIAL_DIFFUSIONCOEFFICIENT
 * SBML_SPATIAL_ADVECTIONCOEFFICIENT
 * SBML_SPATIAL_BOUNDARYCONDITION
 * 
 * or -1 in case no other is defined.
 */
int 
SpatialParameterPlugin::getType() const
{
  if (mSpatialSymbolReference.isSetSpatialId())
  {
    return SBML_SPATIAL_SPATIALSYMBOLREFERENCE;
  }
  if (mDiffusionCoefficient.isSetVariable())
  {
    return SBML_SPATIAL_DIFFUSIONCOEFFICIENT;
  }    
  if (mAdvectionCoefficient.isSetVariable())
  {
    return SBML_SPATIAL_ADVECTIONCOEFFICIENT;
  }    
  if (mBoundaryCondition.isSetVariable())
  {
    return SBML_SPATIAL_BOUNDARYCONDITION;
  }    
  return -1;
}



/*
 * Sets the parent SBML object of this plugin object to
 * this object and child elements (if any).
 * (Creates a child-parent relationship by this plugin object)
 */
void
SpatialParameterPlugin::connectToParent (SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  mSpatialSymbolReference.connectToParent(sbase);
  mDiffusionCoefficient.connectToParent(sbase);
  mAdvectionCoefficient.connectToParent(sbase);
  mBoundaryCondition.connectToParent(sbase);
}


/*
 * Enables/Disables the given package with child elements in this plugin
 * object (if any).
 */
void
SpatialParameterPlugin::enablePackageInternal(const std::string& pkgURI,
                                        const std::string& pkgPrefix, bool flag)
{
  mSpatialSymbolReference.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mDiffusionCoefficient.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mAdvectionCoefficient.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mBoundaryCondition.enablePackageInternal(pkgURI,pkgPrefix,flag);
}

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
