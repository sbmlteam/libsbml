/**
 * @file:   DistribFunctionDefinitionPlugin.cpp
 * @brief:  Implementation of the DistribFunctionDefinitionPlugin class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#include <sbml/packages/distrib/extension/DistribFunctionDefinitionPlugin.h>

#include <sbml/Model.h>

#include <sbml/util/ElementFilter.h>

using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new DistribFunctionDefinitionPlugin
 */
DistribFunctionDefinitionPlugin::DistribFunctionDefinitionPlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               DistribPkgNamespaces* distribns) :
    SBasePlugin(uri, prefix, distribns)
  , mDrawFromDistribution  ( NULL )
{
}


/*
 * Copy constructor for DistribFunctionDefinitionPlugin.
 */
DistribFunctionDefinitionPlugin::DistribFunctionDefinitionPlugin(const DistribFunctionDefinitionPlugin& orig) :
    SBasePlugin(orig)
  , mDrawFromDistribution ( NULL )
{
  if (orig.mDrawFromDistribution != NULL)
    mDrawFromDistribution = orig.mDrawFromDistribution->clone();
}


/*
 * Assignment operator for DistribFunctionDefinitionPlugin.
 */
DistribFunctionDefinitionPlugin& 
DistribFunctionDefinitionPlugin::operator=(const DistribFunctionDefinitionPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mDrawFromDistribution = rhs.mDrawFromDistribution;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribFunctionDefinitionPlugin object.
 */
DistribFunctionDefinitionPlugin* 
DistribFunctionDefinitionPlugin::clone () const
{
  return new DistribFunctionDefinitionPlugin(*this);
}


/*
 * Destructor for DistribFunctionDefinitionPlugin.
 */
DistribFunctionDefinitionPlugin::~DistribFunctionDefinitionPlugin()
{
  if (isSetDrawFromDistribution()) 
    delete mDrawFromDistribution;
}


//---------------------------------------------------------------
//
// overridden virtual functions for read/write/check
//
//---------------------------------------------------------------

/*
 * create object
 */
SBase*
DistribFunctionDefinitionPlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    if (name == "drawFromDistribution" ) 
    { 
      if (isSetDrawFromDistribution()) 
	delete mDrawFromDistribution;
      
      mDrawFromDistribution = new DrawFromDistribution(distribns);

      object = mDrawFromDistribution;

    } 
    delete distribns;
  } 

  return object; 
}


/*
 * write elements
 */
void
DistribFunctionDefinitionPlugin::writeElements (XMLOutputStream& stream) const
{
  if (isSetDrawFromDistribution() == true) 
  { 
    mDrawFromDistribution->write(stream);
  } 
}


/*
 * Checks if this plugin object has all the required elements.
 */
bool
DistribFunctionDefinitionPlugin::hasRequiredElements () const
{
  bool allPresent = true; 

  // TO DO 

  return allPresent; 
}


//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------

List*
DistribFunctionDefinitionPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mDrawFromDistribution, filter);

  return ret;
}


/*
 * Returns the DrawFromDistribution from this DistribFunctionDefinitionPlugin object.
 */
const DrawFromDistribution* 
DistribFunctionDefinitionPlugin::getDrawFromDistribution () const
{
  return mDrawFromDistribution;
}


/*
 * @return @c true if the "DrawFromDistribution" element has been set,
 */
bool 
DistribFunctionDefinitionPlugin::isSetDrawFromDistribution () const
{
  return (mDrawFromDistribution != NULL);
}


/*
 * Sets the DrawFromDistribution element in this DistribFunctionDefinitionPlugin object.
 */
int
DistribFunctionDefinitionPlugin::setDrawFromDistribution(const DrawFromDistribution* drawFromDistribution)
{
  if (drawFromDistribution == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (drawFromDistribution->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != drawFromDistribution->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != drawFromDistribution->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != drawFromDistribution->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mDrawFromDistribution;
    mDrawFromDistribution = static_cast<DrawFromDistribution*>(drawFromDistribution->clone());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DrawFromDistribution object and adds it to the DistribFunctionDefinitionPlugin object.
 */
DrawFromDistribution*
DistribFunctionDefinitionPlugin::createDrawFromDistribution()
{
  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  
  if (isSetDrawFromDistribution()) delete mDrawFromDistribution;
  
  mDrawFromDistribution = new DrawFromDistribution(distribns);

  mDrawFromDistribution->setSBMLDocument(this->getSBMLDocument());

  delete distribns;

  return mDrawFromDistribution;
}


//---------------------------------------------------------------


/*
 * Set the SBMLDocument.
 */
void
DistribFunctionDefinitionPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  if (isSetDrawFromDistribution() == true)
  {
    mDrawFromDistribution->setSBMLDocument(d);
  }
}


/*
 * Connect to parent.
 */
void
DistribFunctionDefinitionPlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  if (isSetDrawFromDistribution() == true)
  {
    mDrawFromDistribution->connectToParent(sbase);
  }
}


/*
 * Enables the given package.
 */
void
DistribFunctionDefinitionPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  if (isSetDrawFromDistribution() == true)
  {
    mDrawFromDistribution->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}


/*
 * Accept the SBMLVisitor.
 */
bool
DistribFunctionDefinitionPlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  return true;
}




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


