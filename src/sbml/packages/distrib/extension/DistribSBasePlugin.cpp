/**
 * @file:   DistribSBasePlugin.cpp
 * @brief:  Implementation of the DistribSBasePlugin class
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


#include <sbml/packages/distrib/extension/DistribSBasePlugin.h>

#include <sbml/Model.h>

#include <sbml/util/ElementFilter.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new DistribSBasePlugin
 */
DistribSBasePlugin::DistribSBasePlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               DistribPkgNamespaces* distribns) :
    SBasePlugin(uri, prefix, distribns)
  , mUncertainty  ( NULL )
{
}


/*
 * Copy constructor for DistribSBasePlugin.
 */
DistribSBasePlugin::DistribSBasePlugin(const DistribSBasePlugin& orig) :
    SBasePlugin(orig)
  , mUncertainty ( NULL )
{
  if (orig.mUncertainty != NULL)
  mUncertainty = orig.mUncertainty->clone();
}


/*
 * Assignment operator for DistribSBasePlugin.
 */
DistribSBasePlugin& 
DistribSBasePlugin::operator=(const DistribSBasePlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mUncertainty = rhs.mUncertainty;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribSBasePlugin object.
 */
DistribSBasePlugin* 
DistribSBasePlugin::clone () const
{
  return new DistribSBasePlugin(*this);
}


/*
 * Destructor for DistribSBasePlugin.
 */
DistribSBasePlugin::~DistribSBasePlugin()
{
  delete mUncertainty;
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
DistribSBasePlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    if (name == "uncertainty" ) 
    { 
      delete mUncertainty;
      mUncertainty = new Uncertainty(distribns);

      object = mUncertainty;

    } 

    delete distribns;
  } 

  return object; 
}


/*
 * write elements
 */
void
DistribSBasePlugin::writeElements (XMLOutputStream& stream) const
{
  if (isSetUncertainty() == true) 
  { 
    mUncertainty->write(stream);
  } 
}


/*
 * Checks if this plugin object has all the required elements.
 */
bool
DistribSBasePlugin::hasRequiredElements () const
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
DistribSBasePlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mUncertainty, filter);

  return ret;
}


/*
 * Returns the Uncertainty from this DistribSBasePlugin object.
 */
const Uncertainty* 
DistribSBasePlugin::getUncertainty () const
{
  return mUncertainty;
}


/*
 * @return @c true if the "Uncertainty" element has been set,
 */
bool 
DistribSBasePlugin::isSetUncertainty () const
{
  return (mUncertainty != NULL);
}


/*
 * Sets the Uncertainty element in this DistribSBasePlugin object.
 */
int
DistribSBasePlugin::setUncertainty(const Uncertainty* uncertainty)
{
  if (uncertainty == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (uncertainty->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != uncertainty->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != uncertainty->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != uncertainty->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mUncertainty;
    mUncertainty = static_cast<Uncertainty*>(uncertainty->clone());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new Uncertainty object and adds it to the DistribSBasePlugin object.
 */
Uncertainty*
DistribSBasePlugin::createUncertainty()
{
  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mUncertainty = new Uncertainty(distribns);

  mUncertainty->setSBMLDocument(this->getSBMLDocument());

  delete distribns;

  return mUncertainty;
}


//---------------------------------------------------------------


/*
 * Set the SBMLDocument.
 */
void
DistribSBasePlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  if (isSetUncertainty() == true)
  {
    mUncertainty->setSBMLDocument(d);
  }
}


/*
 * Connect to parent.
 */
void
DistribSBasePlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  if (isSetUncertainty() == true)
  {
    mUncertainty->connectToParent(sbase);
  }
}


/*
 * Enables the given package.
 */
void
DistribSBasePlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  if (isSetUncertainty() == true)
  {
    mUncertainty->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}


/*
 * Accept the SBMLVisitor.
 */
bool
DistribSBasePlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  return true;
}




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


