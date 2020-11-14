/**
 * @file:   ReqSBasePlugin.cpp
 * @brief:  Implementation of the ReqSBasePlugin class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
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


#include <sbml/packages/req/extension/ReqSBasePlugin.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new ReqSBasePlugin
 */
ReqSBasePlugin::ReqSBasePlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               ReqPkgNamespaces* reqns) :
    SBasePlugin(uri, prefix, reqns)
  , mChangedMaths (reqns)
{
  connectToChild();
}


/*
 * Copy constructor for ReqSBasePlugin.
 */
ReqSBasePlugin::ReqSBasePlugin(const ReqSBasePlugin& orig) :
    SBasePlugin(orig)
  , mChangedMaths ( orig.mChangedMaths)
{
  connectToChild();
}


/*
 * Assignment operator for ReqSBasePlugin.
 */
ReqSBasePlugin& 
ReqSBasePlugin::operator=(const ReqSBasePlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mChangedMaths = rhs.mChangedMaths;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ReqSBasePlugin object.
 */
ReqSBasePlugin* 
ReqSBasePlugin::clone () const
{
  return new ReqSBasePlugin(*this);
}


/*
 * Destructor for ReqSBasePlugin.
 */
ReqSBasePlugin::~ReqSBasePlugin()
{
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
ReqSBasePlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    REQ_CREATE_NS(reqns, getSBMLNamespaces());
    if (name == "listOfChangedMaths" ) 
    { 
      object = &mChangedMaths;

      if (targetPrefix.empty() == true) 
      { 
        mChangedMaths.getSBMLDocument()->enableDefaultNS(mURI, true); 
      } 
    } 

    delete reqns;
  } 

  return object; 
}


/*
 * write elements
 */
void
ReqSBasePlugin::writeElements (XMLOutputStream& stream) const
{
  if (getNumChangedMaths() > 0) 
  { 
    mChangedMaths.write(stream);
  } 
}


/*
 * Checks if this plugin object has all the required elements.
 */
bool
ReqSBasePlugin::hasRequiredElements () const
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
ReqSBasePlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mChangedMaths, filter);

  return ret;
}


/*
 * Returns the ListOfChangedMaths in this plugin object.
 */
const ListOfChangedMaths* 
ReqSBasePlugin::getListOfChangedMaths () const
{
  return &this->mChangedMaths;
}


/*
 * Returns the ListOfChangedMaths in this plugin object.
 */
ListOfChangedMaths* 
ReqSBasePlugin::getListOfChangedMaths ()
{
  return &this->mChangedMaths;
}


/*
 * Returns the ChangedMath object that belongs to the given index.
 */
const ChangedMath*
ReqSBasePlugin::getChangedMath(unsigned int n) const
{
  return static_cast<const ChangedMath*>(mChangedMaths.get(n));
}


/*
 * Returns the ChangedMath object that belongs to the given index.
 */
ChangedMath*
ReqSBasePlugin::getChangedMath(unsigned int n)
{
  return static_cast<ChangedMath*>(mChangedMaths.get(n));
}


/*
 * Returns the ChangedMath object based on its identifier.
 */
const ChangedMath*
ReqSBasePlugin::getChangedMath(const std::string& sid) const
{
  return static_cast<const ChangedMath*>(mChangedMaths.get(sid));
}


/*
 * Returns the ChangedMath object based on its identifier.
 */
ChangedMath*
ReqSBasePlugin::getChangedMath(const std::string& sid)
{
  return static_cast<ChangedMath*>(mChangedMaths.get(sid));
}


/*
 * Adds a copy of the given ChangedMath to the ListOfChangedMaths in this plugin object.
 */
int
ReqSBasePlugin::addChangedMath (const ChangedMath* changedMath)
{
  if (changedMath == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (changedMath->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != changedMath->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != changedMath->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != changedMath->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    mChangedMaths.append(changedMath);
  }

  return LIBSBML_OPERATION_SUCCESS;

}


/*
 * Creates a new ChangedMath object and adds it to the ListOfChangedMaths in this plugin object.
 */
ChangedMath* 
ReqSBasePlugin::createChangedMath ()
{
   ChangedMath* cm = NULL;

  try
  {
    REQ_CREATE_NS(reqns, getSBMLNamespaces());
    cm = new ChangedMath(reqns);
    delete reqns;
  }
  catch(...)
  {
  }

  if (cm != NULL)
  {
    mChangedMaths.appendAndOwn(cm);
  }

  return cm;
}


/*
 * Removes the nth ChangedMath object from this plugin object
 */
ChangedMath* 
ReqSBasePlugin::removeChangedMath(unsigned int n)
{
  return static_cast<ChangedMath*>(mChangedMaths.remove(n));
}


/*
 * Removes the ChangedMath object with the given id from this plugin object
 */
ChangedMath* 
ReqSBasePlugin::removeChangedMath(const std::string& sid)
{
  return static_cast<ChangedMath*>(mChangedMaths.remove(sid));
}


/*
 * Returns the number of ChangedMath objects in this plugin object.
 */
unsigned int 
ReqSBasePlugin::getNumChangedMaths () const
{
  return mChangedMaths.size();
}


//---------------------------------------------------------------


/*
 * Set the SBMLDocument.
 */
void
ReqSBasePlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  if (getNumChangedMaths() > 0)
  {
    mChangedMaths.setSBMLDocument(d);
  }
}


/*
 * Connect to parent.
 */
void
ReqSBasePlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  mChangedMaths.connectToParent(sbase);
}

/** @cond doxygenLibsbmlInternal */
void
ReqSBasePlugin::connectToChild()
{
//  SBasePlugin::connectToChild();
  connectToParent(this->getParentSBMLObject());
}
/** @endcond */

/*
 * Enables the given package.
 */
void
ReqSBasePlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  if (getNumChangedMaths() > 0)
  {
    mChangedMaths.enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}


/*
 * Accept the SBMLVisitor.
 */
bool
ReqSBasePlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  for(unsigned int i = 0; i < getNumChangedMaths(); i++)
  {
    getChangedMath(i)->accept(v);
  }

  return true;
}




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


