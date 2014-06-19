/**
 * @file:   ArraysSBasePlugin.cpp
 * @brief:  Implementation of the ArraysSBasePlugin class
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


#include <sbml/packages/arrays/extension/ArraysSBasePlugin.h>

#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>



using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new ArraysSBasePlugin
 */
ArraysSBasePlugin::ArraysSBasePlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               ArraysPkgNamespaces* arraysns) :
    SBasePlugin(uri, prefix, arraysns)
  , mIndexs (arraysns)
  , mDimensions (arraysns)
{
}


/*
 * Copy constructor for ArraysSBasePlugin.
 */
ArraysSBasePlugin::ArraysSBasePlugin(const ArraysSBasePlugin& orig) :
    SBasePlugin(orig)
  , mIndexs ( orig.mIndexs)
  , mDimensions ( orig.mDimensions)
{
}


/*
 * Assignment operator for ArraysSBasePlugin.
 */
ArraysSBasePlugin& 
ArraysSBasePlugin::operator=(const ArraysSBasePlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mIndexs = rhs.mIndexs;
    mDimensions = rhs.mDimensions;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ArraysSBasePlugin object.
 */
ArraysSBasePlugin* 
ArraysSBasePlugin::clone () const
{
  return new ArraysSBasePlugin(*this);
}


/*
 * Destructor for ArraysSBasePlugin.
 */
ArraysSBasePlugin::~ArraysSBasePlugin()
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
ArraysSBasePlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    ARRAYS_CREATE_NS(arraysns, getSBMLNamespaces());
    if (name == "listOfIndices" ) 
    { 
      object = &mIndexs;

      if (targetPrefix.empty() == true) 
      { 
        mIndexs.getSBMLDocument()->enableDefaultNS(mURI, true); 
      } 
    } 
    else if (name == "listOfDimensions" ) 
    { 
      object = &mDimensions;

      if (targetPrefix.empty() == true) 
      { 
        mDimensions.getSBMLDocument()->enableDefaultNS(mURI, true); 
      } 
    } 
  
    delete arraysns;
  } 

  return object; 
}


/*
 * write elements
 */
void
ArraysSBasePlugin::writeElements (XMLOutputStream& stream) const
{
  if (getNumIndexs() > 0) 
  { 
    mIndexs.write(stream);
  } 
  if (getNumDimensions() > 0) 
  { 
    mDimensions.write(stream);
  } 
}


/*
 * Checks if this plugin object has all the required elements.
 */
bool
ArraysSBasePlugin::hasRequiredElements () const
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
ArraysSBasePlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mIndexs, filter);
  ADD_FILTERED_LIST(ret, sublist, mDimensions, filter);

  return ret;
}


/*
 * Returns the ListOfIndices in this plugin object.
 */
const ListOfIndices* 
ArraysSBasePlugin::getListOfIndices () const
{
  return &this->mIndexs;
}


/*
 * Returns the ListOfIndices in this plugin object.
 */
ListOfIndices* 
ArraysSBasePlugin::getListOfIndices ()
{
  return &this->mIndexs;
}


/*
 * Returns the Index object that belongs to the given index.
 */
const Index*
ArraysSBasePlugin::getIndex(unsigned int n) const
{
  return static_cast<const Index*>(mIndexs.get(n));
}


/*
 * Returns the Index object that belongs to the given index.
 */
Index*
ArraysSBasePlugin::getIndex(unsigned int n)
{
  return static_cast<Index*>(mIndexs.get(n));
}


/*
 * Returns the Index object based on its identifier.
 */
const Index*
ArraysSBasePlugin::getIndex(const std::string& sid) const
{
  return static_cast<const Index*>(mIndexs.get(sid));
}


/*
 * Returns the Index object based on its identifier.
 */
Index*
ArraysSBasePlugin::getIndex(const std::string& sid)
{
  return static_cast<Index*>(mIndexs.get(sid));
}


/*
 * Adds a copy of the given Index to the ListOfIndices in this plugin object.
 */
int
ArraysSBasePlugin::addIndex (const Index* index)
{
  if (index == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (index->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != index->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != index->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != index->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    mIndexs.append(index);
  }

  return LIBSBML_OPERATION_SUCCESS;

}


/*
 * Creates a new Index object and adds it to the ListOfIndices in this plugin object.
 */
Index* 
ArraysSBasePlugin::createIndex ()
{
   Index* i = NULL;

  try
  {
    ARRAYS_CREATE_NS(arraysns, getSBMLNamespaces());
    i = new Index(arraysns);
    delete arraysns;
  }
  catch(...)
  {
  }

  if (i != NULL)
  {
    mIndexs.appendAndOwn(i);
  }

  return i;
}


/*
 * Removes the nth Index object from this plugin object
 */
Index* 
ArraysSBasePlugin::removeIndex(unsigned int n)
{
  return static_cast<Index*>(mIndexs.remove(n));
}


/*
 * Removes the Index object with the given id from this plugin object
 */
Index* 
ArraysSBasePlugin::removeIndex(const std::string& sid)
{
  return static_cast<Index*>(mIndexs.remove(sid));
}


/*
 * Returns the number of Index objects in this plugin object.
 */
unsigned int 
ArraysSBasePlugin::getNumIndexs () const
{
  return mIndexs.size();
}


/*
 * Returns the ListOfDimensions in this plugin object.
 */
const ListOfDimensions* 
ArraysSBasePlugin::getListOfDimensions () const
{
  return &this->mDimensions;
}


/*
 * Returns the ListOfDimensions in this plugin object.
 */
ListOfDimensions* 
ArraysSBasePlugin::getListOfDimensions ()
{
  return &this->mDimensions;
}


/*
 * Returns the Dimension object that belongs to the given index.
 */
const Dimension*
ArraysSBasePlugin::getDimension(unsigned int n) const
{
  return static_cast<const Dimension*>(mDimensions.get(n));
}


/*
 * Returns the Dimension object that belongs to the given index.
 */
Dimension*
ArraysSBasePlugin::getDimension(unsigned int n)
{
  return static_cast<Dimension*>(mDimensions.get(n));
}


/*
 * Returns the Dimension object based on its identifier.
 */
const Dimension*
ArraysSBasePlugin::getDimension(const std::string& sid) const
{
  return static_cast<const Dimension*>(mDimensions.get(sid));
}


/*
 * Returns the Dimension object based on its identifier.
 */
Dimension*
ArraysSBasePlugin::getDimension(const std::string& sid)
{
  return static_cast<Dimension*>(mDimensions.get(sid));
}


/*
 * Adds a copy of the given Dimension to the ListOfDimensions in this plugin object.
 */
int
ArraysSBasePlugin::addDimension (const Dimension* dimension)
{
  if (dimension == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (dimension->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != dimension->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != dimension->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != dimension->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    mDimensions.append(dimension);
  }

  return LIBSBML_OPERATION_SUCCESS;

}


/*
 * Creates a new Dimension object and adds it to the ListOfDimensions in this plugin object.
 */
Dimension* 
ArraysSBasePlugin::createDimension ()
{
   Dimension* d = NULL;

  try
  {
    ARRAYS_CREATE_NS(arraysns, getSBMLNamespaces());
    d = new Dimension(arraysns);
    delete arraysns;
  }
  catch(...)
  {
  }

  if (d != NULL)
  {
    mDimensions.appendAndOwn(d);
  }

  return d;
}


/*
 * Removes the nth Dimension object from this plugin object
 */
Dimension* 
ArraysSBasePlugin::removeDimension(unsigned int n)
{
  return static_cast<Dimension*>(mDimensions.remove(n));
}


/*
 * Removes the Dimension object with the given id from this plugin object
 */
Dimension* 
ArraysSBasePlugin::removeDimension(const std::string& sid)
{
  return static_cast<Dimension*>(mDimensions.remove(sid));
}


/*
 * Returns the number of Dimension objects in this plugin object.
 */
unsigned int 
ArraysSBasePlugin::getNumDimensions () const
{
  return mDimensions.size();
}


//---------------------------------------------------------------


/*
 * Set the SBMLDocument.
 */
void
ArraysSBasePlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  if (getNumIndexs() > 0) 
  { 
    mIndexs.setSBMLDocument(d);
  }
  if (getNumDimensions() > 0) 
  {
    mDimensions.setSBMLDocument(d);
  }
}


/*
 * Connect to parent.
 */
void
ArraysSBasePlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  if (getNumIndexs() > 0) 
  { 
    mIndexs.connectToParent(sbase);
  }
  if (getNumDimensions() > 0) 
  {
    mDimensions.connectToParent(sbase);
  }
}


/*
 * Enables the given package.
 */
void
ArraysSBasePlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  if (getNumIndexs() > 0) 
  { 
    mIndexs.enablePackageInternal(pkgURI, pkgPrefix, flag);
  } 
  if (getNumDimensions() > 0) 
  {
    mDimensions.enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}


/*
 * Accept the SBMLVisitor.
 */
bool
ArraysSBasePlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  for(unsigned int i = 0; i < getNumIndexs(); i++)
  {
    getIndex(i)->accept(v);
  }

  for(unsigned int i = 0; i < getNumDimensions(); i++)
  {
    getDimension(i)->accept(v);
  }

  return true;
}




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


