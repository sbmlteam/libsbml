/**
 * @file:   MultiModelPlugin.cpp
 * @brief:  Implementation of the MultiModelPlugin class
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


#include <sbml/packages/multi/extension/MultiModelPlugin.h>

#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>
#include <sbml/packages/multi/sbml/IntraSpeciesReaction.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new MultiModelPlugin
 */
MultiModelPlugin::MultiModelPlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               MultiPkgNamespaces* multins) :
    SBasePlugin(uri, prefix, multins)
  , mListOfMultiSpeciesTypes (multins)
{
}


/*
 * Copy constructor for MultiModelPlugin.
 */
MultiModelPlugin::MultiModelPlugin(const MultiModelPlugin& orig) :
    SBasePlugin(orig)
  , mListOfMultiSpeciesTypes ( orig.mListOfMultiSpeciesTypes)
{
}


/*
 * Assignment operator for MultiModelPlugin.
 */
MultiModelPlugin& 
MultiModelPlugin::operator=(const MultiModelPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mListOfMultiSpeciesTypes = rhs.mListOfMultiSpeciesTypes;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this MultiModelPlugin object.
 */
MultiModelPlugin* 
MultiModelPlugin::clone () const
{
  return new MultiModelPlugin(*this);
}


/*
 * Destructor for MultiModelPlugin.
 */
MultiModelPlugin::~MultiModelPlugin()
{
}


//---------------------------------------------------------------
//
// overridden virtual functions for read/write/check
//
//---------------------------------------------------------------

/** @cond doxygenLibsbmlInternal */
/*
 * create object
 */
SBase*
MultiModelPlugin::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  const std::string& name = stream.peek().getName();
  const XMLNamespaces& xmlns = stream.peek().getNamespaces();
  std::string prefix(stream.peek().getPrefix());

  const std::string& targetPrefix =
      (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix)
    {
      MULTI_CREATE_NS(multins, getSBMLNamespaces());
      if (!targetPrefix.empty())
        {
          prefix += ":";
        }

      if (name == "listOfSpeciesTypes")
        {
          if (mListOfMultiSpeciesTypes.size() > 0)
            {
              getErrorLog()->logPackageError("multi", MultiLofStps_OnlyOne,
                  getPackageVersion(), getLevel(), getVersion(),
                  "Model may only have one <" + prefix + "listOfSpeciesTypes>",
                  stream.peek().getLine(), stream.peek().getColumn());
            }
          
          // here if we have encountered two lists we take the second
          // if we just return NULL then the rest of the code tries to
          // read the element and ends up reporting several other errors

          //else
          //  {
              object = &mListOfMultiSpeciesTypes;

              if (targetPrefix.empty() == true)
                {
                  mListOfMultiSpeciesTypes.getSBMLDocument()->enableDefaultNS(
                      mURI, true);
                }
          //  }
        }

      delete multins;
    }

  return object;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * write elements
 */
void
MultiModelPlugin::writeElements (XMLOutputStream& stream) const
{
  if (getNumMultiSpeciesTypes() > 0) 
  { 
    mListOfMultiSpeciesTypes.write(stream);
  } 
}
/** @endcond */


/*
 * Checks if this plugin object has all the required elements.
 */
bool
MultiModelPlugin::hasRequiredElements () const
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
MultiModelPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mListOfMultiSpeciesTypes, filter);

  return ret;
}


/*
 * Returns the ListOfMultiSpeciesTypes in this plugin object.
 */
const ListOfMultiSpeciesTypes* 
MultiModelPlugin::getListOfMultiSpeciesTypes () const
{
  return &this->mListOfMultiSpeciesTypes;
}


/*
 * Returns the ListOfMultiSpeciesTypes in this plugin object.
 */
ListOfMultiSpeciesTypes* 
MultiModelPlugin::getListOfMultiSpeciesTypes ()
{
  return &this->mListOfMultiSpeciesTypes;
}


/*
 * Returns the MultiSpeciesType object that belongs to the given index.
 */
const MultiSpeciesType*
MultiModelPlugin::getMultiSpeciesType(unsigned int n) const
{
  return static_cast<const MultiSpeciesType*>(mListOfMultiSpeciesTypes.get(n));
}


/*
 * Returns the MultiSpeciesType object that belongs to the given index.
 */
MultiSpeciesType*
MultiModelPlugin::getMultiSpeciesType(unsigned int n)
{
  return static_cast<MultiSpeciesType*>(mListOfMultiSpeciesTypes.get(n));
}


/*
 * Returns the MultiSpeciesType object based on its identifier.
 */
const MultiSpeciesType*
MultiModelPlugin::getMultiSpeciesType(const std::string& sid) const
{
  return static_cast<const MultiSpeciesType*>(mListOfMultiSpeciesTypes.get(sid));
}


/*
 * Returns the MultiSpeciesType object based on its identifier.
 */
MultiSpeciesType*
MultiModelPlugin::getMultiSpeciesType(const std::string& sid)
{
  return static_cast<MultiSpeciesType*>(mListOfMultiSpeciesTypes.get(sid));
}


/*
 * Adds a copy of the given MultiSpeciesType to the ListOfMultiSpeciesTypes in this plugin object.
 */
int
MultiModelPlugin::addMultiSpeciesType (const MultiSpeciesType* multiSpeciesType)
{
  if (multiSpeciesType == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (multiSpeciesType->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != multiSpeciesType->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != multiSpeciesType->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != multiSpeciesType->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    mListOfMultiSpeciesTypes.append(multiSpeciesType);
  }

  return LIBSBML_OPERATION_SUCCESS;

}


/*
 * Creates a new MultiSpeciesType object and adds it to the ListOfMultiSpeciesTypes in this plugin object.
 */
MultiSpeciesType* 
MultiModelPlugin::createMultiSpeciesType ()
{
   MultiSpeciesType* mst = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    mst = new MultiSpeciesType(multins);
    delete multins;
  }
  catch(...)
  {
  }

  if (mst != NULL)
  {
    mListOfMultiSpeciesTypes.appendAndOwn(mst);
  }

  return mst;
}


/*
 * Creates a new BindingSiteSpeciesType object and adds it to the ListOfMultiSpeciesTypes in this plugin object.
 */
BindingSiteSpeciesType*
MultiModelPlugin::createBindingSiteSpeciesType ()
{
   BindingSiteSpeciesType* bst = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    bst = new BindingSiteSpeciesType(multins);
    delete multins;
  }
  catch(...)
  {
  }

  if (bst != NULL)
  {
    mListOfMultiSpeciesTypes.appendAndOwn(bst);
  }

  return bst;
}


/*
 * Removes the nth MultiSpeciesType object from this plugin object
 */
MultiSpeciesType* 
MultiModelPlugin::removeMultiSpeciesType(unsigned int n)
{
  return static_cast<MultiSpeciesType*>(mListOfMultiSpeciesTypes.remove(n));
}


/*
 * Removes the MultiSpeciesType object with the given id from this plugin object
 */
MultiSpeciesType* 
MultiModelPlugin::removeMultiSpeciesType(const std::string& sid)
{
  return static_cast<MultiSpeciesType*>(mListOfMultiSpeciesTypes.remove(sid));
}


/*
 * Returns the number of MultiSpeciesType objects in this plugin object.
 */
unsigned int 
MultiModelPlugin::getNumMultiSpeciesTypes () const
{
  return mListOfMultiSpeciesTypes.size();
}

/*
 * Creates a new IntraSpeciesReaction object and adds it to the ListOfReactions object of the parent model of this plugin object.
 */
IntraSpeciesReaction*
MultiModelPlugin::createIntraSpeciesReaction ()
{
  IntraSpeciesReaction* intraSpeciesR = NULL;

 try
 {
   MULTI_CREATE_NS(multins, getSBMLNamespaces());
   intraSpeciesR = new IntraSpeciesReaction(multins);
   delete multins;
 }
 catch(...)
 {
 }

 if (intraSpeciesR != NULL)
 {
   Model * model = static_cast<Model*>(this->getParentSBMLObject());
   model->getListOfReactions()->appendAndOwn(intraSpeciesR);
 }

 return intraSpeciesR;
}


//---------------------------------------------------------------


/** @cond doxygenLibsbmlInternal */
/*
 * Set the SBMLDocument.
 */
void
MultiModelPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  mListOfMultiSpeciesTypes.setSBMLDocument(d);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Connect to parent.
 */
void
MultiModelPlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  mListOfMultiSpeciesTypes.connectToParent(sbase);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Enables the given package.
 */
void
MultiModelPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  mListOfMultiSpeciesTypes.enablePackageInternal(pkgURI, pkgPrefix, flag);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Accept the SBMLVisitor.
 */
bool
MultiModelPlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());
  v.visit(*model);

  for(unsigned int i = 0; i < getNumMultiSpeciesTypes(); i++)
  {
    getMultiSpeciesType(i)->accept(v);
  }

  return true;
}
/** @endcond */





#endif /* __cplusplus */


/*
 * Returns a ListOf_t * containing MultiSpeciesType_t objects from this
 * MultiModelPlugin_t.
 */
LIBSBML_EXTERN
ListOf_t*
MultiModelPlugin_getListOfMultiSpeciesTypes(MultiModelPlugin_t* mmp)
{
  return (mmp != NULL) ? mmp->getListOfMultiSpeciesTypes() : NULL;
}


/*
 * Get a MultiSpeciesType_t from the MultiModelPlugin_t.
 */
LIBSBML_EXTERN
MultiSpeciesType_t*
MultiModelPlugin_getMultiSpeciesType(MultiModelPlugin_t* mmp, unsigned int n)
{
  return (mmp != NULL) ? mmp->getMultiSpeciesType(n) : NULL;
}


/*
 * Get a MultiSpeciesType_t from the MultiModelPlugin_t based on its
 * identifier.
 */
LIBSBML_EXTERN
MultiSpeciesType_t*
MultiModelPlugin_getMultiSpeciesTypeById(MultiModelPlugin_t* mmp,
  const char *sid)
{
  return (mmp != NULL && sid != NULL) ? mmp->getMultiSpeciesType(sid) : NULL;
}


/*
 * Adds a copy of the given MultiSpeciesType_t to this MultiModelPlugin_t.
 */
LIBSBML_EXTERN
int
MultiModelPlugin_addMultiSpeciesType(MultiModelPlugin_t* mmp,
  const MultiSpeciesType_t* mst)
{
  return (mmp != NULL) ? mmp->addMultiSpeciesType(mst) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of MultiSpeciesType_t objects in this MultiModelPlugin_t.
 */
LIBSBML_EXTERN
unsigned int
MultiModelPlugin_getNumMultiSpeciesTypes(MultiModelPlugin_t* mmp)
{
  return (mmp != NULL) ? mmp->getNumMultiSpeciesTypes() : SBML_INT_MAX;
}


/*
 * Creates a new MultiSpeciesType_t object, adds it to this MultiModelPlugin_t
 * object and returns the MultiSpeciesType_t object created.
 */
LIBSBML_EXTERN
MultiSpeciesType_t*
MultiModelPlugin_createMultiSpeciesType(MultiModelPlugin_t* mmp)
{
  return (mmp != NULL) ? mmp->createMultiSpeciesType() : NULL;
}


/*
 * Removes the nth MultiSpeciesType_t from this MultiModelPlugin_t and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
MultiSpeciesType_t*
MultiModelPlugin_removeMultiSpeciesType(MultiModelPlugin_t* mmp,
  unsigned int n)
{
  return (mmp != NULL) ? mmp->removeMultiSpeciesType(n) : NULL;
}


/*
 * Removes the MultiSpeciesType_t from this MultiModelPlugin_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
MultiSpeciesType_t*
MultiModelPlugin_removeMultiSpeciesTypeById(MultiModelPlugin_t* mmp,
  const char* sid)
{
  return (mmp != NULL && sid != NULL) ? mmp->removeMultiSpeciesType(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


