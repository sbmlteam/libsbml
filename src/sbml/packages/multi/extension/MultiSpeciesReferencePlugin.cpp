/**
 * @file:   MultiSpeciesReferencePlugin.cpp
 * @brief:  Implementation of the MultiSpeciesReferencePlugin class
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


#include <sbml/packages/multi/extension/MultiSpeciesReferencePlugin.h>

#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>

using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new MultiSpeciesReferencePlugin
 */
MultiSpeciesReferencePlugin::MultiSpeciesReferencePlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               MultiPkgNamespaces* multins) :
    MultiSimpleSpeciesReferencePlugin(uri, prefix, multins)
  , mSpeciesTypeComponentMapInProducts (multins)
{
}


/*
 * Copy constructor for MultiSpeciesReferencePlugin.
 */
MultiSpeciesReferencePlugin::MultiSpeciesReferencePlugin(const MultiSpeciesReferencePlugin& orig) :
    MultiSimpleSpeciesReferencePlugin(orig)
  , mSpeciesTypeComponentMapInProducts ( orig.mSpeciesTypeComponentMapInProducts)
{
}


/*
 * Assignment operator for MultiSpeciesReferencePlugin.
 */
MultiSpeciesReferencePlugin& 
MultiSpeciesReferencePlugin::operator=(const MultiSpeciesReferencePlugin& rhs)
{
  if (&rhs != this)
  {
    this->MultiSimpleSpeciesReferencePlugin::operator=(rhs);
    mSpeciesTypeComponentMapInProducts = rhs.mSpeciesTypeComponentMapInProducts;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this MultiSpeciesReferencePlugin object.
 */
MultiSpeciesReferencePlugin* 
MultiSpeciesReferencePlugin::clone () const
{
  return new MultiSpeciesReferencePlugin(*this);
}


/*
 * Destructor for MultiSpeciesReferencePlugin.
 */
MultiSpeciesReferencePlugin::~MultiSpeciesReferencePlugin()
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
MultiSpeciesReferencePlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    if (name == "listOfSpeciesTypeComponentMapsInProduct" ) 
    { 
      object = &mSpeciesTypeComponentMapInProducts;

      if (targetPrefix.empty() == true) 
      { 
        mSpeciesTypeComponentMapInProducts.getSBMLDocument()->enableDefaultNS(mURI, true); 
      } 
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
MultiSpeciesReferencePlugin::writeElements (XMLOutputStream& stream) const
{
  if (getNumSpeciesTypeComponentMapInProducts() > 0) 
  { 
    mSpeciesTypeComponentMapInProducts.write(stream);
  } 
}
/** @endcond */


/*
 * Checks if this plugin object has all the required elements.
 */
bool
MultiSpeciesReferencePlugin::hasRequiredElements () const
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
MultiSpeciesReferencePlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mSpeciesTypeComponentMapInProducts, filter);

  return ret;
}


/*
 * Returns the ListOfSpeciesTypeComponentMapInProducts in this plugin object.
 */
const ListOfSpeciesTypeComponentMapInProducts* 
MultiSpeciesReferencePlugin::getListOfSpeciesTypeComponentMapInProducts () const
{
  return &this->mSpeciesTypeComponentMapInProducts;
}


/*
 * Returns the ListOfSpeciesTypeComponentMapInProducts in this plugin object.
 */
ListOfSpeciesTypeComponentMapInProducts* 
MultiSpeciesReferencePlugin::getListOfSpeciesTypeComponentMapInProducts ()
{
  return &this->mSpeciesTypeComponentMapInProducts;
}


/*
 * Returns the SpeciesTypeComponentMapInProduct object that belongs to the given index.
 */
const SpeciesTypeComponentMapInProduct*
MultiSpeciesReferencePlugin::getSpeciesTypeComponentMapInProduct(unsigned int n) const
{
  return static_cast<const SpeciesTypeComponentMapInProduct*>(mSpeciesTypeComponentMapInProducts.get(n));
}


/*
 * Returns the SpeciesTypeComponentMapInProduct object that belongs to the given index.
 */
SpeciesTypeComponentMapInProduct*
MultiSpeciesReferencePlugin::getSpeciesTypeComponentMapInProduct(unsigned int n)
{
  return static_cast<SpeciesTypeComponentMapInProduct*>(mSpeciesTypeComponentMapInProducts.get(n));
}


/*
 * Returns the SpeciesTypeComponentMapInProduct object based on its identifier.
 */
const SpeciesTypeComponentMapInProduct*
MultiSpeciesReferencePlugin::getSpeciesTypeComponentMapInProduct(const std::string& sid) const
{
  return static_cast<const SpeciesTypeComponentMapInProduct*>(mSpeciesTypeComponentMapInProducts.get(sid));
}


/*
 * Returns the SpeciesTypeComponentMapInProduct object based on its identifier.
 */
SpeciesTypeComponentMapInProduct*
MultiSpeciesReferencePlugin::getSpeciesTypeComponentMapInProduct(const std::string& sid)
{
  return static_cast<SpeciesTypeComponentMapInProduct*>(mSpeciesTypeComponentMapInProducts.get(sid));
}


/*
 * Adds a copy of the given SpeciesTypeComponentMapInProduct to the ListOfSpeciesTypeComponentMapInProducts in this plugin object.
 */
int
MultiSpeciesReferencePlugin::addSpeciesTypeComponentMapInProduct (const SpeciesTypeComponentMapInProduct* speciesTypeComponentMapInProduct)
{
  if (speciesTypeComponentMapInProduct == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (speciesTypeComponentMapInProduct->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != speciesTypeComponentMapInProduct->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != speciesTypeComponentMapInProduct->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != speciesTypeComponentMapInProduct->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    mSpeciesTypeComponentMapInProducts.append(speciesTypeComponentMapInProduct);
  }

  return LIBSBML_OPERATION_SUCCESS;

}


/*
 * Creates a new SpeciesTypeComponentMapInProduct object and adds it to the ListOfSpeciesTypeComponentMapInProducts in this plugin object.
 */
SpeciesTypeComponentMapInProduct* 
MultiSpeciesReferencePlugin::createSpeciesTypeComponentMapInProduct ()
{
   SpeciesTypeComponentMapInProduct* stcmip = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    stcmip = new SpeciesTypeComponentMapInProduct(multins);
    delete multins;
  }
  catch(...)
  {
  }

  if (stcmip != NULL)
  {
    mSpeciesTypeComponentMapInProducts.appendAndOwn(stcmip);
  }

  return stcmip;
}


/*
 * Removes the nth SpeciesTypeComponentMapInProduct object from this plugin object
 */
SpeciesTypeComponentMapInProduct* 
MultiSpeciesReferencePlugin::removeSpeciesTypeComponentMapInProduct(unsigned int n)
{
  return static_cast<SpeciesTypeComponentMapInProduct*>(mSpeciesTypeComponentMapInProducts.remove(n));
}


/*
 * Removes the SpeciesTypeComponentMapInProduct object with the given id from this plugin object
 */
SpeciesTypeComponentMapInProduct* 
MultiSpeciesReferencePlugin::removeSpeciesTypeComponentMapInProduct(const std::string& sid)
{
  return static_cast<SpeciesTypeComponentMapInProduct*>(mSpeciesTypeComponentMapInProducts.remove(sid));
}


/*
 * Returns the number of SpeciesTypeComponentMapInProduct objects in this plugin object.
 */
unsigned int 
MultiSpeciesReferencePlugin::getNumSpeciesTypeComponentMapInProducts () const
{
  return mSpeciesTypeComponentMapInProducts.size();
}


//---------------------------------------------------------------


/** @cond doxygenLibsbmlInternal */
/*
 * Set the SBMLDocument.
 */
void
MultiSpeciesReferencePlugin::setSBMLDocument(SBMLDocument* d)
{
  MultiSimpleSpeciesReferencePlugin::setSBMLDocument(d);

  mSpeciesTypeComponentMapInProducts.setSBMLDocument(d);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Connect to parent.
 */
void
MultiSpeciesReferencePlugin::connectToParent(SBase* sbase)
{
  MultiSimpleSpeciesReferencePlugin::connectToParent(sbase);

  mSpeciesTypeComponentMapInProducts.connectToParent(sbase);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Enables the given package.
 */
void
MultiSpeciesReferencePlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  mSpeciesTypeComponentMapInProducts.enablePackageInternal(pkgURI, pkgPrefix, flag);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Accept the SBMLVisitor.
 */
bool
MultiSpeciesReferencePlugin::accept(SBMLVisitor& v) const
{
  const SpeciesReference * speciesReference = static_cast<const SpeciesReference * >(this->getParentSBMLObject());
  v.visit(*speciesReference);

  for(unsigned int i = 0; i < getNumSpeciesTypeComponentMapInProducts(); i++)
  {
    getSpeciesTypeComponentMapInProduct(i)->accept(v);
  }

  return true;
}
/** @endcond */






#endif /* __cplusplus */




/*
 * Returns a ListOf_t * containing SpeciesTypeComponentMapInProduct_t objects
 * from this MultiSpeciesReferencePlugin_t.
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesReferencePlugin_getListOfSpeciesTypeComponentMapInProducts(MultiSpeciesReferencePlugin_t*
  msrp)
{
  return (msrp != NULL) ? msrp->getListOfSpeciesTypeComponentMapInProducts() :
    NULL;
}


/*
 * Get a SpeciesTypeComponentMapInProduct_t from the
 * MultiSpeciesReferencePlugin_t.
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t*
MultiSpeciesReferencePlugin_getSpeciesTypeComponentMapInProduct(
  MultiSpeciesReferencePlugin_t* msrp, unsigned int n)
{
  return (msrp != NULL) ? msrp->getSpeciesTypeComponentMapInProduct(n) : NULL;
}


/*
 * Get a SpeciesTypeComponentMapInProduct_t from the
 * MultiSpeciesReferencePlugin_t based on its identifier.
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t*
MultiSpeciesReferencePlugin_getSpeciesTypeComponentMapInProductById(
  MultiSpeciesReferencePlugin_t* msrp, const char *sid)
{
  return (msrp != NULL && sid != NULL) ?
    msrp->getSpeciesTypeComponentMapInProduct(sid) : NULL;
}


/*
 * Adds a copy of the given SpeciesTypeComponentMapInProduct_t to this
 * MultiSpeciesReferencePlugin_t.
 */
LIBSBML_EXTERN
int
MultiSpeciesReferencePlugin_addSpeciesTypeComponentMapInProduct(
  MultiSpeciesReferencePlugin_t* msrp, const SpeciesTypeComponentMapInProduct_t* stcmip)
{
  return (msrp != NULL) ? msrp->addSpeciesTypeComponentMapInProduct(stcmip) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of SpeciesTypeComponentMapInProduct_t objects in this
 * MultiSpeciesReferencePlugin_t.
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesReferencePlugin_getNumSpeciesTypeComponentMapInProducts(MultiSpeciesReferencePlugin_t*
  msrp)
{
  return (msrp != NULL) ? msrp->getNumSpeciesTypeComponentMapInProducts() :
    SBML_INT_MAX;
}


/*
 * Creates a new SpeciesTypeComponentMapInProduct_t object, adds it to this
 * MultiSpeciesReferencePlugin_t object and returns the
 * SpeciesTypeComponentMapInProduct_t object created.
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t*
MultiSpeciesReferencePlugin_createSpeciesTypeComponentMapInProduct(MultiSpeciesReferencePlugin_t*
  msrp)
{
  return (msrp != NULL) ? msrp->createSpeciesTypeComponentMapInProduct() :
    NULL;
}


/*
 * Removes the nth SpeciesTypeComponentMapInProduct_t from this
 * MultiSpeciesReferencePlugin_t and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t*
MultiSpeciesReferencePlugin_removeSpeciesTypeComponentMapInProduct(
  MultiSpeciesReferencePlugin_t* msrp, unsigned int n)
{
  return (msrp != NULL) ? msrp->removeSpeciesTypeComponentMapInProduct(n) :
    NULL;
}


/*
 * Removes the SpeciesTypeComponentMapInProduct_t from this
 * MultiSpeciesReferencePlugin_t based on its identifier and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t*
MultiSpeciesReferencePlugin_removeSpeciesTypeComponentMapInProductById(
  MultiSpeciesReferencePlugin_t* msrp, const char* sid)
{
  return (msrp != NULL && sid != NULL) ?
    msrp->removeSpeciesTypeComponentMapInProduct(sid) : NULL;
}

LIBSBML_CPP_NAMESPACE_END
