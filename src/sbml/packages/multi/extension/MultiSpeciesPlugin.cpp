/**
 * @file:   MultiSpeciesPlugin.cpp
 * @brief:  Implementation of the MultiSpeciesPlugin class
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


#include <sbml/packages/multi/extension/MultiSpeciesPlugin.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>

#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new MultiSpeciesPlugin
 */
MultiSpeciesPlugin::MultiSpeciesPlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               MultiPkgNamespaces* multins) :
    SBasePlugin(uri, prefix, multins)
  , mOutwardBindingSites (multins)
  , mSpeciesFeatures (multins)
   ,mSpeciesType ("")
{
  connectToChild();
}


/*
 * Copy constructor for MultiSpeciesPlugin.
 */
MultiSpeciesPlugin::MultiSpeciesPlugin(const MultiSpeciesPlugin& orig) :
    SBasePlugin(orig)
  , mOutwardBindingSites ( orig.mOutwardBindingSites)
  , mSpeciesFeatures ( orig.mSpeciesFeatures)
  , mSpeciesType  ( orig.mSpeciesType)
{
}


/*
 * Assignment operator for MultiSpeciesPlugin.
 */
MultiSpeciesPlugin& 
MultiSpeciesPlugin::operator=(const MultiSpeciesPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mOutwardBindingSites = rhs.mOutwardBindingSites;
    mSpeciesFeatures = rhs.mSpeciesFeatures;
    mSpeciesType  = rhs.mSpeciesType;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this MultiSpeciesPlugin object.
 */
MultiSpeciesPlugin* 
MultiSpeciesPlugin::clone () const
{
  return new MultiSpeciesPlugin(*this);
}


/*
 * Destructor for MultiSpeciesPlugin.
 */
MultiSpeciesPlugin::~MultiSpeciesPlugin()
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
MultiSpeciesPlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    if (name == "listOfOutwardBindingSites" ) 
    { 
      object = &mOutwardBindingSites;

      if (targetPrefix.empty() == true) 
      { 
        mOutwardBindingSites.getSBMLDocument()->enableDefaultNS(mURI, true); 
      } 
    } 
    else if (name == "listOfSpeciesFeatures" ) 
    { 
      object = &mSpeciesFeatures;

      if (targetPrefix.empty() == true) 
      { 
        mSpeciesFeatures.getSBMLDocument()->enableDefaultNS(mURI, true); 
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
MultiSpeciesPlugin::writeElements (XMLOutputStream& stream) const
{
  if (getNumOutwardBindingSites() > 0) 
  { 
    mOutwardBindingSites.write(stream);
  } 
  if (getNumSpeciesFeatures() > 0 || getNumSubListOfSpeciesFeatures() > 0)
  { 
    mSpeciesFeatures.write(stream);
  } 
}
/** @endcond */


/*
 * Checks if this plugin object has all the required elements.
 */
bool
MultiSpeciesPlugin::hasRequiredElements () const
{
  bool allPresent = true; 

  // TO DO 

  return allPresent; 
}

  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
MultiSpeciesPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  attributes.add("speciesType");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
MultiSpeciesPlugin::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("multi", MultiExSpe_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // speciesType SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("speciesType", mSpeciesType);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mSpeciesType.empty() == true)
    {
      logEmptyString(mSpeciesType, getLevel(), getVersion(), 
        getPackageVersion(), "<MultiSpeciesPlugin>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mSpeciesType) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute speciesType='" + mSpeciesType + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());
    }
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
MultiSpeciesPlugin::writeAttributes (XMLOutputStream& stream) const
{
  if (isSetSpeciesType() == true)
    stream.writeAttribute("speciesType", getPrefix(), mSpeciesType);
}


  /** @endcond */



//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------


/*
 * Returns the value of the "speciesType" attribute of this SpeciesPlugin.
 */
const std::string&
MultiSpeciesPlugin::getSpeciesType() const
{
  return mSpeciesType;
}


/*
 * Returns true/false if speciesType is set.
 */
bool
MultiSpeciesPlugin::isSetSpeciesType() const
{
  return (mSpeciesType.empty() == false);
}


/*
 * Sets speciesType and returns value indicating success.
 */
int
MultiSpeciesPlugin::setSpeciesType(const std::string& speciesType)
{
  if (!(SyntaxChecker::isValidInternalSId(speciesType)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSpeciesType = speciesType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets speciesType and returns value indicating success.
 */
int
MultiSpeciesPlugin::unsetSpeciesType()
{
  mSpeciesType.erase();

  if (mSpeciesType.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}



List*
MultiSpeciesPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mOutwardBindingSites, filter);
  ADD_FILTERED_LIST(ret, sublist, mSpeciesFeatures, filter);

  return ret;
}


/*
 * Returns the ListOfOutwardBindingSites in this plugin object.
 */
const ListOfOutwardBindingSites* 
MultiSpeciesPlugin::getListOfOutwardBindingSites () const
{
  return &this->mOutwardBindingSites;
}


/*
 * Returns the ListOfOutwardBindingSites in this plugin object.
 */
ListOfOutwardBindingSites* 
MultiSpeciesPlugin::getListOfOutwardBindingSites ()
{
  return &this->mOutwardBindingSites;
}


/*
 * Returns the OutwardBindingSite object that belongs to the given index.
 */
const OutwardBindingSite*
MultiSpeciesPlugin::getOutwardBindingSite(unsigned int n) const
{
  return static_cast<const OutwardBindingSite*>(mOutwardBindingSites.get(n));
}


/*
 * Returns the OutwardBindingSite object that belongs to the given index.
 */
OutwardBindingSite*
MultiSpeciesPlugin::getOutwardBindingSite(unsigned int n)
{
  return static_cast<OutwardBindingSite*>(mOutwardBindingSites.get(n));
}


/*
 * Returns the OutwardBindingSite object based on its identifier.
 */
const OutwardBindingSite*
MultiSpeciesPlugin::getOutwardBindingSite(const std::string& sid) const
{
  return static_cast<const OutwardBindingSite*>(mOutwardBindingSites.get(sid));
}


/*
 * Returns the OutwardBindingSite object based on its identifier.
 */
OutwardBindingSite*
MultiSpeciesPlugin::getOutwardBindingSite(const std::string& sid)
{
  return static_cast<OutwardBindingSite*>(mOutwardBindingSites.get(sid));
}


/*
 * Adds a copy of the given OutwardBindingSite to the ListOfOutwardBindingSites in this plugin object.
 */
int
MultiSpeciesPlugin::addOutwardBindingSite (const OutwardBindingSite* outwardBindingSite)
{
  if (outwardBindingSite == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (outwardBindingSite->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != outwardBindingSite->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != outwardBindingSite->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != outwardBindingSite->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    mOutwardBindingSites.append(outwardBindingSite);
  }

  return LIBSBML_OPERATION_SUCCESS;

}


/*
 * Creates a new OutwardBindingSite object and adds it to the ListOfOutwardBindingSites in this plugin object.
 */
OutwardBindingSite* 
MultiSpeciesPlugin::createOutwardBindingSite ()
{
   OutwardBindingSite* obs = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    obs = new OutwardBindingSite(multins);
    delete multins;
  }
  catch(...)
  {
  }

  if (obs != NULL)
  {
    mOutwardBindingSites.appendAndOwn(obs);
  }

  return obs;
}


/*
 * Removes the nth OutwardBindingSite object from this plugin object
 */
OutwardBindingSite* 
MultiSpeciesPlugin::removeOutwardBindingSite(unsigned int n)
{
  return static_cast<OutwardBindingSite*>(mOutwardBindingSites.remove(n));
}


/*
 * Removes the OutwardBindingSite object with the given id from this plugin object
 */
OutwardBindingSite* 
MultiSpeciesPlugin::removeOutwardBindingSite(const std::string& sid)
{
  return static_cast<OutwardBindingSite*>(mOutwardBindingSites.remove(sid));
}


/*
 * Returns the number of OutwardBindingSite objects in this plugin object.
 */
unsigned int 
MultiSpeciesPlugin::getNumOutwardBindingSites () const
{
  return mOutwardBindingSites.size();
}


/*
 * Returns the ListOfSpeciesFeatures in this plugin object.
 */
const ListOfSpeciesFeatures* 
MultiSpeciesPlugin::getListOfSpeciesFeatures () const
{
  return &this->mSpeciesFeatures;
}


/*
 * Returns the ListOfSpeciesFeatures in this plugin object.
 */
ListOfSpeciesFeatures* 
MultiSpeciesPlugin::getListOfSpeciesFeatures ()
{
  return &this->mSpeciesFeatures;
}


/*
 * Returns the SpeciesFeature object that belongs to the given index.
 */
const SpeciesFeature*
MultiSpeciesPlugin::getSpeciesFeature(unsigned int n) const
{
  return static_cast<const SpeciesFeature*>(mSpeciesFeatures.get(n));
}


/*
 * Returns the SpeciesFeature object that belongs to the given index.
 */
SpeciesFeature*
MultiSpeciesPlugin::getSpeciesFeature(unsigned int n)
{
  return static_cast<SpeciesFeature*>(mSpeciesFeatures.get(n));
}


/*
 * Returns the SpeciesFeature object based on its identifier.
 */
const SpeciesFeature*
MultiSpeciesPlugin::getSpeciesFeature(const std::string& sid) const
{
  return static_cast<const SpeciesFeature*>(mSpeciesFeatures.get(sid));
}


/*
 * Returns the SpeciesFeature object based on its identifier.
 */
SpeciesFeature*
MultiSpeciesPlugin::getSpeciesFeature(const std::string& sid)
{
  return static_cast<SpeciesFeature*>(mSpeciesFeatures.get(sid));
}


/*
 * Adds a copy of the given SpeciesFeature to the ListOfSpeciesFeatures in this plugin object.
 */
int
MultiSpeciesPlugin::addSpeciesFeature (const SpeciesFeature* speciesFeature)
{
  if (speciesFeature == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (speciesFeature->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != speciesFeature->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != speciesFeature->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != speciesFeature->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    mSpeciesFeatures.append(speciesFeature);
  }

  return LIBSBML_OPERATION_SUCCESS;

}


/*
 * Creates a new SpeciesFeature object and adds it to the ListOfSpeciesFeatures in this plugin object.
 */
SpeciesFeature* 
MultiSpeciesPlugin::createSpeciesFeature ()
{
   SpeciesFeature* sf = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    sf = new SpeciesFeature(multins);
    delete multins;
  }
  catch(...)
  {
  }

  if (sf != NULL)
  {
    mSpeciesFeatures.appendAndOwn(sf);
  }

  return sf;
}

/*
 * Returns the SubListOfSpeciesFeatures object that belongs to the given index.
 */
const SubListOfSpeciesFeatures*
MultiSpeciesPlugin::getSubListOfSpeciesFeatures(unsigned int n) const
{
  return static_cast<const SubListOfSpeciesFeatures*>(mSpeciesFeatures.getSubListOfSpeciesFeatures(n));
}


/*
 * Returns the SubListOfSpeciesFeatures object that belongs to the given index.
 */
SubListOfSpeciesFeatures*
MultiSpeciesPlugin::getSubListOfSpeciesFeatures(unsigned int n)
{
  return static_cast<SubListOfSpeciesFeatures*>(mSpeciesFeatures.getSubListOfSpeciesFeatures(n));
}


/*
 * Returns the SubListOfSpeciesFeatures object based on its identifier.
 */
const SubListOfSpeciesFeatures*
MultiSpeciesPlugin::getSubListOfSpeciesFeatures(const std::string& sid) const
{
  return static_cast<const SubListOfSpeciesFeatures*>(mSpeciesFeatures.getSubListOfSpeciesFeatures(sid));
}


/*
 * Returns the SubListOfSpeciesFeatures object based on its identifier.
 */
SubListOfSpeciesFeatures*
MultiSpeciesPlugin::getSubListOfSpeciesFeatures(const std::string& sid)
{
  return static_cast<SubListOfSpeciesFeatures*>(mSpeciesFeatures.getSubListOfSpeciesFeatures(sid));
}


/*
 * Adds a copy of the given SubListOfSpeciesFeatures to the ListOfSpeciesFeatures in this plugin object.
 */
int
MultiSpeciesPlugin::addSubListOfSpeciesFeatures (SubListOfSpeciesFeatures* subListOfSpeciesFeatures)
{
  if (subListOfSpeciesFeatures == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (subListOfSpeciesFeatures->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != subListOfSpeciesFeatures->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != subListOfSpeciesFeatures->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != subListOfSpeciesFeatures->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    mSpeciesFeatures.addSubListOfSpeciesFeatures(subListOfSpeciesFeatures);
  }

  return LIBSBML_OPERATION_SUCCESS;

}


/*
 * Creates a new SpeciesFeature object and adds it to the ListOfSpeciesFeatures in this plugin object.
 */
SubListOfSpeciesFeatures*
MultiSpeciesPlugin::createSubListOfSpeciesFeatures ()
{
   SubListOfSpeciesFeatures* losf = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    losf = new SubListOfSpeciesFeatures(multins);
    delete multins;
  }
  catch(...)
  {
  }

  if (losf != NULL)
  {
    mSpeciesFeatures.addSubListOfSpeciesFeatures(losf);
  }

  return losf;
}


/*
 * Removes the nth SpeciesFeature object from this plugin object
 */
SpeciesFeature* 
MultiSpeciesPlugin::removeSpeciesFeature(unsigned int n)
{
  return static_cast<SpeciesFeature*>(mSpeciesFeatures.remove(n));
}


/*
 * Removes the SpeciesFeature object with the given id from this plugin object
 */
SpeciesFeature* 
MultiSpeciesPlugin::removeSpeciesFeature(const std::string& sid)
{
  return static_cast<SpeciesFeature*>(mSpeciesFeatures.remove(sid));
}


/*
 * Returns the number of SpeciesFeature objects in this plugin object.
 */
unsigned int 
MultiSpeciesPlugin::getNumSpeciesFeatures () const
{
  return mSpeciesFeatures.getNumSpeciesFeatures();
}

/*
 * Returns the number of SubListOfSpeciesFeatures objects in this plugin object.
 */
unsigned int
MultiSpeciesPlugin::getNumSubListOfSpeciesFeatures () const
{
  return mSpeciesFeatures.getNumSubListOfSpeciesFeatures();
}


//---------------------------------------------------------------


/** @cond doxygenLibsbmlInternal */
/*
 * Set the SBMLDocument.
 */
void
MultiSpeciesPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  mOutwardBindingSites.setSBMLDocument(d);
  mSpeciesFeatures.setSBMLDocument(d);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Connect to parent.
 */
void
MultiSpeciesPlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  mOutwardBindingSites.connectToParent(sbase);
  mSpeciesFeatures.connectToParent(sbase);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Enables the given package.
 */
void
MultiSpeciesPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  mOutwardBindingSites.enablePackageInternal(pkgURI, pkgPrefix, flag);
  mSpeciesFeatures.enablePackageInternal(pkgURI, pkgPrefix, flag);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Connects to child elements
 */
void
MultiSpeciesPlugin::connectToChild()
{
  connectToParent(getParentSBMLObject());
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Accept the SBMLVisitor.
 */
bool
MultiSpeciesPlugin::accept(SBMLVisitor& v) const
{
  const Species * species = static_cast<const Species * >(this->getParentSBMLObject());
  v.visit(*species);

  for(unsigned int i = 0; i < getNumOutwardBindingSites(); i++)
  {
    getOutwardBindingSite(i)->accept(v);
  }

  for(unsigned int i = 0; i < getNumSpeciesFeatures(); i++)
  {
    getSpeciesFeature(i)->accept(v);
  }

  for(unsigned int i = 0; i < getNumSubListOfSpeciesFeatures(); i++)
  {
    getSubListOfSpeciesFeatures(i)->accept(v);
  }

  return true;
}
/** @endcond */






#endif /* __cplusplus */


/*
 * Returns a ListOf_t * containing SpeciesFeature_t objects from this
 * MultiSpeciesPlugin_t.
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesPlugin_getListOfSpeciesFeatures(MultiSpeciesPlugin_t* msp)
{
  return (msp != NULL) ? msp->getListOfSpeciesFeatures() : NULL;
}


/*
 * Get a SpeciesFeature_t from the MultiSpeciesPlugin_t.
 */
LIBSBML_EXTERN
SpeciesFeature_t*
MultiSpeciesPlugin_getSpeciesFeature(MultiSpeciesPlugin_t* msp,
  unsigned int n)
{
  return (msp != NULL) ? msp->getSpeciesFeature(n) : NULL;
}


/*
 * Get a SpeciesFeature_t from the MultiSpeciesPlugin_t based on its
 * identifier.
 */
LIBSBML_EXTERN
SpeciesFeature_t*
MultiSpeciesPlugin_getSpeciesFeatureById(MultiSpeciesPlugin_t* msp,
  const char *sid)
{
  return (msp != NULL && sid != NULL) ? msp->getSpeciesFeature(sid) : NULL;
}


/*
 * Adds a copy of the given SpeciesFeature_t to this MultiSpeciesPlugin_t.
 */
LIBSBML_EXTERN
int
MultiSpeciesPlugin_addSpeciesFeature(MultiSpeciesPlugin_t* msp,
  const SpeciesFeature_t* sf)
{
  return (msp != NULL) ? msp->addSpeciesFeature(sf) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of SpeciesFeature_t objects in this MultiSpeciesPlugin_t.
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesPlugin_getNumSpeciesFeatures(MultiSpeciesPlugin_t* msp)
{
  return (msp != NULL) ? msp->getNumSpeciesFeatures() : SBML_INT_MAX;
}


/*
 * Creates a new SpeciesFeature_t object, adds it to this MultiSpeciesPlugin_t
 * object and returns the SpeciesFeature_t object created.
 */
LIBSBML_EXTERN
SpeciesFeature_t*
MultiSpeciesPlugin_createSpeciesFeature(MultiSpeciesPlugin_t* msp)
{
  return (msp != NULL) ? msp->createSpeciesFeature() : NULL;
}


/*
 * Removes the nth SpeciesFeature_t from this MultiSpeciesPlugin_t and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
SpeciesFeature_t*
MultiSpeciesPlugin_removeSpeciesFeature(MultiSpeciesPlugin_t* msp,
  unsigned int n)
{
  return (msp != NULL) ? msp->removeSpeciesFeature(n) : NULL;
}


/*
 * Removes the SpeciesFeature_t from this MultiSpeciesPlugin_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesFeature_t*
MultiSpeciesPlugin_removeSpeciesFeatureById(MultiSpeciesPlugin_t* msp,
  const char* sid)
{
  return (msp != NULL && sid != NULL) ? msp->removeSpeciesFeature(sid) : NULL;
}


/*
 * Returns a ListOf_t * containing OutwardBindingSite_t objects from this
 * MultiSpeciesPlugin_t.
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesPlugin_getListOfOutwardBindingSites(MultiSpeciesPlugin_t* msp)
{
  return (msp != NULL) ? msp->getListOfOutwardBindingSites() : NULL;
}


/*
 * Get an OutwardBindingSite_t from the MultiSpeciesPlugin_t.
 */
LIBSBML_EXTERN
OutwardBindingSite_t*
MultiSpeciesPlugin_getOutwardBindingSite(MultiSpeciesPlugin_t* msp,
  unsigned int n)
{
  return (msp != NULL) ? msp->getOutwardBindingSite(n) : NULL;
}


/*
 * Get an OutwardBindingSite_t from the MultiSpeciesPlugin_t based on its
 * identifier.
 */
LIBSBML_EXTERN
OutwardBindingSite_t*
MultiSpeciesPlugin_getOutwardBindingSiteById(MultiSpeciesPlugin_t* msp,
  const char *sid)
{
  return (msp != NULL && sid != NULL) ? msp->getOutwardBindingSite(sid) : NULL;
}


/*
 * Adds a copy of the given OutwardBindingSite_t to this MultiSpeciesPlugin_t.
 */
LIBSBML_EXTERN
int
MultiSpeciesPlugin_addOutwardBindingSite(MultiSpeciesPlugin_t* msp,
  const OutwardBindingSite_t* obs)
{
  return (msp != NULL) ? msp->addOutwardBindingSite(obs) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of OutwardBindingSite_t objects in this MultiSpeciesPlugin_t.
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesPlugin_getNumOutwardBindingSites(MultiSpeciesPlugin_t* msp)
{
  return (msp != NULL) ? msp->getNumOutwardBindingSites() : SBML_INT_MAX;
}


/*
 * Creates a new OutwardBindingSite_t object, adds it to this
 * MultiSpeciesPlugin_t object and returns the OutwardBindingSite_t object
 * created.
 */
LIBSBML_EXTERN
OutwardBindingSite_t*
MultiSpeciesPlugin_createOutwardBindingSite(MultiSpeciesPlugin_t* msp)
{
  return (msp != NULL) ? msp->createOutwardBindingSite() : NULL;
}


/*
 * Removes the nth OutwardBindingSite_t from this MultiSpeciesPlugin_t and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
OutwardBindingSite_t*
MultiSpeciesPlugin_removeOutwardBindingSite(MultiSpeciesPlugin_t* msp,
  unsigned int n)
{
  return (msp != NULL) ? msp->removeOutwardBindingSite(n) : NULL;
}


/*
 * Removes the OutwardBindingSite_t from this MultiSpeciesPlugin_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
OutwardBindingSite_t*
MultiSpeciesPlugin_removeOutwardBindingSiteById(MultiSpeciesPlugin_t* msp,
  const char* sid)
{
  return (msp != NULL && sid != NULL) ? msp->removeOutwardBindingSite(sid) :
    NULL;
}


/*
 * Returns the value of the "speciesType" attribute of this
 * MultiSpeciesPlugin_t.
 */
LIBSBML_EXTERN
char *
MultiSpeciesPlugin_getSpeciesType(const MultiSpeciesPlugin_t * msp)
{
  if (msp == NULL)
  {
    return NULL;
  }

  return msp->getSpeciesType().empty() ? NULL :
    safe_strdup(msp->getSpeciesType().c_str());
}


/*
 * Predicate returning @c 1 (true) if this MultiSpeciesPlugin_t's "speciesType"
 * attribute is set.
 */
LIBSBML_EXTERN
int
MultiSpeciesPlugin_isSetSpeciesType(const MultiSpeciesPlugin_t * msp)
{
  return (msp != NULL) ? static_cast<int>(msp->isSetSpeciesType()) : 0;
}


/*
 * Sets the value of the "speciesType" attribute of this MultiSpeciesPlugin_t.
 */
LIBSBML_EXTERN
int
MultiSpeciesPlugin_setSpeciesType(MultiSpeciesPlugin_t * msp,
  const char * speciesType)
{
  return (msp != NULL) ? msp->setSpeciesType(speciesType) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "speciesType" attribute of this
 * MultiSpeciesPlugin_t.
 */
LIBSBML_EXTERN
int
MultiSpeciesPlugin_unsetSpeciesType(MultiSpeciesPlugin_t * msp)
{
  return (msp != NULL) ? msp->unsetSpeciesType() : LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END




