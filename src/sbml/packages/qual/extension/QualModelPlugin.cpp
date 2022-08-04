/**
 * @file    QualModelPlugin.cpp
 * @brief   Implementation of QualModelPlugin, the plugin class of
 *          qual package for the Model element.
 * @author  Akiya Jouraku
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
 * Copyright (C) 2009-2011 jointly by the following organizations: 
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

#include <sbml/packages/qual/extension/QualModelPlugin.h>
#include <sbml/packages/qual/validator/QualSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>

#include <iostream>
using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Constructor
 */
QualModelPlugin::QualModelPlugin (const std::string& uri, 
                                  const std::string &prefix,
                                  QualPkgNamespaces *qualns)
  : SBasePlugin(uri,prefix, qualns)
   ,mQualitativeSpecies(qualns)
   ,mTransitions(qualns)
{
  // connect child elements to this element.
  connectToChild();
}


/*
 * Copy constructor. Creates a copy of this SBase object.
 */
QualModelPlugin::QualModelPlugin(const QualModelPlugin& orig)
  : SBasePlugin(orig)
  , mQualitativeSpecies(orig.mQualitativeSpecies)
  , mTransitions(orig.mTransitions)
{
  // connect child elements to this element.
  connectToChild();
}


/*
 * Destroy this object.
 */
QualModelPlugin::~QualModelPlugin () {}

/*
 * Assignment operator for QualModelPlugin.
 */
QualModelPlugin& 
QualModelPlugin::operator=(const QualModelPlugin& orig)
{
  if(&orig!=this)
  {
    this->SBasePlugin::operator =(orig);
    mQualitativeSpecies    = orig.mQualitativeSpecies;
    mTransitions    = orig.mTransitions;
    // connect child elements to this element.
    connectToChild();
  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this QualModelPlugin object.
 * 
 * @return a (deep) copy of this SBase object
 */
QualModelPlugin* 
QualModelPlugin::clone () const
{
  return new QualModelPlugin(*this);  
}


/** @cond doxygenLibsbmlInternal */
SBase*
QualModelPlugin::createObject(XMLInputStream& stream)
{
  SBase*        object = 0;

  const std::string&   name   = stream.peek().getName();
  const XMLNamespaces& xmlns  = stream.peek().getNamespaces();
  const std::string&   prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;
  
  if (prefix == targetPrefix)
  {
    if ( name == "listOfQualitativeSpecies" ) 
    {
      if (mQualitativeSpecies.size() != 0)
      {
        getErrorLog()->logPackageError("qual", QualOneListOfTransOrQS, 
          getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
      }
      
      object = &mQualitativeSpecies;
    
      if (targetPrefix.empty())
      {
        //
        // (NOTE)
        //
        // A top-level element (listOfQualitativeSpecies) of the qual extension is located 
        // in a default namespace, and thus xmlns=".." attribute must be added to 
        // the element.
        // This is done by invoking SBMLDocument::enableDefaultNS() function with 
        // the two arguments (the uri of this package and true value).
        //
        mQualitativeSpecies.getSBMLDocument()->enableDefaultNS(mURI,true);
      }
    }          
    else if ( name == "listOfTransitions" ) 
    {
      if (mTransitions.size() != 0)
      {
        getErrorLog()->logPackageError("qual", QualOneListOfTransOrQS, 
          getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
      }
      
      object = &mTransitions;
    
      if (targetPrefix.empty())
      {
        //
        // (NOTE)
        //
        // A top-level element (listOfTransitions) of the qual extension is located 
        // in a default namespace, and thus xmlns=".." attribute must be added to 
        // the element.
        // This is done by invoking SBMLDocument::enableDefaultNS() function with 
        // the two arguments (the uri of this package and true value).
        //
        mTransitions.getSBMLDocument()->enableDefaultNS(mURI,true);
      }
    }          
  }    

  return object;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
bool 
QualModelPlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject()); 
  
  v.visit(*model);
  v.leave(*model);

  for (unsigned int i = 0; i < getNumQualitativeSpecies(); i++)
  {
    getQualitativeSpecies(i)->accept(v);
  }
  for (unsigned int i = 0; i < getNumTransitions(); i++)
  {
    getTransition(i)->accept(v);
  }
  return true;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void
QualModelPlugin::writeElements (XMLOutputStream& stream) const
{
  if (getNumQualitativeSpecies() > 0)
  {
    mQualitativeSpecies.write(stream);
  }    
  if (getNumTransitions() > 0)
  {
    mTransitions.write(stream);
  }    
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this QualModelPlugin.
 */
int
QualModelPlugin::getAttribute(const std::string& attributeName,
                              bool& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this QualModelPlugin.
 */
int
QualModelPlugin::getAttribute(const std::string& attributeName,
                              int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this QualModelPlugin.
 */
int
QualModelPlugin::getAttribute(const std::string& attributeName,
                              double& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this QualModelPlugin.
 */
int
QualModelPlugin::getAttribute(const std::string& attributeName,
                              unsigned int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this QualModelPlugin.
 */
int
QualModelPlugin::getAttribute(const std::string& attributeName,
                              std::string& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this QualModelPlugin's attribute
 * "attributeName" is set.
 */
bool
QualModelPlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBasePlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this QualModelPlugin.
 */
int
QualModelPlugin::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this QualModelPlugin.
 */
int
QualModelPlugin::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this QualModelPlugin.
 */
int
QualModelPlugin::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this QualModelPlugin.
 */
int
QualModelPlugin::setAttribute(const std::string& attributeName,
                              unsigned int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this QualModelPlugin.
 */
int
QualModelPlugin::setAttribute(const std::string& attributeName,
                              const std::string& value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this QualModelPlugin.
 */
int
QualModelPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBasePlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this QualModelPlugin.
 */
SBase*
QualModelPlugin::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "qualitativeSpecies")
  {
    return createQualitativeSpecies();
  }
  else if (elementName == "transition")
  {
    return createTransition();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this QualModelPlugin.
 */
int
QualModelPlugin::addChildObject(const std::string& elementName,
                                const SBase* element)
{
  if (elementName == "qualitativeSpecies" && element->getTypeCode() ==
    SBML_QUAL_QUALITATIVE_SPECIES)
  {
    return addQualitativeSpecies((const QualitativeSpecies*)(element));
  }
  else if (elementName == "transition" && element->getTypeCode() ==
    SBML_QUAL_TRANSITION)
  {
    return addTransition((const Transition*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * QualModelPlugin.
 */
SBase*
QualModelPlugin::removeChildObject(const std::string& elementName,
                                   const std::string& id)
{
  if (elementName == "qualitativeSpecies")
  {
    return removeQualitativeSpecies(id);
  }
  else if (elementName == "transition")
  {
    return removeTransition(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this QualModelPlugin.
 */
unsigned int
QualModelPlugin::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "qualitativeSpecies")
  {
    return getNumQualitativeSpecies();
  }
  else if (elementName == "transition")
  {
    return getNumTransitions();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this QualModelPlugin.
 */
SBase*
QualModelPlugin::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "qualitativeSpecies")
  {
    return getQualitativeSpecies(index);
  }
  else if (elementName == "transition")
  {
    return getTransition(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
QualModelPlugin::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mQualitativeSpecies.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mTransitions.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
QualModelPlugin::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mQualitativeSpecies.getMetaId() == metaid)
  {
    return &mQualitativeSpecies;
  }

  if (mTransitions.getMetaId() == metaid)
  {
    return &mTransitions;
  }

  obj = mQualitativeSpecies.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mTransitions.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 *
 *  (EXTENSION) Additional public functions
 *
 */  

/** @cond doxygenLibsbmlInternal */
int 
QualModelPlugin::appendFrom(const Model* model)
{
  int ret = LIBSBML_OPERATION_SUCCESS;

  if (model==NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  const QualModelPlugin* modplug = 
    static_cast<const QualModelPlugin*>(model->getPlugin(getPrefix()));
  
  // absence of a plugin is not an error
  if (modplug==NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }


  Model* parent = static_cast<Model*>(getParentSBMLObject());

  if (parent==NULL) 
  {
    return LIBSBML_INVALID_OBJECT;
  }
  
  ret = mQualitativeSpecies.appendFrom(modplug->getListOfQualitativeSpecies());

  if (ret != LIBSBML_OPERATION_SUCCESS)
  {
    return ret;
  }

  ret = mTransitions.appendFrom(modplug->getListOfTransitions());
  
  return ret;
}
/** @endcond */



List*
QualModelPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mQualitativeSpecies, filter);
  ADD_FILTERED_LIST(ret, sublist, mTransitions, filter);

  return ret;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the ListOf Qual for this Model.
 */
const ListOfQualitativeSpecies*
QualModelPlugin::getListOfQualitativeSpecies () const
{
  return &this->mQualitativeSpecies;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the ListOf Qual for this Model.
 */
ListOfQualitativeSpecies*
QualModelPlugin::getListOfQualitativeSpecies ()
{
  return &this->mQualitativeSpecies;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the qual object that belongs to the given index. If the index
 * is invalid, NULL is returned.
 */
const QualitativeSpecies*
QualModelPlugin::getQualitativeSpecies (unsigned int index) const
{
  return static_cast<const QualitativeSpecies*>( mQualitativeSpecies.get(index) );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the qual object that belongs to the given index. If the index
 * is invalid, NULL is returned.
 */
QualitativeSpecies*
QualModelPlugin::getQualitativeSpecies (unsigned int index)
{
  return static_cast<QualitativeSpecies*>( mQualitativeSpecies.get(index) );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the qualitativeSpecies object based on its identifier.
 */
QualitativeSpecies*
QualModelPlugin::getQualitativeSpecies (const std::string& sid)
{
  return static_cast<QualitativeSpecies*>( mQualitativeSpecies.get(sid) );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the qualitativeSpecies object based on its identifier.
 */
const QualitativeSpecies* 
QualModelPlugin::getQualitativeSpecies (const std::string& sid) const
{
  return static_cast<const QualitativeSpecies*>( mQualitativeSpecies.get(sid) );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the number of QualitativeSpecies objects.
 */
unsigned int 
QualModelPlugin::getNumQualitativeSpecies() const
{
  return mQualitativeSpecies.size();
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Adds a copy of the qual object to the list of quals.
 */ 
int
QualModelPlugin::addQualitativeSpecies (const QualitativeSpecies* qual)
{
  if (!qual)
  {
    return LIBSBML_OPERATION_FAILED;
  }    
  else if (!qual->hasRequiredElements())
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != qual->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != qual->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != qual->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    return mQualitativeSpecies.append(qual);
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new qual object and adds it to the list of qual objects.
 * A reference to the newly created object is returned.
 */
QualitativeSpecies*
QualModelPlugin::createQualitativeSpecies ()
{
  QualitativeSpecies* g = 0;

  try
  {  
    QUAL_CREATE_NS(qualns, getSBMLNamespaces());
    g = new QualitativeSpecies(qualns);
    mQualitativeSpecies.appendAndOwn(g);
    delete qualns;
  }
  catch(...)
  {
    /* 
     * NULL will be returned if the mSBMLNS is invalid (basically this
     * should not happen) or some exception is thrown (e.g. std::bad_alloc)
     *
     * (Maybe this should be changed so that caller can detect what kind 
     *  of error happened in this function.)
     */
  }

  return g;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Removes the nth QualitativeSpecies object from this Model object and
 * returns a pointer to it.
 */
QualitativeSpecies* 
QualModelPlugin::removeQualitativeSpecies (unsigned int n)
{
  return static_cast<QualitativeSpecies*>(mQualitativeSpecies.remove(n));
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Removes the QualitativeSpecies object with the given id attribute from 
 * this plugin object and returns a pointer to it.
 */
QualitativeSpecies* 
QualModelPlugin::removeQualitativeSpecies (const std::string& sid)
{
  return static_cast<QualitativeSpecies*>(mQualitativeSpecies.remove(sid));
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the ListOf Qual for this Model.
 */
const ListOfTransitions*
QualModelPlugin::getListOfTransitions () const
{
  return &this->mTransitions;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the ListOfTransitions for this Model.
 */
ListOfTransitions*
QualModelPlugin::getListOfTransitions ()
{
  return &this->mTransitions;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the qual object that belongs to the given index. If the index
 * is invalid, NULL is returned.
 */
const Transition*
QualModelPlugin::getTransition (unsigned int index) const
{
  return static_cast<const Transition*>( mTransitions.get(index) );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the qual object that belongs to the given index. If the index
 * is invalid, NULL is returned.
 */
Transition*
QualModelPlugin::getTransition (unsigned int index)
{
  return static_cast<Transition*>( mTransitions.get(index) );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the group object based on its identifier.
 */
Transition*
QualModelPlugin::getTransition (const std::string& sid)
{
  return static_cast<Transition*>( mTransitions.get(sid) );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the group object based on its identifier.
 */
const Transition* 
QualModelPlugin::getTransition (const std::string& sid) const
{
  return static_cast<const Transition*>( mTransitions.get(sid) );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the number of Transition objects.
 */
unsigned int 
QualModelPlugin::getNumTransitions() const
{
  return mTransitions.size();
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
/** @cond doxygenLibsbmlInternal */
/*
 * Adds a copy of the qual object to the list of quals.
 */ 
int
QualModelPlugin::addTransition (const Transition* qual)
{
  if (!qual)
  {
    return LIBSBML_OPERATION_FAILED;
  }    
  else if (!qual->hasRequiredElements())
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != qual->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != qual->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != qual->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    return mTransitions.append(qual);
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new qual object and adds it to the list of qual objects.
 * A reference to the newly created object is returned.
 */
Transition*
QualModelPlugin::createTransition ()
{
  Transition* g = 0;

  try
  {  
    QUAL_CREATE_NS(qualns, getSBMLNamespaces());
    g = new Transition(qualns);
    mTransitions.appendAndOwn(g);
    delete qualns;
  }
  catch(...)
  {
    /* 
     * NULL will be returned if the mSBMLNS is invalid (basically this
     * should not happen) or some exception is thrown (e.g. std::bad_alloc)
     *
     * (Maybe this should be changed so that caller can detect what kind 
     *  of error happened in this function.)
     */
  }

  return g;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Removes the nth Transition object from this Model object and
 * returns a pointer to it.
 */
Transition* 
QualModelPlugin::removeTransition (unsigned int n)
{
  return static_cast<Transition*>(mTransitions.remove(n));
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Removes the Transition object with the given id attribute from 
 * this plugin object and returns a pointer to it.
 */
Transition* 
QualModelPlugin::removeTransition (const std::string& sid)
{
  return static_cast<Transition*>(mTransitions.remove(sid));
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBMLDocument of this SBML object.
 *
 * @param d the SBMLDocument object to use.
 */
void 
QualModelPlugin::setSBMLDocument (SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  mQualitativeSpecies.setSBMLDocument(d);  
  mTransitions.setSBMLDocument(d);  
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void
QualModelPlugin::connectToChild()
{
  connectToParent(getParentSBMLObject());
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBML object of this plugin object to
 * this object and child elements (if any).
 * (Creates a child-parent relationship by this plugin object)
 */
void
QualModelPlugin::connectToParent (SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  mQualitativeSpecies.connectToParent(sbase);
  mTransitions.connectToParent(sbase);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Enables/Disables the given package with child elements in this plugin
 * object (if any).
 */
void
QualModelPlugin::enablePackageInternal(const std::string& pkgURI,
                                        const std::string& pkgPrefix, bool flag)
{
  mQualitativeSpecies.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mTransitions.enablePackageInternal(pkgURI,pkgPrefix,flag);
}
/** @endcond */


#endif  /* __cplusplus */


/*
 * Returns a ListOf_t * containing QualitativeSpecies_t objects from this
 * QualModelPlugin_t.
 */
LIBSBML_EXTERN
ListOf_t*
QualModelPlugin_getListOfQualitativeSpecies(QualModelPlugin_t* qmp)
{
  return (qmp != NULL) ? qmp->getListOfQualitativeSpecies() : NULL;
}


/*
 * Get a QualitativeSpecies_t from the QualModelPlugin_t.
 */
LIBSBML_EXTERN
const QualitativeSpecies_t*
QualModelPlugin_getQualitativeSpecies(QualModelPlugin_t* qmp, unsigned int n)
{
  return (qmp != NULL) ? qmp->getQualitativeSpecies(n) : NULL;
}


/*
 * Get a QualitativeSpecies_t from the QualModelPlugin_t based on its
 * identifier.
 */
LIBSBML_EXTERN
const QualitativeSpecies_t*
QualModelPlugin_getQualitativeSpeciesById(QualModelPlugin_t* qmp,
                                          const char *sid)
{
  return (qmp != NULL && sid != NULL) ? qmp->getQualitativeSpecies(sid) : NULL;
}


/*
 * Adds a copy of the given QualitativeSpecies_t to this QualModelPlugin_t.
 */
LIBSBML_EXTERN
int
QualModelPlugin_addQualitativeSpecies(QualModelPlugin_t* qmp,
                                      const QualitativeSpecies_t* qs)
{
  return (qmp != NULL) ? qmp->addQualitativeSpecies(qs) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of QualitativeSpecies_t objects in this QualModelPlugin_t.
 */
LIBSBML_EXTERN
unsigned int
QualModelPlugin_getNumQualitativeSpecies(QualModelPlugin_t* qmp)
{
  return (qmp != NULL) ? qmp->getNumQualitativeSpecies() : SBML_INT_MAX;
}


/*
 * Creates a new QualitativeSpecies_t object, adds it to this QualModelPlugin_t
 * object and returns the QualitativeSpecies_t object created.
 */
LIBSBML_EXTERN
QualitativeSpecies_t*
QualModelPlugin_createQualitativeSpecies(QualModelPlugin_t* qmp)
{
  return (qmp != NULL) ? qmp->createQualitativeSpecies() : NULL;
}


/*
 * Removes the nth QualitativeSpecies_t from this QualModelPlugin_t and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
QualitativeSpecies_t*
QualModelPlugin_removeQualitativeSpecies(QualModelPlugin_t* qmp,
                                         unsigned int n)
{
  return (qmp != NULL) ? qmp->removeQualitativeSpecies(n) : NULL;
}


/*
 * Removes the QualitativeSpecies_t from this QualModelPlugin_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
QualitativeSpecies_t*
QualModelPlugin_removeQualitativeSpeciesById(QualModelPlugin_t* qmp,
                                             const char* sid)
{
  return (qmp != NULL && sid != NULL) ? qmp->removeQualitativeSpecies(sid) :
    NULL;
}


/*
 * Returns a ListOf_t * containing Transition_t objects from this
 * QualModelPlugin_t.
 */
LIBSBML_EXTERN
ListOf_t*
QualModelPlugin_getListOfTransitions(QualModelPlugin_t* qmp)
{
  return (qmp != NULL) ? qmp->getListOfTransitions() : NULL;
}


/*
 * Get a Transition_t from the QualModelPlugin_t.
 */
LIBSBML_EXTERN
const Transition_t*
QualModelPlugin_getTransition(QualModelPlugin_t* qmp, unsigned int n)
{
  return (qmp != NULL) ? qmp->getTransition(n) : NULL;
}


/*
 * Get a Transition_t from the QualModelPlugin_t based on its identifier.
 */
LIBSBML_EXTERN
const Transition_t*
QualModelPlugin_getTransitionById(QualModelPlugin_t* qmp, const char *sid)
{
  return (qmp != NULL && sid != NULL) ? qmp->getTransition(sid) : NULL;
}


/*
 * Adds a copy of the given Transition_t to this QualModelPlugin_t.
 */
LIBSBML_EXTERN
int
QualModelPlugin_addTransition(QualModelPlugin_t* qmp, const Transition_t* t)
{
  return (qmp != NULL) ? qmp->addTransition(t) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of Transition_t objects in this QualModelPlugin_t.
 */
LIBSBML_EXTERN
unsigned int
QualModelPlugin_getNumTransitions(QualModelPlugin_t* qmp)
{
  return (qmp != NULL) ? qmp->getNumTransitions() : SBML_INT_MAX;
}


/*
 * Creates a new Transition_t object, adds it to this QualModelPlugin_t object
 * and returns the Transition_t object created.
 */
LIBSBML_EXTERN
Transition_t*
QualModelPlugin_createTransition(QualModelPlugin_t* qmp)
{
  return (qmp != NULL) ? qmp->createTransition() : NULL;
}


/*
 * Removes the nth Transition_t from this QualModelPlugin_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
Transition_t*
QualModelPlugin_removeTransition(QualModelPlugin_t* qmp, unsigned int n)
{
  return (qmp != NULL) ? qmp->removeTransition(n) : NULL;
}


/*
 * Removes the Transition_t from this QualModelPlugin_t based on its identifier
 * and returns a pointer to it.
 */
LIBSBML_EXTERN
Transition_t*
QualModelPlugin_removeTransitionById(QualModelPlugin_t* qmp, const char* sid)
{
  return (qmp != NULL && sid != NULL) ? qmp->removeTransition(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


