/**
 * @file:   SpatialCompartmentPlugin.cpp
 * @brief:  Implementation of the SpatialCompartmentPlugin class
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


#include <sbml/packages/spatial/extension/SpatialCompartmentPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpatialCompartmentPlugin
 */
SpatialCompartmentPlugin::SpatialCompartmentPlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               SpatialPkgNamespaces* spatialns) :
    SBasePlugin(uri, prefix, spatialns)
  , mCompartmentMapping  ( NULL )
{
}


/*
 * Copy constructor for SpatialCompartmentPlugin.
 */
SpatialCompartmentPlugin::SpatialCompartmentPlugin(const SpatialCompartmentPlugin& orig) :
    SBasePlugin(orig)
  , mCompartmentMapping ( NULL )
{
    if (orig.mCompartmentMapping != NULL) {
      mCompartmentMapping = orig.mCompartmentMapping->clone();
    }
}


/*
 * Assignment operator for SpatialCompartmentPlugin.
 */
SpatialCompartmentPlugin& 
SpatialCompartmentPlugin::operator=(const SpatialCompartmentPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    delete mCompartmentMapping;
    mCompartmentMapping = NULL;
    if (rhs.mCompartmentMapping != NULL) {
      mCompartmentMapping = rhs.mCompartmentMapping->clone();
    }
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialCompartmentPlugin object.
 */
SpatialCompartmentPlugin* 
SpatialCompartmentPlugin::clone () const
{
  return new SpatialCompartmentPlugin(*this);
}


/*
 * Destructor for SpatialCompartmentPlugin.
 */
SpatialCompartmentPlugin::~SpatialCompartmentPlugin()
{
  delete mCompartmentMapping;
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
SpatialCompartmentPlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    if (name == "compartmentMapping" ) 
    { 
      mCompartmentMapping = new CompartmentMapping(spatialns);

      object = mCompartmentMapping;

    } 

    delete spatialns;
  } 

  return object; 
}


/*
 * write elements
 */
void
SpatialCompartmentPlugin::writeElements (XMLOutputStream& stream) const
{
  if (isSetCompartmentMapping() == true) 
  { 
    mCompartmentMapping->write(stream);
  } 
}


/*
 * Checks if this plugin object has all the required elements.
 */
bool
SpatialCompartmentPlugin::hasRequiredElements () const
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
SpatialCompartmentPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBasePlugin::addExpectedAttributes(attributes);

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpatialCompartmentPlugin::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  SBasePlugin::readAttributes(attributes, expectedAttributes);

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
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

  //bool assigned = false;

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
SpatialCompartmentPlugin::writeAttributes (XMLOutputStream& stream) const
{
  SBasePlugin::writeAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------

List*
SpatialCompartmentPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mCompartmentMapping, filter);

  return ret;
}


/*
 * Returns the CompartmentMapping from this SpatialCompartmentPlugin object.
 */
const CompartmentMapping* 
SpatialCompartmentPlugin::getCompartmentMapping () const
{
  return mCompartmentMapping;
}


/*
 * Returns the CompartmentMapping from this SpatialCompartmentPlugin object.
 */
CompartmentMapping* 
SpatialCompartmentPlugin::getCompartmentMapping ()
{
  return mCompartmentMapping;
}


/*
 * @return @c true if the "CompartmentMapping" element has been set,
 */
bool 
SpatialCompartmentPlugin::isSetCompartmentMapping () const
{
  return (mCompartmentMapping != NULL);
}


/*
 * Sets the CompartmentMapping element in this SpatialCompartmentPlugin object.
 */
int
SpatialCompartmentPlugin::setCompartmentMapping(const CompartmentMapping* compartmentMapping)
{
  if (compartmentMapping == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (compartmentMapping->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != compartmentMapping->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != compartmentMapping->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != compartmentMapping->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mCompartmentMapping;
    mCompartmentMapping = static_cast<CompartmentMapping*>(compartmentMapping->clone());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new CompartmentMapping object and adds it to the SpatialCompartmentPlugin object.
 */
CompartmentMapping*
SpatialCompartmentPlugin::createCompartmentMapping()
{
  delete mCompartmentMapping;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mCompartmentMapping = new CompartmentMapping(spatialns);

  mCompartmentMapping->setSBMLDocument(this->getSBMLDocument());

  delete spatialns;

  return mCompartmentMapping;
}


//---------------------------------------------------------------


/*
 * Set the SBMLDocument.
 */
void
SpatialCompartmentPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  if (isSetCompartmentMapping() == true)
  {
    mCompartmentMapping->setSBMLDocument(d);
  }
}


/*
 * Connect to parent.
 */
void
SpatialCompartmentPlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  if (isSetCompartmentMapping() == true)
  {
    mCompartmentMapping->connectToParent(sbase);
  }
}


/*
 * Enables the given package.
 */
void
SpatialCompartmentPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  if (isSetCompartmentMapping() == true)
  {
    mCompartmentMapping->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}


/*
 * Accept the SBMLVisitor.
 */
bool
SpatialCompartmentPlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  return true;
}




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


