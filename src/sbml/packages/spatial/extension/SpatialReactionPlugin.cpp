/**
 * @file:   SpatialReactionPlugin.cpp
 * @brief:  Implementation of the SpatialReactionPlugin class
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


#include <sbml/packages/spatial/extension/SpatialReactionPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpatialReactionPlugin
 */
SpatialReactionPlugin::SpatialReactionPlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               SpatialPkgNamespaces* spatialns) :
    SBasePlugin(uri, prefix, spatialns)
  , mIsLocal (false)
  , mIsSetIsLocal (false)
{
}


/*
 * Copy constructor for SpatialReactionPlugin.
 */
SpatialReactionPlugin::SpatialReactionPlugin(const SpatialReactionPlugin& orig) :
    SBasePlugin(orig)
{
    mIsLocal  = orig.mIsLocal;
    mIsSetIsLocal  = orig.mIsSetIsLocal;
}


/*
 * Assignment operator for SpatialReactionPlugin.
 */
SpatialReactionPlugin& 
SpatialReactionPlugin::operator=(const SpatialReactionPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mIsLocal  = rhs.mIsLocal;
    mIsSetIsLocal  = rhs.mIsSetIsLocal;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialReactionPlugin object.
 */
SpatialReactionPlugin* 
SpatialReactionPlugin::clone () const
{
  return new SpatialReactionPlugin(*this);
}


/*
 * Destructor for SpatialReactionPlugin.
 */
SpatialReactionPlugin::~SpatialReactionPlugin()
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
SpatialReactionPlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  //const std::string&      name   = stream.peek().getName();
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

    delete spatialns;
  } 

  return object; 
}


/*
 * write elements
 */
void
SpatialReactionPlugin::writeElements (XMLOutputStream& stream) const
{
}


/*
 * Checks if this plugin object has all the required elements.
 */
bool
SpatialReactionPlugin::hasRequiredElements () const
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
SpatialReactionPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBasePlugin::addExpectedAttributes(attributes);

  attributes.add("isLocal");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpatialReactionPlugin::readAttributes (const XMLAttributes& attributes,
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

  //
  // isLocal bool   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetIsLocal = attributes.readInto("isLocal", mIsLocal);

  if (mIsSetIsLocal == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
      else
      {
        std::string message = "Spatial attribute 'isLocal' is missing from 'reaction' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
      }
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
SpatialReactionPlugin::writeAttributes (XMLOutputStream& stream) const
{
  SBasePlugin::writeAttributes(stream);

  if (isSetIsLocal() == true)
    stream.writeAttribute("isLocal", getPrefix(), mIsLocal);

}


  /** @endcond doxygenLibsbmlInternal */


//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------

List*
SpatialReactionPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  //List* sublist = NULL;


  return ret;
}


/*
 * Returns the value of the "isLocal" attribute of this SpatialReactionPlugin.
 */
bool
SpatialReactionPlugin::getIsLocal() const
{
  return mIsLocal;
}


/*
 * Returns true/false if isLocal is set.
 */
bool
SpatialReactionPlugin::isSetIsLocal() const
{
  return mIsSetIsLocal;
}


/*
 * Sets isLocal and returns value indicating success.
 */
int
SpatialReactionPlugin::setIsLocal(bool isLocal)
{
  mIsLocal = isLocal;
  mIsSetIsLocal = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets isLocal and returns value indicating success.
 */
int
SpatialReactionPlugin::unsetIsLocal()
{
  mIsLocal = false;
  mIsSetIsLocal = false;
  return LIBSBML_OPERATION_SUCCESS;
}


//---------------------------------------------------------------


/*
 * Set the SBMLDocument.
 */
void
SpatialReactionPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

}


/*
 * Connect to parent.
 */
void
SpatialReactionPlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

}


/*
 * Enables the given package.
 */
void
SpatialReactionPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
}


/*
 * Accept the SBMLVisitor.
 */
bool
SpatialReactionPlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  return true;
}




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


