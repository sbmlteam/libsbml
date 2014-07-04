/**
 * @file:   SpatialSpeciesPlugin.cpp
 * @brief:  Implementation of the SpatialSpeciesPlugin class
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


#include <sbml/packages/spatial/extension/SpatialSpeciesPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpatialSpeciesPlugin
 */
SpatialSpeciesPlugin::SpatialSpeciesPlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               SpatialPkgNamespaces* spatialns) :
    SBasePlugin(uri, prefix, spatialns)
   ,mIsSpatial (false)
   ,mIsSetIsSpatial (false)
{
}


/*
 * Copy constructor for SpatialSpeciesPlugin.
 */
SpatialSpeciesPlugin::SpatialSpeciesPlugin(const SpatialSpeciesPlugin& orig) :
    SBasePlugin(orig)
{
    mIsSpatial  = orig.mIsSpatial;
    mIsSetIsSpatial  = orig.mIsSetIsSpatial;
}


/*
 * Assignment operator for SpatialSpeciesPlugin.
 */
SpatialSpeciesPlugin& 
SpatialSpeciesPlugin::operator=(const SpatialSpeciesPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mIsSpatial  = rhs.mIsSpatial;
    mIsSetIsSpatial  = rhs.mIsSetIsSpatial;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialSpeciesPlugin object.
 */
SpatialSpeciesPlugin* 
SpatialSpeciesPlugin::clone () const
{
  return new SpatialSpeciesPlugin(*this);
}


/*
 * Destructor for SpatialSpeciesPlugin.
 */
SpatialSpeciesPlugin::~SpatialSpeciesPlugin()
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
SpatialSpeciesPlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
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
SpatialSpeciesPlugin::writeElements (XMLOutputStream& stream) const
{
}


/*
 * Checks if this plugin object has all the required elements.
 */
bool
SpatialSpeciesPlugin::hasRequiredElements () const
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
SpatialSpeciesPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
	SBasePlugin::addExpectedAttributes(attributes);

	attributes.add("isSpatial");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpatialSpeciesPlugin::readAttributes (const XMLAttributes& attributes,
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
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
    }
  }

  bool assigned = false;

  //
  // isSpatial bool   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetIsSpatial = attributes.readInto("isSpatial", mIsSpatial);

  if (mIsSetIsSpatial == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion);
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
SpatialSpeciesPlugin::writeAttributes (XMLOutputStream& stream) const
{
	SBasePlugin::writeAttributes(stream);

	if (isSetIsSpatial() == true)
		stream.writeAttribute("isSpatial", getPrefix(), mIsSpatial);

}


  /** @endcond doxygenLibsbmlInternal */


//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------

List*
SpatialSpeciesPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  return ret;
}


/*
 * Returns the value of the "isSpatial" attribute of this SpatialSpeciesPlugin.
 */
bool
SpatialSpeciesPlugin::getIsSpatial() const
{
  return mIsSpatial;
}


/*
 * Returns true/false if isSpatial is set.
 */
bool
SpatialSpeciesPlugin::isSetIsSpatial() const
{
  return mIsSetIsSpatial;
}


/*
 * Sets isSpatial and returns value indicating success.
 */
int
SpatialSpeciesPlugin::setIsSpatial(bool isSpatial)
{
  mIsSpatial = isSpatial;
  mIsSetIsSpatial = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets isSpatial and returns value indicating success.
 */
int
SpatialSpeciesPlugin::unsetIsSpatial()
{
  mIsSpatial = false;
  mIsSetIsSpatial = false;
  return LIBSBML_OPERATION_SUCCESS;
}


//---------------------------------------------------------------


/*
 * Set the SBMLDocument.
 */
void
SpatialSpeciesPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

}


/*
 * Connect to parent.
 */
void
SpatialSpeciesPlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

}


/*
 * Enables the given package.
 */
void
SpatialSpeciesPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
}


/*
 * Accept the SBMLVisitor.
 */
bool
SpatialSpeciesPlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  return true;
}




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


