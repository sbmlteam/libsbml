/**
 * @file:   SpatialModelPlugin.cpp
 * @brief:  Implementation of the SpatialModelPlugin class
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


#include <sbml/packages/spatial/extension/SpatialModelPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpatialModelPlugin
 */
SpatialModelPlugin::SpatialModelPlugin(const std::string& uri,  
                                       const std::string& prefix, 
                                       SpatialPkgNamespaces* spatialns):
    SBasePlugin(uri, prefix, spatialns)
  , mGeometry  ( NULL )
{
}


/*
 * Copy constructor for SpatialModelPlugin.
 */
SpatialModelPlugin::SpatialModelPlugin(const SpatialModelPlugin& orig) :
    SBasePlugin(orig)
  , mGeometry ( NULL )
{
  if (orig.mGeometry != NULL) {
    mGeometry = orig.mGeometry->clone();
  }
}


/*
 * Assignment operator for SpatialModelPlugin.
 */
SpatialModelPlugin& 
SpatialModelPlugin::operator=(const SpatialModelPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    delete mGeometry;
    mGeometry = NULL;
    if (rhs.mGeometry != NULL) {
      mGeometry = rhs.mGeometry->clone();
    }
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialModelPlugin object.
 */
SpatialModelPlugin* 
SpatialModelPlugin::clone () const
{
  return new SpatialModelPlugin(*this);
}


/*
 * Destructor for SpatialModelPlugin.
 */
SpatialModelPlugin::~SpatialModelPlugin()
{
  delete mGeometry;
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
SpatialModelPlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    if (name == "geometry" ) 
    { 
      mGeometry = new Geometry(spatialns);

      object = mGeometry;

    } 

    delete spatialns;
  } 

  return object; 
}


/*
 * write elements
 */
void
SpatialModelPlugin::writeElements (XMLOutputStream& stream) const
{
  if (isSetGeometry() == true) 
  { 
    mGeometry->write(stream);
  } 
}


/*
 * Checks if this plugin object has all the required elements.
 */
bool
SpatialModelPlugin::hasRequiredElements () const
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
SpatialModelPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBasePlugin::addExpectedAttributes(attributes);

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpatialModelPlugin::readAttributes (const XMLAttributes& attributes,
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
SpatialModelPlugin::writeAttributes (XMLOutputStream& stream) const
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
SpatialModelPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mGeometry, filter);

  return ret;
}


/*
 * Returns the Geometry from this SpatialModelPlugin object.
 */
const Geometry* 
SpatialModelPlugin::getGeometry () const
{
  return mGeometry;
}


/*
 * Returns the Geometry from this SpatialModelPlugin object.
 */
Geometry* 
SpatialModelPlugin::getGeometry ()
{
  return mGeometry;
}


/*
 * @return @c true if the "Geometry" element has been set,
 */
bool 
SpatialModelPlugin::isSetGeometry () const
{
  return (mGeometry != NULL);
}


/*
 * Sets the Geometry element in this SpatialModelPlugin object.
 */
int
SpatialModelPlugin::setGeometry(const Geometry* geometry)
{
  if (geometry == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (geometry->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != geometry->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != geometry->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != geometry->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mGeometry;
    mGeometry = static_cast<Geometry*>(geometry->clone());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new Geometry object and adds it to the SpatialModelPlugin object.
 */
Geometry*
SpatialModelPlugin::createGeometry()
{
  delete mGeometry;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mGeometry = new Geometry(spatialns);

  mGeometry->setSBMLDocument(this->getSBMLDocument());

  delete spatialns;

  return mGeometry;
}


//---------------------------------------------------------------


/*
 * Set the SBMLDocument.
 */
void
SpatialModelPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  if (isSetGeometry() == true)
  {
    mGeometry->setSBMLDocument(d);
  }
}


/*
 * Connect to parent.
 */
void
SpatialModelPlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  if (isSetGeometry() == true)
  {
    mGeometry->connectToParent(sbase);
  }
}


/*
 * Enables the given package.
 */
void
SpatialModelPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  if (isSetGeometry() == true)
  {
    mGeometry->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}


/*
 * Accept the SBMLVisitor.
 */
bool
SpatialModelPlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  return true;
}




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


