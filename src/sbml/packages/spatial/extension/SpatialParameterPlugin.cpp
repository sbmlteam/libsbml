/**
 * @file:   SpatialParameterPlugin.cpp
 * @brief:  Implementation of the SpatialParameterPlugin class
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


#include <sbml/packages/spatial/extension/SpatialParameterPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpatialParameterPlugin
 */
SpatialParameterPlugin::SpatialParameterPlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               SpatialPkgNamespaces* spatialns) :
    SBasePlugin(uri, prefix, spatialns)
  , mSpatialSymbolReference( NULL )
  , mAdvectionCoefficient  ( NULL )
  , mBoundaryCondition     ( NULL )
  , mDiffusionCoefficient  ( NULL )
{
}


/*
 * Copy constructor for SpatialParameterPlugin.
 */
SpatialParameterPlugin::SpatialParameterPlugin(const SpatialParameterPlugin& orig) :
    SBasePlugin(orig)
  , mSpatialSymbolReference( NULL )
  , mAdvectionCoefficient  ( NULL )
  , mBoundaryCondition     ( NULL )
  , mDiffusionCoefficient  ( NULL )
{
  if (orig.mSpatialSymbolReference != NULL) {
    mSpatialSymbolReference = orig.mSpatialSymbolReference->clone();
  }
  if (orig.mAdvectionCoefficient != NULL) {
    mAdvectionCoefficient = orig.mAdvectionCoefficient->clone();
  }
  if (orig.mBoundaryCondition != NULL) {
    mBoundaryCondition = orig.mBoundaryCondition->clone();
  }
  if (orig.mDiffusionCoefficient != NULL) {
    mDiffusionCoefficient = orig.mDiffusionCoefficient->clone();
  }
}


/*
 * Assignment operator for SpatialParameterPlugin.
 */
SpatialParameterPlugin& 
SpatialParameterPlugin::operator=(const SpatialParameterPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    deleteChildren();
    if (rhs.mSpatialSymbolReference != NULL) {
      mSpatialSymbolReference = rhs.mSpatialSymbolReference->clone();
    }
    if (rhs.mAdvectionCoefficient != NULL) {
      mAdvectionCoefficient = rhs.mAdvectionCoefficient->clone();
    }
    if (rhs.mBoundaryCondition != NULL) {
      mBoundaryCondition = rhs.mBoundaryCondition->clone();
    }
    if (rhs.mDiffusionCoefficient != NULL) {
      mDiffusionCoefficient = rhs.mDiffusionCoefficient->clone();
    }
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialParameterPlugin object.
 */
SpatialParameterPlugin* 
SpatialParameterPlugin::clone () const
{
  return new SpatialParameterPlugin(*this);
}


/*
 * Destructor for SpatialParameterPlugin.
 */
SpatialParameterPlugin::~SpatialParameterPlugin()
{
  deleteChildren();
}

void SpatialParameterPlugin::deleteChildren()
{
  delete mSpatialSymbolReference;
  delete mAdvectionCoefficient;
  delete mBoundaryCondition;
  delete mDiffusionCoefficient;
  mSpatialSymbolReference = NULL;
  mAdvectionCoefficient = NULL;
  mBoundaryCondition = NULL;
  mDiffusionCoefficient = NULL;
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
SpatialParameterPlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    if (name == "spatialSymbolReference" ) 
    { 
      mSpatialSymbolReference = new SpatialSymbolReference(spatialns);

      object = mSpatialSymbolReference;

    } 
    else if (name == "advectionCoefficient" ) 
    { 
      mAdvectionCoefficient = new AdvectionCoefficient(spatialns);

      object = mAdvectionCoefficient;

    } 
    else if (name == "boundaryCondition" ) 
    { 
      mBoundaryCondition = new BoundaryCondition(spatialns);

      object = mBoundaryCondition;

    } 
    else if (name == "diffusionCoefficient" ) 
    { 
      mDiffusionCoefficient = new DiffusionCoefficient(spatialns);

      object = mDiffusionCoefficient;

    } 

    delete spatialns;
  } 

  return object; 
}


/*
 * write elements
 */
void
SpatialParameterPlugin::writeElements (XMLOutputStream& stream) const
{
  if (isSetSpatialSymbolReference() == true) 
  { 
    mSpatialSymbolReference->write(stream);
  } 
  if (isSetAdvectionCoefficient() == true) 
  { 
    mAdvectionCoefficient->write(stream);
  } 
  if (isSetBoundaryCondition() == true) 
  { 
    mBoundaryCondition->write(stream);
  } 
  if (isSetDiffusionCoefficient() == true) 
  { 
    mDiffusionCoefficient->write(stream);
  } 
}


/*
 * Checks if this plugin object has all the required elements.
 */
bool
SpatialParameterPlugin::hasRequiredElements () const
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
SpatialParameterPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBasePlugin::addExpectedAttributes(attributes);

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpatialParameterPlugin::readAttributes (const XMLAttributes& attributes,
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
SpatialParameterPlugin::writeAttributes (XMLOutputStream& stream) const
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
SpatialParameterPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mSpatialSymbolReference, filter);
  ADD_FILTERED_POINTER(ret, sublist, mAdvectionCoefficient, filter);
  ADD_FILTERED_POINTER(ret, sublist, mBoundaryCondition, filter);
  ADD_FILTERED_POINTER(ret, sublist, mDiffusionCoefficient, filter);

  return ret;
}


/*
 * Returns the SpatialSymbolReference from this SpatialParameterPlugin object.
 */
const SpatialSymbolReference* 
SpatialParameterPlugin::getSpatialSymbolReference () const
{
  return mSpatialSymbolReference;
}


/*
 * Returns the SpatialSymbolReference from this SpatialParameterPlugin object.
 */
SpatialSymbolReference* 
SpatialParameterPlugin::getSpatialSymbolReference ()
{
  return mSpatialSymbolReference;
}


/*
 * @return @c true if the "SpatialSymbolReference" element has been set,
 */
bool 
SpatialParameterPlugin::isSetSpatialSymbolReference () const
{
  return (mSpatialSymbolReference != NULL);
}


/*
 * Sets the SpatialSymbolReference element in this SpatialParameterPlugin object.
 */
int
SpatialParameterPlugin::setSpatialSymbolReference(const SpatialSymbolReference* spatialSymbolReference)
{
  if (spatialSymbolReference == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (spatialSymbolReference->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != spatialSymbolReference->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != spatialSymbolReference->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != spatialSymbolReference->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mSpatialSymbolReference;
    mSpatialSymbolReference = static_cast<SpatialSymbolReference*>(spatialSymbolReference->clone());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new SpatialSymbolReference object and adds it to the SpatialParameterPlugin object.
 */
SpatialSymbolReference*
SpatialParameterPlugin::createSpatialSymbolReference()
{
  delete mSpatialSymbolReference;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mSpatialSymbolReference = new SpatialSymbolReference(spatialns);

  mSpatialSymbolReference->setSBMLDocument(this->getSBMLDocument());

  delete spatialns;

  return mSpatialSymbolReference;
}


/*
 * Returns the AdvectionCoefficient from this SpatialParameterPlugin object.
 */
const AdvectionCoefficient* 
SpatialParameterPlugin::getAdvectionCoefficient () const
{
  return mAdvectionCoefficient;
}


/*
 * Returns the AdvectionCoefficient from this SpatialParameterPlugin object.
 */
AdvectionCoefficient* 
SpatialParameterPlugin::getAdvectionCoefficient ()
{
  return mAdvectionCoefficient;
}


/*
 * @return @c true if the "AdvectionCoefficient" element has been set,
 */
bool 
SpatialParameterPlugin::isSetAdvectionCoefficient () const
{
  return (mAdvectionCoefficient != NULL);
}


/*
 * Sets the AdvectionCoefficient element in this SpatialParameterPlugin object.
 */
int
SpatialParameterPlugin::setAdvectionCoefficient(const AdvectionCoefficient* advectionCoefficient)
{
  if (advectionCoefficient == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (advectionCoefficient->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != advectionCoefficient->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != advectionCoefficient->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != advectionCoefficient->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mAdvectionCoefficient;
    mAdvectionCoefficient = static_cast<AdvectionCoefficient*>(advectionCoefficient->clone());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new AdvectionCoefficient object and adds it to the SpatialParameterPlugin object.
 */
AdvectionCoefficient*
SpatialParameterPlugin::createAdvectionCoefficient()
{
  delete mAdvectionCoefficient;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mAdvectionCoefficient = new AdvectionCoefficient(spatialns);

  mAdvectionCoefficient->setSBMLDocument(this->getSBMLDocument());

  delete spatialns;

  return mAdvectionCoefficient;
}


/*
 * Returns the BoundaryCondition from this SpatialParameterPlugin object.
 */
const BoundaryCondition* 
SpatialParameterPlugin::getBoundaryCondition () const
{
  return mBoundaryCondition;
}


/*
 * Returns the BoundaryCondition from this SpatialParameterPlugin object.
 */
BoundaryCondition* 
SpatialParameterPlugin::getBoundaryCondition ()
{
  return mBoundaryCondition;
}


/*
 * @return @c true if the "BoundaryCondition" element has been set,
 */
bool 
SpatialParameterPlugin::isSetBoundaryCondition () const
{
  return (mBoundaryCondition != NULL);
}


/*
 * Sets the BoundaryCondition element in this SpatialParameterPlugin object.
 */
int
SpatialParameterPlugin::setBoundaryCondition(const BoundaryCondition* boundaryCondition)
{
  if (boundaryCondition == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (boundaryCondition->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != boundaryCondition->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != boundaryCondition->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != boundaryCondition->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mBoundaryCondition;
    mBoundaryCondition = static_cast<BoundaryCondition*>(boundaryCondition->clone());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new BoundaryCondition object and adds it to the SpatialParameterPlugin object.
 */
BoundaryCondition*
SpatialParameterPlugin::createBoundaryCondition()
{
  delete mBoundaryCondition;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mBoundaryCondition = new BoundaryCondition(spatialns);

  mBoundaryCondition->setSBMLDocument(this->getSBMLDocument());

  delete spatialns;

  return mBoundaryCondition;
}


/*
 * Returns the DiffusionCoefficient from this SpatialParameterPlugin object.
 */
const DiffusionCoefficient* 
SpatialParameterPlugin::getDiffusionCoefficient () const
{
  return mDiffusionCoefficient;
}


/*
 * Returns the DiffusionCoefficient from this SpatialParameterPlugin object.
 */
DiffusionCoefficient* 
SpatialParameterPlugin::getDiffusionCoefficient ()
{
  return mDiffusionCoefficient;
}


/*
 * @return @c true if the "DiffusionCoefficient" element has been set,
 */
bool 
SpatialParameterPlugin::isSetDiffusionCoefficient () const
{
  return (mDiffusionCoefficient != NULL);
}


/*
 * Sets the DiffusionCoefficient element in this SpatialParameterPlugin object.
 */
int
SpatialParameterPlugin::setDiffusionCoefficient(const DiffusionCoefficient* diffusionCoefficient)
{
  if (diffusionCoefficient == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (diffusionCoefficient->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != diffusionCoefficient->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != diffusionCoefficient->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != diffusionCoefficient->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mDiffusionCoefficient;
    mDiffusionCoefficient = static_cast<DiffusionCoefficient*>(diffusionCoefficient->clone());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DiffusionCoefficient object and adds it to the SpatialParameterPlugin object.
 */
DiffusionCoefficient*
SpatialParameterPlugin::createDiffusionCoefficient()
{
  delete mDiffusionCoefficient;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mDiffusionCoefficient = new DiffusionCoefficient(spatialns);

  mDiffusionCoefficient->setSBMLDocument(this->getSBMLDocument());

  delete spatialns;

  return mDiffusionCoefficient;
}


//---------------------------------------------------------------


/*
 * Set the SBMLDocument.
 */
void
SpatialParameterPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  if (isSetSpatialSymbolReference() == true)
  {
    mSpatialSymbolReference->setSBMLDocument(d);
  }
  if (isSetAdvectionCoefficient() == true)
  {
    mAdvectionCoefficient->setSBMLDocument(d);
  }
  if (isSetBoundaryCondition() == true)
  {
    mBoundaryCondition->setSBMLDocument(d);
  }
  if (isSetDiffusionCoefficient() == true)
  {
    mDiffusionCoefficient->setSBMLDocument(d);
  }
}


/*
 * Connect to parent.
 */
void
SpatialParameterPlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  if (isSetSpatialSymbolReference() == true)
  {
    mSpatialSymbolReference->connectToParent(sbase);
  }
  if (isSetAdvectionCoefficient() == true)
  {
    mAdvectionCoefficient->connectToParent(sbase);
  }
  if (isSetBoundaryCondition() == true)
  {
    mBoundaryCondition->connectToParent(sbase);
  }
  if (isSetDiffusionCoefficient() == true)
  {
    mDiffusionCoefficient->connectToParent(sbase);
  }
}


/*
 * Enables the given package.
 */
void
SpatialParameterPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  if (isSetSpatialSymbolReference() == true)
  {
    mSpatialSymbolReference->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
  if (isSetAdvectionCoefficient() == true)
  {
    mAdvectionCoefficient->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
  if (isSetBoundaryCondition() == true)
  {
    mBoundaryCondition->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
  if (isSetDiffusionCoefficient() == true)
  {
    mDiffusionCoefficient->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}


/*
 * Accept the SBMLVisitor.
 */
bool
SpatialParameterPlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  return true;
}


/* 
 * @return true, if either the spatial symbol reference, diffusion coefficient, 
 *   advection coefficient or boundary is set. Otherwise the return value is false.
 */ 
bool 
SpatialParameterPlugin::isSpatialParameter() const
{
  return getType() != -1;
}

/* 
 * Determines the type of the spatial parameter, that is one of: 
 * 
 * SBML_SPATIAL_SPATIALSYMBOLREFERENCE
 * SBML_SPATIAL_DIFFUSIONCOEFFICIENT
 * SBML_SPATIAL_ADVECTIONCOEFFICIENT
 * SBML_SPATIAL_BOUNDARYCONDITION
 * 
 * or -1 in case no other is defined.
 */
int 
SpatialParameterPlugin::getType() const
{
  if (isSetSpatialSymbolReference())
  {
    return SBML_SPATIAL_SPATIALSYMBOLREFERENCE;
  }
  if (isSetDiffusionCoefficient())
  {
    return SBML_SPATIAL_DIFFUSIONCOEFFICIENT;
  }    
  if (isSetAdvectionCoefficient())
  {
    return SBML_SPATIAL_ADVECTIONCOEFFICIENT;
  }    
  if (isSetBoundaryCondition())
  {
    return SBML_SPATIAL_BOUNDARYCONDITION;
  }    
  return -1;
}


LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


