/**
 * @file:   MixedGeometry.cpp
 * @brief:  Implementation of the MixedGeometry class
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


#include <sbml/packages/spatial/sbml/MixedGeometry.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/spatial/sbml/AnalyticGeometry.h>
#include <sbml/packages/spatial/sbml/SampledFieldGeometry.h>
#include <sbml/packages/spatial/sbml/CSGeometry.h>
#include <sbml/packages/spatial/sbml/ParametricGeometry.h>
#include <sbml/packages/spatial/sbml/MixedGeometry.h>



using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new MixedGeometry with the given level, version, and package version.
 */
MixedGeometry::MixedGeometry (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : GeometryDefinition(level, version)
  , mGeometryDefinitions (level, version, pkgVersion)
  , mOrdinalMappings (level, version, pkgVersion)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new MixedGeometry with the given SpatialPkgNamespaces object.
 */
MixedGeometry::MixedGeometry (SpatialPkgNamespaces* spatialns)
  : GeometryDefinition(spatialns)
  , mGeometryDefinitions (spatialns)
  , mOrdinalMappings (spatialns)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for MixedGeometry.
 */
MixedGeometry::MixedGeometry (const MixedGeometry& orig)
  : GeometryDefinition(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mGeometryDefinitions  = orig.mGeometryDefinitions;
    mOrdinalMappings  = orig.mOrdinalMappings;

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for MixedGeometry.
 */
MixedGeometry&
MixedGeometry::operator=(const MixedGeometry& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    GeometryDefinition::operator=(rhs);
    mGeometryDefinitions  = rhs.mGeometryDefinitions;
    mOrdinalMappings  = rhs.mOrdinalMappings;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for MixedGeometry.
 */
MixedGeometry*
MixedGeometry::clone () const
{
  return new MixedGeometry(*this);
}


/*
 * Destructor for MixedGeometry.
 */
MixedGeometry::~MixedGeometry ()
{
}


/*
 * Returns the  "ListOfGeometryDefinitions" in this MixedGeometry object.
 */
const ListOfGeometryDefinitions*
MixedGeometry::getListOfGeometryDefinitions() const
{
  return &mGeometryDefinitions;
}


/*
 * Returns the  "ListOfGeometryDefinitions" in this MixedGeometry object.
 */
ListOfGeometryDefinitions*
MixedGeometry::getListOfGeometryDefinitions()
{
  return &mGeometryDefinitions;
}


/*
 * Removes the nth GeometryDefinition from the ListOfGeometryDefinitions.
 */
GeometryDefinition*
MixedGeometry::removeGeometryDefinition(unsigned int n)
{
	return mGeometryDefinitions.remove(n);
}


/*
 * Removes the a GeometryDefinition with given id from the ListOfGeometryDefinitions.
 */
GeometryDefinition*
MixedGeometry::removeGeometryDefinition(const std::string& sid)
{
	return mGeometryDefinitions.remove(sid);
}


/*
 * Return the nth GeometryDefinition in the ListOfGeometryDefinitions within this MixedGeometry.
 */
GeometryDefinition*
MixedGeometry::getGeometryDefinition(unsigned int n)
{
	return mGeometryDefinitions.get(n);
}


/*
 * Return the nth GeometryDefinition in the ListOfGeometryDefinitions within this MixedGeometry.
 */
const GeometryDefinition*
MixedGeometry::getGeometryDefinition(unsigned int n) const
{
	return mGeometryDefinitions.get(n);
}


/*
 * Return a GeometryDefinition from the ListOfGeometryDefinitions by id.
 */
GeometryDefinition*
MixedGeometry::getGeometryDefinition(const std::string& sid)
{
	return mGeometryDefinitions.get(sid);
}


/*
 * Return a GeometryDefinition from the ListOfGeometryDefinitions by id.
 */
const GeometryDefinition*
MixedGeometry::getGeometryDefinition(const std::string& sid) const
{
	return mGeometryDefinitions.get(sid);
}


/*
 * Adds a copy the given "GeometryDefinition" to this MixedGeometry.
 *
 * @param gd; the GeometryDefinition object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
MixedGeometry::addGeometryDefinition(const GeometryDefinition* gd)
{
  if (gd == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (gd->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != gd->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != gd->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(gd)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mGeometryDefinitions.append(gd);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of GeometryDefinition objects in this MixedGeometry.
 *
 * @return the number of GeometryDefinition objects in this MixedGeometry
 */
unsigned int
MixedGeometry::getNumGeometryDefinitions() const
{
  return mGeometryDefinitions.size();
}


/**
 * Creates a new AnalyticGeometry object, adds it to this MixedGeometrys
 * ListOfGeometryDefinitions and returns the AnalyticGeometry object created. 
 *
 * @return a new AnalyticGeometry object instance
 *
 * @see addGeometryDefinition(const GeometryDefinition*)
 */
AnalyticGeometry* 
MixedGeometry::createAnalyticGeometry()
{
  AnalyticGeometry* ag = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    ag = new AnalyticGeometry(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(ag != NULL)
  {
    mGeometryDefinitions.appendAndOwn(ag);
  }

  return ag;
}


/**
 * Creates a new SampledFieldGeometry object, adds it to this MixedGeometrys
 * ListOfGeometryDefinitions and returns the SampledFieldGeometry object created. 
 *
 * @return a new SampledFieldGeometry object instance
 *
 * @see addGeometryDefinition(const GeometryDefinition*)
 */
SampledFieldGeometry* 
MixedGeometry::createSampledFieldGeometry()
{
  SampledFieldGeometry* sfg = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    sfg = new SampledFieldGeometry(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(sfg != NULL)
  {
    mGeometryDefinitions.appendAndOwn(sfg);
  }

  return sfg;
}


/**
 * Creates a new CSGeometry object, adds it to this MixedGeometrys
 * ListOfGeometryDefinitions and returns the CSGeometry object created. 
 *
 * @return a new CSGeometry object instance
 *
 * @see addGeometryDefinition(const GeometryDefinition*)
 */
CSGeometry* 
MixedGeometry::createCsGeometry()
{
  CSGeometry* csg = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csg = new CSGeometry(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csg != NULL)
  {
    mGeometryDefinitions.appendAndOwn(csg);
  }

  return csg;
}


/**
 * Creates a new ParametricGeometry object, adds it to this MixedGeometrys
 * ListOfGeometryDefinitions and returns the ParametricGeometry object created. 
 *
 * @return a new ParametricGeometry object instance
 *
 * @see addGeometryDefinition(const GeometryDefinition*)
 */
ParametricGeometry* 
MixedGeometry::createParametricGeometry()
{
  ParametricGeometry* pg = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    pg = new ParametricGeometry(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(pg != NULL)
  {
    mGeometryDefinitions.appendAndOwn(pg);
  }

  return pg;
}


/**
 * Creates a new MixedGeometry object, adds it to this MixedGeometrys
 * ListOfGeometryDefinitions and returns the MixedGeometry object created. 
 *
 * @return a new MixedGeometry object instance
 *
 * @see addGeometryDefinition(const GeometryDefinition*)
 */
MixedGeometry* 
MixedGeometry::createMixedGeometry()
{
  MixedGeometry* mg = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    mg = new MixedGeometry(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(mg != NULL)
  {
    mGeometryDefinitions.appendAndOwn(mg);
  }

  return mg;
}


/*
 * Returns the  "ListOfOrdinalMappings" in this MixedGeometry object.
 */
const ListOfOrdinalMappings*
MixedGeometry::getListOfOrdinalMappings() const
{
  return &mOrdinalMappings;
}


/*
 * Returns the  "ListOfOrdinalMappings" in this MixedGeometry object.
 */
ListOfOrdinalMappings*
MixedGeometry::getListOfOrdinalMappings()
{
  return &mOrdinalMappings;
}


/*
 * Removes the nth OrdinalMapping from the ListOfOrdinalMappings.
 */
OrdinalMapping*
MixedGeometry::removeOrdinalMapping(unsigned int n)
{
	return mOrdinalMappings.remove(n);
}


/*
 * Removes the a OrdinalMapping with given id from the ListOfOrdinalMappings.
 */
OrdinalMapping*
MixedGeometry::removeOrdinalMapping(const std::string& sid)
{
	return mOrdinalMappings.remove(sid);
}


/*
 * Return the nth OrdinalMapping in the ListOfOrdinalMappings within this MixedGeometry.
 */
OrdinalMapping*
MixedGeometry::getOrdinalMapping(unsigned int n)
{
	return mOrdinalMappings.get(n);
}


/*
 * Return the nth OrdinalMapping in the ListOfOrdinalMappings within this MixedGeometry.
 */
const OrdinalMapping*
MixedGeometry::getOrdinalMapping(unsigned int n) const
{
	return mOrdinalMappings.get(n);
}


/*
 * Return a OrdinalMapping from the ListOfOrdinalMappings by id.
 */
OrdinalMapping*
MixedGeometry::getOrdinalMapping(const std::string& sid)
{
	return mOrdinalMappings.get(sid);
}


/*
 * Return a OrdinalMapping from the ListOfOrdinalMappings by id.
 */
const OrdinalMapping*
MixedGeometry::getOrdinalMapping(const std::string& sid) const
{
	return mOrdinalMappings.get(sid);
}


/*
 * Adds a copy the given "OrdinalMapping" to this MixedGeometry.
 *
 * @param om; the OrdinalMapping object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
MixedGeometry::addOrdinalMapping(const OrdinalMapping* om)
{
  if (om == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (om->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != om->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != om->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(om)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mOrdinalMappings.append(om);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of OrdinalMapping objects in this MixedGeometry.
 *
 * @return the number of OrdinalMapping objects in this MixedGeometry
 */
unsigned int
MixedGeometry::getNumOrdinalMappings() const
{
  return mOrdinalMappings.size();
}


/*
 * Creates a new OrdinalMapping object, adds it to this MixedGeometrys
 * MixedGeometry and returns the OrdinalMapping object created. 
 *
 * @return a new OrdinalMapping object instance
 *
 * @see addOrdinalMapping(const OrdinalMapping* om)
 */
OrdinalMapping*
MixedGeometry::createOrdinalMapping()
{
  OrdinalMapping* om = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    om = new OrdinalMapping(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(om != NULL)
  {
    mOrdinalMappings.appendAndOwn(om);
  }

  return om;
}


List*
MixedGeometry::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
MixedGeometry::getElementName () const
{
  static const string name = "mixedGeometry";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
MixedGeometry::getTypeCode () const
{
  return SBML_SPATIAL_MIXEDGEOMETRY;
}


/*
 * check if all the required attributes are set
 */
bool
MixedGeometry::hasRequiredAttributes () const
{
  bool allPresent = GeometryDefinition::hasRequiredAttributes();

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
MixedGeometry::hasRequiredElements () const
{
  bool allPresent = GeometryDefinition::hasRequiredElements();

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
MixedGeometry::writeElements (XMLOutputStream& stream) const
{
  GeometryDefinition::writeElements(stream);
  if (getNumGeometryDefinitions() > 0)
  {
    mGeometryDefinitions.write(stream);
  }

  if (getNumOrdinalMappings() > 0)
  {
    mOrdinalMappings.write(stream);
  }

  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
MixedGeometry::accept (SBMLVisitor& v) const
{
  v.visit(*this);

/* VISIT CHILDREN */

  v.leave(*this);

  return true;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
MixedGeometry::setSBMLDocument (SBMLDocument* d)
{
  GeometryDefinition::setSBMLDocument(d);
  mGeometryDefinitions.setSBMLDocument(d);
  mOrdinalMappings.setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
MixedGeometry::connectToChild()
{
  GeometryDefinition::connectToChild();

  mGeometryDefinitions.connectToParent(this);
  mOrdinalMappings.connectToParent(this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
MixedGeometry::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  GeometryDefinition::enablePackageInternal(pkgURI, pkgPrefix, flag);
  mGeometryDefinitions.enablePackageInternal(pkgURI, pkgPrefix, flag);
  mOrdinalMappings.enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
MixedGeometry::createObject(XMLInputStream& stream)
{
  SBase* object = GeometryDefinition::createObject(stream);

  const string& name = stream.peek().getName();

  if (name == "listOfGeometryDefinitions")
  {
    object = &mGeometryDefinitions;
  }
  else if (name == "listOfOrdinalMappings")
  {
    object = &mOrdinalMappings;
  }
  connectToChild();


  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
MixedGeometry::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GeometryDefinition::addExpectedAttributes(attributes);

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
MixedGeometry::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  GeometryDefinition::readAttributes(attributes, expectedAttributes);

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
MixedGeometry::writeAttributes (XMLOutputStream& stream) const
{
  GeometryDefinition::writeAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
MixedGeometry_t *
MixedGeometry_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion)
{
  return new MixedGeometry(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
MixedGeometry_free(MixedGeometry_t * mg)
{
  if (mg != NULL)
    delete mg;
}


LIBSBML_EXTERN
MixedGeometry_t *
MixedGeometry_clone(MixedGeometry_t * mg)
{
  if (mg != NULL)
  {
    return static_cast<MixedGeometry_t*>(mg->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
int
MixedGeometry_addGeometryDefinition(MixedGeometry_t * mg, GeometryDefinition_t * gd)
{
	return  (mg != NULL) ? mg->addGeometryDefinition(gd) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
AnalyticGeometry_t *
MixedGeometry_createAnalyticGeometry(MixedGeometry_t * mg)
{
	return  (mg != NULL) ? mg->createAnalyticGeometry() : NULL;
}

LIBSBML_EXTERN
SampledFieldGeometry_t *
MixedGeometry_createSampledFieldGeometry(MixedGeometry_t * mg)
{
	return  (mg != NULL) ? mg->createSampledFieldGeometry() : NULL;
}

LIBSBML_EXTERN
CSGeometry_t *
MixedGeometry_createCsGeometry(MixedGeometry_t * mg)
{
	return  (mg != NULL) ? mg->createCsGeometry() : NULL;
}

LIBSBML_EXTERN
ParametricGeometry_t *
MixedGeometry_createParametricGeometry(MixedGeometry_t * mg)
{
	return  (mg != NULL) ? mg->createParametricGeometry() : NULL;
}

LIBSBML_EXTERN
MixedGeometry_t *
MixedGeometry_createMixedGeometry(MixedGeometry_t * mg)
{
	return  (mg != NULL) ? mg->createMixedGeometry() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
MixedGeometry_getListOfGeometryDefinitions(MixedGeometry_t * mg)
{
	return  (mg != NULL) ? (ListOf_t *)mg->getListOfGeometryDefinitions() : NULL;
}

LIBSBML_EXTERN
GeometryDefinition_t *
MixedGeometry_getGeometryDefinition(MixedGeometry_t * mg, unsigned int n)
{
	return  (mg != NULL) ? mg->getGeometryDefinition(n) : NULL;
}

LIBSBML_EXTERN
GeometryDefinition_t *
MixedGeometry_getGeometryDefinitionById(MixedGeometry_t * mg, const char * sid)
{
	return  (mg != NULL) ? mg->getGeometryDefinition(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
MixedGeometry_getNumGeometryDefinitions(MixedGeometry_t * mg)
{
	return  (mg != NULL) ? mg->getNumGeometryDefinitions() : SBML_INT_MAX;
}

LIBSBML_EXTERN
GeometryDefinition_t *
MixedGeometry_removeGeometryDefinition(MixedGeometry_t * mg, unsigned int n)
{
	return  (mg != NULL) ? mg->removeGeometryDefinition(n) : NULL;
}

LIBSBML_EXTERN
GeometryDefinition_t *
MixedGeometry_removeGeometryDefinitionById(MixedGeometry_t * mg, const char * sid)
{
	return  (mg != NULL) ? mg->removeGeometryDefinition(sid) : NULL;
}

LIBSBML_EXTERN
int
MixedGeometry_addOrdinalMapping(MixedGeometry_t * mg, OrdinalMapping_t * om)
{
	return  (mg != NULL) ? mg->addOrdinalMapping(om) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
OrdinalMapping_t *
MixedGeometry_createOrdinalMapping(MixedGeometry_t * mg)
{
	return  (mg != NULL) ? mg->createOrdinalMapping() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
MixedGeometry_getListOfOrdinalMappings(MixedGeometry_t * mg)
{
	return  (mg != NULL) ? (ListOf_t *)mg->getListOfOrdinalMappings() : NULL;
}

LIBSBML_EXTERN
OrdinalMapping_t *
MixedGeometry_getOrdinalMapping(MixedGeometry_t * mg, unsigned int n)
{
	return  (mg != NULL) ? mg->getOrdinalMapping(n) : NULL;
}

LIBSBML_EXTERN
OrdinalMapping_t *
MixedGeometry_getOrdinalMappingById(MixedGeometry_t * mg, const char * sid)
{
	return  (mg != NULL) ? mg->getOrdinalMapping(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
MixedGeometry_getNumOrdinalMappings(MixedGeometry_t * mg)
{
	return  (mg != NULL) ? mg->getNumOrdinalMappings() : SBML_INT_MAX;
}

LIBSBML_EXTERN
OrdinalMapping_t *
MixedGeometry_removeOrdinalMapping(MixedGeometry_t * mg, unsigned int n)
{
	return  (mg != NULL) ? mg->removeOrdinalMapping(n) : NULL;
}

LIBSBML_EXTERN
OrdinalMapping_t *
MixedGeometry_removeOrdinalMappingById(MixedGeometry_t * mg, const char * sid)
{
	return  (mg != NULL) ? mg->removeOrdinalMapping(sid) : NULL;
}

LIBSBML_EXTERN
int
MixedGeometry_hasRequiredAttributes(const MixedGeometry_t * mg)
{
  return (mg != NULL) ? static_cast<int>(mg->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
MixedGeometry_hasRequiredElements(const MixedGeometry_t * mg)
{
	return (mg != NULL) ? static_cast<int>(mg->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


