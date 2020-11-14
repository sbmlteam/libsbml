/**
 * @file MixedGeometry.cpp
 * @brief Implementation of the MixedGeometry class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
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




#ifdef __cplusplus


/*
 * Creates a new MixedGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
MixedGeometry::MixedGeometry(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
  : GeometryDefinition(level, version, pkgVersion)
  , mGeometryDefinitions (level, version, pkgVersion)
  , mOrdinalMappings (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new MixedGeometry using the given SpatialPkgNamespaces object.
 */
MixedGeometry::MixedGeometry(SpatialPkgNamespaces *spatialns)
  : GeometryDefinition(spatialns)
  , mGeometryDefinitions (spatialns)
  , mOrdinalMappings (spatialns)
{
  setElementNamespace(spatialns->getURI());
  connectToChild();
  loadPlugins(spatialns);
}


/*
 * Copy constructor for MixedGeometry.
 */
MixedGeometry::MixedGeometry(const MixedGeometry& orig)
  : GeometryDefinition( orig )
  , mGeometryDefinitions ( orig.mGeometryDefinitions )
  , mOrdinalMappings ( orig.mOrdinalMappings )
{
  connectToChild();
}


/*
 * Assignment operator for MixedGeometry.
 */
MixedGeometry&
MixedGeometry::operator=(const MixedGeometry& rhs)
{
  if (&rhs != this)
  {
    GeometryDefinition::operator=(rhs);
    mGeometryDefinitions = rhs.mGeometryDefinitions;
    mOrdinalMappings = rhs.mOrdinalMappings;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this MixedGeometry object.
 */
MixedGeometry*
MixedGeometry::clone() const
{
  return new MixedGeometry(*this);
}


/*
 * Destructor for MixedGeometry.
 */
MixedGeometry::~MixedGeometry()
{
}


/*
 * Returns the ListOfGeometryDefinitions from this MixedGeometry.
 */
const ListOfGeometryDefinitions*
MixedGeometry::getListOfGeometryDefinitions() const
{
  return &mGeometryDefinitions;
}


/*
 * Returns the ListOfGeometryDefinitions from this MixedGeometry.
 */
ListOfGeometryDefinitions*
MixedGeometry::getListOfGeometryDefinitions()
{
  return &mGeometryDefinitions;
}


/*
 * Get a GeometryDefinition from the MixedGeometry.
 */
GeometryDefinition*
MixedGeometry::getGeometryDefinition(unsigned int n)
{
  return mGeometryDefinitions.get(n);
}


/*
 * Get a GeometryDefinition from the MixedGeometry.
 */
const GeometryDefinition*
MixedGeometry::getGeometryDefinition(unsigned int n) const
{
  return mGeometryDefinitions.get(n);
}


/*
 * Get a GeometryDefinition from the MixedGeometry based on its identifier.
 */
GeometryDefinition*
MixedGeometry::getGeometryDefinition(const std::string& sid)
{
  return mGeometryDefinitions.get(sid);
}


/*
 * Get a GeometryDefinition from the MixedGeometry based on its identifier.
 */
const GeometryDefinition*
MixedGeometry::getGeometryDefinition(const std::string& sid) const
{
  return mGeometryDefinitions.get(sid);
}


/*
 * Adds a copy of the given GeometryDefinition to this MixedGeometry.
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
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(gd)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (gd->isSetId() && (mGeometryDefinitions.get(gd->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mGeometryDefinitions.append(gd);
  }
}


/*
 * Get the number of GeometryDefinition objects in this MixedGeometry.
 */
unsigned int
MixedGeometry::getNumGeometryDefinitions() const
{
  return mGeometryDefinitions.size();
}


/*
 * Creates a new AnalyticGeometry object, adds it to this MixedGeometry object
 * and returns the AnalyticGeometry object created.
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
  }

  if (ag != NULL)
  {
    mGeometryDefinitions.appendAndOwn(ag);
  }

  return ag;
}


/*
 * Creates a new SampledFieldGeometry object, adds it to this MixedGeometry
 * object and returns the SampledFieldGeometry object created.
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
  }

  if (sfg != NULL)
  {
    mGeometryDefinitions.appendAndOwn(sfg);
  }

  return sfg;
}


/*
 * Creates a new CSGeometry object, adds it to this MixedGeometry object and
 * returns the CSGeometry object created.
 */
CSGeometry*
MixedGeometry::createCSGeometry()
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
  }

  if (csg != NULL)
  {
    mGeometryDefinitions.appendAndOwn(csg);
  }

  return csg;
}


/*
 * Creates a new ParametricGeometry object, adds it to this MixedGeometry
 * object and returns the ParametricGeometry object created.
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
  }

  if (pg != NULL)
  {
    mGeometryDefinitions.appendAndOwn(pg);
  }

  return pg;
}


/*
 * Creates a new MixedGeometry object, adds it to this MixedGeometry object and
 * returns the MixedGeometry object created.
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
  }

  if (mg != NULL)
  {
    mGeometryDefinitions.appendAndOwn(mg);
  }

  return mg;
}


/*
 * Removes the nth GeometryDefinition from this MixedGeometry and returns a
 * pointer to it.
 */
GeometryDefinition*
MixedGeometry::removeGeometryDefinition(unsigned int n)
{
  return mGeometryDefinitions.remove(n);
}


/*
 * Removes the GeometryDefinition from this MixedGeometry based on its
 * identifier and returns a pointer to it.
 */
GeometryDefinition*
MixedGeometry::removeGeometryDefinition(const std::string& sid)
{
  return mGeometryDefinitions.remove(sid);
}


/*
 * Returns the ListOfOrdinalMappings from this MixedGeometry.
 */
const ListOfOrdinalMappings*
MixedGeometry::getListOfOrdinalMappings() const
{
  return &mOrdinalMappings;
}


/*
 * Returns the ListOfOrdinalMappings from this MixedGeometry.
 */
ListOfOrdinalMappings*
MixedGeometry::getListOfOrdinalMappings()
{
  return &mOrdinalMappings;
}


/*
 * Get an OrdinalMapping from the MixedGeometry.
 */
OrdinalMapping*
MixedGeometry::getOrdinalMapping(unsigned int n)
{
  return mOrdinalMappings.get(n);
}


/*
 * Get an OrdinalMapping from the MixedGeometry.
 */
const OrdinalMapping*
MixedGeometry::getOrdinalMapping(unsigned int n) const
{
  return mOrdinalMappings.get(n);
}


/*
 * Get an OrdinalMapping from the MixedGeometry based on the GeometryDefinition
 * to which it refers.
 */
const OrdinalMapping*
MixedGeometry::getOrdinalMappingByGeometryDefinition(const std::string& sid)
  const
{
  return mOrdinalMappings.getByGeometryDefinition(sid);
}


/*
 * Get an OrdinalMapping from the MixedGeometry based on the GeometryDefinition
 * to which it refers.
 */
OrdinalMapping*
MixedGeometry::getOrdinalMappingByGeometryDefinition(const std::string& sid)
{
  return mOrdinalMappings.getByGeometryDefinition(sid);
}


/*
 * Adds a copy of the given OrdinalMapping to this MixedGeometry.
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
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(om)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mOrdinalMappings.append(om);
  }
}


/*
 * Get the number of OrdinalMapping objects in this MixedGeometry.
 */
unsigned int
MixedGeometry::getNumOrdinalMappings() const
{
  return mOrdinalMappings.size();
}


/*
 * Creates a new OrdinalMapping object, adds it to this MixedGeometry object
 * and returns the OrdinalMapping object created.
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
  }

  if (om != NULL)
  {
    mOrdinalMappings.appendAndOwn(om);
  }

  return om;
}


/*
 * Removes the nth OrdinalMapping from this MixedGeometry and returns a pointer
 * to it.
 */
OrdinalMapping*
MixedGeometry::removeOrdinalMapping(unsigned int n)
{
  return mOrdinalMappings.remove(n);
}


/*
 * Returns the XML element name of this MixedGeometry object.
 */
const std::string&
MixedGeometry::getElementName() const
{
  static const string name = "mixedGeometry";
  return name;
}


/*
 * Returns the libSBML type code for this MixedGeometry object.
 */
int
MixedGeometry::getTypeCode() const
{
  return SBML_SPATIAL_MIXEDGEOMETRY;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * MixedGeometry object have been set.
 */
bool
MixedGeometry::hasRequiredAttributes() const
{
  bool allPresent = GeometryDefinition::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * MixedGeometry object have been set.
 */
bool
MixedGeometry::hasRequiredElements() const
{
  bool allPresent = GeometryDefinition::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
MixedGeometry::writeElements(XMLOutputStream& stream) const
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

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
MixedGeometry::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mGeometryDefinitions.accept(v);

  mOrdinalMappings.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
MixedGeometry::setSBMLDocument(SBMLDocument* d)
{
  GeometryDefinition::setSBMLDocument(d);

  mGeometryDefinitions.setSBMLDocument(d);

  mOrdinalMappings.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
MixedGeometry::connectToChild()
{
  GeometryDefinition::connectToChild();

  mGeometryDefinitions.connectToParent(this);

  mOrdinalMappings.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
MixedGeometry::enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag)
{
  GeometryDefinition::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mGeometryDefinitions.enablePackageInternal(pkgURI, pkgPrefix, flag);

  mOrdinalMappings.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
MixedGeometry::updateSBMLNamespace(const std::string& package,
                                   unsigned int level,
                                   unsigned int version)
{
  GeometryDefinition::updateSBMLNamespace(package, level, version);

  mGeometryDefinitions.updateSBMLNamespace(package, level, version);

  mOrdinalMappings.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this MixedGeometry.
 */
int
MixedGeometry::getAttribute(const std::string& attributeName,
                            bool& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this MixedGeometry.
 */
int
MixedGeometry::getAttribute(const std::string& attributeName,
                            int& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this MixedGeometry.
 */
int
MixedGeometry::getAttribute(const std::string& attributeName,
                            double& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this MixedGeometry.
 */
int
MixedGeometry::getAttribute(const std::string& attributeName,
                            unsigned int& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this MixedGeometry.
 */
int
MixedGeometry::getAttribute(const std::string& attributeName,
                            std::string& value) const
{
  int return_value = GeometryDefinition::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this MixedGeometry's attribute
 * "attributeName" is set.
 */
bool
MixedGeometry::isSetAttribute(const std::string& attributeName) const
{
  bool value = GeometryDefinition::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this MixedGeometry.
 */
int
MixedGeometry::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this MixedGeometry.
 */
int
MixedGeometry::setAttribute(const std::string& attributeName, int value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this MixedGeometry.
 */
int
MixedGeometry::setAttribute(const std::string& attributeName, double value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this MixedGeometry.
 */
int
MixedGeometry::setAttribute(const std::string& attributeName,
                            unsigned int value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this MixedGeometry.
 */
int
MixedGeometry::setAttribute(const std::string& attributeName,
                            const std::string& value)
{
  int return_value = GeometryDefinition::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this MixedGeometry.
 */
int
MixedGeometry::unsetAttribute(const std::string& attributeName)
{
  int value = GeometryDefinition::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this MixedGeometry.
 */
SBase*
MixedGeometry::createChildObject(const std::string& elementName)
{
  GeometryDefinition* obj = NULL;

  if (elementName == "analyticGeometry")
  {
    return createAnalyticGeometry();
  }
  else if (elementName == "sampledFieldGeometry")
  {
    return createSampledFieldGeometry();
  }
  else if (elementName == "csGeometry")
  {
    return createCSGeometry();
  }
  else if (elementName == "parametricGeometry")
  {
    return createParametricGeometry();
  }
  else if (elementName == "mixedGeometry")
  {
    return createMixedGeometry();
  }
  else if (elementName == "ordinalMapping")
  {
    return createOrdinalMapping();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this MixedGeometry.
 */
int
MixedGeometry::addChildObject(const std::string& elementName,
                              const SBase* element)
{
  if (elementName == "analyticGeometry" && element->getTypeCode() ==
    SBML_SPATIAL_ANALYTICGEOMETRY)
  {
    return addGeometryDefinition((const GeometryDefinition*)(element));
  }
  else if (elementName == "sampledFieldGeometry" && element->getTypeCode() ==
    SBML_SPATIAL_SAMPLEDFIELDGEOMETRY)
  {
    return addGeometryDefinition((const GeometryDefinition*)(element));
  }
  else if (elementName == "csGeometry" && element->getTypeCode() ==
    SBML_SPATIAL_CSGEOMETRY)
  {
    return addGeometryDefinition((const GeometryDefinition*)(element));
  }
  else if (elementName == "parametricGeometry" && element->getTypeCode() ==
    SBML_SPATIAL_PARAMETRICGEOMETRY)
  {
    return addGeometryDefinition((const GeometryDefinition*)(element));
  }
  else if (elementName == "mixedGeometry" && element->getTypeCode() ==
    SBML_SPATIAL_MIXEDGEOMETRY)
  {
    return addGeometryDefinition((const GeometryDefinition*)(element));
  }
  else if (elementName == "ordinalMapping" && element->getTypeCode() ==
    SBML_SPATIAL_ORDINALMAPPING)
  {
    return addOrdinalMapping((const OrdinalMapping*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * MixedGeometry.
 */
SBase*
MixedGeometry::removeChildObject(const std::string& elementName,
                                 const std::string& id)
{
  if (elementName == "analyticGeometry")
  {
    return removeGeometryDefinition(id);
  }
  else if (elementName == "sampledFieldGeometry")
  {
    return removeGeometryDefinition(id);
  }
  else if (elementName == "csGeometry")
  {
    return removeGeometryDefinition(id);
  }
  else if (elementName == "parametricGeometry")
  {
    return removeGeometryDefinition(id);
  }
  else if (elementName == "mixedGeometry")
  {
    return removeGeometryDefinition(id);
  }
  else if (elementName == "ordinalMapping")
  {
    for (unsigned int i = 0; i < getNumOrdinalMappings(); i++)
    {
      if (getOrdinalMapping(i)->getId() == id)
      {
        return removeOrdinalMapping(i);
      }
    }
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this MixedGeometry.
 */
unsigned int
MixedGeometry::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "geometryDefinition")
  {
    return getNumGeometryDefinitions();
  }
  else if (elementName == "ordinalMapping")
  {
    return getNumOrdinalMappings();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this MixedGeometry.
 */
SBase*
MixedGeometry::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "geometryDefinition")
  {
    return getGeometryDefinition(index);
  }
  else if (elementName == "ordinalMapping")
  {
    return getOrdinalMapping(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
MixedGeometry::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mGeometryDefinitions.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mOrdinalMappings.getElementBySId(id);

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
MixedGeometry::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mGeometryDefinitions.getMetaId() == metaid)
  {
    return &mGeometryDefinitions;
  }

  if (mOrdinalMappings.getMetaId() == metaid)
  {
    return &mOrdinalMappings;
  }

  obj = mGeometryDefinitions.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mOrdinalMappings.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
MixedGeometry::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mGeometryDefinitions, filter);
  ADD_FILTERED_LIST(ret, sublist, mOrdinalMappings, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
MixedGeometry::createObject(XMLInputStream& stream)
{
  SBase* obj = GeometryDefinition::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfGeometryDefinitions")
  {
    if (mGeometryDefinitions.size() != 0)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialMixedGeometryAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    obj = &mGeometryDefinitions;
  }
  else if (name == "listOfOrdinalMappings")
  {
    if (mOrdinalMappings.size() != 0)
    {
      getErrorLog()->logPackageError("spatial",
        SpatialMixedGeometryAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    obj = &mOrdinalMappings;
  }

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
MixedGeometry::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GeometryDefinition::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
MixedGeometry::readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  GeometryDefinition::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialUnknown, pkgVersion, level,
          version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialMixedGeometryAllowedCoreAttributes, pkgVersion, level, version,
            details);
      }
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
MixedGeometry::writeAttributes(XMLOutputStream& stream) const
{
  GeometryDefinition::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new MixedGeometry_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
MixedGeometry_t *
MixedGeometry_create(unsigned int level,
                     unsigned int version,
                     unsigned int pkgVersion)
{
  return new MixedGeometry(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this MixedGeometry_t object.
 */
LIBSBML_EXTERN
MixedGeometry_t*
MixedGeometry_clone(const MixedGeometry_t* mg)
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


/*
 * Frees this MixedGeometry_t object.
 */
LIBSBML_EXTERN
void
MixedGeometry_free(MixedGeometry_t* mg)
{
  if (mg != NULL)
  {
    delete mg;
  }
}


/*
 * Returns a ListOf_t * containing GeometryDefinition_t objects from this
 * MixedGeometry_t.
 */
LIBSBML_EXTERN
ListOf_t*
MixedGeometry_getListOfGeometryDefinitions(MixedGeometry_t* mg)
{
  return (mg != NULL) ? mg->getListOfGeometryDefinitions() : NULL;
}


/*
 * Get a GeometryDefinition_t from the MixedGeometry_t.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
MixedGeometry_getGeometryDefinition(MixedGeometry_t* mg, unsigned int n)
{
  return (mg != NULL) ? mg->getGeometryDefinition(n) : NULL;
}


/*
 * Get a GeometryDefinition_t from the MixedGeometry_t based on its identifier.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
MixedGeometry_getGeometryDefinitionById(MixedGeometry_t* mg, const char *sid)
{
  return (mg != NULL && sid != NULL) ? mg->getGeometryDefinition(sid) : NULL;
}


/*
 * Adds a copy of the given GeometryDefinition_t to this MixedGeometry_t.
 */
LIBSBML_EXTERN
int
MixedGeometry_addGeometryDefinition(MixedGeometry_t* mg,
                                    const GeometryDefinition_t* gd)
{
  return (mg != NULL) ? mg->addGeometryDefinition(gd) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of GeometryDefinition_t objects in this MixedGeometry_t.
 */
LIBSBML_EXTERN
unsigned int
MixedGeometry_getNumGeometryDefinitions(MixedGeometry_t* mg)
{
  return (mg != NULL) ? mg->getNumGeometryDefinitions() : SBML_INT_MAX;
}


/*
 * Creates a new AnalyticGeometry_t object, adds it to this MixedGeometry_t
 * object and returns the AnalyticGeometry_t object created.
 */
LIBSBML_EXTERN
AnalyticGeometry_t*
MixedGeometry_createAnalyticGeometry(MixedGeometry_t* mg)
{
  return (mg != NULL) ? mg->createAnalyticGeometry() : NULL;
}


/*
 * Creates a new SampledFieldGeometry_t object, adds it to this MixedGeometry_t
 * object and returns the SampledFieldGeometry_t object created.
 */
LIBSBML_EXTERN
SampledFieldGeometry_t*
MixedGeometry_createSampledFieldGeometry(MixedGeometry_t* mg)
{
  return (mg != NULL) ? mg->createSampledFieldGeometry() : NULL;
}


/*
 * Creates a new CSGeometry_t object, adds it to this MixedGeometry_t object
 * and returns the CSGeometry_t object created.
 */
LIBSBML_EXTERN
CSGeometry_t*
MixedGeometry_createCSGeometry(MixedGeometry_t* mg)
{
  return (mg != NULL) ? mg->createCSGeometry() : NULL;
}


/*
 * Creates a new ParametricGeometry_t object, adds it to this MixedGeometry_t
 * object and returns the ParametricGeometry_t object created.
 */
LIBSBML_EXTERN
ParametricGeometry_t*
MixedGeometry_createParametricGeometry(MixedGeometry_t* mg)
{
  return (mg != NULL) ? mg->createParametricGeometry() : NULL;
}


/*
 * Creates a new MixedGeometry_t object, adds it to this MixedGeometry_t object
 * and returns the MixedGeometry_t object created.
 */
LIBSBML_EXTERN
MixedGeometry_t*
MixedGeometry_createMixedGeometry(MixedGeometry_t* mg)
{
  return (mg != NULL) ? mg->createMixedGeometry() : NULL;
}


/*
 * Removes the nth GeometryDefinition_t from this MixedGeometry_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
MixedGeometry_removeGeometryDefinition(MixedGeometry_t* mg, unsigned int n)
{
  return (mg != NULL) ? mg->removeGeometryDefinition(n) : NULL;
}


/*
 * Removes the GeometryDefinition_t from this MixedGeometry_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
MixedGeometry_removeGeometryDefinitionById(MixedGeometry_t* mg,
                                           const char* sid)
{
  return (mg != NULL && sid != NULL) ? mg->removeGeometryDefinition(sid) :
    NULL;
}


/*
 * Returns a ListOf_t * containing OrdinalMapping_t objects from this
 * MixedGeometry_t.
 */
LIBSBML_EXTERN
ListOf_t*
MixedGeometry_getListOfOrdinalMappings(MixedGeometry_t* mg)
{
  return (mg != NULL) ? mg->getListOfOrdinalMappings() : NULL;
}


/*
 * Get an OrdinalMapping_t from the MixedGeometry_t.
 */
LIBSBML_EXTERN
OrdinalMapping_t*
MixedGeometry_getOrdinalMapping(MixedGeometry_t* mg, unsigned int n)
{
  return (mg != NULL) ? mg->getOrdinalMapping(n) : NULL;
}


/*
 * Get an OrdinalMapping_t from the MixedGeometry_t based on the
 * GeometryDefinition to which it refers.
 */
LIBSBML_EXTERN
OrdinalMapping_t*
MixedGeometry_getOrdinalMappingByGeometryDefinition(MixedGeometry_t* mg,
                                                    const char *sid)
{
  return (mg != NULL && sid != NULL) ?
    mg->getOrdinalMappingByGeometryDefinition(sid) : NULL;
}


/*
 * Adds a copy of the given OrdinalMapping_t to this MixedGeometry_t.
 */
LIBSBML_EXTERN
int
MixedGeometry_addOrdinalMapping(MixedGeometry_t* mg,
                                const OrdinalMapping_t* om)
{
  return (mg != NULL) ? mg->addOrdinalMapping(om) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of OrdinalMapping_t objects in this MixedGeometry_t.
 */
LIBSBML_EXTERN
unsigned int
MixedGeometry_getNumOrdinalMappings(MixedGeometry_t* mg)
{
  return (mg != NULL) ? mg->getNumOrdinalMappings() : SBML_INT_MAX;
}


/*
 * Creates a new OrdinalMapping_t object, adds it to this MixedGeometry_t
 * object and returns the OrdinalMapping_t object created.
 */
LIBSBML_EXTERN
OrdinalMapping_t*
MixedGeometry_createOrdinalMapping(MixedGeometry_t* mg)
{
  return (mg != NULL) ? mg->createOrdinalMapping() : NULL;
}


/*
 * Removes the nth OrdinalMapping_t from this MixedGeometry_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
OrdinalMapping_t*
MixedGeometry_removeOrdinalMapping(MixedGeometry_t* mg, unsigned int n)
{
  return (mg != NULL) ? mg->removeOrdinalMapping(n) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * MixedGeometry_t object have been set.
 */
LIBSBML_EXTERN
int
MixedGeometry_hasRequiredAttributes(const MixedGeometry_t * mg)
{
  return (mg != NULL) ? static_cast<int>(mg->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * MixedGeometry_t object have been set.
 */
LIBSBML_EXTERN
int
MixedGeometry_hasRequiredElements(const MixedGeometry_t * mg)
{
  return (mg != NULL) ? static_cast<int>(mg->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


