/**
 * @file SpatialParameterPlugin.cpp
 * @brief Implementation of the SpatialParameterPlugin class.
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
#include <sbml/packages/spatial/extension/SpatialParameterPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new SpatialParameterPlugin using the given URI, prefix and package
 * namespace.
 */
SpatialParameterPlugin::SpatialParameterPlugin(const std::string& uri,
                                               const std::string& prefix,
                                               SpatialPkgNamespaces* spatialns)
  : SBasePlugin(uri, prefix, spatialns)
  , mSpatialSymbolReference (NULL)
  , mAdvectionCoefficient (NULL)
  , mBoundaryCondition (NULL)
  , mDiffusionCoefficient (NULL)
{
  connectToChild();
}


/*
 * Copy constructor for SpatialParameterPlugin.
 */
SpatialParameterPlugin::SpatialParameterPlugin(const SpatialParameterPlugin&
  orig)
  : SBasePlugin( orig )
  , mSpatialSymbolReference ( NULL )
  , mAdvectionCoefficient ( NULL )
  , mBoundaryCondition ( NULL )
  , mDiffusionCoefficient ( NULL )
{
  if (orig.mSpatialSymbolReference != NULL)
  {
    mSpatialSymbolReference = orig.mSpatialSymbolReference->clone();
  }

  if (orig.mAdvectionCoefficient != NULL)
  {
    mAdvectionCoefficient = orig.mAdvectionCoefficient->clone();
  }

  if (orig.mBoundaryCondition != NULL)
  {
    mBoundaryCondition = orig.mBoundaryCondition->clone();
  }

  if (orig.mDiffusionCoefficient != NULL)
  {
    mDiffusionCoefficient = orig.mDiffusionCoefficient->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for SpatialParameterPlugin.
 */
SpatialParameterPlugin&
SpatialParameterPlugin::operator=(const SpatialParameterPlugin& rhs)
{
  if (&rhs != this)
  {
    SBasePlugin::operator=(rhs);
    delete mSpatialSymbolReference;
    if (rhs.mSpatialSymbolReference != NULL)
    {
      mSpatialSymbolReference = rhs.mSpatialSymbolReference->clone();
    }
    else
    {
      mSpatialSymbolReference = NULL;
    }

    delete mAdvectionCoefficient;
    if (rhs.mAdvectionCoefficient != NULL)
    {
      mAdvectionCoefficient = rhs.mAdvectionCoefficient->clone();
    }
    else
    {
      mAdvectionCoefficient = NULL;
    }

    delete mBoundaryCondition;
    if (rhs.mBoundaryCondition != NULL)
    {
      mBoundaryCondition = rhs.mBoundaryCondition->clone();
    }
    else
    {
      mBoundaryCondition = NULL;
    }

    delete mDiffusionCoefficient;
    if (rhs.mDiffusionCoefficient != NULL)
    {
      mDiffusionCoefficient = rhs.mDiffusionCoefficient->clone();
    }
    else
    {
      mDiffusionCoefficient = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialParameterPlugin object.
 */
SpatialParameterPlugin*
SpatialParameterPlugin::clone() const
{
  return new SpatialParameterPlugin(*this);
}


/*
 * Destructor for SpatialParameterPlugin.
 */
SpatialParameterPlugin::~SpatialParameterPlugin()
{
  delete mSpatialSymbolReference;
  mSpatialSymbolReference = NULL;
  delete mAdvectionCoefficient;
  mAdvectionCoefficient = NULL;
  delete mBoundaryCondition;
  mBoundaryCondition = NULL;
  delete mDiffusionCoefficient;
  mDiffusionCoefficient = NULL;
}


/*
 * Returns the value of the "spatialSymbolReference" element of this
 * SpatialParameterPlugin.
 */
const SpatialSymbolReference*
SpatialParameterPlugin::getSpatialSymbolReference() const
{
  return mSpatialSymbolReference;
}


/*
 * Returns the value of the "spatialSymbolReference" element of this
 * SpatialParameterPlugin.
 */
SpatialSymbolReference*
SpatialParameterPlugin::getSpatialSymbolReference()
{
  return mSpatialSymbolReference;
}


/*
 * Returns the value of the "advectionCoefficient" element of this
 * SpatialParameterPlugin.
 */
const AdvectionCoefficient*
SpatialParameterPlugin::getAdvectionCoefficient() const
{
  return mAdvectionCoefficient;
}


/*
 * Returns the value of the "advectionCoefficient" element of this
 * SpatialParameterPlugin.
 */
AdvectionCoefficient*
SpatialParameterPlugin::getAdvectionCoefficient()
{
  return mAdvectionCoefficient;
}


/*
 * Returns the value of the "boundaryCondition" element of this
 * SpatialParameterPlugin.
 */
const BoundaryCondition*
SpatialParameterPlugin::getBoundaryCondition() const
{
  return mBoundaryCondition;
}


/*
 * Returns the value of the "boundaryCondition" element of this
 * SpatialParameterPlugin.
 */
BoundaryCondition*
SpatialParameterPlugin::getBoundaryCondition()
{
  return mBoundaryCondition;
}


/*
 * Returns the value of the "diffusionCoefficient" element of this
 * SpatialParameterPlugin.
 */
const DiffusionCoefficient*
SpatialParameterPlugin::getDiffusionCoefficient() const
{
  return mDiffusionCoefficient;
}


/*
 * Returns the value of the "diffusionCoefficient" element of this
 * SpatialParameterPlugin.
 */
DiffusionCoefficient*
SpatialParameterPlugin::getDiffusionCoefficient()
{
  return mDiffusionCoefficient;
}


/*
 * Predicate returning @c true if this SpatialParameterPlugin's
 * "spatialSymbolReference" element is set.
 */
bool
SpatialParameterPlugin::isSetSpatialSymbolReference() const
{
  return (mSpatialSymbolReference != NULL);
}


/*
 * Predicate returning @c true if this SpatialParameterPlugin's
 * "advectionCoefficient" element is set.
 */
bool
SpatialParameterPlugin::isSetAdvectionCoefficient() const
{
  return (mAdvectionCoefficient != NULL);
}


/*
 * Predicate returning @c true if this SpatialParameterPlugin's
 * "boundaryCondition" element is set.
 */
bool
SpatialParameterPlugin::isSetBoundaryCondition() const
{
  return (mBoundaryCondition != NULL);
}


/*
 * Predicate returning @c true if this SpatialParameterPlugin's
 * "diffusionCoefficient" element is set.
 */
bool
SpatialParameterPlugin::isSetDiffusionCoefficient() const
{
  return (mDiffusionCoefficient != NULL);
}


/*
 * Sets the value of the "spatialSymbolReference" element of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::setSpatialSymbolReference(const SpatialSymbolReference*
  spatialSymbolReference)
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
    mSpatialSymbolReference =
      static_cast<SpatialSymbolReference*>(spatialSymbolReference->clone());
    connectToChild();
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "advectionCoefficient" element of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::setAdvectionCoefficient(const AdvectionCoefficient*
  advectionCoefficient)
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
    mAdvectionCoefficient =
      static_cast<AdvectionCoefficient*>(advectionCoefficient->clone());
    connectToChild();
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "boundaryCondition" element of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::setBoundaryCondition(const BoundaryCondition*
  boundaryCondition)
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
    mBoundaryCondition =
      static_cast<BoundaryCondition*>(boundaryCondition->clone());
    connectToChild();
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "diffusionCoefficient" element of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::setDiffusionCoefficient(const DiffusionCoefficient*
  diffusionCoefficient)
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
    mDiffusionCoefficient =
      static_cast<DiffusionCoefficient*>(diffusionCoefficient->clone());
    connectToChild();
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new SpatialSymbolReference object, adds it to this
 * SpatialParameterPlugin object and returns the SpatialSymbolReference object
 * created.
 */
SpatialSymbolReference*
SpatialParameterPlugin::createSpatialSymbolReference()
{
  if (mSpatialSymbolReference != NULL)
  {
    delete mSpatialSymbolReference;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mSpatialSymbolReference = new SpatialSymbolReference(spatialns);

  mSpatialSymbolReference->setSBMLDocument(this->getSBMLDocument());

  delete spatialns;

  connectToChild();

  return mSpatialSymbolReference;
}


/*
 * Creates a new AdvectionCoefficient object, adds it to this
 * SpatialParameterPlugin object and returns the AdvectionCoefficient object
 * created.
 */
AdvectionCoefficient*
SpatialParameterPlugin::createAdvectionCoefficient()
{
  if (mAdvectionCoefficient != NULL)
  {
    delete mAdvectionCoefficient;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mAdvectionCoefficient = new AdvectionCoefficient(spatialns);

  mAdvectionCoefficient->setSBMLDocument(this->getSBMLDocument());

  delete spatialns;

  connectToChild();

  return mAdvectionCoefficient;
}


/*
 * Creates a new BoundaryCondition object, adds it to this
 * SpatialParameterPlugin object and returns the BoundaryCondition object
 * created.
 */
BoundaryCondition*
SpatialParameterPlugin::createBoundaryCondition()
{
  if (mBoundaryCondition != NULL)
  {
    delete mBoundaryCondition;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mBoundaryCondition = new BoundaryCondition(spatialns);

  mBoundaryCondition->setSBMLDocument(this->getSBMLDocument());

  delete spatialns;

  connectToChild();

  return mBoundaryCondition;
}


/*
 * Creates a new DiffusionCoefficient object, adds it to this
 * SpatialParameterPlugin object and returns the DiffusionCoefficient object
 * created.
 */
DiffusionCoefficient*
SpatialParameterPlugin::createDiffusionCoefficient()
{
  if (mDiffusionCoefficient != NULL)
  {
    delete mDiffusionCoefficient;
  }

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mDiffusionCoefficient = new DiffusionCoefficient(spatialns);

  mDiffusionCoefficient->setSBMLDocument(this->getSBMLDocument());

  delete spatialns;

  connectToChild();

  return mDiffusionCoefficient;
}


/*
 * Unsets the value of the "spatialSymbolReference" element of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::unsetSpatialSymbolReference()
{
  delete mSpatialSymbolReference;
  mSpatialSymbolReference = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "advectionCoefficient" element of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::unsetAdvectionCoefficient()
{
  delete mAdvectionCoefficient;
  mAdvectionCoefficient = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "boundaryCondition" element of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::unsetBoundaryCondition()
{
  delete mBoundaryCondition;
  mBoundaryCondition = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "diffusionCoefficient" element of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::unsetDiffusionCoefficient()
{
  delete mDiffusionCoefficient;
  mDiffusionCoefficient = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
SpatialParameterPlugin::writeElements(XMLOutputStream& stream) const
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

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
SpatialParameterPlugin::accept(SBMLVisitor& v) const
{
  const Parameter* p = static_cast<const
    Parameter*>(this->getParentSBMLObject());
  v.visit(*p);
  v.leave(*p);

  if (mSpatialSymbolReference != NULL)
  {
    mSpatialSymbolReference->accept(v);
  }

  if (mAdvectionCoefficient != NULL)
  {
    mAdvectionCoefficient->accept(v);
  }

  if (mBoundaryCondition != NULL)
  {
    mBoundaryCondition->accept(v);
  }

  if (mDiffusionCoefficient != NULL)
  {
    mDiffusionCoefficient->accept(v);
  }

  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
SpatialParameterPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  if (mSpatialSymbolReference != NULL)
  {
    mSpatialSymbolReference->setSBMLDocument(d);
  }

  if (mAdvectionCoefficient != NULL)
  {
    mAdvectionCoefficient->setSBMLDocument(d);
  }

  if (mBoundaryCondition != NULL)
  {
    mBoundaryCondition->setSBMLDocument(d);
  }

  if (mDiffusionCoefficient != NULL)
  {
    mDiffusionCoefficient->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
SpatialParameterPlugin::connectToChild()
{
  connectToParent(getParentSBMLObject());
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to parent element
 */
void
SpatialParameterPlugin::connectToParent(SBase* base)
{
  SBasePlugin::connectToParent(base);

  if (mSpatialSymbolReference != NULL)
  {
    mSpatialSymbolReference->connectToParent(base);
  }

  if (mAdvectionCoefficient != NULL)
  {
    mAdvectionCoefficient->connectToParent(base);
  }

  if (mBoundaryCondition != NULL)
  {
    mBoundaryCondition->connectToParent(base);
  }

  if (mDiffusionCoefficient != NULL)
  {
    mDiffusionCoefficient->connectToParent(base);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
SpatialParameterPlugin::enablePackageInternal(const std::string& pkgURI,
                                              const std::string& pkgPrefix,
                                              bool flag)
{
  if (isSetSpatialSymbolReference())
  {
    mSpatialSymbolReference->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetAdvectionCoefficient())
  {
    mAdvectionCoefficient->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetBoundaryCondition())
  {
    mBoundaryCondition->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetDiffusionCoefficient())
  {
    mDiffusionCoefficient->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
SpatialParameterPlugin::updateSBMLNamespace(const std::string& package,
                                            unsigned int level,
                                            unsigned int version)
{
  SBasePlugin::updateSBMLNamespace(package, level, version);

  if (mSpatialSymbolReference != NULL)
  {
    mSpatialSymbolReference->updateSBMLNamespace(package, level, version);
  }

  if (mAdvectionCoefficient != NULL)
  {
    mAdvectionCoefficient->updateSBMLNamespace(package, level, version);
  }

  if (mBoundaryCondition != NULL)
  {
    mBoundaryCondition->updateSBMLNamespace(package, level, version);
  }

  if (mDiffusionCoefficient != NULL)
  {
    mDiffusionCoefficient->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::getAttribute(const std::string& attributeName,
                                     bool& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::getAttribute(const std::string& attributeName,
                                     int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::getAttribute(const std::string& attributeName,
                                     double& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::getAttribute(const std::string& attributeName,
                                     unsigned int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::getAttribute(const std::string& attributeName,
                                     std::string& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this SpatialParameterPlugin's attribute
 * "attributeName" is set.
 */
bool
SpatialParameterPlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBasePlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::setAttribute(const std::string& attributeName,
                                     bool value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::setAttribute(const std::string& attributeName,
                                     int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::setAttribute(const std::string& attributeName,
                                     double value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::setAttribute(const std::string& attributeName,
                                     unsigned int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::setAttribute(const std::string& attributeName,
                                     const std::string& value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBasePlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * SpatialParameterPlugin.
 */
SBase*
SpatialParameterPlugin::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "spatialSymbolReference")
  {
    return createSpatialSymbolReference();
  }
  else if (elementName == "advectionCoefficient")
  {
    return createAdvectionCoefficient();
  }
  else if (elementName == "boundaryCondition")
  {
    return createBoundaryCondition();
  }
  else if (elementName == "diffusionCoefficient")
  {
    return createDiffusionCoefficient();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this SpatialParameterPlugin.
 */
int
SpatialParameterPlugin::addChildObject(const std::string& elementName,
                                       const SBase* element)
{
  if (elementName == "spatialSymbolReference" && element->getTypeCode() ==
    SBML_SPATIAL_SPATIALSYMBOLREFERENCE)
  {
    return setSpatialSymbolReference((const SpatialSymbolReference*)(element));
  }
  else if (elementName == "advectionCoefficient" && element->getTypeCode() ==
    SBML_SPATIAL_ADVECTIONCOEFFICIENT)
  {
    return setAdvectionCoefficient((const AdvectionCoefficient*)(element));
  }
  else if (elementName == "boundaryCondition" && element->getTypeCode() ==
    SBML_SPATIAL_BOUNDARYCONDITION)
  {
    return setBoundaryCondition((const BoundaryCondition*)(element));
  }
  else if (elementName == "diffusionCoefficient" && element->getTypeCode() ==
    SBML_SPATIAL_DIFFUSIONCOEFFICIENT)
  {
    return setDiffusionCoefficient((const DiffusionCoefficient*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * SpatialParameterPlugin.
 */
SBase*
SpatialParameterPlugin::removeChildObject(const std::string& elementName,
                                          const std::string& id)
{
  if (elementName == "spatialSymbolReference")
  {
    SpatialSymbolReference * obj = getSpatialSymbolReference();
    if (unsetSpatialSymbolReference() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "advectionCoefficient")
  {
    AdvectionCoefficient * obj = getAdvectionCoefficient();
    if (unsetAdvectionCoefficient() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "boundaryCondition")
  {
    BoundaryCondition * obj = getBoundaryCondition();
    if (unsetBoundaryCondition() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "diffusionCoefficient")
  {
    DiffusionCoefficient * obj = getDiffusionCoefficient();
    if (unsetDiffusionCoefficient() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this SpatialParameterPlugin.
 */
unsigned int
SpatialParameterPlugin::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "spatialSymbolReference")
  {
    if (isSetSpatialSymbolReference())
    {
      return 1;
    }
  }
  else if (elementName == "advectionCoefficient")
  {
    if (isSetAdvectionCoefficient())
    {
      return 1;
    }
  }
  else if (elementName == "boundaryCondition")
  {
    if (isSetBoundaryCondition())
    {
      return 1;
    }
  }
  else if (elementName == "diffusionCoefficient")
  {
    if (isSetDiffusionCoefficient())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this SpatialParameterPlugin.
 */
SBase*
SpatialParameterPlugin::getObject(const std::string& elementName,
                                  unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "spatialSymbolReference")
  {
    return getSpatialSymbolReference();
  }
  else if (elementName == "advectionCoefficient")
  {
    return getAdvectionCoefficient();
  }
  else if (elementName == "boundaryCondition")
  {
    return getBoundaryCondition();
  }
  else if (elementName == "diffusionCoefficient")
  {
    return getDiffusionCoefficient();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
SpatialParameterPlugin::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mSpatialSymbolReference != NULL)
  {
    if (mSpatialSymbolReference->getId() == id)
    {
      return mSpatialSymbolReference;
    }

    obj = mSpatialSymbolReference->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mAdvectionCoefficient != NULL)
  {
    if (mAdvectionCoefficient->getId() == id)
    {
      return mAdvectionCoefficient;
    }

    obj = mAdvectionCoefficient->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mBoundaryCondition != NULL)
  {
    if (mBoundaryCondition->getId() == id)
    {
      return mBoundaryCondition;
    }

    obj = mBoundaryCondition->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mDiffusionCoefficient != NULL)
  {
    if (mDiffusionCoefficient->getId() == id)
    {
      return mDiffusionCoefficient;
    }

    obj = mDiffusionCoefficient->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
SpatialParameterPlugin::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mSpatialSymbolReference != NULL)
  {
    if (mSpatialSymbolReference->getMetaId() == metaid)
    {
      return mSpatialSymbolReference;
    }

    obj = mSpatialSymbolReference->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mAdvectionCoefficient != NULL)
  {
    if (mAdvectionCoefficient->getMetaId() == metaid)
    {
      return mAdvectionCoefficient;
    }

    obj = mAdvectionCoefficient->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mBoundaryCondition != NULL)
  {
    if (mBoundaryCondition->getMetaId() == metaid)
    {
      return mBoundaryCondition;
    }

    obj = mBoundaryCondition->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mDiffusionCoefficient != NULL)
  {
    if (mDiffusionCoefficient->getMetaId() == metaid)
    {
      return mDiffusionCoefficient;
    }

    obj = mDiffusionCoefficient->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
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



/** @cond doxygenLibsbmlInternal */

/*
 * Append items from model (used in comp flattening)
 */
int
SpatialParameterPlugin::appendFrom(const Model* model)
{
  int ret = LIBSBML_OPERATION_SUCCESS;

  if (model == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  const SpatialParameterPlugin* plug = static_cast<const
    SpatialParameterPlugin*>(model->getPlugin(getPrefix()));

  if (plug == NULL)
  {
    return ret;
  }

  Model* parent = static_cast<Model*>(getParentSBMLObject());

  if (parent == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  return ret;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
SpatialParameterPlugin::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();
  const XMLNamespaces& xmlns = stream.peek().getNamespaces();
  const std::string& prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ?
    xmlns.getPrefix(mURI) : mPrefix;

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (prefix == targetPrefix)
  {
    string err = "A <parameter> ";
    Parameter* param = static_cast<Parameter*>(getParentSBMLObject());
    if (param->isSetId()) {
      err += "with the id '" + param->getIdAttribute() + "' ";
    }
    if (name == "spatialSymbolReference")
    {
      if (isSetSpatialSymbolReference())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
            getVersion(), err + "has multiple <spatialSymbolReference> children.");
      }
      else if (isSetAdvectionCoefficient())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has both a <spatialSymbolReference> and an <advectionCoefficient> child.");
      }
      else if (isSetBoundaryCondition())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has both a <spatialSymbolReference> and a <boundaryCondition> child.");
      }
      else if (isSetDiffusionCoefficient())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has both a <spatialSymbolReference> and a <diffusionCoefficient> child.");
      }

      delete mSpatialSymbolReference;
      mSpatialSymbolReference = NULL;
      mSpatialSymbolReference = new SpatialSymbolReference(spatialns);
      obj = mSpatialSymbolReference;
    }
    else if (name == "advectionCoefficient")
    {
      if (isSetSpatialSymbolReference())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has both an <advectionCoefficient> and a <spatialSymbolReference> child.");
      }
      else if (isSetAdvectionCoefficient())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has multiple <advectionCoefficient> children.");
      }
      else if (isSetBoundaryCondition())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has both an <advectionCoefficient> and a <boundaryCondition> child.");
      }
      else if (isSetDiffusionCoefficient())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has both an <advectionCoefficient> and a <diffusionCoefficient> child.");
      }

      delete mAdvectionCoefficient;
      mAdvectionCoefficient = NULL;
      mAdvectionCoefficient = new AdvectionCoefficient(spatialns);
      obj = mAdvectionCoefficient;
    }
    else if (name == "boundaryCondition")
    {
      if (isSetSpatialSymbolReference())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has both a <boundaryCondition> and a <spatialSymbolReference> child.");
      }
      else if (isSetAdvectionCoefficient())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has both a <boundaryCondition> and an <advectionCoefficient> child.");
      }
      else if (isSetBoundaryCondition())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has multiple <boundaryCondition> children.");
      }
      else if (isSetDiffusionCoefficient())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has both a <boundaryCondition> and a <diffusionCoefficient> child.");
      }

      delete mBoundaryCondition;
      mBoundaryCondition = NULL;
      mBoundaryCondition = new BoundaryCondition(spatialns);
      obj = mBoundaryCondition;
    }
    else if (name == "diffusionCoefficient")
    {
      if (isSetSpatialSymbolReference())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has both a <diffusionCoefficient> and a <spatialSymbolReference> child.");
      }
      else if (isSetAdvectionCoefficient())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has both a <diffusionCoefficient> and an <advectionCoefficient> child.");
      }
      else if (isSetBoundaryCondition())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has both a <diffusionCoefficient> and a <boundaryCondition> child.");
      }
      else if (isSetDiffusionCoefficient())
      {
        getErrorLog()->logPackageError("spatial",
          SpatialParameterAllowedElements, getPackageVersion(), getLevel(),
          getVersion(), err + "has multiple <diffusionCoefficient> children.");
      }

      delete mDiffusionCoefficient;
      mDiffusionCoefficient = NULL;
      mDiffusionCoefficient = new DiffusionCoefficient(spatialns);
      obj = mDiffusionCoefficient;
    }
  }

  delete spatialns;

  connectToChild();

  return obj;
}

/** @endcond */


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


#endif /* __cplusplus */


/*
 * Returns the value of the "spatialSymbolReference" element of this
 * SpatialParameterPlugin_t.
 */
LIBSBML_EXTERN
const SpatialSymbolReference_t*
SpatialParameterPlugin_getSpatialSymbolReference(const SpatialParameterPlugin_t
  * spp)
{
  if (spp == NULL)
  {
    return NULL;
  }

  return (SpatialSymbolReference_t*)(spp->getSpatialSymbolReference());
}


/*
 * Returns the value of the "advectionCoefficient" element of this
 * SpatialParameterPlugin_t.
 */
LIBSBML_EXTERN
const AdvectionCoefficient_t*
SpatialParameterPlugin_getAdvectionCoefficient(const SpatialParameterPlugin_t *
  spp)
{
  if (spp == NULL)
  {
    return NULL;
  }

  return (AdvectionCoefficient_t*)(spp->getAdvectionCoefficient());
}


/*
 * Returns the value of the "boundaryCondition" element of this
 * SpatialParameterPlugin_t.
 */
LIBSBML_EXTERN
const BoundaryCondition_t*
SpatialParameterPlugin_getBoundaryCondition(const SpatialParameterPlugin_t *
  spp)
{
  if (spp == NULL)
  {
    return NULL;
  }

  return (BoundaryCondition_t*)(spp->getBoundaryCondition());
}


/*
 * Returns the value of the "diffusionCoefficient" element of this
 * SpatialParameterPlugin_t.
 */
LIBSBML_EXTERN
const DiffusionCoefficient_t*
SpatialParameterPlugin_getDiffusionCoefficient(const SpatialParameterPlugin_t *
  spp)
{
  if (spp == NULL)
  {
    return NULL;
  }

  return (DiffusionCoefficient_t*)(spp->getDiffusionCoefficient());
}


/*
 * Predicate returning @c 1 (true) if this SpatialParameterPlugin_t's
 * "spatialSymbolReference" element is set.
 */
LIBSBML_EXTERN
int
SpatialParameterPlugin_isSetSpatialSymbolReference(const
  SpatialParameterPlugin_t * spp)
{
  return (spp != NULL) ? static_cast<int>(spp->isSetSpatialSymbolReference()) :
    0;
}


/*
 * Predicate returning @c 1 (true) if this SpatialParameterPlugin_t's
 * "advectionCoefficient" element is set.
 */
LIBSBML_EXTERN
int
SpatialParameterPlugin_isSetAdvectionCoefficient(const SpatialParameterPlugin_t
  * spp)
{
  return (spp != NULL) ? static_cast<int>(spp->isSetAdvectionCoefficient()) :
    0;
}


/*
 * Predicate returning @c 1 (true) if this SpatialParameterPlugin_t's
 * "boundaryCondition" element is set.
 */
LIBSBML_EXTERN
int
SpatialParameterPlugin_isSetBoundaryCondition(const SpatialParameterPlugin_t *
  spp)
{
  return (spp != NULL) ? static_cast<int>(spp->isSetBoundaryCondition()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SpatialParameterPlugin_t's
 * "diffusionCoefficient" element is set.
 */
LIBSBML_EXTERN
int
SpatialParameterPlugin_isSetDiffusionCoefficient(const SpatialParameterPlugin_t
  * spp)
{
  return (spp != NULL) ? static_cast<int>(spp->isSetDiffusionCoefficient()) :
    0;
}


/*
 * Sets the value of the "spatialSymbolReference" element of this
 * SpatialParameterPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialParameterPlugin_setSpatialSymbolReference(
                                                 SpatialParameterPlugin_t *
                                                   spp,
                                                 const
                                                   SpatialSymbolReference_t*
                                                     spatialSymbolReference)
{
  return (spp != NULL) ? spp->setSpatialSymbolReference(spatialSymbolReference)
    : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "advectionCoefficient" element of this
 * SpatialParameterPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialParameterPlugin_setAdvectionCoefficient(SpatialParameterPlugin_t * spp,
                                               const AdvectionCoefficient_t*
                                                 advectionCoefficient)
{
  return (spp != NULL) ? spp->setAdvectionCoefficient(advectionCoefficient) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "boundaryCondition" element of this
 * SpatialParameterPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialParameterPlugin_setBoundaryCondition(SpatialParameterPlugin_t * spp,
                                            const BoundaryCondition_t*
                                              boundaryCondition)
{
  return (spp != NULL) ? spp->setBoundaryCondition(boundaryCondition) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "diffusionCoefficient" element of this
 * SpatialParameterPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialParameterPlugin_setDiffusionCoefficient(SpatialParameterPlugin_t * spp,
                                               const DiffusionCoefficient_t*
                                                 diffusionCoefficient)
{
  return (spp != NULL) ? spp->setDiffusionCoefficient(diffusionCoefficient) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new SpatialSymbolReference_t object, adds it to this
 * SpatialParameterPlugin_t object and returns the SpatialSymbolReference_t
 * object created.
 */
LIBSBML_EXTERN
SpatialSymbolReference_t*
SpatialParameterPlugin_createSpatialSymbolReference(SpatialParameterPlugin_t*
  spp)
{
  if (spp == NULL)
  {
    return NULL;
  }

  return (SpatialSymbolReference_t*)(spp->createSpatialSymbolReference());
}


/*
 * Creates a new AdvectionCoefficient_t object, adds it to this
 * SpatialParameterPlugin_t object and returns the AdvectionCoefficient_t
 * object created.
 */
LIBSBML_EXTERN
AdvectionCoefficient_t*
SpatialParameterPlugin_createAdvectionCoefficient(SpatialParameterPlugin_t*
  spp)
{
  if (spp == NULL)
  {
    return NULL;
  }

  return (AdvectionCoefficient_t*)(spp->createAdvectionCoefficient());
}


/*
 * Creates a new BoundaryCondition_t object, adds it to this
 * SpatialParameterPlugin_t object and returns the BoundaryCondition_t object
 * created.
 */
LIBSBML_EXTERN
BoundaryCondition_t*
SpatialParameterPlugin_createBoundaryCondition(SpatialParameterPlugin_t* spp)
{
  if (spp == NULL)
  {
    return NULL;
  }

  return (BoundaryCondition_t*)(spp->createBoundaryCondition());
}


/*
 * Creates a new DiffusionCoefficient_t object, adds it to this
 * SpatialParameterPlugin_t object and returns the DiffusionCoefficient_t
 * object created.
 */
LIBSBML_EXTERN
DiffusionCoefficient_t*
SpatialParameterPlugin_createDiffusionCoefficient(SpatialParameterPlugin_t*
  spp)
{
  if (spp == NULL)
  {
    return NULL;
  }

  return (DiffusionCoefficient_t*)(spp->createDiffusionCoefficient());
}


/*
 * Unsets the value of the "spatialSymbolReference" element of this
 * SpatialParameterPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialParameterPlugin_unsetSpatialSymbolReference(SpatialParameterPlugin_t *
  spp)
{
  return (spp != NULL) ? spp->unsetSpatialSymbolReference() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "advectionCoefficient" element of this
 * SpatialParameterPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialParameterPlugin_unsetAdvectionCoefficient(SpatialParameterPlugin_t *
  spp)
{
  return (spp != NULL) ? spp->unsetAdvectionCoefficient() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "boundaryCondition" element of this
 * SpatialParameterPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialParameterPlugin_unsetBoundaryCondition(SpatialParameterPlugin_t * spp)
{
  return (spp != NULL) ? spp->unsetBoundaryCondition() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "diffusionCoefficient" element of this
 * SpatialParameterPlugin_t.
 */
LIBSBML_EXTERN
int
SpatialParameterPlugin_unsetDiffusionCoefficient(SpatialParameterPlugin_t *
  spp)
{
  return (spp != NULL) ? spp->unsetDiffusionCoefficient() :
    LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END


