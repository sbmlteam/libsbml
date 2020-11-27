/**
* @cond doxygenLibsbmlInternal
*
* @file    SpatialUniqueSampledVolumeValueCheck.cpp
* @brief   Ensure that spatial compartment mappings' unit sizes sum to one.
* @author  Sarah Keating, Lucian Smith
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
* ---------------------------------------------------------------------- -->*/

#include "SpatialUniqueSampledVolumeValueCheck.h"
#include <sbml/packages/spatial/extension/SpatialParameterPlugin.h>
#include <sbml/packages/spatial/extension/SpatialModelPlugin.h>
#include <sbml/packages/spatial/sbml/SampledFieldGeometry.h>
#include <sbml/packages/spatial/sbml/MixedGeometry.h>

#include <map>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus


/*
* Creates a new Constraint with the given constraint id.
*/
SpatialUniqueSampledVolumeValueCheck::SpatialUniqueSampledVolumeValueCheck(unsigned int id, SpatialValidator & v):
  TConstraint<Model>(id, v)
{
}

/*
* Destroys this Constraint.
*/
SpatialUniqueSampledVolumeValueCheck::~SpatialUniqueSampledVolumeValueCheck ()
{
}


/*
* Checks that all ids on the following Model objects are unique:
* event assignments and assignment rules.
*/
void
SpatialUniqueSampledVolumeValueCheck::check_(const Model& m, const Model&)
{
  if (m.getLevel() < 3) {
    return;
  }
  const SpatialModelPlugin* mplug = static_cast<const SpatialModelPlugin*>(m.getPlugin("spatial"));
  if (mplug == NULL) {
    return;
  }
  const Geometry* geom = mplug->getGeometry();
  if (geom == NULL) {
    return;
  }
  for (unsigned long g = 0; g < geom->getNumGeometryDefinitions(); g++) {
    checkGeometryDefinition(geom->getGeometryDefinition(g), m);
  }
}

void SpatialUniqueSampledVolumeValueCheck::checkGeometryDefinition(const GeometryDefinition * gd, const Model& m)
{
  if (gd->getTypeCode() == SBML_SPATIAL_SAMPLEDFIELDGEOMETRY) {
    checkSampledFieldGeometry(static_cast<const SampledFieldGeometry*>(gd), m);
  }
  if (gd->getTypeCode() == SBML_SPATIAL_MIXEDGEOMETRY) {
    const MixedGeometry* mg = static_cast<const MixedGeometry*>(gd);
    for (unsigned long g = 0; g < mg->getNumGeometryDefinitions(); g++) {
      checkGeometryDefinition(mg->getGeometryDefinition(g), m);
    }
  }
}

void SpatialUniqueSampledVolumeValueCheck::checkSampledFieldGeometry(const SampledFieldGeometry * sfg, const Model& m)
{
  bool fail = false;
  map<double, string> values;
  for (unsigned long sv = 0; sv < sfg->getNumSampledVolumes(); sv++) {
    const SampledVolume* svol = sfg->getSampledVolume(sv);
    if (svol->isSetSampledValue()) {
      double val = svol->getSampledValue();
      if (values.find(val) != values.end()) {
        stringstream ss_msg;
        ss_msg << "A <sampledVolume>";
        if (svol->isSetId())
        {
          ss_msg << " with id '" << svol->getId() << "'";
        }
        ss_msg << " has a 'spatial:sampledValue' attribute value of '";
        ss_msg << svol->getSampledValue();
        ss_msg << "', which is the sampledValue of a different <sampledVolume>";
        string svid = values[val];
        if (!svid.empty()) {
          ss_msg << " with id '" << svid << "'";
        }
        ss_msg << ".";
        msg = ss_msg.str();
        logFailure(m);
      }
      else {
        values.insert(make_pair(val, svol->getId()));
      }
    }
  }
}

#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
