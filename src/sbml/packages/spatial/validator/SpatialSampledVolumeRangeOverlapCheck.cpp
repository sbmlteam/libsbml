/**
* @cond doxygenLibsbmlInternal
*
* @file    SpatialSampledVolumeRangeOverlapCheck.cpp
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

#include "SpatialSampledVolumeRangeOverlapCheck.h"
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
SpatialSampledVolumeRangeOverlapCheck::SpatialSampledVolumeRangeOverlapCheck(unsigned int id, SpatialValidator & v):
  TConstraint<Model>(id, v)
{
}

/*
* Destroys this Constraint.
*/
SpatialSampledVolumeRangeOverlapCheck::~SpatialSampledVolumeRangeOverlapCheck ()
{
}


/*
* Checks that all ids on the following Model objects are unique:
* event assignments and assignment rules.
*/
void
SpatialSampledVolumeRangeOverlapCheck::check_(const Model& m, const Model&)
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

void SpatialSampledVolumeRangeOverlapCheck::checkGeometryDefinition(const GeometryDefinition * gd, const Model& m)
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

void SpatialSampledVolumeRangeOverlapCheck::checkSampledFieldGeometry(const SampledFieldGeometry * sfg, const Model& m)
{
  bool fail = false;
  map<pair<double, double>, string> ranges;
  for (unsigned long sv = 0; sv < sfg->getNumSampledVolumes(); sv++) {
    const SampledVolume* svol = sfg->getSampledVolume(sv);
    if (svol->isSetMinValue() && svol->isSetMaxValue() && !svol->isSetSampledValue()) {
      double min2 = svol->getMinValue();
      double max2 = svol->getMaxValue();
      string id2 = svol->getId();
      for (map<pair<double, double>, string>::iterator range = ranges.begin(); range != ranges.end(); range++) {
        double min = range->first.first;
        double max = range->first.second;
        string id = range->second;
        if ((min2 < max && min2 >= min) || (max2 <= max && max2 > min)
          || (min < max2 && min >= min2) || (max <= max2 && max > min2)) {
          stringstream ss_msg;
          ss_msg << "The first <sampledVolume> ";
          if (!id.empty())
          {
            ss_msg << " with id '" << id << "'";
          }
          ss_msg << " has a rage of " << min << " to " << max << ", while a second <sampledVolume>";
          if (!id2.empty()) {
            ss_msg << " with id '" << id2 << "'";
          }
          ss_msg << " has a range of " << min2 << " to " << max2 << ".";
          msg = ss_msg.str();
          logFailure(m);
        }
      }
      ranges.insert(make_pair(make_pair(min2, max2), id2));
    }
  }
}

#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
