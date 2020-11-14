/**
* @cond doxygenLibsbmlInternal
*
* @file    SpatialUniqueDiffusionCoefficientsCheck.cpp
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

#include "SpatialUniqueDiffusionCoefficientsCheck.h"
#include <sbml/packages/spatial/extension/SpatialParameterPlugin.h>

#include <set>
#include <map>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus


/*
* Creates a new Constraint with the given constraint id.
*/
SpatialUniqueDiffusionCoefficientsCheck::SpatialUniqueDiffusionCoefficientsCheck(unsigned int id, SpatialValidator & v):
  TConstraint<Model>(id, v)
{
}

/*
* Destroys this Constraint.
*/
SpatialUniqueDiffusionCoefficientsCheck::~SpatialUniqueDiffusionCoefficientsCheck ()
{
}


/*
* Checks that all ids on the following Model objects are unique:
* event assignments and assignment rules.
*/
void
SpatialUniqueDiffusionCoefficientsCheck::check_ (const Model& m, const Model&)
{
  if (m.getLevel() < 3) {
    return;
  }
  map<string, string> xaxis, yaxis, zaxis, xyplane, xzplane, yzplane;
  for (unsigned long p = 0; p < m.getNumParameters(); p++) {
    const Parameter* param = m.getParameter(p);
    const SpatialParameterPlugin* spp = static_cast<const SpatialParameterPlugin*>(param->getPlugin("spatial"));
    if (spp == NULL) {
      continue;
    }
    if (spp->isSetDiffusionCoefficient()) {
      const DiffusionCoefficient* diff = spp->getDiffusionCoefficient();
      if (!diff->isSetVariable()) {
        continue;
      }
      string var = diff->getVariable();
      string difftype = "<diffusionCoefficient>";
      if (diff->isSetId()) {
        difftype += " (id '" + diff->getId() + "')";
      }
      switch(diff->getType()) {
      case SPATIAL_DIFFUSIONKIND_ISOTROPIC:
        difftype += " with a 'type' of 'isotropic'";
        if (addVariableToAxisAndCheck(xaxis, var, difftype, "X axis", xyplane, xzplane, diff, m)) break;
        if (addVariableToAxisAndCheck(yaxis, var, difftype, "Y axis", xyplane, yzplane, diff, m)) break;
        if (addVariableToAxisAndCheck(zaxis, var, difftype, "Z axis", yzplane, xzplane, diff, m)) break;
        break;
      case SPATIAL_DIFFUSIONKIND_ANISOTROPIC:
        difftype += " with a 'type' of 'anisotropic'";
        switch (diff->getCoordinateReference1()) {
        case SPATIAL_COORDINATEKIND_CARTESIAN_X:
          addVariableToAxisAndCheck(xaxis, var, difftype, "X axis", xyplane, xzplane, diff, m);
          break;
        case SPATIAL_COORDINATEKIND_CARTESIAN_Y:
          addVariableToAxisAndCheck(yaxis, var, difftype, "Y axis", xyplane, yzplane, diff, m);
          break;
        case SPATIAL_COORDINATEKIND_CARTESIAN_Z:
          addVariableToAxisAndCheck(zaxis, var, difftype, "Z axis", yzplane, xzplane, diff, m);
          break;
        }
        break;
      case SPATIAL_DIFFUSIONKIND_TENSOR:
        difftype += " with a 'type' of 'tensor'";
        switch (diff->getCoordinateReference1()) {
        case SPATIAL_COORDINATEKIND_CARTESIAN_X:
          switch (diff->getCoordinateReference2()) {
          case SPATIAL_COORDINATEKIND_CARTESIAN_Y:
            addVariableToPlaneAndCheck(xyplane, var, difftype, "XY plane", xaxis, yaxis, diff, m);
            break;
          case SPATIAL_COORDINATEKIND_CARTESIAN_Z:
            addVariableToPlaneAndCheck(xzplane, var, difftype, "XZ plane", xaxis, zaxis, diff, m);
            break;
          }
          break;

        case SPATIAL_COORDINATEKIND_CARTESIAN_Y:
          switch (diff->getCoordinateReference2()) {
          case SPATIAL_COORDINATEKIND_CARTESIAN_X:
            addVariableToPlaneAndCheck(xyplane, var, difftype, "XY plane", xaxis, yaxis, diff, m);
            break;
          case SPATIAL_COORDINATEKIND_CARTESIAN_Z:
            addVariableToPlaneAndCheck(yzplane, var, difftype, "YZ plane", yaxis, zaxis, diff, m);
            break;
          }
          break;

        case SPATIAL_COORDINATEKIND_CARTESIAN_Z:
          switch (diff->getCoordinateReference2()) {
          case SPATIAL_COORDINATEKIND_CARTESIAN_X:
            addVariableToPlaneAndCheck(xzplane, var, difftype, "XZ plane", xaxis, zaxis, diff, m);
            break;
          case SPATIAL_COORDINATEKIND_CARTESIAN_Y:
            addVariableToPlaneAndCheck(yzplane, var, difftype, "YZ plane", yaxis, zaxis, diff, m);
            break;
          }
          break;

        }
      }
    }
  }
}

bool
SpatialUniqueDiffusionCoefficientsCheck::addVariableToAxisAndCheck(map<string, string>& axismap, const string& var, const string& difftype, const string& axisOrPlane, map<string, string>& planemap1, map<string, string>& planemap2, const DiffusionCoefficient* diff, const Model& m)
{
  if (axismap.find(var) != axismap.end()) {
    msg = "A ";
    msg += difftype;
    msg += " defines diffusion for the variable '" + diff->getVariable() + "' in the " + axisOrPlane + ", but diffusion here for this variable was already covered by a different " + axismap[var] + " with the same variable.";
    logFailure(m);
    return true;
  }
  if (planemap1.find(var) != planemap1.end()) {
    msg = "A ";
    msg += difftype;
    msg += " defines diffusion for the variable '" + diff->getVariable() + "' in the " + axisOrPlane + ", but diffusion here for this variable was already covered by a different " + planemap1[var] + " with the same variable.";
    logFailure(m);
    return true;
  }
  if (planemap2.find(var) != planemap2.end()) {
    msg = "A ";
    msg += difftype;
    msg += " defines diffusion for the variable '" + diff->getVariable() + "' in the " + axisOrPlane + ", but diffusion here for this variable was already covered by a different " + planemap2[var] + " with the same variable.";
    logFailure(m);
    return true;
  }
  axismap[var] = difftype;
  return false;
}


bool
SpatialUniqueDiffusionCoefficientsCheck::addVariableToPlaneAndCheck(map<string, string>& planemap, const string& var, const string& difftype, const string& axisOrPlane, map<string, string>& axismap1, map<string, string>& axismap2, const DiffusionCoefficient* diff, const Model& m)
{
  if (planemap.find(var) != planemap.end()) {
    msg = "A ";
    msg += difftype;
    msg += " defines diffusion for the variable '" + diff->getVariable() + "' in the " + axisOrPlane + ", but diffusion here for this variable was already covered by a different " + planemap[var] + " with the same variable.";
    logFailure(m);
    return true;
  }
  if (axismap1.find(var) != axismap1.end()) {
    msg = "A ";
    msg += difftype;
    msg += " defines diffusion for the variable '" + diff->getVariable() + "' in the " + axisOrPlane + ", but diffusion here for this variable was already covered by a different " + axismap1[var] + " with the same variable.";
    logFailure(m);
    return true;
  }
  if (axismap2.find(var) != axismap2.end()) {
    msg = "A ";
    msg += difftype;
    msg += " defines diffusion for the variable '" + diff->getVariable() + "' in the " + axisOrPlane + ", but diffusion here for this variable was already covered by a different " + axismap2[var] + " with the same variable.";
    logFailure(m);
    return true;
  }
  planemap[var] = difftype;
  return false;
}


#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
