/**
* @cond doxygenLibsbmlInternal
*
* @file    SpatialUniqueBoundaryConditionsCheck.cpp
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

#include "SpatialUniqueBoundaryConditionsCheck.h"
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
SpatialUniqueBoundaryConditionsCheck::SpatialUniqueBoundaryConditionsCheck(unsigned int id, SpatialValidator & v):
  TConstraint<Model>(id, v)
{
}

/*
* Destroys this Constraint.
*/
SpatialUniqueBoundaryConditionsCheck::~SpatialUniqueBoundaryConditionsCheck ()
{
}


/*
* Checks that all ids on the following Model objects are unique:
* event assignments and assignment rules.
*/
void
SpatialUniqueBoundaryConditionsCheck::check_(const Model& m, const Model&)
{
  if (m.getLevel() < 3) {
    return;
  }
  set<pair<string, string> > dirichlet_targets, neumann_targets;
  for (unsigned long p = 0; p < m.getNumParameters(); p++) {
    const Parameter* param = m.getParameter(p);
    const SpatialParameterPlugin* spp = static_cast<const SpatialParameterPlugin*>(param->getPlugin("spatial"));
    if (spp == NULL) {
      continue;
    }
    if (spp->isSetBoundaryCondition()) {
      const BoundaryCondition* bc = spp->getBoundaryCondition();
      if (!bc->isSetVariable()) {
        continue;
      }
      string var = bc->getVariable();
      msg = "A <boundaryCondition>";
      if (bc->isSetId()) {
        msg += " with an id of '" + bc->getId() + "'";
      }
      msg += " has a variable of '";
      msg += var + "'";
      string target = "";
      if (bc->isSetCoordinateBoundary()) {
        target = bc->getCoordinateBoundary();
        msg += " and a coordinateBoundary of '";
        msg += target + "'";
      }
      else if (bc->isSetBoundaryDomainType()) {
        target = bc->getBoundaryDomainType();
        msg += " and a boundaryDomain of '";
        msg += target + "'";
      }
      if (target == "") {
        continue;
      }
      pair<string, string> bc_pair = make_pair(var, target);
      msg += ", with a type of '" + bc->getTypeAsString() + "'";
      BoundaryKind_t bk = bc->getType();
      if (dirichlet_targets.find(bc_pair) != dirichlet_targets.end()) {
        msg += ", but another <boundaryCondition> of type 'Dirichlet' already exists for that species boundary.";
        logFailure(m);
        continue;
      }
      if (neumann_targets.find(bc_pair) != neumann_targets.end()) {
        msg += ", but another <boundaryCondition> of type 'Neumann' already exists for that species boundary.";
        logFailure(m);
        continue;
      }


      switch (bk) {
      case SPATIAL_BOUNDARYKIND_DIRICHLET:
        dirichlet_targets.insert(bc_pair);
        break;
      case SPATIAL_BOUNDARYKIND_NEUMANN:
        neumann_targets.insert(bc_pair);
        break;
      }
    }
  }
}


#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
