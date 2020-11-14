/**
* @cond doxygenLibsbmlInternal
*
* @file    SpatialSpatialSymbolReferenceUniqueRefCheck.cpp
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

#include "SpatialSpatialSymbolReferenceUniqueRefCheck.h"
#include <sbml/packages/spatial/extension/SpatialParameterPlugin.h>

#include <set>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus


/*
* Creates a new Constraint with the given constraint id.
*/
SpatialSpatialSymbolReferenceUniqueRefCheck::SpatialSpatialSymbolReferenceUniqueRefCheck(unsigned int id, SpatialValidator & v):
  TConstraint<Model>(id, v)
{
}

/*
* Destroys this Constraint.
*/
SpatialSpatialSymbolReferenceUniqueRefCheck::~SpatialSpatialSymbolReferenceUniqueRefCheck ()
{
}


/*
* Checks that all ids on the following Model objects are unique:
* event assignments and assignment rules.
*/
void
SpatialSpatialSymbolReferenceUniqueRefCheck::check_ (const Model& m, const Model&)
{
  if (m.getLevel() < 3) {
    return;
  }
  set<string> referencedIDs;
  set<string> duplicates;
  for (unsigned long p = 0; p < m.getNumParameters(); p++) {
    const Parameter* param = m.getParameter(p);
    const SpatialParameterPlugin* spp = static_cast<const SpatialParameterPlugin*>(param->getPlugin("spatial"));
    if (spp == NULL) {
      continue;
    }
    if (spp->isSetSpatialSymbolReference()) {
      const SpatialSymbolReference* ssr = spp->getSpatialSymbolReference();
      if (ssr->isSetSpatialRef()) {
        string ref = ssr->getSpatialRef();
        if (referencedIDs.find(ref) != referencedIDs.end()) {
          duplicates.insert(ref);
        }
        referencedIDs.insert(ref);
      }
    }
  }
  for (set<string>::iterator dup = duplicates.begin(); dup != duplicates.end(); dup++) {
    msg = "Multiple <spatialSymbolReference> objects were found with the same spatialRef value, '";
    msg += *dup + "'.";
    logFailure(m);
  }
}

#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
