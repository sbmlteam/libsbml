/**
* @cond doxygenLibsbmlInternal
*
* @file    SpatialCompartmentMappingUnitSizesCheck.cpp
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

#include "SpatialCompartmentMappingUnitSizesCheck.h"
#include <sbml/packages/spatial/extension/SpatialCompartmentPlugin.h>

#include <set>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus


/*
* Creates a new Constraint with the given constraint id.
*/
SpatialCompartmentMappingUnitSizesCheck::SpatialCompartmentMappingUnitSizesCheck (unsigned int id, SpatialValidator& v) :
  TConstraint<Model>(id, v)
{
}


/*
* Destroys this Constraint.
*/
SpatialCompartmentMappingUnitSizesCheck::~SpatialCompartmentMappingUnitSizesCheck ()
{
}


/*
* Checks that all ids on the following Model objects are unique:
* event assignments and assignment rules.
*/
void
SpatialCompartmentMappingUnitSizesCheck::check_ (const Model& m, const Model&)
{
  if (m.getLevel() < 3) {
    return;
  }
  set<string> domainTypes;
  set<const CompartmentMapping*> compartmentMappings;
  for (unsigned long c = 0; c < m.getNumCompartments(); c++) {
    const Compartment* comp = m.getCompartment(c);
    const SpatialCompartmentPlugin* scp = static_cast<const SpatialCompartmentPlugin*>(comp->getPlugin("spatial"));
    if (scp == NULL) {
      continue;
    }
    if (scp->isSetCompartmentMapping()) {
      const CompartmentMapping* cm = scp->getCompartmentMapping();
      if (cm->isSetDomainType() && cm->isSetUnitSize()) {
        domainTypes.insert(cm->getDomainType());
        compartmentMappings.insert(cm);
      }
    }
  }

  double epsilon = 0.001;
  for (set<string>::iterator dti = domainTypes.begin(); dti != domainTypes.end(); dti++) {
    string domainType = *dti;
    double sum = 0;
    for (set<const CompartmentMapping*>::iterator cmi = compartmentMappings.begin(); cmi != compartmentMappings.end(); cmi++) {
      if ((*cmi)->getDomainType() == domainType) {
        sum += (*cmi)->getUnitSize();
      }
    }
    if (sum > 1 + epsilon || sum < 1 - epsilon) {
      stringstream ss;
      ss << "The unitSize values of the <compartmentMapping> elements that have '" << domainType;
      ss << "' as their domainType add up to " << sum << ".";
      msg = ss.str();

      logFailure(m);
    }
  }
}

#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
