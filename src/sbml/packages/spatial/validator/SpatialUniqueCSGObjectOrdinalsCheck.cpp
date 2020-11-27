/**
* @cond doxygenLibsbmlInternal
*
* @file    SpatialUniqueCSGObjectOrdinalsCheck.cpp
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

#include "SpatialUniqueCSGObjectOrdinalsCheck.h"
#include <sbml/packages/spatial/sbml/CSGeometry.h>

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
SpatialUniqueCSGObjectOrdinalsCheck::SpatialUniqueCSGObjectOrdinalsCheck(unsigned int id, SpatialValidator & v):
  TConstraint<CSGeometry>(id, v)
{
}

/*
* Destroys this Constraint.
*/
SpatialUniqueCSGObjectOrdinalsCheck::~SpatialUniqueCSGObjectOrdinalsCheck ()
{
}


/*
* Checks that all ids on the following Model objects are unique:
* event assignments and assignment rules.
*/
void
SpatialUniqueCSGObjectOrdinalsCheck::check_ (const Model& m, const CSGeometry& object)
{
  set<int> ordinals;
  for (unsigned long c = 0; c < object.getNumCSGObjects(); c++)
  {
    const CSGObject* csgo = object.getCSGObject(c);
    if (!csgo->isSetOrdinal()) {
      continue;
    }
    int ordinal = csgo->getOrdinal();
    if (ordinals.find(ordinal) != ordinals.end())
    {
      stringstream ss_msg;
      ss_msg << "A <csgObject>";
      if (csgo->isSetId())
      {
        ss_msg << " with the id '" << csgo->getId() << "'";
      }
      ss_msg << " has an 'ordinal' value of '" << ordinal << "', which was already used by a different <csgObject>.";
      msg = ss_msg.str();
      logFailure(m);
    }
    ordinals.insert(ordinal);
  }
}


#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
