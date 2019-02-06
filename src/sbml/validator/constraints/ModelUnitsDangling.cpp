/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    ModelUnitsDangling.cpp
 * @brief   Base class for Id constraints
 * @author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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

#include "ModelUnitsDangling.h"

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new ModelUnitsDangling with the given constraint id.
 */
ModelUnitsDangling::ModelUnitsDangling (unsigned int id, Validator& v) : TConstraint<Model>(id, v)
{
}


/*
 * Destroys this Constraint.
 */
ModelUnitsDangling::~ModelUnitsDangling ()
{
}


/*
 * Checks that all ids for some given subset of the Model adhere to this
 * Constraint.  Override the doCheck() method to define your own subset.
 */
void
ModelUnitsDangling::check_ (const Model& m, const Model&)
{
  doCheck(m);
}


/*
 * Logs a message that the given @p id (and its corresponding object) have
 * failed to satisfy this constraint.
 */
void
ModelUnitsDangling::logConflict (const std::string& type, const std::string& unit,
                                 const SBase& object)
{
  msg = "The " ;
  msg += type;
  msg += "Units '";
  msg += unit;
  msg += "' do not refer to a valid unit kind/built-in unit ";
  msg += "or the identifier of an existing <unitDefinition>. ";

  logFailure(object, msg);
}

void
ModelUnitsDangling::doCheck (const Model& m)
{
  if (m.isSetExtentUnits())
  {
    if (!checkUnit(m.getExtentUnits(), m))
    {
      logConflict("extent", m.getExtentUnits(), m);
    }
  }
  if (m.isSetTimeUnits())
  {
    if (!checkUnit(m.getTimeUnits(), m))
    {
      logConflict("time", m.getTimeUnits(), m);
    }
  }
  if (m.isSetLengthUnits())
  {
    if (!checkUnit(m.getLengthUnits(), m))
    {
      logConflict("length", m.getLengthUnits(), m);
    }
  }
  if (m.isSetAreaUnits())
  {
    if (!checkUnit(m.getAreaUnits(), m))
    {
      logConflict("area", m.getAreaUnits(), m);
    }
  }
  if (m.isSetVolumeUnits())
  {
    if (!checkUnit(m.getVolumeUnits(), m))
    {
      logConflict("volume", m.getVolumeUnits(), m);
    }
  }
  if (m.isSetSubstanceUnits())
  {
    if (!checkUnit(m.getSubstanceUnits(), m))
    {
      logConflict("substance", m.getSubstanceUnits(), m);
    }
  }

}


bool 
ModelUnitsDangling::checkUnit(const std::string& unit, const Model& m)
{
  bool valid = true;

  const UnitDefinition *ud = m.getUnitDefinition(unit);
  if (ud == NULL && !Unit::isUnitKind(unit, m.getLevel(), m.getVersion()) 
    && !Unit::isBuiltIn(unit, m.getLevel()))
  {
    valid = false;
  }

  return valid;
}

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
