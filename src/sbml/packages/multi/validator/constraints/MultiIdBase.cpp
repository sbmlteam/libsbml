/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    MultiIdBase.cpp
 * @brief   Base class for Id constraints
 * @author  Fengkai Zhang
 * @author  Ben Bornstein
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
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include "MultiIdBase.h"
#include <sbml/packages/multi/validator/MultiValidator.h>
#include <sbml/ModifierSpeciesReference.h>
#include <sbml/Rule.h>
#include <sbml/AssignmentRule.h>
#include <sbml/RateRule.h>

/** @cond doxygenIgnored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus


/*
 * Creates a new MultiIdBase with the given constraint id.
 */
MultiIdBase::MultiIdBase (unsigned int id, MultiValidator& v) : TConstraint<Model>(id, v)
{
}


/*
 * Destroys this Constraint.
 */
MultiIdBase::~MultiIdBase ()
{
}


/*
 * @return the fieldname to use logging constraint violations.  If not
 * overridden, "id" is returned.
 */
const char*
MultiIdBase::getFieldname ()
{
  return "id";
}


/*
 * @return the preamble to use when logging constraint violations.  The
 * preamble will be prepended to each log message.  If not overriden,
 * returns an empty string.
 */
const char*
MultiIdBase::getPreamble ()
{
  return "";
}


/*
 * Checks that all ids for some given subset of the Model adhere to this
 * Constraint.  Override the doCheck() method to define your own subset.
 */
void
MultiIdBase::check_ (const Model& m, const Model& object)
{
  doCheck(m);
}


/*
 * These convenience methods simply wrap (delegate to) doCheckId(const
 * std::string&, const SBase& object).  This is necessary because getId()
 * is not (yet) defined on SBase for SBML objects.
 *
 * For Rules and EventAssignments, it calls getVariable() instead.
 */

/* id is no longer on SBase */
//void
//MultiIdBase::checkId (const SBase& x)
//{
////  if (x.isSetId()) doCheckId(x.getId(), x);
//}


void
MultiIdBase::checkId (const Model& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}


void
MultiIdBase::checkId (const MultiSpeciesType& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}


void
MultiIdBase::checkId (const FunctionDefinition& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}


void
MultiIdBase::checkId (const UnitDefinition& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}


void
MultiIdBase::checkId (const Compartment& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}


void
MultiIdBase::checkId (const Species& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}


void
MultiIdBase::checkId (const Parameter& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}


void
MultiIdBase::checkId (const Rule& x)
{
  switch (x.getTypeCode())
  {
    case SBML_ASSIGNMENT_RULE:
      doCheckId(static_cast<const AssignmentRule&>(x).getVariable(), x);
      break;

    case SBML_RATE_RULE:
      doCheckId(static_cast<const RateRule&>(x).getVariable(), x);
      break;

    default:
      break;
  }
}


void
MultiIdBase::checkId (const Reaction& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}


void
MultiIdBase::checkId (const Event& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}


void
MultiIdBase::checkId (const EventAssignment& x)
{
  if (x.isSetVariable()) doCheckId(x.getVariable(), x);
}

void
MultiIdBase::checkId (const SpeciesReference& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}

void
MultiIdBase::checkId (const ModifierSpeciesReference& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}

void
MultiIdBase::checkId (const InitialAssignment& x)
{
  if (x.isSetSymbol()) doCheckId(x.getSymbol(), x);
}

void
MultiIdBase::checkId (const SpeciesTypeInstance& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}

void
MultiIdBase::checkId (const SpeciesTypeComponentIndex& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}

void
MultiIdBase::checkId (const InSpeciesTypeBond& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}

void
MultiIdBase::checkId (const SpeciesFeatureType& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}

void
MultiIdBase::checkId (const PossibleSpeciesFeatureValue& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}

void
MultiIdBase::checkId (const SubListOfSpeciesFeatures& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}

void
MultiIdBase::checkId (const SpeciesFeature& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}


void
MultiIdBase::checkId (const CompartmentReference& x)
{
  if (x.isSetId()) doCheckId(x.getId(), x);
}


/*
 * @return the typename of the given SBase object.
 */
const char*
MultiIdBase::getTypename (const SBase& object)
{
  return SBMLTypeCode_toString( object.getTypeCode(), object.getPackageName().c_str() );
}


/*
 * Logs a message that the given @p id (and its corresponding object) have
 * failed to satisfy this constraint.
 */
void
MultiIdBase::logIdConflict (const std::string& id, const SBase& object)
{
  logFailure(object, getMessage(id, object));
}

#endif  /* __cplusplus */

LIBSBML_CPP_NAMESPACE_END

/** @endcond */

