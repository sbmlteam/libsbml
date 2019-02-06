/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    SBOConsistencyConstraints.cpp
 * @brief   SBOConsistency check constraints.  See SBML Wiki
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

#ifndef AddingConstraintsToValidator

//#include <string>

#include <sbml/SBMLTypeCodes.h>
#include <sbml/SBO.h>
#include <sbml/validator/VConstraint.h>
#include <sbml/units/UnitFormulaFormatter.h>
#include <sbml/units/FormulaUnitsData.h>
#include <sbml/AssignmentRule.h>
#include <sbml/RateRule.h>
#include <sbml/AlgebraicRule.h>

#include <sbml/util/List.h>

#include "CompartmentOutsideCycles.h"
#include "FunctionDefinitionVars.h"

#include "UniqueIdsForUnitDefinitions.h"
#include "UniqueIdsInKineticLaw.h"
#include "UniqueIdsInModel.h"
#include "UniqueVarsInEventAssignments.h"
#include "UniqueVarsInRules.h"
#include "UniqueVarsInEventsAndRules.h"
#include "UniqueMetaId.h"

#include "FunctionReferredToExists.h"
#include "SpeciesReactionOrRule.h"
#include "UniqueSpeciesTypesInCompartment.h"
#include "UniqueSymbolsInInitialAssignments.h"
#include "UniqueVarsInInitialAssignmentsAndRules.h"
#include "StoichiometryMathVars.h"
#include "KineticLawVars.h"
#include "AssignmentCycles.h"

//#include "FormulaUnitsCheck.h"

//#include "PowerUnitsCheck.h"
//#include "ExponentUnitsCheck.h"
#include "ArgumentsUnitsCheck.h"

#include "LogicalArgsMathCheck.h"
#include "NumericArgsMathCheck.h"
#include "PieceBooleanMathCheck.h"
#include "PiecewiseValueMathCheck.h"
#include "EqualityArgsMathCheck.h"
#include "FunctionApplyMathCheck.h"
#include "CiElementMathCheck.h"
#include "LambdaMathCheck.h"
#include "NumericReturnMathCheck.h"
#include "LocalParameterMathCheck.h"
#include "NumberArgsMathCheck.h"

#include "OverDeterminedCheck.h"

#endif


#include <sbml/validator/ConstraintMacros.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

// General SBO validation

START_CONSTRAINT(99701, Model, m1)
{
  pre(m1.getLevel() > 1);
  if (m1.getLevel() == 2) 
  {
    pre( m1.getVersion() > 1);
  }
  pre(m1.isSetSBOTerm());
  msg = "Unknown SBO term '" + m1.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)m1.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)m1.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)m1.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)m1.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)m1.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)m1.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)m1.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)m1.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, FunctionDefinition, fd)
{
  pre(fd.getLevel() > 1);
  if (fd.getLevel() == 2) 
  {
    pre( fd.getVersion() > 1);
  }
  pre(fd.isSetSBOTerm());
  msg = "Unknown SBO term '" + fd.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)fd.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)fd.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)fd.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)fd.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)fd.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)fd.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)fd.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)fd.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99701, Parameter, p)
{
  pre(p.getLevel() > 1);
  if (p.getLevel() == 2) 
  {
    pre( p.getVersion() > 1);
  }
  pre(p.isSetSBOTerm());
  msg = "Unknown SBO term '" + p.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)p.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)p.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)p.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)p.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)p.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)p.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)p.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)p.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, InitialAssignment, ia)
{
  pre(ia.getLevel() > 1);
  if (ia.getLevel() == 2) 
  {
    pre( ia.getVersion() > 1);
  }
  pre(ia.isSetSBOTerm());
  msg = "Unknown SBO term '" + ia.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)ia.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)ia.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)ia.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)ia.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)ia.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)ia.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)ia.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)ia.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, AssignmentRule, ar)
{
  pre(ar.getLevel() > 1);
  if (ar.getLevel() == 2)
  {
    pre( ar.getVersion() > 1);
  }
  pre(ar.isSetSBOTerm());
  msg = "Unknown SBO term '" + ar.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)ar.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, RateRule, rr)
{
  pre(rr.getLevel() > 1);
  if (rr.getLevel() == 2)
  {
    pre( rr.getVersion() > 1);
  }
  pre(rr.isSetSBOTerm());
  msg = "Unknown SBO term '" + rr.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)rr.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)rr.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)rr.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)rr.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)rr.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)rr.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)rr.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)rr.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, AlgebraicRule, ar)
{
  pre(ar.getLevel() > 1);
  if (ar.getLevel() == 2)
  {
    pre( ar.getVersion() > 1);
  }
  pre(ar.isSetSBOTerm());
  msg = "Unknown SBO term '" + ar.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)ar.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)ar.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, Constraint, c)
{
  pre(c.getLevel() > 1);
  if (c.getLevel() == 2)
  {
    pre( c.getVersion() > 1);
  }
  pre(c.isSetSBOTerm());
  msg = "Unknown SBO term '" + c.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)c.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, Reaction, r)
{
  pre(r.getLevel() > 1);
  if (r.getLevel() == 2)
  {
    pre( r.getVersion() > 1);
  }
  pre(r.isSetSBOTerm());
  msg = "Unknown SBO term '" + r.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)r.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)r.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)r.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)r.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)r.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)r.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)r.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)r.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, SpeciesReference, sr)
{
  pre(sr.getLevel() > 1);
  if (sr.getLevel() == 2)
  {
    pre( sr.getVersion() > 1);
  }
  pre(sr.isSetSBOTerm());
  msg = "Unknown SBO term '" + sr.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)sr.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)sr.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)sr.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)sr.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)sr.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)sr.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)sr.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)sr.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, KineticLaw, kl)
{
  pre(kl.getLevel() > 1);
  if (kl.getLevel() == 2)
  {
    pre( kl.getVersion() > 1);
  }
  pre(kl.isSetSBOTerm());
  msg = "Unknown SBO term '" + kl.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)kl.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)kl.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)kl.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)kl.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)kl.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)kl.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)kl.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)kl.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, Event, e)
{
  pre(e.getLevel() > 1);
  if (e.getLevel() == 2)
  {
    pre( e.getVersion() > 1);
  }
  pre(e.isSetSBOTerm());
  msg = "Unknown SBO term '" + e.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)e.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)e.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)e.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)e.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)e.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)e.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)e.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)e.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, EventAssignment, ea)
{
  pre(ea.getLevel() > 1);
  if (ea.getLevel() == 2)
  {
    pre( ea.getVersion() > 1);
  }
  pre(ea.isSetSBOTerm());
  msg = "Unknown SBO term '" + ea.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)ea.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)ea.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)ea.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)ea.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)ea.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)ea.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)ea.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)ea.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, Compartment, c)
{
  pre(c.getLevel() > 1);
  if (c.getLevel() == 2)
  {
    pre( c.getVersion() > 2);
  }
  pre(c.isSetSBOTerm());
  msg = "Unknown SBO term '" + c.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)c.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)c.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, Species, s)
{
  pre(s.getLevel() > 1);
  if (s.getLevel() == 2)
  {
    pre( s.getVersion() > 2);
  }
  pre(s.isSetSBOTerm());
  msg = "Unknown SBO term '" + s.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)s.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)s.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)s.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)s.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)s.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)s.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)s.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)s.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, CompartmentType, ct)
{
  pre(ct.getLevel() > 1);
  if (ct.getLevel() == 2)
  {
    pre( ct.getVersion() > 2);
  }
  pre(ct.isSetSBOTerm());
  msg = "Unknown SBO term '" + ct.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)ct.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)ct.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)ct.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)ct.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)ct.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)ct.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)ct.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)ct.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, SpeciesType, st)
{
  pre(st.getLevel() > 1);
  if (st.getLevel() == 2)
  {
    pre( st.getVersion() > 2);
  }
  pre(st.isSetSBOTerm());
  msg = "Unknown SBO term '" + st.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)st.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)st.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)st.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)st.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)st.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)st.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)st.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)st.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, Trigger, t)
{
  pre(t.getLevel() > 1);
  if (t.getLevel() == 2)
  {
    pre( t.getVersion() > 2);
  }
  pre(t.isSetSBOTerm());
  msg = "Unknown SBO term '" + t.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)t.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)t.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)t.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)t.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)t.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)t.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)t.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)t.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99701, Delay, d)
{
  pre(d.getLevel() > 1);
  if (d.getLevel() == 2)
  {
    pre( d.getVersion() > 2);
  }
  pre(d.isSetSBOTerm());
  msg = "Unknown SBO term '" + d.getSBOTermID() + "'.";

  inv_or(SBO::isModellingFramework           ((unsigned int)d.getSBOTerm()));
  inv_or(SBO::isMathematicalExpression       ((unsigned int)d.getSBOTerm()));
  inv_or(SBO::isParticipantRole              ((unsigned int)d.getSBOTerm()));
  inv_or(SBO::isMetadataRepresentation       ((unsigned int)d.getSBOTerm()));
  inv_or(SBO::isSystemsDescriptionParameter  ((unsigned int)d.getSBOTerm()));
  inv_or(SBO::isOccurringEntityRepresentation ((unsigned int)d.getSBOTerm()));
  inv_or(SBO::isPhysicalEntityRepresentation ((unsigned int)d.getSBOTerm()));
  inv_or(SBO::isObselete                     ((unsigned int)d.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(10701, Model, m1)
{
  pre(m1.getLevel() > 1);
  if (m1.getLevel() == 2)
  {
    pre( m1.getVersion() > 1);
  }
  pre(m1.isSetSBOTerm());
  msg = "SBO term '" + m1.getSBOTermID() + "' on the <model> is not in the appropriate branch.";

  //L2V1-3 must be modelling framework
  // L2v5 - occuring entity representation
  // L3v1 - occuring entitiy representation
  // l3V1R2 + L3V2 either is allowed

  if (m1.getLevel() == 2 && m1.getVersion() < 4)
  {
    inv(SBO::isModellingFramework((unsigned int)m1.getSBOTerm()));
  }
  else if (m1.getLevel() == 2 && m1.getVersion() == 5)
  {
    inv(SBO::isOccurringEntityRepresentation((unsigned int)m1.getSBOTerm()));
  }
  else
  {
    inv_or(SBO::isModellingFramework((unsigned int)m1.getSBOTerm()));
    inv_or(SBO::isOccurringEntityRepresentation((unsigned int)m1.getSBOTerm()));
  }
}
END_CONSTRAINT


START_CONSTRAINT(10702, FunctionDefinition, fd)
{
  pre(fd.getLevel() > 1);
  if (fd.getLevel() == 2)
  {
    pre( fd.getVersion() > 1);
  }
  pre(fd.isSetSBOTerm());
  msg = "SBO term '" + fd.getSBOTermID() + "' on the <functionDefinition> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <functionDefinition> must be "
  //  "an SBO identifier (http://www.biomodels.net/SBO/) referring to a "
  //  "mathematical expression (i.e., terms derived from SBO:0000064, "
  //  "\"mathematical expression\"). "
  //  "(References: L2V2 Section 4.3.3; L2V3 Section 4.3.3.)";

  inv(SBO::isMathematicalExpression((unsigned int)fd.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10703, Parameter, p)
{
  pre(p.getLevel() > 1);
  if (p.getLevel() == 2)
  {
    pre( p.getVersion() > 1);
  }
  pre(p.isSetSBOTerm());
  pre(p.getTypeCode() == SBML_PARAMETER);
  msg = "SBO term '" + p.getSBOTermID() + "' on the <parameter> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <parameter> must be an "
  //  "SBO identifier (http://www.biomodels.net/SBO/) referring to a "
  //  "quantitative parameter defined in SBO (i.e., terms derived from "
  //  "SBO:0000002, \"quantitative parameter\"). "
  //  "(References: L2V2 Section 4.9.5; L2V3 Section 4.9.5.)";

  inv(SBO::isQuantitativeSystemsDescriptionParameter((unsigned int)p.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10704, InitialAssignment, ia)
{
  pre(ia.getLevel() > 1);
  if (ia.getLevel() == 2)
  {
    pre( ia.getVersion() > 1);
  }
  pre(ia.isSetSBOTerm());
  msg = "SBO term '" + ia.getSBOTermID() + "' on the <initialAssignment> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on an <initialAssignment> must "
  //  "be an SBO identifier (http://www.biomodels.net/SBO/) referring to a "
  //  "mathematical expression (i.e., terms derived from SBO:0000064, "
  //  "\"mathematical expression\"). "
  //  "(References: L2V2 Section 4.10.3; L2V3 Section 4.10.3.)";

  inv(SBO::isMathematicalExpression((unsigned int)ia.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10705, AssignmentRule, r)
{
  pre(r.getLevel() > 1);
  if (r.getLevel() == 2)
  {
    pre( r.getVersion() > 1);
  }
  pre(r.isSetSBOTerm());
  msg = "SBO term '" + r.getSBOTermID() + "' on the <assignmentRule> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a rule must be an SBO identifier "
  //  "(http://www.biomodels.net/SBO/) referring to a mathematical expression "
  //  "(i.e., terms derived from SBO:0000064, \"mathematical expression\"). "
  //  "Note: This applies to Algebraic Rules in addition to Rate and Assignment "
  //  "Rules. (References: L2V2 Section 4.11.1; L2V3 Section 4.11.1.)";

  inv(SBO::isMathematicalExpression((unsigned int)r.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10705, RateRule, r)
{
  pre(r.getLevel() > 1);
  if (r.getLevel() == 2)
  {
    pre( r.getVersion() > 1);
  }
  pre(r.isSetSBOTerm());
  msg = "SBO term '" + r.getSBOTermID() + "' on the <rateRule> is not in the appropriate branch.";

  //msg = 
    //"The value of the 'sboTerm' attribute on a rule must be an SBO identifier "
    //"(http://www.biomodels.net/SBO/) referring to a mathematical expression "
    //"(i.e., terms derived from SBO:0000064, \"mathematical expression\"). Note: "
    //"This applies to Algebraic Rules in addition to Rate and Assignment Rules. "
    //"(References: L2V2 Section 4.11.1; L2V3 Section 4.11.1.)";

  inv(SBO::isMathematicalExpression((unsigned int)r.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10705, AlgebraicRule, r)
{
  pre(r.getLevel() > 1);
  if (r.getLevel() == 2)
  {
    pre( r.getVersion() > 1);
  }
  pre(r.isSetSBOTerm());
  msg = "SBO term '" + r.getSBOTermID() + "' on the <algebraicRule> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a rule must be an SBO identifier "
  //  "(http://www.biomodels.net/SBO/) referring to a mathematical expression "
  //  "(i.e., terms derived from SBO:0000064, \"mathematical expression\"). Note: "
  //  "This applies to Algebraic Rules in addition to Rate and Assignment Rules. "
  //  "(References: L2V2 Section 4.11.1; L2V3 Section 4.11.1.)";

  inv(SBO::isMathematicalExpression((unsigned int)r.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10706, Constraint, c)
{
  pre(c.getLevel() > 1);
  if (c.getLevel() == 2)
  {
    pre( c.getVersion() > 1);
  }
  pre(c.isSetSBOTerm());
  msg = "SBO term '" + c.getSBOTermID() + "' on the <constraint> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <constraint> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a mathematical "
  //  "expression (i.e., terms derived from SBO:0000064, \"mathematical "
  //  "expression\"). "
  //  "(References: L2V2 Section 4.12.3; L2V3 Section 4.12.3.)";

  inv(SBO::isMathematicalExpression((unsigned int)c.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10707, Reaction, r)
{
  pre(r.getLevel() > 1);
  if (r.getLevel() == 2)
  {
    pre( r.getVersion() > 1);
  }
  pre(r.isSetSBOTerm());
  msg = "SBO term '" + r.getSBOTermID() + "' on the <reaction> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <reaction> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to an event defined "
  //  "in SBO (i.e., terms derived from SBO:0000231, \"event\"). "
  //  "(References: L2V2 Section 4.13.1; L2V3 Section 4.13.1)";

  if (r.getLevel() == 2 && r.getVersion() < 4)
  {
    inv(SBO::isEvent((unsigned int)r.getSBOTerm()));
  }
  else
  {
    inv(SBO::isOccurringEntityRepresentation((unsigned int)r.getSBOTerm()));
  }
}
END_CONSTRAINT


START_CONSTRAINT(10708, SpeciesReference, sr)
{
  pre(sr.getLevel() > 1);
  if (sr.getLevel() == 2)
  {
    pre( sr.getVersion() > 1);
  }
  pre(sr.isSetSBOTerm());
  msg = "SBO term '" + sr.getSBOTermID() + "' on the <speciesReference> is not in the appropriate branch.";

   //msg = 
   //  "The value of the 'sboTerm' attribute on a <speciesReference> "
   //  "or <modifierSpeciesReference> must be an SBO "
   //  "identifier (http://www.biomodels.net/SBO/) referring to a participant "
   //  "role. The appropriate term depends on whether the object is a reactant, "
   //  "product or modifier. If a reactant, then it should be a term in the "
   //  "SBO:0000010, \"reactant\" hierarchy; if a product, then it should be a "
   //  "term in the SBO:0000011, \"product\" hierarchy; and if a modifier, then "
   //  "it should be a term in the SBO:0000019, \"modifier\" hierarchy. "
   //  "(References: L2V2 Section 4.13.2; L2V3 Sections 4.13.1 and 5.)";

  if (!sr.isModifier())
  {
    inv_or(SBO::isProduct((unsigned int)sr.getSBOTerm()));
    inv_or(SBO::isReactant((unsigned int)sr.getSBOTerm()));
  }
  else
  {
    inv(SBO::isModifier((unsigned int)sr.getSBOTerm()));
  }
}
END_CONSTRAINT


START_CONSTRAINT(10709, KineticLaw, kl)
{
  pre(kl.getLevel() > 1);
  if (kl.getLevel() == 2)
  {
    pre( kl.getVersion() > 1);
  }
  pre(kl.isSetSBOTerm());
  msg = "SBO term '" + kl.getSBOTermID() + "' on the <kineticLaw> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <kineticLaw> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring rate law defined "
  //  "in SBO (i.e., terms derived from SBO:0000001, \"rate law\"). "
  //  "(References: L2V2 Section 4.13.5.)";

  inv(SBO::isRateLaw((unsigned int)kl.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10710, Event, e)
{
  pre(e.getLevel() > 1);
  if (e.getLevel() == 2)
  {
    pre( e.getVersion() > 1);
  }
  pre(e.isSetSBOTerm());
  msg = "SBO term '" + e.getSBOTermID() + "' on the <event> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on an <event> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to an event "
  //  "defined in SBO (i.e., terms derived from SBO:0000231, \"event\"). "
  //  "(References: L2V2 Section 4.14.1; L2V3 Section 4.14.1.)";
 
  if (e.getLevel() == 2 && e.getVersion() < 4)
  {
    inv(SBO::isEvent((unsigned int)e.getSBOTerm()));
  }
  else
  {
    inv(SBO::isOccurringEntityRepresentation((unsigned int)e.getSBOTerm()));
  }
}
END_CONSTRAINT


START_CONSTRAINT(10711, EventAssignment, ea)
{
  pre(ea.getLevel() > 1);
  if (ea.getLevel() == 2)
  {
    pre( ea.getVersion() > 1);
  }
  pre(ea.isSetSBOTerm());
  msg = "SBO term '" + ea.getSBOTermID() + "' on the <eventAssignment> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on an <eventAssignment> must be an "
  //  "SBO identifier (http://www.biomodels.net/SBO/) referring to a "
  //  "mathematical expression (i.e., terms derived from SBO:0000064, "
  //  "\"mathematical expression\"). "
  //  "(References: L2V2 Section 4.14.2; L2V3 Section 4.14.1.)";

  inv(SBO::isMathematicalExpression((unsigned int)ea.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10712, Compartment, c)
{
  pre(c.getLevel() > 1);
  if (c.getLevel() == 2)
  {
    pre( c.getVersion() > 2);
  }
  pre(c.isSetSBOTerm());
  msg = "SBO term '" + c.getSBOTermID() + "' on the <compartment> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <compartment> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a participant "
  //  "physical type (i.e., terms derived from SBO:0000236, \"participant "
  //  "physical type\"). (References: L2V3 Section 5.2.2.)";

  if (c.getLevel() == 2 && c.getVersion() == 3)
  {
    inv(SBO::isPhysicalParticipant((unsigned int)c.getSBOTerm()));
  }
  else
  {
    inv(SBO::isMaterialEntity((unsigned int)c.getSBOTerm()));
  }
}
END_CONSTRAINT


START_CONSTRAINT(10713, Species, s)
{
  pre(s.getLevel() > 1);
  if (s.getLevel() == 2)
  {
    pre( s.getVersion() > 2);
  }
  pre(s.isSetSBOTerm());
  msg = "SBO term '" + s.getSBOTermID() + "' on the <species> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <species> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a participant "
  //  "physical type (i.e., terms derived from SBO:0000236, \"participant "
  //  "physical type\"). (References: L2V3 Section 5.2.2.)";

  if (s.getLevel() == 2 && s.getVersion() == 3)
  {
    inv(SBO::isPhysicalParticipant((unsigned int)s.getSBOTerm()));
  }
  else
  {
    inv(SBO::isMaterialEntity((unsigned int)s.getSBOTerm()));
  }
}
END_CONSTRAINT


START_CONSTRAINT(10714, CompartmentType, c)
{
  pre(c.getLevel() > 1);
  if (c.getLevel() == 2)
  {
    pre( c.getVersion() > 2);
  }
  pre(c.isSetSBOTerm());
  msg = "SBO term '" + c.getSBOTermID() + "' on the <compartmentType> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <compartmentType> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a participant "
  //  "physical type (i.e., terms derived from SBO:0000236, \"participant "
  //  "physical type\"). (References: L2V3 Section 5.2.2.)";

  if (c.getLevel() == 2 && c.getVersion() == 3)
  {
    inv(SBO::isPhysicalParticipant((unsigned int)c.getSBOTerm()));
  }
  else
  {
    inv(SBO::isMaterialEntity((unsigned int)c.getSBOTerm()));
  }
}
END_CONSTRAINT


START_CONSTRAINT(10715, SpeciesType, s)
{
  pre(s.getLevel() > 1);
  if (s.getLevel() == 2)
  {
    pre( s.getVersion() > 2);
  }
  pre(s.isSetSBOTerm());
  msg = "SBO term '" + s.getSBOTermID() + "' on the <speciesType> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <speciesType> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a participant "
  //  "physical type (i.e., terms derived from SBO:0000236, \"participant "
  //  "physical type\"). (References: L2V3 Section 5.2.2.)";

  if (s.getLevel() == 2 && s.getVersion() == 3)
  {
    inv(SBO::isPhysicalParticipant((unsigned int)s.getSBOTerm()));
  }
  else
  {
    inv(SBO::isMaterialEntity((unsigned int)s.getSBOTerm()));
  }
}
END_CONSTRAINT


START_CONSTRAINT(10716, Trigger, t)
{
  pre(t.getLevel() > 1);
  if (t.getLevel() == 2)
  {
    pre( t.getVersion() > 2);
  }
  pre(t.isSetSBOTerm());
  msg = "SBO term '" + t.getSBOTermID() + "' on the <trigger> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <trigger> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a mathematical "
  //  "expression (i.e., terms derived from SBO:0000064, \"mathematical "
  //  "expression\"). (References: L2V3 Section 5.2.2.)";

  inv(SBO::isMathematicalExpression((unsigned int)t.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10717, Delay, d)
{
  pre(d.getLevel() > 1);
  if (d.getLevel() == 2)
  {
    pre( d.getVersion() > 2);
  }
  pre(d.isSetSBOTerm());
  msg = "SBO term '" + d.getSBOTermID() + "' on the <delay> is not in the appropriate branch.";

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <delay> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a mathematical "
  //  "expression (i.e., terms derived from SBO:0000064, \"mathematical "
  //  "expression\"). (References: L2V3 Section 5.2.2.)";

  inv(SBO::isMathematicalExpression((unsigned int)d.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10718, LocalParameter, p)
{
  pre(p.getLevel() > 2);
  pre(p.isSetSBOTerm());
  pre(p.getTypeCode() == SBML_LOCAL_PARAMETER);

  inv(SBO::isQuantitativeSystemsDescriptionParameter(p.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(99702, Model, m1)
{
  pre(m1.getLevel() > 1);
  if (m1.getLevel() == 2) 
  {
    pre( m1.getVersion() > 1);
  }
  pre(m1.isSetSBOTerm());
  msg = "Obsolete SBO term '" + m1.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)m1.getSBOTerm()));
}
END_CONSTRAINT

START_CONSTRAINT(99702, FunctionDefinition, fd)
{
  pre(fd.getLevel() > 1);
  if (fd.getLevel() == 2) 
  {
    pre( fd.getVersion() > 1);
  }
  pre(fd.isSetSBOTerm());
  msg = "Obsolete SBO term '" + fd.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)fd.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, Parameter, p)
{
  pre(p.getLevel() > 1);
  if (p.getLevel() == 2) 
  {
    pre( p.getVersion() > 1);
  }
  pre(p.isSetSBOTerm());
  msg = "Obsolete SBO term '" + p.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)p.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, InitialAssignment, ia)
{
  pre(ia.getLevel() > 1);
  if (ia.getLevel() == 2) 
  {
    pre( ia.getVersion() > 1);
  }
  pre(ia.isSetSBOTerm());
  msg = "Obsolete SBO term '" + ia.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)ia.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, AssignmentRule, ar)
{
  pre(ar.getLevel() > 1);
  if (ar.getLevel() == 2)
  {
    pre( ar.getVersion() > 1);
  }
  pre(ar.isSetSBOTerm());
  msg = "Obsolete SBO term '" + ar.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)ar.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, RateRule, rr)
{
  pre(rr.getLevel() > 1);
  if (rr.getLevel() == 2)
  {
    pre( rr.getVersion() > 1);
  }
  pre(rr.isSetSBOTerm());
  msg = "Obsolete SBO term '" + rr.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)rr.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, AlgebraicRule, ar)
{
  pre(ar.getLevel() > 1);
  if (ar.getLevel() == 2)
  {
    pre( ar.getVersion() > 1);
  }
  pre(ar.isSetSBOTerm());
  msg = "Obsolete SBO term '" + ar.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)ar.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, Constraint, c)
{
  pre(c.getLevel() > 1);
  if (c.getLevel() == 2)
  {
    pre( c.getVersion() > 1);
  }
  pre(c.isSetSBOTerm());
  msg = "Obsolete SBO term '" + c.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)c.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, Reaction, r)
{
  pre(r.getLevel() > 1);
  if (r.getLevel() == 2)
  {
    pre( r.getVersion() > 1);
  }
  pre(r.isSetSBOTerm());
  msg = "Obsolete SBO term '" + r.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)r.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, SpeciesReference, sr)
{
  pre(sr.getLevel() > 1);
  if (sr.getLevel() == 2)
  {
    pre( sr.getVersion() > 1);
  }
  pre(sr.isSetSBOTerm());
  msg = "Obsolete SBO term '" + sr.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)sr.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, KineticLaw, kl)
{
  pre(kl.getLevel() > 1);
  if (kl.getLevel() == 2)
  {
    pre( kl.getVersion() > 1);
  }
  pre(kl.isSetSBOTerm());
  msg = "Obsolete SBO term '" + kl.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)kl.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, Event, e)
{
  pre(e.getLevel() > 1);
  if (e.getLevel() == 2)
  {
    pre( e.getVersion() > 1);
  }
  pre(e.isSetSBOTerm());
  msg = "Obsolete SBO term '" + e.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)e.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, EventAssignment, ea)
{
  pre(ea.getLevel() > 1);
  if (ea.getLevel() == 2)
  {
    pre( ea.getVersion() > 1);
  }
  pre(ea.isSetSBOTerm());
  msg = "Obsolete SBO term '" + ea.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)ea.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, Compartment, c)
{
  pre(c.getLevel() > 1);
  if (c.getLevel() == 2)
  {
    pre( c.getVersion() > 2);
  }
  pre(c.isSetSBOTerm());
  msg = "Obsolete SBO term '" + c.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)c.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, Species, s)
{
  pre(s.getLevel() > 1);
  if (s.getLevel() == 2)
  {
    pre( s.getVersion() > 2);
  }
  pre(s.isSetSBOTerm());
  msg = "Obsolete SBO term '" + s.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)s.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, CompartmentType, ct)
{
  pre(ct.getLevel() > 1);
  if (ct.getLevel() == 2)
  {
    pre( ct.getVersion() > 2);
  }
  pre(ct.isSetSBOTerm());
  msg = "Obsolete SBO term '" + ct.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)ct.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, SpeciesType, st)
{
  pre(st.getLevel() > 1);
  if (st.getLevel() == 2)
  {
    pre( st.getVersion() > 2);
  }
  pre(st.isSetSBOTerm());
  msg = "Obsolete SBO term '" + st.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)st.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, Trigger, t)
{
  pre(t.getLevel() > 1);
  if (t.getLevel() == 2)
  {
    pre( t.getVersion() > 2);
  }
  pre(t.isSetSBOTerm());
  msg = "Obsolete SBO term '" + t.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)t.getSBOTerm()));

}
END_CONSTRAINT

START_CONSTRAINT(99702, Delay, d)
{
  pre(d.getLevel() > 1);
  if (d.getLevel() == 2)
  {
    pre( d.getVersion() > 2);
  }
  pre(d.isSetSBOTerm());
  msg = "Obsolete SBO term '" + d.getSBOTermID() + "'.";

  inv(! SBO::isObselete               ((unsigned int)d.getSBOTerm()));

}
END_CONSTRAINT
/** @endcond */

