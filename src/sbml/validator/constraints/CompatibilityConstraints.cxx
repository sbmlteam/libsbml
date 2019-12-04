/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    CompatibilityConstraints.cxx
 * @brief   Compability constraint code
 * @author  Sarah Keating
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
#include <sbml/validator/VConstraint.h>
#include <sbml/math/ASTNode.h>
#include "ReportEmptyListOf.h"
#include "IdNameNewOnSBase.h"
#include "NumericArgsMathCheck.h"
#include "LogicalArgsMathCheck.h"
#include "PieceBooleanMathCheck.h"
#endif

#include <sbml/SBMLTypes.h>
#include <sbml/validator/ConstraintMacros.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

START_CONSTRAINT (92011, Event, e)
{
  pre (e.getLevel() > 2);
  inv((e.isSetPriority() && e.getPriority()->isSetMath()) == false );
}
END_CONSTRAINT



START_CONSTRAINT (98001, Unit, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.isSetExponent() == true);

  inv( ceil(obj.getExponentAsDouble()) == floor(obj.getExponentAsDouble() ));
}
END_CONSTRAINT


START_CONSTRAINT (98002, FunctionDefinition, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);
  pre (obj.isSetMath() == true);

  msg = "The functionDefinition with id '" + obj.getId() + "' uses L3V2 math.";

  inv( obj.getMath()->usesL3V2MathConstructs() == false );
}
END_CONSTRAINT


START_CONSTRAINT (98002, InitialAssignment, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);
  pre (obj.isSetMath() == true);
  pre (obj.getMath()->usesL3V2MathConstructs() == true);

  msg = "The initialAssignment with symbol '" + obj.getSymbol() + "' uses rateOf math.";

  inv( obj.getMath()->usesRateOf() == false );
}
END_CONSTRAINT


START_CONSTRAINT (98002, AssignmentRule, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);
  pre (obj.isSetMath() == true);

  msg = "The assignmentRule with variable '" + obj.getVariable() + "' uses L3V2 math.";

  inv( obj.getMath()->usesL3V2MathConstructs() == false );
}
END_CONSTRAINT


START_CONSTRAINT (98002, RateRule, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);
  pre (obj.isSetMath() == true);

  msg = "The rateRule with variable '" + obj.getVariable() + "' uses L3V2 math.";

  inv( obj.getMath()->usesL3V2MathConstructs() == false );
}
END_CONSTRAINT


START_CONSTRAINT (98002, AlgebraicRule, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);
  pre (obj.isSetMath() == true);

  msg = "The algebraicRule uses L3V2 math.";

  inv( obj.getMath()->usesL3V2MathConstructs() == false );
}
END_CONSTRAINT


START_CONSTRAINT (98002, Constraint, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);
  pre (obj.isSetMath() == true);

  msg = "The constraint with id '" + obj.getId() + "' uses L3V2 math.";

  inv( obj.getMath()->usesL3V2MathConstructs() == false );
}
END_CONSTRAINT


START_CONSTRAINT (98002, KineticLaw, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);
  pre (obj.isSetMath() == true);

  msg = "The kineticLaw with id '" + obj.getId() + "' uses L3V2 math.";

  inv( obj.getMath()->usesL3V2MathConstructs() == false );
}
END_CONSTRAINT


START_CONSTRAINT (98002, Trigger, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);
  pre (obj.isSetMath() == true);

  msg = "The trigger with id '" + obj.getId() + "' uses L3V2 math.";

  inv( obj.getMath()->usesL3V2MathConstructs() == false );
}
END_CONSTRAINT


START_CONSTRAINT (98002, Priority, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);
  pre (obj.isSetMath() == true);

  msg = "The priority with id '" + obj.getId() + "' uses L3V2 math.";

  inv( obj.getMath()->usesL3V2MathConstructs() == false );
}
END_CONSTRAINT


START_CONSTRAINT (98002, Delay, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);
  pre (obj.isSetMath() == true);

  msg = "The delay with id '" + obj.getId() + "' uses L3V2 math.";

  inv( obj.getMath()->usesL3V2MathConstructs() == false );
}
END_CONSTRAINT


START_CONSTRAINT (98002, EventAssignment, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);
  pre (obj.isSetMath() == true);

  msg = "The eventAssignment with variable '" + obj.getVariable() + "' uses L3V2 math.";

  inv( obj.getMath()->usesL3V2MathConstructs() == false );
}
END_CONSTRAINT

EXTERN_CONSTRAINT( 98003, ReportEmptyListOf )

START_CONSTRAINT (98004, FunctionDefinition, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The functionDefinition with id '" + obj.getId() + "' is missing " +
    "the <math> element.";

  inv( obj.isSetMath() == true );
}
END_CONSTRAINT


START_CONSTRAINT (98004, InitialAssignment, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The inialAssignment with symbol '" + obj.getSymbol() + "' is missing " +
    "the <math> element.";

  inv( obj.isSetMath() == true );
}
END_CONSTRAINT


START_CONSTRAINT (98004, AssignmentRule, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The assignmentRule with variable '" + obj.getVariable() + "' is missing " +
    "the <math> element.";

  inv( obj.isSetMath() == true );
}
END_CONSTRAINT


START_CONSTRAINT (98004, RateRule, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The rateRule with variable '" + obj.getVariable() + "' is missing " +
    "the <math> element.";

  inv( obj.isSetMath() == true );
}
END_CONSTRAINT


START_CONSTRAINT (98004, AlgebraicRule, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The algebraicRule is missing math.";

  inv( obj.isSetMath() == true );
}
END_CONSTRAINT


START_CONSTRAINT (98004, Constraint, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The constraint with id '" + obj.getId() + "' is missing " +
    "the <math> element.";

  inv( obj.isSetMath() == true );
}
END_CONSTRAINT


START_CONSTRAINT (98004, KineticLaw, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The kineticLaw with id '" + obj.getId() + "' is missing " +
    "the <math> element.";

  inv( obj.isSetMath() == true );
}
END_CONSTRAINT


START_CONSTRAINT (98004, Trigger, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The trigger with id '" + obj.getId() + "' is missing " +
    "the <math> element.";

  inv( obj.isSetMath() == true );
}
END_CONSTRAINT


START_CONSTRAINT (98004, Priority, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The priority with id '" + obj.getId() + "' is missing " +
    "the <math> element.";

  inv( obj.isSetMath() == true );
}
END_CONSTRAINT


START_CONSTRAINT (98004, Delay, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The delay with id '" + obj.getId() + "' is missing " +
    "the <math> element.";

  inv( obj.isSetMath() == true );
}
END_CONSTRAINT


START_CONSTRAINT (98004, EventAssignment, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The eventAssignment with variable '" + obj.getVariable() + "' is missing " +
    "the <math> element.";

  inv( obj.isSetMath() == true );
}
END_CONSTRAINT


START_CONSTRAINT (98005, Event, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The event with id '" + obj.getId() + "' is missing " +
    "the <trigger> element.";

  inv( obj.isSetTrigger() == true );
}
END_CONSTRAINT

EXTERN_CONSTRAINT( 98006, NumericArgsMathCheck   )

EXTERN_CONSTRAINT(98006, LogicalArgsMathCheck)

EXTERN_CONSTRAINT(98006, PieceBooleanMathCheck)

EXTERN_CONSTRAINT( 98007, IdNameNewOnSBase )

START_CONSTRAINT (98008, Reaction, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);

  msg = "The reaction with id '" + obj.getId() + "' is missing " +
    "both the <listOfReactants> and the <listOfProducts> elements.";

  unsigned int num = obj.getNumProducts() + obj.getNumReactants();
  inv ( num != 0 );
}
END_CONSTRAINT

START_CONSTRAINT (98009, InitialAssignment, obj)
{
  pre (obj.getLevel() == 3);
  pre (obj.getVersion() > 1);
  pre (obj.isSetMath() == true);
  pre (obj.getMath()->usesRateOf() == false);

  msg = "The initialAssignment with symbol '" + obj.getSymbol() + "' uses L3V2 math.";

  inv( obj.getMath()->usesL3V2MathConstructs() == false );
}
END_CONSTRAINT

START_CONSTRAINT (98010, Reaction, obj)
{
  pre (obj.isSetFast() == true);

  msg = "The reaction with id '" + obj.getId() + "' is a fast reaction.";

  inv( obj.getFast() == false );
}
END_CONSTRAINT




  /** @endcond */
