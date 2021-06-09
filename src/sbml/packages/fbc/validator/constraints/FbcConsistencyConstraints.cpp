/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    FbcConsistencyConstraints.cpp
 * @brief   FbcConsistency check constraints.
 * @author  Sarah Keating
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
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#ifndef AddingConstraintsToValidator

#include <sbml/validator/VConstraint.h>
#include <sbml/packages/fbc/sbml/Objective.h>
#include <sbml/packages/fbc/sbml/FluxBound.h>
#include <sbml/packages/fbc/sbml/GeneProductRef.h>
#include <sbml/packages/fbc/sbml/FbcAnd.h>
#include <sbml/packages/fbc/sbml/FbcOr.h>
#include <sbml/packages/fbc/extension/FbcModelPlugin.h>
#include <sbml/packages/fbc/extension/FbcReactionPlugin.h>

#include <sbml/packages/fbc/validator/FbcSBMLError.h>

#include "FluxBoundsConsistent.h"
#include "UniqueGeneProductLabels.h"
#endif

#include <sbml/validator/ConstraintMacros.h>

/** @cond doxygenIgnored */
using namespace std;
// 20101 - caught at read
// 20102 - caught at read
// 20103 - caught at read

// 20201 - caught at read
// 20202 - caught at read
// 20203 - caught at read
// 20204 - caught at read
// 20205 - caught at read
// 20206 - caught at read
// 20207 - caught at read

// 20208 - TO DO
START_CONSTRAINT (FbcActiveObjectiveRefersObjective, ListOfObjectives, loObj)
{
  pre (loObj.isSetActiveObjective());

  bool fail = false;

  msg = "<lisOfObjectives> has an activeObjective '";
  msg += loObj.getActiveObjective() ;
  msg += "' that does not refer to an <objective> within the <model>.";

  std::string activeObj = loObj.getActiveObjective();
  
  if (loObj.get(activeObj) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20301 - caught at read
// 20302 - caught at read
// 20303 - TO DO

// 20401 - caught at read
// 20402 - caught at read
// 20403 - caught at read
// 20404 - caught at read
// 20405 - caught at read
// 20406 - caught at read
// 20407 - caught at read

// 20408
START_CONSTRAINT (FbcFluxBoundReactionMustExist, FluxBound, fb)
{
  pre (fb.isSetReaction());

  bool fail = false;

  msg = "<fluxBound> '";
  msg += fb.getId() ;
  msg += "' refers to reaction with id '";
  msg += fb.getReaction();
  msg += "' that does not exist within the <model>.";

  std::string reaction = fb.getReaction();
  
  if (m.getReaction(reaction) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20409
EXTERN_CONSTRAINT( FbcFluxBoundsForReactionConflict, FluxBoundsConsistent)

// 20501 - caught at read
// 20502 - caught at read
// 20503 - caught at read
// 20505 - caught at read
// 20505 - caught at read
// 20506
START_CONSTRAINT (FbcObjectiveOneListOfObjectives, Objective, obj)
{
  bool fail = false;

  msg = "<objective> '";
  msg += obj.getId() ;
  msg += "' has no listOfFluxObjectives.";

  if (obj.getIsSetListOfFluxObjectives() == false)
  {
    fail = true;
  }
  else if (obj.getNumFluxObjectives() == 0)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20507 - caught at read
// 20508 - caught at read
// 20509 - caught at read

// 20601 - caught at read
// 20602 - caught at read
// 20603 - caught at read
// 20606 - caught at read
// 20606 - caught at read

// 20606
START_CONSTRAINT (FbcFluxObjectReactionMustExist, FluxObjective, fo)
{
  pre (fo.isSetReaction());

  bool fail = false;

  msg = "The <fluxObjective> ";
  if (fo.isSetId()) {
    msg += "with the id '" + fo.getId() + "' ";
  }
  msg += "refers to a reaction with id '";
  msg += fo.getReaction();
  msg += "' that does not exist within the <model>.";

  std::string reaction = fo.getReaction();
  
  if (m.getReaction(reaction) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20607 - caught at read

// 20608
START_CONSTRAINT(FbcFluxObjectCoefficientWhenStrict, FluxObjective, fo)
{
  pre(fo.getPackageVersion() > 1);
  pre(fo.isSetCoefficient());
  
  // is model strict
  const FbcModelPlugin * plugin = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));
  pre (plugin != NULL);
  pre (plugin->getStrict());

  bool fail = false;

  if (util_isNaN(fo.getCoefficient()) || util_isInf(fo.getCoefficient()))
  {
    fail = true;
  }

  inv(fail == false);

}
END_CONSTRAINT

// 20701 - caught at read
// 20702 - caught at read
// 20703 - caught at read
// 20704 - caught at read

// 20705
START_CONSTRAINT (FbcReactionLwrBoundRefExists, Reaction, r)
{
  const FbcReactionPlugin* plug = 
    static_cast<const FbcReactionPlugin*>(r.getPlugin("fbc"));
  pre(plug != NULL);
  pre(plug->getPackageVersion() > 1);
  pre(plug->isSetLowerFluxBound());
  
  bool fail = false;

  std::string fb = plug->getLowerFluxBound();
  
  msg = "<Reaction> '";
  msg += r.getId() ;
  msg += "' refers to lowerBound with id '";
  msg += fb;
  msg += "' that does not exist within the <model>.";

  if (m.getParameter(fb) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20706
START_CONSTRAINT (FbcReactionUpBoundRefExists, Reaction, r)
{
  const FbcReactionPlugin* plug = 
    static_cast<const FbcReactionPlugin*>(r.getPlugin("fbc"));
  pre(plug != NULL);
  pre(plug->getPackageVersion() > 1);
  pre(plug->isSetUpperFluxBound());
  
  bool fail = false;

  std::string fb = plug->getUpperFluxBound();
  
  msg = "<Reaction> '";
  msg += r.getId() ;
  msg += "' refers to upperBound with id '";
  msg += fb;
  msg += "' that does not exist within the <model>.";

  if (m.getParameter(fb) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT


// 20707
START_CONSTRAINT (FbcReactionMustHaveBoundsStrict, Reaction, r)
{
  // is model strict
  const FbcModelPlugin * mplugin = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));
  pre (mplugin != NULL);
  pre (mplugin->getStrict());

  const FbcReactionPlugin* plug = 
    static_cast<const FbcReactionPlugin*>(r.getPlugin("fbc"));
  pre(plug != NULL);
  pre(plug->getPackageVersion() > 1 );
  
  bool fail = false;

  msg = "<Reaction> '";
  msg += r.getId() ;
  msg += "'";

  if (!(plug->isSetLowerFluxBound()) && !(plug->isSetUpperFluxBound()))
  {
    msg += " is missing both upperBound and lowerBound attributes.";
    fail = true;
  }
  else if (!(plug->isSetLowerFluxBound()))
  {
    msg += " is missing the lowerBound attribute.";
    fail = true;
  }
  else if (!(plug->isSetUpperFluxBound()))
  {
    msg += " is missing the upperBound attribute.";
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20708
START_CONSTRAINT (FbcReactionConstantBoundsStrict, Reaction, r)
{
  // is model strict
  const FbcModelPlugin * mplugin = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));
  pre (mplugin != NULL);
  pre (mplugin->getStrict());

  const FbcReactionPlugin* plug = 
    static_cast<const FbcReactionPlugin*>(r.getPlugin("fbc"));
  pre(plug != NULL);
  pre(plug->getPackageVersion() > 1);
  pre(plug->isSetLowerFluxBound());
  pre(plug->isSetUpperFluxBound());
  
  bool fail = false;

  std::string up = plug->getUpperFluxBound();
  std::string low = plug->getLowerFluxBound();
  pre(m.getParameter(up) != NULL);
  pre(m.getParameter(low) != NULL);
  
  msg = "<Reaction> '";
  msg += r.getId() ;

  if (!(m.getParameter(up)->getConstant()) && !(m.getParameter(low)->getConstant()))
  {
    msg += "' refers to upperBound with id '";
    msg += up;
    msg += "' and lowerBound with id '";
    msg += "' that are not constant parameters.";
    fail = true;
  }
  else if (!(m.getParameter(up)->getConstant()))
  {
    msg += "' refers to upperBound with id '";
    msg += up;
    msg += "' that is not a constant parameter.";
    fail = true;
  }
  else if (!(m.getParameter(low)->getConstant()))
  {
    msg += "' refers to lowerBound with id '";
    msg += low;
    msg += "' that is not a constant parameter.";
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20709
START_CONSTRAINT (FbcReactionBoundsMustHaveValuesStrict, Reaction, r)
{
  // is model strict
  const FbcModelPlugin * mplugin = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));
  pre (mplugin != NULL);
  pre (mplugin->getStrict());

  const FbcReactionPlugin* plug = 
    static_cast<const FbcReactionPlugin*>(r.getPlugin("fbc"));
  pre(plug != NULL);
  pre(plug->getPackageVersion() > 1);
  pre(plug->isSetLowerFluxBound());
  pre(plug->isSetUpperFluxBound());
  
  bool fail = false;

  std::string up = plug->getUpperFluxBound();
  std::string low = plug->getLowerFluxBound();
  pre(m.getParameter(up) != NULL);
  pre(m.getParameter(low) != NULL);

  msg = "<Reaction> '";
  msg += r.getId() ;

  if (util_isNaN(m.getParameter(up)->getValue()) && 
    util_isNaN(m.getParameter(low)->getValue()))
  {
    msg += "' refers to upperBound with id '";
    msg += up;
    msg += "' and lowerBound with id '";
    msg += "' that are have no defined value.";
    fail = true;
  }
  else if (util_isNaN(m.getParameter(up)->getValue()))
  {
    msg += "' refers to upperBound with id '";
    msg += up;
    msg += "' that has no defined value.";
    fail = true;
  }
  else if (util_isNaN(m.getParameter(low)->getValue()))
  {
    msg += "' refers to lowerBound with id '";
    msg += low;
    msg += "' that has no defined value.";
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20710
START_CONSTRAINT (FbcReactionBoundsNotAssignedStrict, Reaction, r)
{
  // is model strict
  const FbcModelPlugin * mplugin = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));
  pre (mplugin != NULL);
  pre (mplugin->getStrict());

  const FbcReactionPlugin* plug = 
    static_cast<const FbcReactionPlugin*>(r.getPlugin("fbc"));
  pre(plug != NULL);
  pre(plug->getPackageVersion() > 1);
  pre(plug->isSetLowerFluxBound());
  pre(plug->isSetUpperFluxBound());
  
  bool fail = false;

  std::string up = plug->getUpperFluxBound();
  std::string low = plug->getLowerFluxBound();
  pre(m.getParameter(up) != NULL);
  pre(m.getParameter(low) != NULL);
  
  msg = "<Reaction> '";
  msg += r.getId() ;

  if (m.getInitialAssignmentBySymbol(up) != NULL && 
    m.getInitialAssignmentBySymbol(low) != NULL)
  {
    msg += "' refers to upperBound with id '";
    msg += up;
    msg += "' and lowerBound with id '";
    msg += "' that are the targets of initialAssignments.";
    fail = true;
  }
  else if (m.getInitialAssignmentBySymbol(up) != NULL)
  {
    msg += "' refers to upperBound with id '";
    msg += up;
    msg += "' that is the target of an initialAssignment.";
    fail = true;
  }
  else if (m.getInitialAssignmentBySymbol(low) != NULL)
  {
    msg += "' refers to lowerBound with id '";
    msg += low;
    msg += "' that is the target of an initialAssignment.";
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20711
START_CONSTRAINT (FbcReactionLwrBoundNotInfStrict, Reaction, r)
{
  // is model strict
  const FbcModelPlugin * mplugin = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));
  pre (mplugin != NULL);
  pre (mplugin->getStrict());

  const FbcReactionPlugin* plug = 
    static_cast<const FbcReactionPlugin*>(r.getPlugin("fbc"));
  pre(plug != NULL);
  pre(plug->getPackageVersion() >1);
  pre(plug->isSetLowerFluxBound());
  pre(plug->isSetUpperFluxBound());
  
  bool fail = false;

  std::string low = plug->getLowerFluxBound();
  pre(m.getParameter(low) != NULL);

  msg = "<Reaction> '";
  msg += r.getId() ;

  if (util_isInf(m.getParameter(low)->getValue()) == 1)
  {
    msg += "' and lowerBound with id '";
    msg += low;
    msg += "' that has an infinite value.";
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20712
START_CONSTRAINT (FbcReactionUpBoundNotNegInfStrict, Reaction, r)
{
  // is model strict
  const FbcModelPlugin * mplugin = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));
  pre (mplugin != NULL);
  pre (mplugin->getStrict());

  const FbcReactionPlugin* plug = 
    static_cast<const FbcReactionPlugin*>(r.getPlugin("fbc"));
  pre(plug != NULL);
  pre(plug->isSetLowerFluxBound());
  pre(plug->isSetUpperFluxBound());
  
  bool fail = false;

  std::string up = plug->getUpperFluxBound();
  pre(m.getParameter(up) != NULL);

  msg = "<Reaction> '";
  msg += r.getId() ;

  if (util_isInf(m.getParameter(up)->getValue()) == -1)
  {
    msg += "' and upperBound with id '";
    msg += up;
    msg += "' that has a negative infinite value.";
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20713
START_CONSTRAINT (FbcReactionLwrLessThanUpStrict, Reaction, r)
{
  // is model strict
  const FbcModelPlugin * mplugin = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));
  pre (mplugin != NULL);
  pre (mplugin->getStrict());

  const FbcReactionPlugin* plug = 
    static_cast<const FbcReactionPlugin*>(r.getPlugin("fbc"));
  pre(plug != NULL);
  pre(plug->getPackageVersion() > 1);
  pre(plug->isSetLowerFluxBound());
  pre(plug->isSetUpperFluxBound());
  
  bool fail = false;

  std::string up = plug->getUpperFluxBound();
  std::string low = plug->getLowerFluxBound();
  pre(m.getParameter(up) != NULL);
  pre(m.getParameter(low) != NULL);
  
  double up_value = m.getParameter(up)->getValue();
  double low_value = m.getParameter(low)->getValue();

  pre(util_isFinite(up_value));
  pre(util_isFinite(low_value));
  
  
  msg = "In <Reaction> '";
  msg += r.getId() ;
  msg += "' the upperBound with id '";
  msg += up;
  msg += "' has a value that is not greater than or equal to the lowerBound with id '";
  msg += low;
  msg += "'.";
  
  if (up_value < low_value)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20714
START_CONSTRAINT (FbcSpeciesReferenceConstantStrict, SpeciesReference, sr)
{
  // is model strict
  const FbcModelPlugin * mplugin = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));
  pre (mplugin != NULL);
  pre (mplugin->getStrict());

  const Reaction * r = static_cast<const Reaction*>(
    sr.getAncestorOfType(SBML_REACTION));

  
  bool fail = false;

  msg = "<Reaction> '";
  msg += r->getId() ;
  msg += "' has the speciesReference to '";
  msg += sr.getSpecies();
  msg += "' which is not constant.";

  if (sr.getConstant() == false)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20715
START_CONSTRAINT (FbcSpeciesRefsStoichMustBeRealStrict, SpeciesReference, sr)
{
  // is model strict
  const FbcModelPlugin * mplugin = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));
  pre (mplugin != NULL);
  pre (mplugin->getStrict());

  const Reaction * r = static_cast<const Reaction*>(
    sr.getAncestorOfType(SBML_REACTION));

  
  bool fail = false;

  msg = "<Reaction> '";
  msg += r->getId() ;
  msg += "' has the speciesReference to '";
  msg += sr.getSpecies();
  msg += "' which is does not have a valid stoichiometry.";

  if (util_isFinite(sr.getStoichiometry()) == false)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 20716
START_CONSTRAINT (FbcSpeciesRefNotAssignedStrict, SpeciesReference, sr)
{
  pre(sr.isSetId());

  // is model strict
  const FbcModelPlugin * mplugin = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));
  pre (mplugin != NULL);
  pre (mplugin->getStrict());

  const Reaction * r = static_cast<const Reaction*>(
    sr.getAncestorOfType(SBML_REACTION));

  
  bool fail = false;

  msg = "<Reaction> '";
  msg += r->getId() ;
  msg += "' has the speciesReference with id '";
  msg += sr.getId();
  msg += "' which is the target of an <initialAssignment>.";

  if (m.getInitialAssignmentBySymbol(sr.getId()) != NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

  // 20801 - caught at read
  // 20802 - caught at read
  // 20803 - caught at read
  // 20804 - caught at read
  // 20805 - caught at read

START_CONSTRAINT(FbcGeneProdAssocContainsOneElement, GeneProductAssociation, gpa)
{
  inv(gpa.isSetAssociation());

}
END_CONSTRAINT
  // 20806 - caught at read

  // 20901 - caught at read
  // 20902 - caught at read
  // 20903 - caught at read
  // 20904 - caught at read

// 20908
START_CONSTRAINT (FbcGeneProdRefGeneProductExists, GeneProductRef, r)
{
  pre(r.isSetGeneProduct());

  const FbcModelPlugin * mplugin = 
    static_cast<const FbcModelPlugin*>(m.getPlugin("fbc"));
  pre (mplugin != NULL);

  bool fail = false;

  std::string gp = r.getGeneProduct();

  const Reaction * react = static_cast<const Reaction *>
    (r.getAncestorOfType(SBML_REACTION));

  msg = "<GeneProductRef> in the <reaction> with id '";
  msg += react->getId() ;
  msg += "' refers to a geneProduct with id '";
  msg += gp;
  msg += "' that does not exist within the <model>.";

  if (mplugin->getGeneProduct(gp) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

  // 21001 - caught at read
  // 21002 - caught at read
  
// 21003
START_CONSTRAINT (FbcAndTwoChildren, FbcAnd, r)
{
  bool fail = false;

  const Reaction * react = static_cast<const Reaction *>
    (r.getAncestorOfType(SBML_REACTION));

  msg = "The <And> element in the <reaction> with id '";
  msg += react->getId() ;
  msg += "' does not have two child elements.";

  if (r.getNumAssociations() < 2)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

  // 21101 - caught at read
  // 21102 - caught at read
  
// 21103
START_CONSTRAINT (FbcOrTwoChildren, FbcOr, r)
{
  bool fail = false;

  const Reaction * react = static_cast<const Reaction *>
    (r.getAncestorOfType(SBML_REACTION));

  msg = "The <Or> element in the <reaction> with id '";
  msg += react->getId() ;
  msg += "' does not have two child elements.";

  if (r.getNumAssociations() < 2)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

  // 21201 - caught at read
  // 21202 - caught at read
  // 21203 - caught at read
  // 21204 - caught at read

//21205
EXTERN_CONSTRAINT( FbcGeneProductLabelMustBeUnique, UniqueGeneProductLabels)

  // 21206 - caught at read
 
//21207
START_CONSTRAINT (FbcGeneProductAssocSpeciesMustExist, GeneProduct, gp)
{
  pre (gp.isSetAssociatedSpecies());

  std::string as = gp.getAssociatedSpecies();

  bool fail = false;

  msg = "The <GeneProduct> with id '";
  msg += gp.getId() ;
  msg += "' refers to an associatedSpecies '";
  msg += as;
  msg += "' that does not exist within the <model>.";

  if (m.getSpecies(as) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT


// 21301 - caught at read
// 21302 - caught at read
// 21303 - caught at read
// 21304 - caught at read

// 21305
START_CONSTRAINT(FbcUserDefinedConstraintComponentVariableMustBeReactionOrParameter, UserDefinedConstraintComponent, udcc)
{
  pre(udcc.isSetVariable());

  std::string as = udcc.getVariable();

  bool fail = false;

  msg = "The <UserDefinedConstraintComponent> with id '";
  msg += udcc.getId();
  msg += "' refers to a variable '";
  msg += as;
  msg += "' that does not exist within the <model>.";

  if (m.getReaction(as) == NULL && m.getParameter(as) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 21306 - caught at read
// 21307 - sring

// 21401 - caught at read
// 21402 - caught at read
// 21403 - caught at read
// 21404 - caught at read
// 21405
START_CONSTRAINT(FbcUserDefinedConstraintLowerBoundMustBeParameter, UserDefinedConstraint, udc)
{
  pre(udc.isSetLowerBound());

  std::string as = udc.getLowerBound();

  bool fail = false;

  msg = "The <UserDefinedConstraint> with id '";
  msg += udc.getId();
  msg += "' refers to a lowerBound '";
  msg += as;
  msg += "' that does not exist within the <model>.";

  if (m.getParameter(as) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT


// 21406
START_CONSTRAINT(FbcUserDefinedConstraintUpperBoundMustBeParameter, UserDefinedConstraint, udc)
{
  pre(udc.isSetUpperBound());

  std::string as = udc.getUpperBound();

  bool fail = false;

  msg = "The <UserDefinedConstraint> with id '";
  msg += udc.getId();
  msg += "' refers to an upperBound '";
  msg += as;
  msg += "' that does not exist within the <model>.";

  if (m.getParameter(as) == NULL)
  {
    fail = true;
  }

  inv(fail == false);
}
END_CONSTRAINT

// 21407 - string

// 21408 - caught at read
// 21409 - caught at read



/** @endcond */


