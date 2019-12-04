/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    ConsistencyConstraintsDeclared.cxx
 * @brief   Declarations of constraints
 * @author  SBML Team
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

addConstraint(new FunctionApplyMathCheck(10214, *this));

addConstraint(new VConstraintModel20204(*this));

addConstraint(new VConstraintModel20216(*this));

addConstraint(new VConstraintModel20705(*this));

addConstraint(new VConstraintFunctionDefinition20301(*this));

addConstraint(new FunctionReferredToExists(20302, *this));

addConstraint(new FunctionDefinitionRecursion(20303, *this));

addConstraint(new FunctionDefinitionVars(20304, *this));

addConstraint(new VConstraintFunctionDefinition20305(*this));

addConstraint(new VConstraintFunctionDefinition20306(*this));

addConstraint(new VConstraintFunctionDefinition99301(*this));

addConstraint(new VConstraintFunctionDefinition99302(*this));

addConstraint(new VConstraintFunctionDefinition99304(*this));

addConstraint(new VConstraintUnitDefinition20401(*this));

addConstraint(new VConstraintUnitDefinition20402(*this));

addConstraint(new VConstraintUnitDefinition20403(*this));

addConstraint(new VConstraintUnitDefinition20404(*this));

addConstraint(new VConstraintUnitDefinition20405(*this));

addConstraint(new VConstraintUnitDefinition20406(*this));

addConstraint(new VConstraintUnitDefinition20407(*this));

addConstraint(new VConstraintUnitDefinition20408(*this));

addConstraint(new VConstraintUnitDefinition20410(*this));

addConstraint(new VConstraintUnitDefinition20411(*this));

addConstraint(new VConstraintUnit20412(*this));

addConstraint(new VConstraintParameter20412(*this));

addConstraint(new VConstraintCompartment20501(*this));

addConstraint(new VConstraintCompartment20502(*this));

addConstraint(new VConstraintCompartment20503(*this));

addConstraint(new VConstraintCompartment20504(*this));

addConstraint(new CompartmentOutsideCycles(20505, *this));

addConstraint(new VConstraintCompartment20506(*this));

addConstraint(new VConstraintCompartment20510(*this));

addConstraint(new VConstraintSpecies20601(*this));

addConstraint(new VConstraintSpecies20602(*this));

addConstraint(new VConstraintSpecies20603(*this));

addConstraint(new VConstraintSpecies20604(*this));

addConstraint(new VConstraintSpecies20605(*this));

addConstraint(new VConstraintSpecies20606(*this));

addConstraint(new VConstraintSpecies20607(*this));

addConstraint(new VConstraintSpecies20609(*this));

addConstraint(new SpeciesReactionOrRule(20610, *this));

addConstraint(new VConstraintSpeciesReference20611(*this));

addConstraint(new VConstraintSpecies20612(*this));

addConstraint(new UniqueSpeciesTypesInCompartment(20613, *this));

addConstraint(new VConstraintSpecies20614(*this));

addConstraint(new VConstraintSpecies20615(*this));

addConstraint(new VConstraintSpecies20617(*this));

addConstraint(new VConstraintSpecies20705(*this));

addConstraint(new VConstraintInitialAssignment20801(*this));

addConstraint(new UniqueSymbolsInInitialAssignments(20802, *this));

addConstraint(new UniqueVarsInInitialAssignmentsAndRules(20803, *this));

addConstraint(new VConstraintInitialAssignment20804(*this));

addConstraint(new VConstraintInitialAssignment20806(*this));

addConstraint(new VConstraintAssignmentRule20901(*this));

addConstraint(new VConstraintRateRule20902(*this));

addConstraint(new VConstraintAssignmentRule20903(*this));

addConstraint(new VConstraintRateRule20904(*this));

addConstraint(new AssignmentCycles(20906, *this));

addConstraint(new AssignmentRuleOrdering(99106, *this));

addConstraint(new VConstraintAssignmentRule20907(*this));

addConstraint(new VConstraintRateRule20907(*this));

addConstraint(new VConstraintAlgebraicRule20907(*this));

addConstraint(new VConstraintAssignmentRule20911(*this));

addConstraint(new VConstraintRateRule20911(*this));

addConstraint(new RateOfCycles(20912, *this));

addConstraint(new VConstraintConstraint21001(*this));

addConstraint(new VConstraintConstraint21007(*this));

addConstraint(new VConstraintReaction21101(*this));

addConstraint(new VConstraintReaction21107(*this));

addConstraint(new VConstraintSpeciesReference21111(*this));

addConstraint(new VConstraintSpeciesReference21113(*this));

addConstraint(new VConstraintSpeciesReference99131(*this));

addConstraint(new KineticLawVars(21121, *this));

addConstraint(new VConstraintKineticLaw21124(*this));

addConstraint(new VConstraintKineticLaw21125(*this));

addConstraint(new VConstraintKineticLaw21126(*this));

addConstraint(new VConstraintKineticLaw21130(*this));

addConstraint(new VConstraintKineticLaw99129(*this));

addConstraint(new VConstraintAssignmentRule99129(*this));

addConstraint(new VConstraintRateRule99129(*this));

addConstraint(new StoichiometryMathVars(21131, *this));

addConstraint(new VConstraintReaction21152(*this));

addConstraint(new VConstraintLocalParameter21173(*this));

addConstraint(new VConstraintEvent21201(*this));

addConstraint(new VConstraintTrigger21202(*this));

addConstraint(new VConstraintEvent21203(*this));

addConstraint(new VConstraintEvent21204(*this));

addConstraint(new VConstraintEvent99206(*this));

addConstraint(new VConstraintEvent21206(*this));

addConstraint(new VConstraintEvent21207(*this));

addConstraint(new VConstraintTrigger21209(*this));

addConstraint(new VConstraintDelay21210(*this));

addConstraint(new VConstraintEventAssignment21211(*this));

addConstraint(new VConstraintEventAssignment21212(*this));

addConstraint(new VConstraintEventAssignment21213(*this));

addConstraint(new VConstraintPriority21231(*this));
/** @endcond */

