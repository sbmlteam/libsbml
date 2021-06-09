/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    FbcConsistencyConstraintsDeclared.cxx
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

addConstraint(new VConstraintListOfObjectivesFbcActiveObjectiveRefersObjective(*this));

addConstraint(new VConstraintFluxBoundFbcFluxBoundReactionMustExist(*this));

addConstraint(new FluxBoundsConsistent(FbcFluxBoundsForReactionConflict, *this));

addConstraint(new VConstraintObjectiveFbcObjectiveOneListOfObjectives(*this));

addConstraint(new VConstraintFluxObjectiveFbcFluxObjectReactionMustExist(*this));

addConstraint(new VConstraintFluxObjectiveFbcFluxObjectCoefficientWhenStrict(*this));

addConstraint(new VConstraintReactionFbcReactionLwrBoundRefExists(*this));

addConstraint(new VConstraintReactionFbcReactionUpBoundRefExists(*this));

addConstraint(new VConstraintReactionFbcReactionMustHaveBoundsStrict(*this));

addConstraint(new VConstraintReactionFbcReactionConstantBoundsStrict(*this));

addConstraint(new VConstraintReactionFbcReactionBoundsMustHaveValuesStrict(*this));

addConstraint(new VConstraintReactionFbcReactionBoundsNotAssignedStrict(*this));

addConstraint(new VConstraintReactionFbcReactionLwrBoundNotInfStrict(*this));

addConstraint(new VConstraintReactionFbcReactionUpBoundNotNegInfStrict(*this));

addConstraint(new VConstraintReactionFbcReactionLwrLessThanUpStrict(*this));

addConstraint(new VConstraintSpeciesReferenceFbcSpeciesReferenceConstantStrict(*this));

addConstraint(new VConstraintSpeciesReferenceFbcSpeciesRefsStoichMustBeRealStrict(*this));

addConstraint(new VConstraintSpeciesReferenceFbcSpeciesRefNotAssignedStrict(*this));

addConstraint(new VConstraintGeneProductAssociationFbcGeneProdAssocContainsOneElement(*this));

addConstraint(new VConstraintGeneProductRefFbcGeneProdRefGeneProductExists(*this));

addConstraint(new VConstraintFbcAndFbcAndTwoChildren(*this));

addConstraint(new VConstraintFbcOrFbcOrTwoChildren(*this));

addConstraint(new UniqueGeneProductLabels(FbcGeneProductLabelMustBeUnique, *this));

addConstraint(new VConstraintGeneProductFbcGeneProductAssocSpeciesMustExist(*this));

addConstraint(new VConstraintUserDefinedConstraintComponentFbcUserDefinedConstraintComponentVariableMustBeReactionOrParameter(*this));

addConstraint(new VConstraintUserDefinedConstraintFbcUserDefinedConstraintLowerBoundMustBeParameter(*this));

addConstraint(new VConstraintUserDefinedConstraintFbcUserDefinedConstraintUpperBoundMustBeParameter(*this));

/** @endcond */

