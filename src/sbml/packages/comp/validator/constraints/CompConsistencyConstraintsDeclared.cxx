/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    CompConsistencyConstraintsDeclared.cxx
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

addConstraint(new VConstraintExternalModelDefinitionCompReferenceMustBeL3(*this));

addConstraint(new VConstraintExternalModelDefinitionCompModReferenceMustIdOfModel(*this));

addConstraint(new ExtModelReferenceCycles(CompCircularExternalModelReference, *this));

addConstraint(new VConstraintExternalModelDefinitionCompUnresolvedReference(*this));

addConstraint(new VConstraintSubmodelCompSubmodelMustReferenceModel(*this));

addConstraint(new VConstraintSubmodelCompSubmodelCannotReferenceSelf(*this));

addConstraint(new SubmodelReferenceCycles(CompModCannotCircularlyReferenceSelf, *this));

addConstraint(new VConstraintSubmodelCompTimeConversionMustBeParameter(*this));

addConstraint(new VConstraintSubmodelCompExtentConversionMustBeParameter(*this));

addConstraint(new VConstraintDeletionCompPortRefMustReferencePort(*this));

addConstraint(new VConstraintReplacedElementCompPortRefMustReferencePort(*this));

addConstraint(new VConstraintReplacedByCompPortRefMustReferencePort(*this));

addConstraint(new VConstraintSBaseRefCompPortRefMustReferencePort(*this));

addConstraint(new VConstraintPortCompIdRefMustReferenceObject(*this));

addConstraint(new VConstraintDeletionCompIdRefMustReferenceObject(*this));

addConstraint(new VConstraintReplacedElementCompIdRefMustReferenceObject(*this));

addConstraint(new VConstraintReplacedByCompIdRefMustReferenceObject(*this));

addConstraint(new VConstraintSBaseRefCompIdRefMustReferenceObject(*this));

addConstraint(new VConstraintPortCompUnitRefMustReferenceUnitDef(*this));

addConstraint(new VConstraintDeletionCompUnitRefMustReferenceUnitDef(*this));

addConstraint(new VConstraintReplacedElementCompUnitRefMustReferenceUnitDef(*this));

addConstraint(new VConstraintReplacedByCompUnitRefMustReferenceUnitDef(*this));

addConstraint(new VConstraintSBaseRefCompUnitRefMustReferenceUnitDef(*this));

addConstraint(new VConstraintPortCompMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintDeletionCompMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintReplacedElementCompMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintReplacedByCompMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintSBaseRefCompMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintPortCompParentOfSBRefChildMustBeSubmodel(*this));

addConstraint(new VConstraintDeletionCompParentOfSBRefChildMustBeSubmodel(*this));

addConstraint(new VConstraintReplacedElementCompParentOfSBRefChildMustBeSubmodel(*this));

addConstraint(new VConstraintReplacedByCompParentOfSBRefChildMustBeSubmodel(*this));

addConstraint(new VConstraintSBaseRefCompParentOfSBRefChildMustBeSubmodel(*this));

addConstraint(new VConstraintSBaseRefCompSBaseRefMustReferenceObject(*this));

addConstraint(new VConstraintSBaseRefCompSBaseRefMustReferenceOnlyOneObject(*this));

addConstraint(new VConstraintPortCompPortMustReferenceObject(*this));

addConstraint(new VConstraintPortCompPortMustReferenceOnlyOneObject(*this));

addConstraint(new UniquePortReferences(CompPortReferencesUnique, *this));

addConstraint(new VConstraintDeletionCompDeletionMustReferenceObject(*this));

addConstraint(new VConstraintDeletionCompDeletionMustReferOnlyOneObject(*this));

addConstraint(new VConstraintReplacedElementCompReplacedElementMustRefObject(*this));

addConstraint(new VConstraintReplacedElementCompReplacedElementMustRefOnlyOne(*this));

addConstraint(new VConstraintReplacedElementCompReplacedElementSubModelRef(*this));

addConstraint(new VConstraintReplacedElementCompReplacedElementDeletionRef(*this));

addConstraint(new VConstraintReplacedElementCompReplacedElementConvFactorRef(*this));

addConstraint(new UniqueReplacedReferences(CompReplacedElementSameReference, *this));

addConstraint(new VConstraintReplacedElementCompReplacedElementNoDelAndConvFact(*this));

addConstraint(new VConstraintReplacedByCompReplacedByMustRefObject(*this));

addConstraint(new VConstraintReplacedByCompReplacedByMustRefOnlyOne(*this));

addConstraint(new VConstraintReplacedByCompReplacedBySubModelRef(*this));

addConstraint(new ClassReplacements(CompMustReplaceSameClass, *this));

addConstraint(new PackageIdReplacementCheck(CompMustReplacePackageIDs, *this));

addConstraint(new VConstraintPortCompIdRefMayReferenceUnknownPackage(*this));

addConstraint(new VConstraintDeletionCompIdRefMayReferenceUnknownPackage(*this));

addConstraint(new VConstraintReplacedElementCompIdRefMayReferenceUnknownPackage(*this));

addConstraint(new VConstraintSBaseRefCompIdRefMayReferenceUnknownPackage(*this));

addConstraint(new VConstraintPortCompMetaIdRefMayReferenceUnknownPkg(*this));

addConstraint(new VConstraintDeletionCompMetaIdRefMayReferenceUnknownPkg(*this));

addConstraint(new VConstraintReplacedElementCompMetaIdRefMayReferenceUnknownPkg(*this));

addConstraint(new VConstraintSBaseRefCompMetaIdRefMayReferenceUnknownPkg(*this));


/** @endcond */

