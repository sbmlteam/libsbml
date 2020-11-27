  /** @cond doxygenLibsbmlInternal */

/**
 * @file:   MultiIdentifierConsistencyConstraints.cpp
 * @brief:  Implementation of the MultiIdentifierConsistencyConstraints class
 * @author: Fengkai Zhang
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
 * ------------------------------------------------------------------------ -->
 */

#ifndef  AddingConstraintsToValidator

#include <sbml/validator/VConstraint.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>
#include "UniqueMultiComponentIds.h"
#include "UniqueSTIIdsWithinMultiSpeciesType.h"
#include "UniqueSTCIdsWithinMultiSpeciesType.h"
#include "UniqueISTBIdsWithinMultiSpeciesType.h"
#include "UniqueSFTIdsWithinMultiSpeciesType.h"
#include "UniqueSLOSFIdsWithinSpecies.h"
#include "UniqueSpeciesFeatureIdsWithinSpecies.h"
#include "UniqueCRefIdsWithinCompartment.h"

#endif  /* AddingConstrainstToValidator */

#include <sbml/validator/ConstraintMacros.h>

/** @cond doxygenIgnored */

using namespace std;

/** @endcond */

/** PUT CONSTRAINTS HERE */

//************************************
// General rules about identifiers

// MultiDupClaId                         = 7010301
/*!< Duplicate 'id' attribute value */
EXTERN_CONSTRAINT( MultiDupClaId, UniqueMultiComponentIds )

// MultiInvSIdSyn                        = 7010302 /*!< Invalid SId syntax */
// check at read

// MultiInvNamSyn                        = 7010303 /*!< Invalid name syntax */
// - string syntax - since an attribute value is a string
// by the time it is read in we cannot check this （cited from comp package)

//************************************
// scoping rules for identifiers, sec 3.28

// MultiUnqId_SptIns                     =  7010304 - SK renumbered from 7010401
/*!< SpeciesTypeInstance must have unique ids within the parent speciesType */
EXTERN_CONSTRAINT( MultiUnqId_SptIns, UniqueSpeciesTypeInstanceIdsWithinMultiSpeciesType )

// MultiUnqId_SptCpoInd                  = 7010305 - SK renumbered from 7010402
/*!< SpeciesTypeComponentIndex must have unique ids within the parent speciesType */
EXTERN_CONSTRAINT( MultiUnqId_SptCpoInd, UniqueSpeciesTypeComponentIndexIdsWithinMultiSpeciesType )

// MultiUnqId_InSptBnd                   = 7010306 - SK renumbered from 7010403
/*!< InSpeciesTypeBond must have unique ids within the parent speciesType */
EXTERN_CONSTRAINT( MultiUnqId_InSptBnd, UniqueInSpeciesTypeBondIdsWithinMultiSpeciesType )

// MultiUnqId_Sft                        = 7010307 - SK renumbered from 7010404
/*!< SpeciesFeatureType must have unique ids within the parent speciesType */
EXTERN_CONSTRAINT( MultiUnqId_Sft, UniqueSpeciesFeatureTypeIdsWithinMultiSpeciesType )

// MultiUnqId_PblSpecFtrVal              = 7010308 - SK renumbered from 7010405 // add at v1.0.6
/*!< PossibleSpeciesFeatureValue must have unique ids within the parent speciesFeatureType */
EXTERN_CONSTRAINT( MultiUnqId_SubListOfSfs, UniqueSubListOfSpeciesFeaturesIdsWithinSpecies )

// MultiUnqId_SpeFtr                     = 7010309 - SK renumbered from 7010406
/*!< SpeciesFeature must have unique ids within a species */
EXTERN_CONSTRAINT( MultiUnqId_SpeFtr, UniqueSpeciesFeatureIdsWithinSpecies )

// MultiUnqId_CpaRef                     = 7010310 - SK renumbered from 7010408
/*!< CompartmentReference must have unique ids within a compartment */
EXTERN_CONSTRAINT( MultiUnqId_CpaRef, UniqueCompartmentReferenceIdsWithinCompartment )




// *********************************************************
// the following "Invalid syntax" errors are checked at read

//************************************
// SIdRef under SpeciesType

// MultiInvSIdRefSyn_Spt_CpaAtt          = 7010311 - SK renumbered from 7010501 /*!< Invalid SIdRef syntax: 'compartment' attribute of SpeciesType */
// MultiInvSIdRefSyn_PslSpeFtrVal_NumAtt = 7010312 - SK renumbered from 7010502 /*!< Invalid SIdRef syntax: 'numericValue' attribute of PossibleSpeciesFeatureValue */
// MultiInvSIdRefSyn_SptIns_SptAtt       = 7010313 - SK renumbered from 7010503 /*!< Invalid SIdRef syntax: 'speciesType' attribute // peciesTypeInstance */
// MultiInvSIdRefSyn_SptIns_CpaRefAtt    = 7010314 - SK renumbered from 7010504 /*!< Invalid SIdRef syntax: 'compartmentReference' attribute of SpeciesTypeInstance */
// MultiInvSIdRefSyn_SptCpoInd_CpoAtt    = 7010315 - SK renumbered from 7010505 /*!< Invalid SIdRef syntax: 'component' attribute of SpeciesTypeComponentIndex */
// MultiInvSIdRefSyn_SptCpoInd_ParAtt    = 7010316 - SK renumbered from 7010506 /*!< Invalid SIdRef syntax: 'identifyingParent' attribute of SpeciesTypeComponentIndex */
// MultiInvSIdRefSyn_InSptBnd_Bst1Att    = 7010317 - SK renumbered from 7010508 /*!< Invalid SIdRef syntax: 'bindingSite1' attribute of InSpeciesTypeBond */
// MultiInvSIdRefSyn_InSptBnd_Bst2Att    = 7010318 - SK renumbered from 7010509 /*!< Invalid SIdRef syntax: 'bindingSite2' attribute of InSpeciesTypeBond */

//************************************
// SIdRef under Species

// MultiInvSIdRefSyn_Spe_SptAtt          = 7010319 - SK renumbered from 7010601 /*!< Invalid SIdRef syntax: 'speciesType' attribute of extended Species */
// MultiInvSIdRefSyn_OutBst_CpoAtt       = 7010320 - SK renumbered from 7010602 /*!< Invalid SIdRef syntax: 'component' attribute of OutwardBindingSite */
// MultiInvSIdRefSyn_SpeFtr_SpeFtrTypAtt = 7010321 - SK renumbered from 7010603 /*!< Invalid SIdRef syntax: 'speciesFeatureType' attribute of SpeciesFeature */
// MultiInvSIdRefSyn_SpeFtr_CpoAtt       = 7010322 - SK renumbered from 7010604 /*!< Invalid SIdRef syntax: 'component' attribute of SpeciesFeature */
// MultiInvSIdRefSyn_SpeFtrVal_ValAtt    = 7010323 - SK renumbered from 7010605 /*!< Invalid SIdRef syntax: 'value' attribute of SpeciesFeatureValue */

//************************************
// SIdRef under Reaction

// MultiInvSIdRefSyn_SplSpeRef_CompRefAtt = 7010324 - SK renumbered from 7010701 /*!< Invalid SIdRef syntax: 'compartmentReference' attribute of extended SimpleSpeciesReference */
// MultiInvSIdRefSyn_StpCpoMapInPro_RctAtt = 7010325 - SK renumbered from 7010702 /*!< Invalid SIdRef syntax: 'reactant' attribute of SpeciesTypeComponentMapInProduct */
// MultiInvSIdRefSyn_StpCpoMapInPro_RctCpoAtt = 7010326 - SK renumbered from 7010703 /*!< Invalid SIdRef syntax: 'reactantComponent' attribute of SpeciesTypeComponentMapInProduct */
// MultiInvSIdRefSyn_StpCpoMapInPro_ProCpoAtt = 7010327 - SK renumbered from 7010704 /*!< Invalid SIdRef syntax: 'productComponent' attribute of SpeciesTypeComponentMapInProduct */

//************************************
// SIdRef under Compartment

// MultiInvSIdRefSyn_Cpa_CpaTypAtt       = 7010328 - SK renumbered from 7010801 /*!< Invalid SIdRef syntax: 'compartmentType' attribute of extended Compartment */
// MultiInvSIdRefSyn_CpaRef_CpaAtt       = 7010329 - SK renumbered from 7010802 /*!< Invalid SIdRef syntax: 'compartment' attribute of CompartmentReference */



  /** @endcond */


