/**
 * @file    SBMLError.h
 * @brief   Represents SBML errors and other diagnostics
 * @author  Michael Hucka
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class SBMLError
 * @brief Representation of errors, warnings and other diagnostics
 *
 * When a libSBML operation on SBML content results in an error, or when
 * there is something wrong with the SBML content, the problems are
 * reported as SBMLError objects.  These are generally stored in an
 * SBMLErrorLog object maintained with the SBMLDocument object containing
 * the SBML content in question.
 *
 * Each SBMLError object instance has an identification number that
 * identifies the nature of the problem.  This number will be up to five
 * digits long, and will be listed in one of two enumerations,
 * XMLError::Code or SBMLError::SBMLCode.  The latter enumeration in
 * SBMLError contains all the SBML validation rule numbers listed in the
 * appendices of the SBML specification documents.
 *
 * SBMLError also records a @em category code, drawn from the enumeration
 * SBMLCategory.  Categories are used to partition errors into distinct
 * groups.  In particular, the SBML validation system in libSBML identifies
 * itself using different category codes, such as SBMLConsistencyIdentifier
 * for identifier consistency checking and SBMLConsistencyMathML for MathML
 * consistency checking.
 *
 * In addition, SBMLError also has a @em severity code, drawn from the
 * enumeration SBMLSeverity.  Severity levels currently range from
 * informational (SBMLError::Info) to fatal errors (SBMLError::Fatal).
 *
 * SBMLError also logs a text message suitable for displaying to humans and
 * describing the nature of the problem.
 *
 * Finally, SBMLError records the line and column near where the problem
 * occurred in the SBML content.  We say "near", because a lot of factors
 * affect how accurate the line/column information ultimately is.  For
 * example, different XML parsers have different conventions for which line
 * and column number they report for a particular problem (which makes a
 * difference when a problem involves an opening XML tag on one line and a
 * closing tag on another line).  When communicating problems to humans, it
 * is generally best to provide all three pieces of information (message,
 * line, column), to help them determine the actual error.
 */

#ifndef SBMLError_h
#define SBMLError_h

#include <sbml/common/extern.h>
#include <sbml/xml/XMLError.h>


class LIBSBML_EXTERN SBMLError : public XMLError
{
public:

  /**
   * Codes for all SBML-level errors and warnings.
   *
   * These are distinguished from the XML layer (LIBLAX) error codes by
   * being numbered > 10000, while the XML layer's codes are < 9999.
   * Calling programs may wish to check whether a given SBMLError object's
   * error identifier is actually from SBMLCode or XMLError::Code.  This
   * distinction corresponds to whether a given error represents a
   * low-level XML problem or an SBML problem.
   */
  enum SBMLCode
  {
    UnknownError                     =     0 /*!< 0 */
  , NotUTF8                          = 10101 /*!< SBML L2v3 validation rule #10101 */
  , UnrecognizedElement              = 10102 /*!< SBML L2v3 validation rule #10102 */
  , NotSchemaConformant              = 10103 /*!< SBML L2v3 validation rule #10103 */
  , InvalidMathElement               = 10201 /*!< SBML L2v3 validation rule #10201 */
  , DisallowedMathMLSymbol           = 10202 /*!< SBML L2v3 validation rule #10202 */
  , DisallowedMathMLEncodingUse      = 10203 /*!< SBML L2v3 validation rule #10203 */
  , DisallowedDefinitionURLUse       = 10204 /*!< SBML L2v3 validation rule #10204 */
  , BadCsymbolDefinitionURLValue     = 10205 /*!< SBML L2v3 validation rule #10205 */
  , DisallowedMathTypeAttributeUse   = 10206 /*!< SBML L2v3 validation rule #10206 */
  , DisallowedMathTypeAttributeValue = 10207 /*!< SBML L2v3 validation rule #10207 */
  , LambdaOnlyAllowedInFunctionDef   = 10208 /*!< SBML L2v3 validation rule #10208 */
  , BooleanOpsNeedBooleanArgs        = 10209 /*!< SBML L2v3 validation rule #10209 */
  , NumericOpsNeedNumericArgs        = 10210 /*!< SBML L2v3 validation rule #10210 */
  , ArgsToEqNeedSameType             = 10211 /*!< SBML L2v3 validation rule #10211 */
  , PiecewiseNeedsConsistentTypes    = 10212 /*!< SBML L2v3 validation rule #10212 */
  , PieceNeedsBoolean                = 10213 /*!< SBML L2v3 validation rule #10213 */
  , ApplyCiMustBeUserFunction        = 10214 /*!< SBML L2v3 validation rule #10214 */
  , ApplyCiMustBeModelComponent      = 10215 /*!< SBML L2v3 validation rule #10215 */
  , KineticLawParametersAreLocalOnly = 10216 /*!< SBML L2v3 validation rule #10216 */
  , MathResultMustBeNumeric          = 10217 /*!< SBML L2v3 validation rule #10217 */
  , OpsNeedCorrectNumberOfArgs       = 10218 /*!< SBML L2v3 validation rule #10218 */
  , DuplicateComponentId             = 10301 /*!< SBML L2v3 validation rule #10301 */
  , DuplicateUnitDefinitionId        = 10302 /*!< SBML L2v3 validation rule #10302 */
  , DuplicateLocalParameterId        = 10303 /*!< SBML L2v3 validation rule #10303 */
  , MultipleAssignmentOrRateRules    = 10304 /*!< SBML L2v3 validation rule #10304 */
  , MultipleEventAssignmentsForId    = 10305 /*!< SBML L2v3 validation rule #10305 */
  , EventAndAssignmentRuleForId      = 10306 /*!< SBML L2v3 validation rule #10306 */
  , DuplicateMetaId                  = 10307 /*!< SBML L2v3 validation rule #10307 */
  , InvalidSBOTermSyntax             = 10308 /*!< SBML L2v3 validation rule #10308 */
  , InvalidMetaidSyntax              = 10309 /*!< SBML L2v3 validation rule #10309 */
  , InvalidIdSyntax                  = 10310 /*!< SBML L2v3 validation rule #10310 */
  , InvalidUnitIdSyntax              = 10311 /*!< SBML L2v3 validation rule #10311 */
  , MissingAnnotationNamespace       = 10401 /*!< SBML L2v3 validation rule #10401 */
  , DuplicateAnnotationNamespaces    = 10402 /*!< SBML L2v3 validation rule #10402 */
  , SBMLNamespaceInAnnotation        = 10403 /*!< SBML L2v3 validation rule #10403 */
  , InconsistentArgUnits             = 10501 /*!< SBML L2v3 validation rule #10501 */
  , AssignRuleCompartmentMismatch    = 10511 /*!< SBML L2v3 validation rule #10511 */
  , AssignRuleSpeciesMismatch        = 10512 /*!< SBML L2v3 validation rule #10512 */
  , AssignRuleParameterMismatch      = 10513 /*!< SBML L2v3 validation rule #10513 */
  , InitAssignCompartmenMismatch     = 10521 /*!< SBML L2v3 validation rule #10521 */
  , InitAssignSpeciesMismatch        = 10522 /*!< SBML L2v3 validation rule #10522 */
  , InitAssignParameterMismatch      = 10523 /*!< SBML L2v3 validation rule #10523 */
  , RateRuleCompartmentMismatch      = 10531 /*!< SBML L2v3 validation rule #10531 */
  , RateRuleSpeciesMismatch          = 10532 /*!< SBML L2v3 validation rule #10532 */
  , RateRuleParameterMismatch        = 10533 /*!< SBML L2v3 validation rule #10533 */
  , KineticLawNotSubstancePerTime    = 10541 /*!< SBML L2v3 validation rule #10541 */
  , DelayUnitsNotTime                = 10551 /*!< SBML L2v3 validation rule #10551 */
  , EventAssignCompartmentMismatch   = 10561 /*!< SBML L2v3 validation rule #10561 */
  , EventAssignSpeciesMismatch       = 10562 /*!< SBML L2v3 validation rule #10562 */
  , EventAssignParameterMismatch     = 10563 /*!< SBML L2v3 validation rule #10563 */
  , OverdeterminedSystem             = 10601 /*!< SBML L2v3 validation rule #10601 */
  , InvalidModelSBOTerm              = 10701 /*!< SBML L2v3 validation rule #10701 */
  , InvalidFunctionDefSBOTerm        = 10702 /*!< SBML L2v3 validation rule #10702 */
  , InvalidParameterSBOTerm          = 10703 /*!< SBML L2v3 validation rule #10703 */
  , InvalidInitAssignSBOTerm         = 10704 /*!< SBML L2v3 validation rule #10704 */
  , InvalidRuleSBOTerm               = 10705 /*!< SBML L2v3 validation rule #10705 */
  , InvalidConstraintSBOTerm         = 10706 /*!< SBML L2v3 validation rule #10706 */
  , InvalidReactionSBOTerm           = 10707 /*!< SBML L2v3 validation rule #10707 */
  , InvalidSpeciesReferenceSBOTerm   = 10708 /*!< SBML L2v3 validation rule #10708 */
  , InvalidKineticLawSBOTerm         = 10709 /*!< SBML L2v3 validation rule #10709 */
  , InvalidEventSBOTerm              = 10710 /*!< SBML L2v3 validation rule #10710 */
  , InvalidEventAssignmentSBOTerm    = 10711 /*!< SBML L2v3 validation rule #10711 */
  , InvalidCompartmentSBOTerm        = 10712 /*!< SBML L2v3 validation rule #10712 */
  , InvalidSpeciesSBOTerm            = 10713 /*!< SBML L2v3 validation rule #10713 */
  , InvalidCompartmentTypeSBOTerm    = 10714 /*!< SBML L2v3 validation rule #10714 */
  , InvalidSpeciesTypeSBOTerm        = 10715 /*!< SBML L2v3 validation rule #10715 */
  , InvalidTriggerSBOTerm            = 10716 /*!< SBML L2v3 validation rule #10716 */
  , InvalidDelaySBOTerm              = 10717 /*!< SBML L2v3 validation rule #10717 */
  , NotesNotInXHTMLNamespace         = 10801 /*!< SBML L2v3 validation rule #10801 */
  , NotesContainsXMLDecl             = 10802 /*!< SBML L2v3 validation rule #10802 */
  , NotesContainsDOCTYPE             = 10803 /*!< SBML L2v3 validation rule #10803 */
  , InvalidNotesContent              = 10804 /*!< SBML L2v3 validation rule #10804 */
  , InvalidNamespaceOnSBML           = 20101 /*!< SBML L2v3 validation rule #20101 */
  , MissingOrInconsistentLevel       = 20102 /*!< SBML L2v3 validation rule #20102 */
  , MissingOrInconsistentVersion     = 20103 /*!< SBML L2v3 validation rule #20103 */
  , AnnotationNotesNotAllowedLevel1  = 20104 /*!< SBML L2v3 validation rule #20141 */
  , MissingModel                     = 20201 /*!< SBML L2v3 validation rule #20201 */
  , IncorrectOrderInModel            = 20202 /*!< SBML L2v3 validation rule #20202 */
  , EmptyListElement                 = 20203 /*!< SBML L2v3 validation rule #20203 */
  , NeedCompartmentIfHaveSpecies     = 20204 /*!< SBML L2v3 validation rule #20204 */
  , FunctionDefMathNotLambda         = 20301 /*!< SBML L2v3 validation rule #20301 */
  , InvalidApplyCiInLambda           = 20302 /*!< SBML L2v3 validation rule #20302 */
  , RecursiveFunctionDefinition      = 20303 /*!< SBML L2v3 validation rule #20303 */
  , InvalidCiInLambda                = 20304 /*!< SBML L2v3 validation rule #20304 */
  , InvalidFunctionDefReturnType     = 20305 /*!< SBML L2v3 validation rule #20305 */
  , InvalidUnitDefId                 = 20401 /*!< SBML L2v3 validation rule #20401 */
  , InvalidSubstanceRedefinition     = 20402 /*!< SBML L2v3 validation rule #20402 */
  , InvalidLengthRedefinition        = 20403 /*!< SBML L2v3 validation rule #20403 */
  , InvalidAreaRedefinition          = 20404 /*!< SBML L2v3 validation rule #20404 */
  , InvalidTimeRedefinition          = 20405 /*!< SBML L2v3 validation rule #20405 */
  , InvalidVolumeRedefinition        = 20406 /*!< SBML L2v3 validation rule #20406 */
  , VolumeLitreDefExponentNotOne     = 20407 /*!< SBML L2v3 validation rule #20407 */
  , VolumeMetreDefExponentNot3       = 20408 /*!< SBML L2v3 validation rule #20408 */
  , EmptyListOfUnits                 = 20409 /*!< SBML L2v3 validation rule #20409 */
  , InvalidUnitKind                  = 20410 /*!< SBML L2v3 validation rule #20410 */
  , OffsetNoLongerValid              = 20411 /*!< SBML L2v3 validation rule #20411 */
  , CelsiusNoLongerValid             = 20412 /*!< SBML L2v3 validation rule #20412 */
  , ZeroDimensionalCompartmentSize   = 20501 /*!< SBML L2v3 validation rule #20501 */
  , ZeroDimensionalCompartmentUnits  = 20502 /*!< SBML L2v3 validation rule #20502 */
  , ZeroDimensionalCompartmentConst  = 20503 /*!< SBML L2v3 validation rule #20503 */
  , UndefinedOutsideCompartment      = 20504 /*!< SBML L2v3 validation rule #20504 */
  , RecursiveCompartmentContainment  = 20505 /*!< SBML L2v3 validation rule #20505 */
  , ZeroDCompartmentContainment      = 20506 /*!< SBML L2v3 validation rule #20506 */
  , Invalid1DCompartmentUnits        = 20507 /*!< SBML L2v3 validation rule #20507 */
  , Invalid2DCompartmentUnits        = 20508 /*!< SBML L2v3 validation rule #20508 */
  , Invalid3DCompartmentUnits        = 20509 /*!< SBML L2v3 validation rule #20509 */
  , InvalidCompartmentTypeRef        = 20510 /*!< SBML L2v3 validation rule #20510 */
  , InvalidSpeciesCompartmentRef     = 20601 /*!< SBML L2v3 validation rule #20601 */
  , HasOnlySubsNoSpatialUnits        = 20602 /*!< SBML L2v3 validation rule #20602 */
  , NoSpatialUnitsInZeroD            = 20603 /*!< SBML L2v3 validation rule #20603 */
  , NoConcentrationInZeroD           = 20604 /*!< SBML L2v3 validation rule #20604 */
  , SpatialUnitsInOneD               = 20605 /*!< SBML L2v3 validation rule #20605 */
  , SpatialUnitsInTwoD               = 20606 /*!< SBML L2v3 validation rule #20606 */
  , SpatialUnitsInThreeD             = 20607 /*!< SBML L2v3 validation rule #20607 */
  , InvalidSpeciesSusbstanceUnits    = 20608 /*!< SBML L2v3 validation rule #20608 */
  , BothAmountAndConcentrationSet    = 20609 /*!< SBML L2v3 validation rule #20609 */
  , NonBoundarySpeciesAssignedAndUsed= 20610 /*!< SBML L2v3 validation rule #20610 */
  , NonConstantSpeciesUsed           = 20611 /*!< SBML L2v3 validation rule #20611 */
  , InvalidSpeciesTypeRef            = 20612 /*!< SBML L2v3 validation rule #20612 */
  , MultSpeciesSameTypeInCompartment = 20613 /*!< SBML L2v3 validation rule #20613 */
  , MissingSpeciesCompartment        = 20614 /*!< SBML L2v3 validation rule #20614 */
  , SpatialSizeUnitsRemoved          = 20615 /*!< SBML L2v3 validation rule #20615 */
  , InvalidParameterUnits            = 20701 /*!< SBML L2v3 validation rule #20701 */
  , InvalidInitAssignSymbol          = 20801 /*!< SBML L2v3 validation rule #20801 */
  , MultipleInitAssignments          = 20802 /*!< SBML L2v3 validation rule #20802 */
  , InitAssignmentAndRuleForSameId   = 20803 /*!< SBML L2v3 validation rule #20803 */
  , InvalidAssignRuleVariable        = 20901 /*!< SBML L2v3 validation rule #20901 */
  , InvalidRateRuleVariable          = 20902 /*!< SBML L2v3 validation rule #20902 */
  , AssignmentToConstantEntity       = 20903 /*!< SBML L2v3 validation rule #20903 */
  , RateRuleForConstantEntity        = 20904 /*!< SBML L2v3 validation rule #20904 */
  , RepeatedRule10304                = 20905 /*!< SBML L2v3 validation rule #20905 */
  , CircularRuleDependency           = 20906 /*!< SBML L2v3 validation rule #20906 */
  , ConstraintMathNotBoolean         = 21001 /*!< SBML L2v3 validation rule #21001 */
  , IncorrectOrderInConstraint       = 21002 /*!< SBML L2v3 validation rule #21002 */
  , ConstraintNotInXHTMLNamespace    = 21003 /*!< SBML L2v3 validation rule #21003 */
  , ConstraintContainsXMLDecl        = 21004 /*!< SBML L2v3 validation rule #21004 */
  , ConstraintContainsDOCTYPE        = 21005 /*!< SBML L2v3 validation rule #21005 */
  , InvalidConstraintContent         = 21006 /*!< SBML L2v3 validation rule #21006 */
  , NoReactantsOrProducts            = 21101 /*!< SBML L2v3 validation rule #21101 */
  , IncorrectOrderInReaction         = 21102 /*!< SBML L2v3 validation rule #21102 */
  , EmptyListInReaction              = 21103 /*!< SBML L2v3 validation rule #21103 */
  , InvalidReactantsProductsList     = 21104 /*!< SBML L2v3 validation rule #21104 */
  , InvalidModifiersList             = 21105 /*!< SBML L2v3 validation rule #21105 */
  , InvalidSpeciesReference          = 21111 /*!< SBML L2v3 validation rule #21111 */
  , RepeatedRule20611                = 21112 /*!< SBML L2v3 validation rule #21112 */
  , BothStoichiometryAndMath         = 21113 /*!< SBML L2v3 validation rule #21113 */
  , UndeclaredSpeciesRef             = 21121 /*!< SBML L2v3 validation rule #21121 */
  , IncorrectOrderInKineticLaw       = 21122 /*!< SBML L2v3 validation rule #21122 */
  , EmptyListInKineticLaw            = 21123 /*!< SBML L2v3 validation rule #21123 */
  , NonConstantLocalParameter        = 21124 /*!< SBML L2v3 validation rule #21124 */
  , SubsUnitsNoLongerValid           = 21125 /*!< SBML L2v3 validation rule #21125 */
  , TimeUnitsNoLongerValid           = 21126 /*!< SBML L2v3 validation rule #21126 */
  , UndeclaredSpeciesInStoichMath    = 21131 /*!< SBML L2v3 validation rule #21131 */
  , MissingTriggerInEvent            = 21201 /*!< SBML L2v3 validation rule #21201 */
  , TriggerMathNotBoolean            = 21202 /*!< SBML L2v3 validation rule #21202 */
  , MissingEventAssignment           = 21203 /*!< SBML L2v3 validation rule #21203 */
  , TimeUnitsEvent                   = 21204 /*!< SBML L2v3 validation rule #21204 */
  , IncorrectOrderInEvent            = 21205 /*!< SBML L2v3 validation rule #21205 */
  , TimeUnitsRemoved                 = 21206 /*!< SBML L2v3 validation rule #21206 */
  , InvalidEventAssignmentVariable   = 21211 /*!< SBML L2v3 validation rule #21211 */
  , EventAssignmentForConstantEntity = 21212 /*!< SBML L2v3 validation rule #21212 */

  , GeneralWarningNotSpecified       = 29999

  // undecided on number for this FIXME
  , CompartmentShouldHaveSize        = 50501
    // Lower bound for additional error codes returned by libSBML but not
    // defined in SBML specifications.

  , LibSBMLAdditionalCodesLowerBound = 90000

  , CannotConvertToL1V1              = 90001

  //L1Compatability
  , NoEventsInL1		                 = 91001
  , NoFunctionDefinitionsInL1	       = 91002
  , NoConstraintsInL1		             = 91003
  , NoInitialAssignmentsInL1	       = 91004
  , NoSpeciesTypesInL1		           = 91005
  , NoCompartmentTypeInL1	           = 91006
  , NoNon3DComparmentsInL1	         = 91007
  , NoFancyStoichiometryMathInL1     = 91008
  , NoNonIntegerStoichiometryInL1    = 91009
  , NoUnitMultipliersOrOffsetsInL1   = 91010
  , SpeciesCompartmentRequiredInL1   = 91011
  , NoSpeciesSpatialSizeUnitsInL1    = 91012
  , NoSBOTermsInL1		               = 91013

  //L2v1 compatability
  , NoConstraintsInL2v1		           = 92001
  , NoInitialAssignmentsInL2v1	     = 92002
  , NoSpeciesTypeInL2v1		           = 92003
  , NoCompartmentTypeInL2v1	         = 92004
  , NoSBOTermsInL2v1		             = 92005
  , NoIdOnSpeciesReferenceInL2v1     = 92006

  //l2v2 compatability
  , SBOTermNotUniversalInL2v2	       = 93001
  , NoUnitOffsetInL2v2		           = 93002
  , NoKineticLawTimeUnitsInL2v2	     = 93003
  , NoKineticLawSubstanceUnitsInL2v2 = 93004

  //l2v3 compatability 
  , NoUnitOffsetInL2v3		           = 94001
  , NoKineticLawTimeUnitsInL2v3	     = 94002
  , NoKineticLawSubstanceUnitsInL2v3 = 94003
  , NoSpeciesSpatialSizeUnitsInL2v3  = 94004
  , NoEventTimeUnitsInL2v3	         = 94005

  // These are errors checked by libSBML that were never
  // published in a spec

  , SubsUnitsAllowedInKL             = 99127
  , TimeUnitsAllowedInKL             = 99128
  , FormulaInLevel1KL                = 99129

  // These are internal errors that reverts to 10501

    /** @cond doxygen-libsbml-internal */
  , InconsistentArgUnitsWarnings     = 99502 /*!< SBML L2v3 validation rule #10501 */
  , InconsistentPowerUnitsWarnings   = 99503 /*!< SBML L2v3 validation rule #10501 */
  , InconsistentExponUnitsWarnings   = 99504 /*!< SBML L2v3 validation rule #10501 */
    /** @endcond doxygen-libsbml-internal */
  , BadMathML                        = 99219

    // Bounds
  , SBMLCodesUpperBound              = 99999 /*!< 99999, the upper bound of
					      * all libSBML codes.
					      * Application-specific codes
					      * should begin at 100000. */
  };


  /**
   * Category codes for SBMLError diagnostics.
   *
   * Note that these are distinct from XMLError's category codes.  User
   * programs receiving an SBMLError object can use this distinction to
   * check whether the error represents a low-level XML problem or an
   * SBML problem.
   *
   * @see SBMLError
   */
  enum SBMLCategory
  {
    Internal                  = XMLError::Internal
  , System                    = XMLError::System
  , XML                       = XMLError::XML
  , SBML                      = 3  /*!< General SBML error. */
  , SBMLL1Compatibility       = 4  /*!< Error in converting to SBML Level 1. */
  , SBMLL2v1Compatibility     = 5  /*!< Error in converting to SBML L1V1. */
  , SBMLL2v2Compatibility     = 6  /*!< Error in converting to SBML L1V2. */
  , SBMLConsistency           = 7  /*!< Error in validating SBML consistency.*/
  , SBMLConsistencyIdentifier = 8  /*!< Error in validating identifiers. */
  , SBMLConsistencyUnits      = 9  /*!< Error in validating units. */
  , SBMLConsistencyMathML     = 10 /*!< Error in validating MathML. */
  , SBMLConsistencySBO        = 11 /*!< Error in validation SBO. */
  , SBMLOverdetermined        = 12 /*!< Error in equations of model. */
  , SBMLL2v3Compatibility     = 13 /*!< Error in converting to SBML L2V3. */
  , SBMLModelingPractice      = 14
  };


  /**
   * Severity codes for SBMLError diagnostics.
   *
   * @see SBMLError
   */
  enum SBMLSeverity
  {
    Info    = XMLError::Info    /*!< The error is actually informational
                                 * and not necessarily a serious problem. */

  , Warning = XMLError::Warning /*!< The error object represents a 
                                 * problem that is not serious enough to
                                 * necessarily stop the parser, but
                                 * applications should take note of the
                                 * problem and evaluate what its
                                 * implications may be. */

  , Error   = XMLError::Error   /*!< The error object represents a serious
                                 * problem.  The application may continue
                                 * running but it is unlikely to be able to
                                 * continue processing the same XML file or
                                 * data stream. */

  , Fatal   = XMLError::Fatal   /*!< A unrecoverable error occurred, such as
                                 * an out-of-memory condition, and the
                                 * software should terminate
                                 * immediately. */

  /** @cond doxygen-libsbml-internal **/

  /* The following are used internally in SBMLErrorTable, but publicly,
   * we only report one of the 4 categories above.  Translation of the
   * codes is done in SBMLError.cpp.
   */

  , SchemaError                 /*!< The XML content does not conform to
                                 * the relevant version of the SBML XML 
                                 * Schema.  The content is not valid SBML. */

  , GeneralWarning              /*!< The XML content is invalid for some
                                 * levels/versions of SBML, and while it
                                 * may be valid in others, it is something
                                 * that is best avoided anyway.  LibSBML
                                 * will issue warnings in those cases it
                                 * can recognize. */

  , NotApplicable               /*!< This error code is only a placeholder
				 * for errors that have relevance to some
				 * versions of SBML but not others. */

  /** @endcond doxygen-libsbml-internal **/

  };


  /**
   * Creates a new SBMLError to report that something occurred during SBML
   * processing.
   *
   * SBMLError objects have identification numbers to indicate the nature
   * of the exception.  These numbers are drawn from the enumeration
   * SBMLError::Code.  The argument @p errorId to this constructor @em can
   * be (but does not have to be) a value from this enumeration.  If it is
   * a value from SBMLError::Code, the SBMLError class assumes it the error
   * is an SBML error and prepends a predefined error message to any string
   * passed in @p details.  In addition, all SBMLError::Code errors have
   * associated severity and category codes, and these fields are filled-in
   * as well from the enumerations SBMLError::Severity and
   * SBMLError::Category, respectively
   *
   * If the error identifier @p errorId is a number greater than 99999, the
   * SBMLError class assumes the error was generated from another part of
   * the software and does not do additional filling in of values beyond
   * the default in the constructor itself.  This allows SBMLError to serve
   * as a base class for other errors, such as for user-defined validation
   * rules (see Validator).  Callers should fill in all the parameters with
   * suitable values if generating errors with codes greater than 99999 to
   * make maximum use of the SBMLError facilities.
   *
   * As mentioned above, there are two other enumerations,
   * SBMLError::Severity and SBMLError::Category, used for indicating the
   * severity and category of error for the predefined SBMLError codes.
   * The values passed in @p severity and @p category override the defaults
   * assigned based on the error code.  If the error identifier is a code
   * number from SBMLError::Code, callers do not need to fill in @p
   * severity and @p category.  Conversely, if @p errorId is not a value
   * from SBMLError::Code, callers can use other values (not just those
   * from SBMLError::Severity and SBMLError::Category, but their own
   * special values) for @p severity and @p category.
   *
   * @param errorId an unsigned int, the identification number of the error.
   *
   * @param level the SBML Level of the SBML model
   *
   * @param version the SBML Version within the Level of the SBML model
   * 
   * @param details a string containing additional details about the error.
   * If the error code in @p errorId is one that is recognized by SBMLError,
   * the given message is @em appended to a predefined message associated
   * with the given code.  If the error code is not recognized, the message
   * is stored as-is as the text of the error.
   * 
   * @param line an unsigned int, the line number at which the error occured.
   * 
   * @param column an unsigned int, the column number at which the error occured.
   * 
   * @param severity an integer indicating severity of the error.
   * 
   * @param category an integer indicating the category to which the error
   * belongs.
   */
  SBMLError
  (
     const unsigned int errorId       = 0
   , const unsigned int level         = 2
   , const unsigned int version       = 3
   , const std::string& details       = ""
   , const unsigned int line          = 0
   , const unsigned int column        = 0
   , const SBMLSeverity severity      = Error
   , const SBMLCategory category      = SBML
  );

};


#endif /* SBMLError_h */
