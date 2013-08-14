/**
 * @file    CompSBMLError.h
 * @brief   Represents SBML errors and other diagnostics
 * @author  Michael Hucka
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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
 *
 * @class CompSBMLError
 * @ingroup Comp
 * @brief @htmlinclude pkg-marker-comp.html
 * Representaton of errors, warnings and other diagnostics for the 'comp'
 * package.
 */

#ifndef CompSBMLError_h
#define CompSBMLError_h


LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Codes for all SBML-level errors and warnings.
 *
 * These are distinguished from the XML layer (LIBLAX) error codes by being
 * numbered > 10000, while the XML layer's codes are < 9999.  Calling
 * programs may wish to check whether a given SBMLError object's error
 * identifier is actually from SBMLErrorCode_t or XMLError::XMLErrorCode_t.
 * This distinction corresponds to whether a given error represents a
 * low-level XML problem or an SBML problem.
 */
typedef enum
{
  CompUnknown                           = 1010100 
, CompNSUndeclared                      = 1010101 
, CompElementNotInNs                    = 1010102

, CompDuplicateComponentId              = 1010301
, CompUniqueModelIds                    = 1010302
, CompUniquePortIds                     = 1010303
, CompInvalidSIdSyntax                  = 1010304 
, CompInvalidSubmodelRefSyntax          = 1010308 
, CompInvalidDeletionSyntax             = 1010309
, CompInvalidConversionFactorSyntax     = 1010310
, CompInvalidNameSyntax                 = 1010311

, CompOneListOfReplacedElements         = 1020101
, CompLOReplaceElementsAllowedElements  = 1020102
, CompLOReplacedElementsAllowedAttribs  = 1020103
, CompEmptyLOReplacedElements           = 1020104
, CompOneReplacedByElement              = 1020105

, CompAttributeRequiredMissing          = 1020201
, CompAttributeRequiredMustBeBoolean    = 1020202
, CompRequiredTrueIfElementsRemain      = 1020203
, CompRequiredFalseIfAllElementsReplaced= 1020204
, CompOneListOfModelDefinitions         = 1020205
, CompEmptyLOModelDefs                  = 1020206
, CompLOModelDefsAllowedElements        = 1020207
, CompLOExtModelDefsAllowedElements     = 1020208
, CompLOModelDefsAllowedAttributes      = 1020209
, CompLOExtModDefsAllowedAttributes     = 1020210
, CompOneListOfExtModelDefinitions      = 1020211
, CompAttributeRequiredMustBeTrue       = 1020212

, CompExtModDefAllowedCoreAttributes    = 1020301
, CompExtModDefAllowedElements          = 1020302
, CompExtModDefAllowedAttributes        = 1020303
, CompReferenceMustBeL3                 = 1020304
, CompModReferenceMustIdOfModel         = 1020305
, CompExtModMd5DoesNotMatch             = 1020306
, CompInvalidSourceSyntax               = 1020307
, CompInvalidModelRefSyntax             = 1020308
, CompInvalidMD5Syntax                  = 1020309
, CompCircularExternalModelReference    = 1020310

, CompOneListOfOnModel                  = 1020501
, CompNoEmptyListOfOnModel              = 1020502
, CompLOSubmodelsAllowedElements        = 1020503
, CompLOPortsAllowedElements            = 1020504
, CompLOSubmodelsAllowedAttributes      = 1020505
, CompLOPortsAllowedAttributes          = 1020506

, CompSubmodelAllowedCoreAttributes     = 1020601
, CompSubmodelAllowedElements           = 1020602
, CompOneListOfDeletionOnSubmodel       = 1020603
, CompSubmodelNoEmptyLODeletions        = 1020604
, CompLODeletionsAllowedElements        = 1020605
, CompLODeletionAllowedAttributes       = 1020606
, CompSubmodelAllowedAttributes         = 1020607
, CompModReferenceSyntax                = 1020608
, CompInvalidTimeConvFactorSyntax       = 1020613
, CompInvalidExtentConvFactorSyntax     = 1020614
, CompSubmodelMustReferenceModel        = 1020615
, CompSubmodelCannotReferenceSelf       = 1020616
, CompModCannotCircularlyReferenceSelf  = 1020617
, CompTimeConversionMustBeParameter     = 1020622
, CompExtentConversionMustBeParameter   = 1020623

, CompPortRefMustReferencePort          = 1020701
, CompIdRefMustReferenceObject          = 1020702
, CompUnitRefMustReferenceUnitDef       = 1020703
, CompMetaIdRefMustReferenceObject      = 1020704
, CompParentOfSBRefChildMustBeSubmodel  = 1020705
, CompInvalidPortRefSyntax              = 1020706
, CompInvalidIdRefSyntax                = 1020707
, CompInvalidUnitRefSyntax              = 1020708
, CompInvalidMetaIdRefSyntax            = 1020709
, CompOneSBaseRefOnly                   = 1020710
, CompDeprecatedSBaseRefSpelling        = 1020711
, CompSBaseRefMustReferenceObject       = 1020712
, CompSBaseRefMustReferenceOnlyOneObject= 1020713
, CompNoMultipleReferences              = 1020714

, CompPortMustReferenceObject           = 1020801
, CompPortMustReferenceOnlyOneObject    = 1020802
, CompPortAllowedAttributes             = 1020803
, CompPortReferencesUnique              = 1020804 

, CompDeletionMustReferenceObject       = 1020901
, CompDeletionMustReferOnlyOneObject    = 1020902
, CompDeletionAllowedAttributes         = 1020903

, CompReplacedElementMustRefObject      = 1021001
, CompReplacedElementMustRefOnlyOne     = 1021002
, CompReplacedElementAllowedAttributes  = 1021003
, CompReplacedElementSubModelRef        = 1021004
, CompReplacedElementDeletionRef        = 1021005
, CompReplacedElementConvFactorRef      = 1021006
, CompReplacedElementSameReference      = 1021010

, CompReplacedByMustRefObject           = 1021101
, CompReplacedByMustRefOnlyOne          = 1021102
, CompReplacedByAllowedAttributes       = 1021103
, CompReplacedBySubModelRef             = 1021104

, CompMustReplaceSameClass              = 1021201
, CompMustReplaceIDs                    = 1021202
, CompMustReplaceMetaIDs                = 1021203
, CompMustReplacePackageIDs             = 1021204
, CompReplacedUnitsShouldMatch          = 1021205

, CompUnresolvedReference               = 1090101
, CompNoModelInReference                = 1090102
, CompExtModDefBad                      = 1090103
, CompModelFlatteningFailed             = 1090104
, CompFlatModelNotValid                 = 1090105
, CompLineNumbersUnreliable             = 1090106

, CompFlatteningNotRecognisedReqd       = 1090107
, CompFlatteningNotRecognisedNotReqd    = 1090108
, CompFlatteningNotImplementedNotReqd   = 1090109
, CompFlatteningNotImplementedReqd      = 1090110
, CompFlatteningWarning                 = 1090111

} CompSBMLErrorCode_t;


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END



#endif /* SBMLError_h */
