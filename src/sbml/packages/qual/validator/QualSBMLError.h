/**
 * @file    QualSBMLError.h
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
 */

#ifndef QualSBMLError_h
#define QualSBMLError_h



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
  QualUnknown                           = 3010100 
, QualNSUndeclared                      = 3010101 
, QualElementNotInNs                    = 3010102

, QualFunctionTermBool                  = 3010201
, QualMathCSymbolDisallowed             = 3010202

, QualDuplicateComponentId              = 3010301

, QualAttributeRequiredMissing          = 3020101
, QualAttributeRequiredMustBeBoolean    = 3020102
, QualRequiredTrueIfTransitions         = 3020103

, QualOneListOfTransOrQS                = 3020201
, QualEmptyLONotAllowed                 = 3020202
, QualLOTransitiondAllowedElements      = 3020203
, QualLOQualSpeciesAllowedElements      = 3020204
, QualLOQualSpeciesAllowedAttributes    = 3020205
, QualLOTransitionsAllowedAttributes    = 3020206

, QualQualSpeciesAllowedCoreAttributes  = 3020301
, QualQualSpeciesAllowedElements        = 3020302
, QualQualSpeciesAllowedAttributes      = 3020303
, QualConstantMustBeBool                = 3020304
, QualNameMustBeString                  = 3020305
, QualInitialLevelMustBeInt             = 3020306
, QualMaxLevelMustBeInt                 = 3020307
, QualCompartmentMustReferExisting      = 3020308
, QualInitialLevelCannotExceedMax       = 3020309
, QualConstantQSCannotBeOutput          = 3020310
, QualQSAssignedOnlyOnce                = 3020311
, QualInitalLevelNotNegative            = 3020312
, QualMaxLevelNotNegative               = 3020313

, QualTransitionAllowedCoreAttributes   = 3020401
, QualTransitionAllowedElements         = 3020402
, QualTransitionAllowedAttributes       = 3020403
, QualTransitionNameMustBeString        = 3020404
, QualTransitionLOElements              = 3020405
, QualTransitionEmptyLOElements         = 3020406
, QualTransitionLOInputElements         = 3020407
, QualTransitionLOOutputElements        = 3020408
, QualTransitionLOFuncTermElements      = 3020409
, QualTransitionLOInputAttributes       = 3020410
, QualTransitionLOOutputAttributes      = 3020411
, QualTransitionLOFuncTermAttributes    = 3020412
, QualTransitionLOFuncTermExceedMax     = 3020413
, QualTransitionLOFuncTermNegative      = 3020414

, QualInputAllowedCoreAttributes        = 3020501
, QualInputAllowedElements              = 3020502
, QualInputAllowedAttributes            = 3020503
, QualInputNameMustBeString             = 3020504
, QualInputSignMustBeSignEnum           = 3020505
, QualInputTransEffectMustBeInputEffect = 3020506
, QualInputThreshMustBeInteger          = 3020507
, QualInputQSMustBeExistingQS           = 3020508
, QualInputConstantCannotBeConsumed     = 3020509
, QualInputThreshMustBeNonNegative      = 3020510

, QualOutputAllowedCoreAttributes       = 3020601
, QualOutputAllowedElements             = 3020602
, QualOutputAllowedAttributes           = 3020603
, QualOutputNameMustBeString            = 3020604
, QualOutputTransEffectMustBeOutput     = 3020605
, QualOutputLevelMustBeInteger          = 3020606
, QualOutputQSMustBeExistingQS          = 3020607
, QualOutputConstantMustBeFalse         = 3020608
, QualOutputProductionMustHaveLevel     = 3020609
, QualOutputLevelMustBeNonNegative      = 3020610

, QualDefaultTermAllowedCoreAttributes  = 3020701
, QualDefaultTermAllowedElements        = 3020702
, QualDefaultTermAllowedAttributes      = 3020703
, QualDefaultTermResultMustBeInteger    = 3020704
, QualDefaultTermResultMustBeNonNeg     = 3020705

, QualFuncTermAllowedCoreAttributes     = 3020801
, QualFuncTermAllowedElements           = 3020802
, QualFuncTermAllowedAttributes         = 3020803
, QualFuncTermOnlyOneMath               = 3020804
, QualFuncTermResultMustBeInteger       = 3020805
, QualFuncTermResultMustBeNonNeg        = 3020806

} QualSBMLErrorCode_t;


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END



#endif /* SBMLError_h */
