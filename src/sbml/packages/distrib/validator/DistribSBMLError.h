/**
 * @file DistribSBMLError.h
 * @brief Definition of the DistribSBMLError class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#ifndef DistribSBMLError_H__
#define DistribSBMLError_H__




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * @enum DistribSBMLErrorCode_t
 * Codes for all SBML-level errors and warnings from the 'distrib' package.
 *
 * These are distinguished from other SBML error codes by having a number
 * between 1500000 and 1599999.
 *
 * @copydetails doc_sbml_error_code_ranges
 */
typedef enum
{
  DistribUnknown                                              = 1510100
, DistribNSUndeclared                                         = 1510101
, DistribElementNotInNs                                       = 1510102
, DistribDuplicateComponentId                                 = 1510301
, DistribIdSyntaxRule                                         = 1510302
, DistribAttributeRequiredMissing                             = 1520101
, DistribAttributeRequiredMustBeBoolean                       = 1520102
, DistribAttributeRequiredMustHaveValue                       = 1520103
, DistribSBaseAllowedElements                                 = 1520201
, DistribDistributionAllowedCoreAttributes                    = 1520301
, DistribDistributionAllowedCoreElements                      = 1520302
, DistribDistributionAllowedElements                          = 1520303
, DistribUncertValueAllowedCoreAttributes                     = 1520401
, DistribUncertValueAllowedCoreElements                       = 1520402
, DistribUncertValueAllowedAttributes                         = 1520403
, DistribUncertValueValueMustBeDouble                         = 1520404
, DistribUncertValueVarMustBeSBase                            = 1520405
, DistribUncertValueUnitsMustBeUnitSId                        = 1520406
, DistribExternalParameterAllowedCoreAttributes               = 1520501
, DistribExternalParameterAllowedCoreElements                 = 1520502
, DistribExternalParameterAllowedAttributes                   = 1520503
, DistribExternalParameterAllowedElements                     = 1520504
, DistribExternalParameterDefinitionURLMustBeString           = 1520505
, DistribExternalParameterLOExternalParametersAllowedCoreElements= 1520506
, DistribExternalParameterLOExternalParametersAllowedCoreAttributes= 1520507
, DistribExternalParameterLOExternalParametersAllowedAttributes= 1520508
, DistribLOExternalParametersNameMustBeString                 = 1520509
, DistribUncertaintyAllowedCoreAttributes                     = 1520601
, DistribUncertaintyAllowedCoreElements                       = 1520602
, DistribUncertaintyAllowedElements                           = 1520603
, DistribUncertaintyLOExternalParametersAllowedCoreElements   = 1520604
, DistribUncertaintyLOExternalParametersAllowedCoreAttributes = 1520605
, DistribUncertaintyLOExternalParametersAllowedAttributes     = 1520606
, DistribUncertStatisticSpanAllowedCoreAttributes             = 1520701
, DistribUncertStatisticSpanAllowedCoreElements               = 1520702
, DistribUncertStatisticSpanAllowedAttributes                 = 1520703
, DistribUncertStatisticSpanVarLowerMustBeSBase               = 1520704
, DistribUncertStatisticSpanValueLowerMustBeDouble            = 1520705
, DistribUncertStatisticSpanVarUpperMustBeSBase               = 1520706
, DistribUncertStatisticSpanValueUpperMustBeDouble            = 1520707
, DistribDistribBaseAllowedCoreAttributes                     = 1520801
, DistribDistribBaseAllowedCoreElements                       = 1520802
, DistribDistribBaseAllowedAttributes                         = 1520803
, DistribDistribBaseNameMustBeString                          = 1520804
} DistribSBMLErrorCode_t;


END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !DistribSBMLError_H__ */


