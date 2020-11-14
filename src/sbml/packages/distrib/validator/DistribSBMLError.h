/**
 * @file DistribSBMLError.h
 * @brief Definition of the DistribSBMLError class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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
, DistribSBaseLOUncertaintiesAllowedCoreElements              = 1520202
, DistribSBaseLOUncertaintiesAllowedCoreAttributes            = 1520203
, DistribUncertParameterAllowedCoreAttributes                 = 1520301
, DistribUncertParameterAllowedCoreElements                   = 1520302
, DistribUncertParameterAllowedAttributes                     = 1520303
, DistribUncertParameterAllowedElements                       = 1520304
, DistribUncertParameterValueMustBeDouble                     = 1520305
, DistribUncertParameterVarMustBeSBase                        = 1520306
, DistribUncertParameterUnitsMustBeUnitSId                    = 1520307
, DistribUncertParameterTypeMustBeUncertTypeEnum              = 1520308
, DistribUncertParameterDefinitionURLMustBeString             = 1520309
, DistribUncertParameterLOUncertParametersAllowedCoreElements = 1520310
, DistribUncertParameterLOUncertParametersAllowedCoreAttributes= 1520311
, DistribUncertaintyAllowedCoreAttributes                     = 1520401
, DistribUncertaintyAllowedCoreElements                       = 1520402
, DistribUncertaintyAllowedElements                           = 1520403
, DistribUncertSpanAllowedCoreAttributes                      = 1520501
, DistribUncertSpanAllowedCoreElements                        = 1520502
, DistribUncertSpanAllowedAttributes                          = 1520503
, DistribUncertSpanVarLowerMustBeSBase                        = 1520504
, DistribUncertSpanValueLowerMustBeDouble                     = 1520505
, DistribUncertSpanVarUpperMustBeSBase                        = 1520506
, DistribUncertSpanValueUpperMustBeDouble                     = 1520507
, DistribDistribBaseAllowedCoreAttributes                     = 1520601
, DistribDistribBaseAllowedCoreElements                       = 1520602
, DistribDistribBaseAllowedAttributes                         = 1520603
, DistribDistribBaseNameMustBeString                          = 1520604
} DistribSBMLErrorCode_t;


END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !DistribSBMLError_H__ */


