/**
 * @file ArraysSBMLError.h
 * @brief Definition of the ArraysSBMLError class.
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
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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


#ifndef ArraysSBMLError_H__
#define ArraysSBMLError_H__




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


typedef enum
{
  ArraysUnknown                                               = 8010100
, ArraysNSUndeclared                                          = 8010101
, ArraysElementNotInNs                                        = 8010102
, ArraysAttributeRequiredMissing							  = 8010201
, ArraysAttributeRequiredMustBeBoolean						  = 8010202
, ArraysAttributeRequiredMustHaveValue						  = 8010203
, arrays_8010204  = 8010204
, arrays_8010205  = 8010205
, arrays_8010206  = 8010206
, arrays_8010207  = 8010207
, arrays_8010208  = 8010208
, arrays_8010209  = 8010209
, arrays_8010210  = 8010210
, arrays_8010211  = 8010211
, arrays_8010212  = 8010212
, arrays_8010213  = 8010213
, ArraysDuplicateComponentId								  = 8010301
, ArraysIdSyntaxRule										  = 8010302
, arrays_8020101 = 8020101
, ArraysSBaseLODimensionsAllowedCoreElements                  = 8020102
, arrays_8020103 = 8020103
, arrays_8020104 = 8020104
, ArraysSBaseLODimensionsAllowedCoreAttributes				  = 8020105
, arrays_8020106 = 8020106
, arrays_8020107 = 8020107
, arrays_8020108 = 8020108
, arrays_8020109 = 8020109
, ArraysSBaseLOIndicesAllowedCoreElements					  = 8020110
, arrays_8020111 = 8020111
, arrays_8020112 = 8020112
, ArraysSBaseLOIndicesAllowedCoreAttributes                   = 8020113
, arrays_8020114 = 8020114
, arrays_8020115 = 8020115
, arrays_8020116 = 8020116
, ArraysSBaseAllowedElements = 8020117 // SK need to look at this
, ArraysDimensionAllowedCoreAttributes						  = 8020201
, ArraysDimensionAllowedAttributes							  = 8020202
, ArraysDimensionArrayDimensionMustBeUnInteger				  = 8020203
, ArraysDimensionSizeMustBeSBase							  = 8020204
, arrays_8020205 = 8020205
, ArraysIndexAllowedCoreAttributes                            = 8020301
, ArraysIndexAllowedAttributes								  = 8020302
, ArraysIndexReferencedAttributeMustBeString				  = 8020303
, ArraysIndexArrayDimensionMustBeUnInteger					  = 8020304
, arrays_8020305 = 8020305
, ArraysIndexAllowedElements								  = 8020306
, arrays_8020307 = 8020307
, arrays_8020308 = 8020308
, ArraysIndexAllowedCoreElements                              = 8020309
, ArraysDimensionAllowedCoreElements                          = 8020402
, ArraysDimensionNameMustBeString                             = 8020403
} ArraysSBMLErrorCode_t;


END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !ArraysSBMLError_H__ */


