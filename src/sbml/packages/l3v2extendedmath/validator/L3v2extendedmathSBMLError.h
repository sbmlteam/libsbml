/**
 * @file L3v2extendedmathSBMLError.h
 * @brief Definition of the L3v2extendedmathSBMLError class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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


#ifndef L3v2extendedmathSBMLError_H__
#define L3v2extendedmathSBMLError_H__




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * @enum L3v2extendedmathSBMLErrorCode_t
 * Codes for all SBML-level errors and warnings from the 'l3v2extendedmath'
 * package.
 *
 * These are distinguished from other SBML error codes by having a number
 * between 1400000 and 1499999.
 *
 * @copydetails doc_sbml_error_code_ranges
 */
typedef enum
{
  L3v2extendedmathUnknown                                     = 1410100
, L3v2extendedmathNSUndeclared                                = 1410101
, L3v2extendedmathElementNotInNs                              = 1410102
, L3v2EMOpsNeedCorrectNumberOfArgs                            = 1410218
, L3v2extendedmathDuplicateComponentId                        = 1410301
, L3v2extendedmathIdSyntaxRule                                = 1410302
, L3v2EMInconsistentArgUnits                                  = 1410501
, L3v2extendedmathAttributeRequiredMissing                    = 1420101
, L3v2extendedmathAttributeRequiredMustBeBoolean              = 1420102
, L3v2extendedmathAttributeRequiredMustHaveValue              = 1420103
} L3v2extendedmathSBMLErrorCode_t;


END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !L3v2extendedmathSBMLError_H__ */


