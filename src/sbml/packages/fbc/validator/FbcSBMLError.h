/**
 * @file    FbcSBMLError.h
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
 * @class FbcSBMLError
 * @ingroup fbc
 * @brief @htmlinclude pkg-marker-fbc.html
 * Representaton of errors, warnings and other diagnostics for the 'fbc'
 * package.
 */

#ifndef FbcSBMLError_h
#define FbcSBMLError_h

#include <sbml/common/extern.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLNamespaces.h>


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
  FbcUnknown                           = 2010100 
, FbcNSUndeclared                      = 2010101 
, FbcElementNotInNs                    = 2010102
, FbcDuplicateComponentId              = 2010301
, FbcSBMLSIdSyntax                     = 2010302

, FbcAttributeRequiredMissing          = 2020101
, FbcAttributeRequiredMustBeBoolean    = 2020102
, FbcRequiredFalse                     = 2020103

, FbcOnlyOneEachListOf                 = 2020201
, FbcNoEmptyListOfs                    = 2020202
, FbcLOFluxBoundsAllowedElements       = 2020203
, FbcLOObjectivesAllowedElements       = 2020204
, FbcLOFluxBoundsAllowedAttributes     = 2020205
, FbcLOObjectivesAllowedAttributes     = 2020206
, FbcActiveObjectiveSyntax             = 2020207
, FbcActiveObjectiveRefersObjective    = 2020208

, FbcSpeciesAllowedL3Attributes        = 2020301
, FbcSpeciesChargeMustBeInteger        = 2020302
, FbcSpeciesFormulaMustBeString        = 2020303

, FbcFluxBoundAllowedL3Attributes      = 2020401
, FbcFluxBoundAllowedElements          = 2020402
, FbcFluxBoundRequiredAttributes       = 2020403
, FbcFluxBoundRectionMustBeSIdRef      = 2020404
, FbcFluxBoundNameMustBeString         = 2020405
, FbcFluxBoundOperationMustBeEnum      = 2020406
, FbcFluxBoundValueMustBeDouble        = 2020407
, FbcFluxBoundReactionMustExist        = 2020408
, FbcFluxBoundsForReactionConflict     = 2020409

, FbcObjectiveAllowedL3Attributes      = 2020501
, FbcObjectiveAllowedElements          = 2020502
, FbcObjectiveRequiredAttributes       = 2020503
, FbcObjectiveNameMustBeString         = 2020504
, FbcObjectiveTypeMustBeEnum           = 2020505
, FbcObjectiveOneListOfObjectives      = 2020506
, FbcObjectiveLOFluxObjMustNotBeEmpty  = 2020507
, FbcObjectiveLOFluxObjOnlyFluxObj     = 2020508
, FbcObjectiveLOFluxObjAllowedAttribs  = 2020509

, FbcFluxObjectAllowedL3Attributes     = 2020601
, FbcFluxObjectAllowedElements         = 2020602
, FbcFluxObjectRequiredAttributes      = 2020603
, FbcFluxObjectNameMustBeString        = 2020604
, FbcFluxObjectReactionMustBeSIdRef    = 2020605
, FbcFluxObjectReactionMustExist       = 2020606
, FbcFluxObjectCoefficientMustBeDouble = 2020607

} FbcSBMLErrorCode_t;


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END



#endif /* SBMLError_h */
