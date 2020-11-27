  /** @cond doxygenLibsbmlInternal */

/**
 * @file:   MultiMathMLConsistencyConstraints.cpp
 * @brief:  Implementation of the MultiConsistencyConstraints class
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
#include <sbml/packages/multi/common/MultiExtensionTypes.h>


#endif  /* AddingConstrainstToValidator */

#include <sbml/validator/ConstraintMacros.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>

#include "MultiMathCiCheckSpeciesReference.h"
#include "MultiMathCiCheckRepresentationType.h"

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

//************************************
// Rules for extended ci elements in Math objects

// MultiMathCi_AllowedMultiAtts          = 7010201 - caught at read and report error code 10201

// MultiMathCi_SpeRefAtt_Ref             = 7010202
/*!< Math ci element: 'speciesReference' must be the 'id' of a speciesReference */
EXTERN_CONSTRAINT (MultiMathCi_SpeRefAtt_Ref, MultiMathCiCheckSpeciesReference)

// MultiMathCi_RepTypAtt_Ref             = 7010203
/*!< Math ci element: 'representationType' must be a value of the Multi data type 'RepresentationType' */
EXTERN_CONSTRAINT (MultiMathCi_RepTypAtt_Ref, MultiMathCiCheckRepresentationType)

  /** @endcond */


