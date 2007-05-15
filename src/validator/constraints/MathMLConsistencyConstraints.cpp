/**
 * @file    MathMLConsistencyConstraints.cpp
 * @brief   MathMLConsistency check constraints.  See SBML Wiki
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#ifndef AddingConstraintsToValidator

//#include <string>

#include <sbml/validator/VConstraint.h>
#include "LogicalArgsMathCheck.h"
#include "NumericArgsMathCheck.h"
#include "PieceBooleanMathCheck.h"
#include "PiecewiseValueMathCheck.h"
#include "EqualityArgsMathCheck.h"
#include "FunctionApplyMathCheck.h"
#include "CiElementMathCheck.h"
#include "LambdaMathCheck.h"
#include "NumericReturnMathCheck.h"
#include "LocalParameterMathCheck.h"
#include "NumberArgsMathCheck.h"


#endif


#include <sbml/validator/ConstraintMacros.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


// General XML validation

// 10101: utf-8 - caught at read
// 10102: undfeined element - caught at read
// 10103: schema conformance - caught in various places

//General MathML validation

// 10201: namespace - caught at read
// 10202: elements - caught at read
// 10203: encoding - caught at read
// 10204: url - caught at read
// 10205: time/delay url - caught at read
// 10206: type - caught at read
// 10207: values for type - caught at read

EXTERN_CONSTRAINT( 10208, LambdaMathCheck        )
EXTERN_CONSTRAINT( 10209, LogicalArgsMathCheck   )
EXTERN_CONSTRAINT( 10210, NumericArgsMathCheck   )
EXTERN_CONSTRAINT( 10211, EqualityArgsMathCheck  )
EXTERN_CONSTRAINT( 10212, PiecewiseValueMathCheck)
EXTERN_CONSTRAINT( 10213, PieceBooleanMathCheck  )
EXTERN_CONSTRAINT( 10214, FunctionApplyMathCheck )
EXTERN_CONSTRAINT( 10215, CiElementMathCheck     )
EXTERN_CONSTRAINT( 10216, LocalParameterMathCheck)
EXTERN_CONSTRAINT( 10217, NumericReturnMathCheck )
EXTERN_CONSTRAINT( 10218, NumberArgsMathCheck )


