/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    MathMLConsistencyConstraintsDeclared.cxx
 * @brief   Declarations of constraints
 * @author  SBML Team
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
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
 * ---------------------------------------------------------------------- -->*/

addConstraint(new LambdaMathCheck(10208, *this));

addConstraint(new LogicalArgsMathCheck(10209, *this));

addConstraint(new NumericArgsMathCheck(10210, *this));

addConstraint(new EqualityArgsMathCheck(10211, *this));

addConstraint(new PiecewiseValueMathCheck(10212, *this));

addConstraint(new PieceBooleanMathCheck(10213, *this));

addConstraint(new FunctionApplyMathCheck(10214, *this));

addConstraint(new CiElementMathCheck(10215, *this));

addConstraint(new LocalParameterMathCheck(10216, *this));

addConstraint(new NumericReturnMathCheck(10217, *this));

addConstraint(new NumberArgsMathCheck(10218, *this));

addConstraint(new FunctionNoArgsMathCheck(10219, *this));

addConstraint(new ValidCnUnitsValue(10221, *this));

addConstraint(new CiElementNot0DComp(10222, *this));

addConstraint(new RateOfCiTargetMathCheck(10223, *this));

addConstraint(new RateOfAssignmentMathCheck(10224, *this));

addConstraint(new RateOfCompartmentMathCheck(10225, *this));

/** @endcond */

