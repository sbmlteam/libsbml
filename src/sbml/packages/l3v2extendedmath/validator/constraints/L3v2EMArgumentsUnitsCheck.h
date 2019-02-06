/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    L3v2EMArgumentsUnitsCheck.h
 * @brief   Ensures units consistent with math
 * @author  Sarah Keating
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

#ifndef L3v2EMArgumentsUnitsCheck_h
#define L3v2EMArgumentsUnitsCheck_h


#ifdef __cplusplus


#include <string>
#include <sstream>
#include <math.h>

#include <sbml/validator/VConstraint.h>

#include <sbml/validator/constraints/ArgumentsUnitsCheck.h>


LIBSBML_CPP_NAMESPACE_BEGIN

class ASTNode;


class L3v2EMArgumentsUnitsCheck: public ArgumentsUnitsCheck
{
public:

  /**
   * Creates a new Constraint with the given @p id.
   */
  L3v2EMArgumentsUnitsCheck (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~L3v2EMArgumentsUnitsCheck ();

  ///**
  // * Checks that the units of the function are consistent
  // * for a function returning value with same units as argument(s)
  // *
  // * If inconsistent units are found, an error message is logged.
  // */
  //void checkSameUnitsAsArgs (const Model& m, const ASTNode& node, 
  //                            const SBase & sb, bool inKL, int reactNo);


protected:

  /**
   * Checks that the units of the ASTnode 
   * are appropriate for the function being performed
   *
   * If inconsistent units are found, an error message is logged.
   */
  virtual void checkUnits (const Model& m, const ASTNode& node, const SBase & sb,
    bool inKL = false, int reactNo = -1);
  

  ///**
  //* Logs a message about a function that should return same units
  //* as the arguments
  //*/
  //void logInconsistentSameUnits (const ASTNode & node, const SBase & sb);

};


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* ArgumentsUnitsCheck_h */
/** @endcond */

