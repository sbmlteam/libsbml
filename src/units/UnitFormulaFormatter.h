/**
 * \file    UnitFormulaFormatter.h
 * \brief   Formats an AST formula tree as a unit definition
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Sarah Keating
 *
 *     The SBML Team
 *     Science and Technology Research Institute
 *     University of Hertfordshire
 *     Hatfield, UK
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef UnitFormulaFormatter_h
#define UnitFormulaFormatter_h


#include "common/extern.h"
#include "util/StringBuffer.h"
#include "sbml/UnitDefinition.h"
#include "sbml/Unit.h"
#include "sbml/Model.h"
#include "sbml/FunctionDefinition.h"
#include "sbml/Compartment.h"
#include "sbml/Species.h"
#include "sbml/Parameter.h"
#include "sbml/Reaction.h"
#include "sbml/KineticLaw.h"

#include "math/ASTNode.h"

#include "Utils_UnitDefinition.h"

#ifdef __cplusplus

class UnitFormulaFormatter
{
public:
  /** 
   * constructor
   */
  LIBSBML_EXTERN
  UnitFormulaFormatter(const Model *);


  /** 
   * destructor
   */
  LIBSBML_EXTERN
  ~UnitFormulaFormatter();



  /**
   * visits the ASTNode and returns the unitDefinition of the formula
   * this function is really a dispatcher to the other
   * getUnitdefinition functions
   */
  LIBSBML_EXTERN
  UnitDefinition * getUnitDefinition(const ASTNode *);

  /** 
   * returns the unitDefinition for the ASTNode from a function
   */
  UnitDefinition * getUnitDefinitionFromFunction(const ASTNode *);

  /** 
   * returns the unitDefinition for the ASTNode from a times function
   */
  UnitDefinition * getUnitDefinitionFromTimes(const ASTNode *);

  /** 
   * returns the unitDefinition for the ASTNode from a divide function
   */
  UnitDefinition * getUnitDefinitionFromDivide(const ASTNode *);

  /** 
   * returns the unitDefinition for the ASTNode from a power function
   */
  UnitDefinition * getUnitDefinitionFromPower(const ASTNode *);

  /** 
   * returns the unitDefinition for the ASTNode from 
   * a piecewise function
   */
  UnitDefinition * getUnitDefinitionFromPiecewise(const ASTNode *);

  /** 
   * returns the unitDefinition for the ASTNode from a root function
   */
  UnitDefinition * getUnitDefinitionFromRoot(const ASTNode *);

  /** 
   * returns the unitDefinition for the ASTNode from 
   * a function returning dimensionless value
   */
  UnitDefinition * getUnitDefinitionFromDimensionlessReturnFunction(const ASTNode *);

  /** 
   * returns the unitDefinition for the ASTNode from 
   * a function returning value with same units as argument(s)
   */
  UnitDefinition * getUnitDefinitionFromArgUnitsReturnFunction(const ASTNode *);

  /** 
   * returns the unitDefinition for the ASTNode from 
   * a delay function
   */
  UnitDefinition * getUnitDefinitionFromDelay(const ASTNode * node);

  /** 
   * returns the unitDefinition for the ASTNode from anything else
   */
  UnitDefinition * getUnitDefinitionFromOther(const ASTNode *);

  /** 
    * returns the unitDefinition for the units of the compartment
    */
  LIBSBML_EXTERN
  UnitDefinition * getUnitDefinitionFromCompartment(const Compartment *);

 /** 
  * returns the unitDefinition for the units of the species
  */
  LIBSBML_EXTERN
  UnitDefinition * getUnitDefinitionFromSpecies(const Species *);

  /** 
    * returns the unitDefinition for the units of the parameter
    */
  LIBSBML_EXTERN
  UnitDefinition * getUnitDefinitionFromParameter(const Parameter *);

  /** 
    * returns 1 if the math contains 
    * a parameter that has undeclared units 0 otherwise
    */
  LIBSBML_EXTERN
  unsigned int hasUndeclaredUnits(const ASTNode *);
  

  /** 
    * returns canIgnoreUndeclaredUnits value
    */
  LIBSBML_EXTERN
  unsigned int getCanIgnoreUndeclaredUnits();
  

private:
  const Model * model;
  unsigned int undeclaredUnits;
  unsigned int canIgnoreUndeclaredUnits;

};


#endif  /* !cplusplus */
#endif  /* UnitFormulaFormatter_h */
