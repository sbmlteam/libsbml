/**
 * @cond doxygen-libsbml-internal
 *
 * @file    UnitFormulaFormatter.h
 * @brief   Formats an AST formula tree as a unit definition
 * @author  Sarah Keating
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#ifndef UnitFormulaFormatter_h
#define UnitFormulaFormatter_h


#include <sbml/common/extern.h>
#include <sbml/util/StringBuffer.h>

#include <sbml/UnitDefinition.h>
#include <sbml/Unit.h>
#include <sbml/Model.h>
#include <sbml/FunctionDefinition.h>
#include <sbml/Compartment.h>
#include <sbml/Species.h>
#include <sbml/Parameter.h>
#include <sbml/Reaction.h>
#include <sbml/KineticLaw.h>
#include <sbml/Event.h>

#include <sbml/math/ASTNode.h>

#include <sbml/units/Utils_UnitDefinition.h>

#ifdef __cplusplus

class   LIBSBML_EXTERN UnitFormulaFormatter
{
public:

  /**
   * Creates a new UnitFormulaFormatter.
   *
   * @param m pointer to the Model object for which the units
   * are to be derived.
   *
   * @note The UnitFormulaFormatter creates a deep copy of the 
   * Model object.
   */
  UnitFormulaFormatter(const Model * m);


  /**
   * Destroys this UnitFormulaFormatter.
   */
  ~UnitFormulaFormatter();


  /**
   * Visits the ASTNode and returns the unitDefinition of the formula.
   * This function is really a dispatcher to the other
   * getUnitDefinition() methods.
   *
   * @param node the ASTNode for which the unitDefinition is to be 
   * constructed.
   * *param inKL
   *
   * @docnote The native C++ implementation of this method defines a
   * default argument value.  In the documentation generated for different
   * libSBML language bindings, you may or may not see corresponding
   * arguments in the method declarations.  For example, in Java, a default
   * argument is handled by declaring two separate methods, with one of
   * them having the argument and the other one lacking the argument.
   * However, the libSBML documentation will be @em identical for both
   * methods.  Consequently, if you are reading this and do not see an
   * argument even though one is described, please look for descriptions of
   * other variants of this method near where this one appears in the
   * documentation.
   */
  UnitDefinition * getUnitDefinition(const ASTNode * node, 
    bool inKL = false, int reactNo = -1);

  /** 
   * returns the unitDefinition for the ASTNode from a function
   */
  UnitDefinition * getUnitDefinitionFromFunction(const ASTNode *node, 
    bool inKL, int reactNo);

  /** 
   * returns the unitDefinition for the ASTNode from a times function
   */
  UnitDefinition * getUnitDefinitionFromTimes(const ASTNode *node, 
    bool inKL, int reactNo);
  
  /** 
   * returns the unitDefinition for the ASTNode from a divide function
   */
  UnitDefinition * getUnitDefinitionFromDivide(const ASTNode *node, 
    bool inKL, int reactNo);

  /** 
   * returns the unitDefinition for the ASTNode from a power function
   */
  UnitDefinition * getUnitDefinitionFromPower(const ASTNode *node, 
    bool inKL, int reactNo);

  /** 
   * returns the unitDefinition for the ASTNode from 
   * a piecewise function
   */
  UnitDefinition * getUnitDefinitionFromPiecewise(const ASTNode *node, 
    bool inKL, int reactNo);


  /** 
   * returns the unitDefinition for the ASTNode from a root function
   */
  UnitDefinition * getUnitDefinitionFromRoot(const ASTNode *node, 
    bool inKL, int reactNo);

  /** 
   * returns the unitDefinition for the ASTNode from 
   * a function returning dimensionless value
   */
  UnitDefinition * getUnitDefinitionFromDimensionlessReturnFunction(const ASTNode *node, 
    bool inKL, int reactNo);

  /** 
   * returns the unitDefinition for the ASTNode from 
   * a function returning value with same units as argument(s)
   */
  UnitDefinition * getUnitDefinitionFromArgUnitsReturnFunction(const ASTNode *node, 
    bool inKL, int reactNo);

  /** 
   * returns the unitDefinition for the ASTNode from 
   * a delay function
   */
  UnitDefinition * getUnitDefinitionFromDelay(const ASTNode * node, 
    bool inKL, int reactNo);

  /** 
   * returns the unitDefinition for the ASTNode from anything else
   */
  UnitDefinition * getUnitDefinitionFromOther(const ASTNode * node,
    bool inKL, int reactNo);

  /** 
    * returns the unitDefinition for the units of the compartment
    */
  UnitDefinition * getUnitDefinitionFromCompartment(const Compartment *);

 /** 
  * returns the unitDefinition for the units of the species
  */
  UnitDefinition * getUnitDefinitionFromSpecies(const Species *);

  /** 
    * returns the unitDefinition for the units of the parameter
    */
  UnitDefinition * getUnitDefinitionFromParameter(const Parameter *);

  /** 
    * returns the unitDefinition for the time units of the event
    */
  UnitDefinition * getUnitDefinitionFromEventTime(const Event * event);

  /** 
    * returns canIgnoreUndeclaredUnits value
    */
  unsigned int getCanIgnoreUndeclaredUnits();

  /**
   * returns the undeclaredUnits flag
   */
  unsigned int getUndeclaredUnits();

  /** 
   * resets the undeclaredUnits and canIgnoreUndeclaredUnits flags
   * since these will different for each math formula
   */
  void resetFlags();

private:
  const Model * model;
  unsigned int undeclaredUnits;
  unsigned int canIgnoreUndeclaredUnits;
  unsigned int mInKineticlaw;
  int mReactionNo;

  /* a depth of recursive call of getUnitDefinition()*/
  int depthRecursiveCall;

  map<const ASTNode*, UnitDefinition*> unitDefinitionMap;
  map<const ASTNode*, unsigned int>    undeclaredUnitsMap;
  map<const ASTNode*, unsigned int>    canIgnoreUndeclaredUnitsMap;  

};


#endif  /* !cplusplus */
#endif  /* UnitFormulaFormatter_h */


/** @endcond doxygen-libsbml-internal */
