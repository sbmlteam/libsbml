/**
 * \file    OverDeterminedCheck.h
 * \brief   Checks for over determined models.
 * \author  Sarah Keating
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


#ifndef OverDeterminedCheck_h
#define OverDeterminedCheck_h


#ifdef __cplusplus


#include <string>
#include <vector>
#include <map>

#include <sbml/validator/Constraint.h>
#include "IdList.h"

typedef std::map< std::string, IdList> graph;
//typedef std::vector<edges>  graph;


class Model;
class Compartment;
class Validator;


class OverDeterminedCheck: public TConstraint<Model>
{
public:

  /**
   * Creates a new Constraint with the given id.
   */
  OverDeterminedCheck (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~OverDeterminedCheck ();


protected:

  /**
   * Checks that no Compartments in Model have a cycle via their 'outside'
   * attribute.
   *
   * Sets mHolds to true if no cycles are found, false otherwise.
   */
  virtual void check_ (const Model& m, const Model& object);

  /** 
   * creates equation vertexes according to the L2V2 spec 4.11.5 for every
   * 1. a Species structure that has the boundaryCondition field set to false 
   * and constant field set to false and which is referenced by one or more 
   * reactant or product lists of a Reaction structure containing a KineticLaw structure
   * 2. a Rule structure
   * 3. a KineticLaw structure
   */
  void writeEquationVertexes(const Model &);

  /**
   * creates variable vertexes according to the L2V2 spec 4.11.5 for
   * (a) every Species, Compartment and Parameter structure which has the
   * Constant field set to false; and 
   * (b) for every Reaction structure.
   */
  void writeVariableVertexes(const Model &);

  IdList findMatching();

  void createGraph(const Model &);

  unsigned int Recurse(std::string);
  /**
   * Checks for a cycle by following Compartment c's 'outside' attribute.
   * If a cycle is found, it is added to the list of found cycles, mCycles.
   */
  void checkForCycle (const Model& m, const Compartment* c);

  /**
   * @return true if Compartment c is contained in one of the already found
   * cycles, false otherwise.
   */
  bool isInCycle (const Compartment* c);

  /**
   * Logs a message about a cycle found starting at Compartment c.
   */
  void logOverDetermined (const Model &, const IdList& unmatched);


  IdList mEquations;
  IdList mVariables;
  graph mGraph;

  /* these are to enable the bipartite matching without passing variables */
  graph mMatching;
  graph mVarNeighInPrev;
  graph mEqnNeighInPrev;

};


#endif  /* __cplusplus */
#endif  /* OverDeterminedCheck_h */
