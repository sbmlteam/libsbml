/**
 * @cond doxygen-libsbml-internal
 *
 * @file    AssignmentCycles.h
 * @brief   Ensures unique variables assigned by rules and events
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
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


#ifndef AssignmentCycles_h
#define AssignmentCycles_h


#ifdef __cplusplus

#include <string>
#include <sbml/validator/VConstraint.h>

#include "IdList.h"

LIBSBML_CPP_NAMESPACE_BEGIN

typedef std::multimap<const std::string, std::string> IdMap;
typedef IdMap::iterator                               IdIter;
typedef std::pair<IdIter, IdIter>                     IdRange;

class AssignmentCycles: public TConstraint<Model>
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  AssignmentCycles (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~AssignmentCycles ();


protected:

  virtual void check_ (const Model& m, const Model& object);

  
  /* create pairs of ids that depend on each other */
  void addInitialAssignmentDependencies(const Model &, 
                                        const InitialAssignment &);
  
  void addReactionDependencies(const Model &, const Reaction &);
  
  
  void addRuleDependencies(const Model &, const Rule &);

  
  void determineAllDependencies();


  /* helper function to check if a pair already exists */
  bool alreadyExistsInMap(IdMap map, 
                          std::pair<const std::string, std::string> dependency);

  
  /* check for explicit use of original variable */
  void checkForSelfAssignment(const Model &);


  /* find cycles in the map of dependencies */
  void determineCycles(const Model& m);


  /* if a rule for a compartment refers to a species
   * within that compartment it is an implicit reference
   */
  void checkForImplicitCompartmentReference(const Model& m);
  
  /**
   * functions for logging messages about the cycle
   */
  void logCycle (const SBase* object, const SBase* conflict);
  
  
  void logCycle (const Model& m, std::string id, std::string id1);
  
  
  void logMathRefersToSelf (const ASTNode * node,
                                             const SBase* object);
  
  
  void logMathRefersToSelf (const Model& m, std::string id);

  
  void logImplicitReference (const SBase* object, const Species* conflict);


  void logImplicitReference (const Model& m, std::string id, 
                             const Species* conflict);

  
  IdMap mIdMap;

};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* AssignmentCycles_h */

/** @endcond doxygen-libsbml-internal */
