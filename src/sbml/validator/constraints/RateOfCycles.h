/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    RateOfCycles.h
 * @brief   Ensures unique variables assigned by rules and events
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

#ifndef RateOfCycles_h
#define RateOfCycles_h


#ifdef __cplusplus

#include <string>
#include <sbml/validator/VConstraint.h>

#include <sbml/util/IdList.h>

LIBSBML_CPP_NAMESPACE_BEGIN

typedef std::multimap<const std::string, std::string> IdMap;
typedef IdMap::iterator                               IdIter;
typedef std::pair<IdIter, IdIter>                     IdRange;

class RateOfCycles: public TConstraint<Model>
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  RateOfCycles (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~RateOfCycles ();


protected:

  virtual void check_ (const Model& m, const Model& object);

  
  void addReactionDependencies(const Model &, const Reaction &);
  
  
  void addRuleDependencies(const Model &, const Rule &);

  
  void addAssignmentRuleDependencies(const Model &, const Rule &);


  void addRnSpeciesDependencies(const std::string& name, const Reaction &r);
  

  void addInitialAssignmentDependencies(const Model &m, const InitialAssignment &ia);
  
  // return true if the id passed is a product/species in any reaction
  bool assignedByReaction(const Model&, const std::string& id);


  // return true if the id is the variable of an assignmentrule that
  // uses a rateOf expression
  bool isEdgeCaseAssignment(const Model& m, const std::string& id);


  void determineAllDependencies();


  /* helper function to check if a pair already exists */
  bool alreadyExistsInMap(IdMap map, 
                          std::pair<const std::string, std::string> dependency);


  bool alreadyExistsInCycle(std::vector<IdList> cycle, IdList list);

  bool containSameElements(IdList listA, IdList listB);


  /* check for explicit use of original variable */
  void checkForSelfAssignment(const Model &);


  /* find cycles in the map of dependencies */
  void determineCycles(const Model& m);


  /**
   * functions for logging messages about the cycle
   */
  void logCycle (const SBase* object, std::string& message);
  
  
  void logCycle (const Model& m, IdList cycle);
  

  void getReference(const SBase* object, std::string& ref);

  /**
   * Returns the fieldname to use when logging constraint violations
   * ("variable" or "symbol", depending on the type)
   *
   * @return the fieldname ("variable" or "symbol") to use when logging constraint
   * violations, depending on the typecode passed in.  If an unknown code 
   * is passed in, "variable or symbol" is returned.
   */

  void logMathRefersToSelf (const ASTNode * node,
                                             const SBase* object);
  
  
  void logMathRefersToSelf (const Model& m, std::string id);

  
  IdMap mIdMap;

  IdMap mRnSpeciesMap;

};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* RateOfCycles_h */
/** @endcond */

