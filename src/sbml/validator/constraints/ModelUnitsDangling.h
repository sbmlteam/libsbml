/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    ModelUnitsDangling.h
 * @brief   Base class for Id constraints
 * @author  Ben Bornstein
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

#ifndef ModelUnitsDangling_h
#define ModelUnitsDangling_h


#ifdef __cplusplus


#include <string>
#include <sstream>
#include <map>

#include <sbml/validator/VConstraint.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class SBase;
class Validator;


class ModelUnitsDangling: public TConstraint<Model>
{
public:

  /**
   * Creates a new ModelUnitsDangling with the given constraint id.
   */
  ModelUnitsDangling (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~ModelUnitsDangling ();


protected:
  /**
   * Called by check().  Override this method to define your own subset.
   */
  void doCheck (const Model& m);

  /**
   * Checks that all ids for some given subset of the Model adhere to this
   * Constraint.  Override the doCheck() method to define your own subset.
   */
  virtual void check_ (const Model& m, const Model& object);

  /**
   * Logs a message that the given @p id (and its corresponding object) have
   * failed to satisfy this constraint.
   */
  void logConflict (const std::string& type, const std::string& unit, const SBase& object);

  bool checkUnit(const std::string& unit, const Model& m);

};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* ModelUnitsDangling_h */
/** @endcond */
