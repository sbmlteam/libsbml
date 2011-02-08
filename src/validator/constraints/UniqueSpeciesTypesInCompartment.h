/**
 * @cond doxygen-libsbml-internal
 *
 * @file    UniqueSpeciesTypesInCompartment.h
 * @brief   Ensures unique variables assigned by rules and events
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 */
/* Copyright (C) 2009-2011 jointly by the following organizations: 
/*     1. California Institute of Technology, Pasadena, CA, USA
/*     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
/*  
/* Copyright (C) 2006-2008 by the California Institute of Technology,
/*     Pasadena, CA, USA 
/*  
/* Copyright (C) 2002-2005 jointly by the following organizations: 
/*     1. California Institute of Technology, Pasadena, CA, USA
/*     2. Japan Science and Technology Agency, Japan
/* 
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


#ifndef UniqueSpeciesTypesInCompartment_h
#define UniqueSpeciesTypesInCompartment_h


#ifdef __cplusplus

#include <string>

#include <sbml/validator/VConstraint.h>

#include "IdList.h"

LIBSBML_CPP_NAMESPACE_BEGIN

class UniqueSpeciesTypesInCompartment: public TConstraint<Model>
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  UniqueSpeciesTypesInCompartment (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~UniqueSpeciesTypesInCompartment ();


protected:

  /**
   * Checks that any species with boundary condition false
   * is not set by reaction and rules
   */
  virtual void check_ (const Model& m, const Model& object);

  /**
   * Logs a message about species with boundary condition false
   * being set by reaction and rules
   */
  void logConflict (const Species& s, const Compartment& c);

  IdList mSpecies;
  IdList mSpeciesTypes;

};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* UniqueSpeciesTypesInCompartment_h */

/** @endcond */
