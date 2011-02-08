/**
 * @cond doxygen-libsbml-internal
 *
 * @file    UniqueSymbolsInInitialAssignments.h
 * @brief   Ensures the ids for all UnitDefinitions in a Model are unique
 * @author  Ben Bornstein
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


#ifndef UniqueSymbolsInInitialAssignments_h
#define UniqueSymbolsInInitialAssignments_h


#ifdef __cplusplus

#include <string>

#include "UniqueIdBase.h"

LIBSBML_CPP_NAMESPACE_BEGIN

class UniqueSymbolsInInitialAssignments: public UniqueIdBase
{
public:

  /**
   * Creates a new Constraint with the given constraint id.
   */
  UniqueSymbolsInInitialAssignments (unsigned int id, Validator& v);

  /**
   * Destroys this Constraint.
   */
  virtual ~UniqueSymbolsInInitialAssignments ();


protected:

  /**
   * @return the preamble to use when logging constraint violations.
   */
  virtual const char* getPreamble ();

  /**
   * Checks that all ids on UnitDefinitions are unique.
   */
  virtual void doCheck (const Model& m);
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* UniqueSymbolsInInitialAssignments_h */

/** @endcond */
