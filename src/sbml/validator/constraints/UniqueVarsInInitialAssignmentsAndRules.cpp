/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    UniqueVarsInInitialAssignmentsAndRules.cpp
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

#include <sbml/Model.h>
#include <sbml/Rule.h>
#include <sbml/Event.h>
#include <sbml/EventAssignment.h>

#include "UniqueVarsInInitialAssignmentsAndRules.h"

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

static const char* PREAMBLE =
    "The value of a 'symbol' field in any <initialAssignment> definition "
    "cannot also appear as the value of a 'variable' field in an "
    "<assignmentRule>. (References: L2V2 Section 4.10.)";


/*
 * Creates a new Constraint with the given constraint id.
 */
UniqueVarsInInitialAssignmentsAndRules::UniqueVarsInInitialAssignmentsAndRules (unsigned int id, Validator& v) :
  UniqueIdBase(id, v)
{
}


/*
 * Destroys this Constraint.
 */
UniqueVarsInInitialAssignmentsAndRules::~UniqueVarsInInitialAssignmentsAndRules ()
{
}


/*
 * @return the preamble to use when logging constraint violations.
 */
const char*
UniqueVarsInInitialAssignmentsAndRules::getPreamble ()
{
  return PREAMBLE;
}


/*
 * Checks that all ids on the following Model objects are unique:
 * event assignments and assignment rules.
 */
void
UniqueVarsInInitialAssignmentsAndRules::doCheck (const Model& m)
{
  unsigned int n, nr;

  for (n = 0; n < m.getNumInitialAssignments(); ++n)
  {
    checkId(*m.getInitialAssignment(n));

    for (nr = 0; nr < m.getNumRules(); ++nr) 
    {
      const Rule* r = m.getRule(nr);

      if (r->isAssignment()) {
        checkId( *m.getRule(nr) );
      }
    }

    mIdObjectMap.clear();
    
  }  
}


const string
UniqueVarsInInitialAssignmentsAndRules::getMessage (const string& id, const SBase& object)
{
  IdObjectMap::iterator iter = mIdObjectMap.find(id);


  if (iter == mIdObjectMap.end())
  {
    return
      "Internal (but non-fatal) Validator error in "
      "UniqueVarsInInitialAssignmentsAndRules::getMessage().  The SBML object with duplicate id was "
      "not found when it came time to construct a descriptive error message.";
  }


  ostringstream oss_msg;
  const SBase&  previous = *(iter->second);


  //oss_msg << getPreamble();

  //
  // Example message: 
  //
  // The <compartment> id 'cell' conflicts with the previously defined
  // <parameter> id 'cell' at line 10.
  //

  oss_msg << "  The <" << object.getElementName() << "> " << getFieldname(object.getTypeCode())
      << " '" << id << "' conflicts with the previously defined <"
      << previous.getElementName() << "> " << getFieldname(previous.getTypeCode())
      << " '" << id << "'";

  if (previous.getLine() != 0)
  {
    oss_msg << " at line " << previous.getLine();
  }

  oss_msg << '.';

  return oss_msg.str();
}

const char*
UniqueVarsInInitialAssignmentsAndRules::getFieldname ()
{
  return "variable or symbol";
}


const char*
UniqueVarsInInitialAssignmentsAndRules::getFieldname (int typecode)
{
  switch(typecode) {
  case SBML_INITIAL_ASSIGNMENT:
    return "symbol";
  case SBML_ASSIGNMENT_RULE:
    return "variable";
  default:
    return "variable or symbol";
  }
}



LIBSBML_CPP_NAMESPACE_END
/** @endcond */
