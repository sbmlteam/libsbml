/**
 * @cond doxygen-libsbml-internal
 *
 * @file    UniqueVarsInEventsAndRules.cpp
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


#include <sbml/Model.h>
#include <sbml/Rule.h>
#include <sbml/Event.h>
#include <sbml/EventAssignment.h>

#include "UniqueVarsInEventsAndRules.h"

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

static const char* PREAMBLE =
    "An identifier used as the value of 'variable' in an <eventAssignment> "
    "cannot also appear as the value of 'variable' in an <assignmentRule>. "
    "(References: L2V1 Section 4.10.5; L2V2 Section 4.14.)";


/**
 * Creates a new Constraint with the given constraint id.
 */
UniqueVarsInEventsAndRules::UniqueVarsInEventsAndRules (unsigned int id, Validator& v) :
  UniqueIdBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
UniqueVarsInEventsAndRules::~UniqueVarsInEventsAndRules ()
{
}


/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
UniqueVarsInEventsAndRules::getPreamble ()
{
  return PREAMBLE;
}


/**
 * Checks that all ids on the following Model objects are unique:
 * event assignments and assignment rules.
 */
void
UniqueVarsInEventsAndRules::doCheck (const Model& m)
{
  unsigned int n, ea, nr;

  for (n = 0; n < m.getNumEvents(); ++n)
  {
    const Event* e = m.getEvent(n);

    /* for each event assignment check that the variable is not used
     * in an assignment rule 
     * needs to be this way to avoid logging an error for
     * repeated use of a variable in event assignments (904)
     */
    for (ea = 0; ea < e->getNumEventAssignments(); ++ea)
    {
      checkId( *e->getEventAssignment(ea) );
     
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
}

LIBSBML_CPP_NAMESPACE_END

/** @endcond */
