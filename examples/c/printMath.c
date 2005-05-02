/**
 * \file    printMath.c
 * \brief   Prints Rule, Reaction, and Event formulas in a given SBML Document
 * \author  Ben Bornstein
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
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include <stdio.h>
#include <stdlib.h>

#include "sbml/SBMLReader.h"
#include "sbml/SBMLTypes.h"


void
printRuleMath (unsigned int n, const Rule_t *r)
{
  char *formula;


  if ( Rule_isSetMath(r) )
  {
    formula = SBML_formulaToString( Rule_getMath(r) );
    printf("Rule %d, formula: %s\n", n, formula);
    free(formula);
  }
}


void
printReactionMath (unsigned int n, const Reaction_t *r)
{
  char         *formula;
  KineticLaw_t *kl;


  if (Reaction_isSetKineticLaw(r))
  {
    kl = Reaction_getKineticLaw(kl);

    if ( KineticLaw_isSetMath(kl) )
    {
      formula = SBML_formulaToString( KineticLaw_getMath(r) );
      printf("Reaction %d, formula: %s\n", n, formula);
      free(formula);
    }
  }
}


void
printEventAssignmentMath (unsigned int n, const EventAssignment_t *ea)
{
  const char *variable;
  char       *formula;


  if ( EventAssignment_isSetMath(ea) )
  {
    variable = EventAssignment_getVariable(ea);
    formula  = SBML_formulaToString( EventAssignment_getMath(ea) );

    printf("  EventAssignment %d, trigger: %s = %s\n", n, variable, formula);

    free(formula);
  }
}


void
printEventMath (unsigned int n, const Event_t *e)
{
  char         *formula;
  unsigned int i;


  if ( Event_isSetDelay(e) )
  {
    formula = SBML_formulaToString( Event_getDelay(e) );
    printf("Event %d delay: %s\n", n, formula);
    free(formula);
  }

  if ( Event_isSetTrigger(e) )
  {
    formula = SBML_formulaToString( Event_getTrigger(e) );
    printf("Event %d trigger: %s\n", n, formula);
    free(formula);
  }

  for (i = 0; i < Event_getNumEventAssignments(e); ++i)
  {
    printEventAssignmentMath(i + 1, Event_getEventAssignment(e, i));
  }

  printf("\n");
}


void
printMath (const Model_t *m)
{
  unsigned int  n;
  Reaction_t   *r;


  for (n = 0; n < Model_getNumRules(m); ++n)
  {
    printRuleMath(n + 1, Model_getRule(m, n));
  }

  printf("\n");

  for (n = 0; n < Model_getNumReactions(m); ++n)
  {
    printReactionMath(n + 1, Model_getReaction(m, n);
  }

  printf("\n");

  for (n = 0; n < Model_getNumEvents(m); ++n)
  {
    printEventMath(n + 1, Model_getEvent(m, n));
  }
}


int
main (int argc, char *argv[])
{
  SBMLDocument_t *d;
  Model_t        *m;


  if (argc != 2)
  {
    printf("\n  usage: printMath <filename>\n\n");
    return 1;
  }

  d = readSBML(argv[1]);
  m = SBMLDocument_getModel(d);

  SBMLDocument_printWarnings(d, stdout);
  SBMLDocument_printErrors  (d, stdout);
  SBMLDocument_printFatals  (d, stdout);

  printMath(m);
  printf("\n");

  SBMLDocument_free(d);
  return 0;
}
