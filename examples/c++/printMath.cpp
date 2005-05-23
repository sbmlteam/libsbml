/**
 * \file    printMath.cpp
 * \brief   Prints Rule, Reaction, and Event formulas in a given SBML Document
 * \author  Ben Bornstein and Sarah Keating
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


#include "sbml/SBMLReader.h"
#include "sbml/SBMLTypes.h"

#include <iostream>

using namespace std;


void
printFunctionDefinition (unsigned int n, const FunctionDefinition *fd)
{
  const ASTNode *math;
  char *formula;


  if ( fd->isSetMath() )
  {
    cout << "FunctionDefinition " << n << ", " << fd->getId();

    math = fd->getMath();

    /* Print function arguments. */
    if (math->getNumChildren() > 1)
    {
      cout << ( math->getLeftChild() )->getName();

      for (n = 1; n < math->getNumChildren() - 1; ++n)
      {
        cout <<", " << ( math->getChild(n) )->getName();
      }
    }

    cout <<") := ";

    /* Print function body. */
    if (math->getNumChildren() == 0)
    {
      cout << "(no body defined)";
    }
    else
    {
      math    = math->getChild(math->getNumChildren() - 1);
      formula = SBML_formulaToString(math);
      cout << formula << "\n";
      free(formula);
    }
  }
}


void
printRuleMath (unsigned int n, const Rule *r)
{
  char *formula;


  if ( r->isSetMath() )
  {
    formula = SBML_formulaToString( r->getMath() );
    cout << "Rule " << n << ", " << formula << "\n";
    free(formula);
  }
}


void
printReactionMath (unsigned int n, const Reaction *r)
{
  char         *formula;
  KineticLaw *kl;


  if (r->isSetKineticLaw())
  {
    kl = r->getKineticLaw();

    if ( kl->isSetMath() )
    {
      formula = SBML_formulaToString( kl->getMath() );
      cout << "Reaction " << n << ", " << formula << "\n";
      free(formula);
    }
  }
}


void
printEventAssignmentMath (unsigned int n, const EventAssignment *ea)
{
  std::string variable;
  char       *formula;


  if ( ea->isSetMath() )
  {
    variable = ea->getVariable();
    formula  = SBML_formulaToString( ea->getMath() );

    cout <<"  EventAssignment " << n << ", trigger: " << variable << " = " << formula << "\n";

    free(formula);
  }
}


void
printEventMath (unsigned int n, const Event *e)
{
  char         *formula;
  unsigned int i;


  if ( e->isSetDelay() )
  {
    formula = SBML_formulaToString( e->getDelay() );
    cout << "Event " << n << " delay: " << formula << "\n";
    free(formula);
  }

  if ( e->isSetTrigger() )
  {
    formula = SBML_formulaToString( e->getTrigger() );
    cout << "Event " << n << " trigger: " << formula << "\n";
    free(formula);
  }

  for (i = 0; i < e->getNumEventAssignments(); ++i)
  {
    printEventAssignmentMath(i + 1, e->getEventAssignment(i));
  }

  cout << "\n";
}


void
printMath (const Model *m)
{
  unsigned int  n;


  for (n = 0; n < m->getNumFunctionDefinitions(); ++n)
  {
    printFunctionDefinition(n + 1, m->getFunctionDefinition(n));
  }

  for (n = 0; n < m->getNumRules(); ++n)
  {
    printRuleMath(n + 1, m->getRule(n));
  }

  cout << endl;

  for (n = 0; n < m->getNumReactions(); ++n)
  {
    printReactionMath(n + 1, m->getReaction(n));
  }

  cout << endl;

  for (n = 0; n < m->getNumEvents(); ++n)
  {
    printEventMath(n + 1, m->getEvent(n));
  }
}


int
main (int argc, char *argv[])
{
  const char* filename;

  SBMLDocument* d;
  Model*        m;

  if (argc != 2)
  {
    cout << "\n  usage: printMath <filename>\n\n";
    return 1;
  }
    
  filename = argv[1];

  d        = readSBML(filename);

  d->printWarnings(cout);
  d->printErrors  (cout);
  d->printFatals  (cout);

  m = d->getModel();
  if (!m) return 2;

  printMath(m);
  cout << endl;

  delete d;
  return 0;
}
