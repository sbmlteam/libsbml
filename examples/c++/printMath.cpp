/**
 * \file    printMath.cpp
 * \brief   Prints Rule, Reaction, and Event formulas in a given SBML Document
 * \author  Ben Bornstein and Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
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


#include <iostream>
#include <sbml/SBMLTypes.h>


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
      cout << "(" << ( math->getLeftChild() )->getName();

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
      cout << formula << endl;
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
    cout << "Rule " << n << ", " << formula << endl;
    free(formula);
  }
}


void
printReactionMath (unsigned int n, const Reaction *r)
{
  char         *formula;
  const KineticLaw *kl;


  if (r->isSetKineticLaw())
  {
    kl = r->getKineticLaw();

    if ( kl->isSetMath() )
    {
      formula = SBML_formulaToString( kl->getMath() );
      cout << "Reaction " << n << ", " << formula << endl;
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

    cout <<"  EventAssignment " << n
         << ", trigger: " << variable << " = " << formula << endl;

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
    cout << "Event " << n << " delay: " << formula << endl;
    free(formula);
  }

  if ( e->isSetTrigger() )
  {
    formula = SBML_formulaToString( e->getTrigger() );
    cout << "Event " << n << " trigger: " << formula << endl;
    free(formula);
  }

  for (i = 0; i < e->getNumEventAssignments(); ++i)
  {
    printEventAssignmentMath(i + 1, e->getEventAssignment(i));
  }

  cout << endl;
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
    cout << endl << "  usage: printMath <filename>" << endl << endl;
    return 1;
  }
    
  filename = argv[1];
  d        = readSBML(filename);

  d->printErrors(cout);

  m = d->getModel();

  if (m == 0)
  {
    cout << "No model present." << endl;
    return 1;
  }

  printMath(m);
  cout << endl;

  delete d;
  return 0;
}
