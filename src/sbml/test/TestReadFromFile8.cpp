/**
 * \file    TestReadFromFile8.cpp
 * \brief   Reads test-data/l2v4-new.xml into memory and tests it.
 * \author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 */
/* Copyright 2004 California Institute of Technology and
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


#include <sbml/common/common.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLWriter.h>
#include <sbml/SBMLTypes.h>

#include <string>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;


START_TEST (test_read_l2v4_new)
{
  SBMLReader        reader;
  SBMLDocument*     d;
  Model*            m;
  Compartment*      c;
  Event*            e;
  Trigger*          trigger;
  EventAssignment*  ea;
  
  const ASTNode*   ast;

  std::string filename(TestDataDirectory);
  filename += "l2v4-new.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"l2v4-new.xml\") returned a NULL pointer.");
  }



  //
  // <sbml level="2" version="3" ...>
  //
  fail_unless( d->getLevel  () == 2, NULL );
  fail_unless( d->getVersion() == 4, NULL );


  //
  // <model id="l2v4_all">
  //
  m = d->getModel();
  fail_unless( m != NULL, NULL );

  fail_unless(m->getId() == "l2v4_all", NULL);


  //<listOfCompartments>
  //  <compartment id="a" size="1" constant="false"/>
  //</listOfCompartments>
  fail_unless( m->getNumCompartments() == 1, NULL );

  c = m->getCompartment(0);
  fail_unless( c          != NULL  , NULL );
  fail_unless( c->getId() == "a", NULL );
  fail_unless( c->getSize() == 1, NULL );
  fail_unless( !c->getConstant(), NULL );


  //<event useValuesFromTriggerTime="true">
  //  <trigger>
  //    <math xmlns="http://www.w3.org/1998/Math/MathML">
  //      <apply>
  //        <lt/>
  //        <ci> x </ci>
  //        <cn type="integer"> 3 </cn>
  //      </apply>
  //    </math>
  //  </trigger>
  //  <listOfEventAssignments>
  //    <eventAssignment variable="a">
  //      <math xmlns="http://www.w3.org/1998/Math/MathML">
  //        <apply>
  //          <times/>
  //          <ci> x </ci>
  //          <ci> p3 </ci>
  //        </apply>
  //      </math>
  //    </eventAssignment>
  //  </listOfEventAssignments>
  //</event>
  fail_unless( m->getNumEvents() == 1, NULL );

  e = m->getEvent(0);
  fail_unless(e != NULL, NULL);

  fail_unless(e->getUseValuesFromTriggerTime(), NULL);

  fail_unless(e->isSetTrigger(), NULL);
  
  trigger = e->getTrigger();
  fail_unless(trigger != NULL, NULL);

  ast = trigger->getMath();
  fail_unless(!strcmp(SBML_formulaToString(ast), "lt(x, 3)"), NULL);

  fail_unless( e->getNumEventAssignments() == 1, NULL );

  ea = e->getEventAssignment(0);
  fail_unless(ea != NULL, NULL);

  fail_unless(ea->getVariable() == "a", NULL);

  ast = ea->getMath();
  fail_unless(!strcmp(SBML_formulaToString(ast), "x * p3"), NULL);


  delete d;
}
END_TEST


Suite *
create_suite_TestReadFromFile8 (void)
{ 
  Suite *suite = suite_create("test-data/l2v4-new.xml");
  TCase *tcase = tcase_create("test-data/l2v4-new.xml");


  tcase_add_test(tcase, test_read_l2v4_new);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
