/**
 * Filename    : TestReadFromFile5.c
 * Description : Reads test-data/l2v1-assignment.xml into memory and tests it.
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2004-07-18
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2004 California Institute of Technology and
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


#include <check.h>

#include "sbml/common.h"
#include "sbml/common.hpp"
#include "sbml/SBMLReader.hpp"
#include "sbml/SBMLWriter.h"
#include "sbml/SBMLTypes.hpp"

#include <string>


BEGIN_C_DECLS


extern char *TestDataDirectory;


START_TEST (test_read_l2v1_assignment)
{
  SBMLReader        reader;
  SBMLDocument*     d;
  Model*            m;
  Compartment*      c;
  Species*          s;
  Parameter*        p;
  AssignmentRule*   ar;
  Reaction*         r;
  SpeciesReference* sr;
  KineticLaw*       kl;

  std::string filename(TestDataDirectory);
  filename += "l2v1-assignment.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"l2v1-assignment.xml\") returned a NULL pointer.");
  }



  //
  // <sbml level="2" version="1" ...>
  //
  fail_unless( d->getLevel  () == 2, NULL );
  fail_unless( d->getVersion() == 1, NULL );


  //
  // <model>
  //
  m = d->getModel();
  fail_unless( m != NULL, NULL );


  //
  // <listOfCompartments>
  //   <compartment id="cell"/>
  // </listOfCompartments>
  //
  fail_unless( m->getNumCompartments() == 1, NULL );

  c = m->getCompartment(0);
  fail_unless( c          != NULL  , NULL );
  fail_unless( c->getId() == "cell", NULL );


  //
  // <listOfSpecies>
  //   <species id="X0" compartment="cell" initialConcentration="1"/>
  //   <species id="X1" compartment="cell" initialConcentration="0"/>
  //   <species id="T"  compartment="cell" initialConcentration="0"/>
  //   <species id="S1" compartment="cell" initialConcentration="0"/>
  //   <species id="S2" compartment="cell" initialConcentration="0"/>
  // </listOfSpecies>
  //
  fail_unless( m->getNumSpecies() == 5, NULL );

  s = m->getSpecies(0);
  fail_unless( s                            != NULL  , NULL );
  fail_unless( s->getId()                   == "X0"  , NULL );
  fail_unless( s->getCompartment()          == "cell", NULL );
  fail_unless( s->getInitialConcentration() == 1.0   , NULL );

  s = m->getSpecies(1);
  fail_unless( s                            != NULL  , NULL );
  fail_unless( s->getId()                   == "X1"  , NULL );
  fail_unless( s->getCompartment()          == "cell", NULL );
  fail_unless( s->getInitialConcentration() == 0.0   , NULL );

  s = m->getSpecies(2);
  fail_unless( s                            != NULL  , NULL );
  fail_unless( s->getId()                   == "T"   , NULL );
  fail_unless( s->getCompartment()          == "cell", NULL );
  fail_unless( s->getInitialConcentration() == 0.0   , NULL );

  s = m->getSpecies(3);
  fail_unless( s                            != NULL  , NULL );
  fail_unless( s->getId()                   == "S1"  , NULL );
  fail_unless( s->getCompartment()          == "cell", NULL );
  fail_unless( s->getInitialConcentration() == 0.0   , NULL );

  s = m->getSpecies(4);
  fail_unless( s                            != NULL  , NULL );
  fail_unless( s->getId()                   == "S2"  , NULL );
  fail_unless( s->getCompartment()          == "cell", NULL );
  fail_unless( s->getInitialConcentration() == 0.0   , NULL );


  //
  // <listOfParameters>
  //   <parameter id="Keq" value="2.5"/>
  // </listOfParameters>
  //
  fail_unless( m->getNumParameters() == 1, NULL );

  p = m->getParameter(0);

  fail_unless( p             != NULL , NULL );
  fail_unless( p->getId()    == "Keq", NULL );
  fail_unless( p->getValue() == 2.5  , NULL );

  //
  // <listOfRules> ... </listOfRules>
  //
  fail_unless( m->getNumRules() == 2, NULL );

  //
  // <assignmentRule variable="S1">
  //   <math xmlns="http://www.w3.org/1998/Math/MathML">
  //     <apply>
  //       <divide/>
  //       <ci> T </ci>
  //       <apply>
  //         <plus/>
  //         <cn> 1 </cn>
  //         <ci> Keq </ci>
  //       </apply>
  //     </apply>
  //   </math>
  // </assignmentRule>
  //
  ar = static_cast<AssignmentRule*>( m->getRule(0) );
  fail_unless( ar != NULL, NULL );
  fail_unless( ar->getVariable() == "S1"           , NULL );
  fail_unless( ar->getFormula()  == "T / (1 + Keq)", NULL );

  //
  // <assignmentRule variable="S2">
  //   <math xmlns="http://www.w3.org/1998/Math/MathML">
  //     <apply>
  //       <times/>
  //       <ci> Keq </ci>
  //       <ci> S1 </ci>
  //     </apply>
  //   </math>
  // </assignmentRule>
  //
  ar = static_cast<AssignmentRule*>( m->getRule(1) );
  fail_unless( ar != NULL, NULL );
  fail_unless( ar->getVariable() == "S2"      , NULL );
  fail_unless( ar->getFormula()  == "Keq * S1", NULL );


  //
  // <listOfReactions> ... </listOfReactions>
  //
  fail_unless( m->getNumReactions() == 2, NULL );

  //
  // <reaction id="in">
  //   <listOfReactants>
  //     <speciesReference species="X0"/>
  //   </listOfReactants>
  //   <listOfProducts>
  //     <speciesReference species="T"/>
  //   </listOfProducts>
  //   <kineticLaw>
  //     <math xmlns="http://www.w3.org/1998/Math/MathML">
  //       <apply>
  //         <times/>
  //         <ci> k1 </ci>
  //         <ci> X0 </ci>
  //       </apply>
  //     </math>
  //     <listOfParameters>
  //       <parameter id="k1" value="0.1"/>
  //     </listOfParameters>
  //   </kineticLaw>
  // </reaction>
  //
  r = m->getReaction(0);

  fail_unless( r          != NULL, NULL );
  fail_unless( r->getId() == "in", NULL );

  fail_unless( r->getNumReactants() == 1, NULL );
  fail_unless( r->getNumProducts () == 1, NULL );

  sr = r->getReactant(0);
  fail_unless( sr               != NULL, NULL );
  fail_unless( sr->getSpecies() == "X0", NULL );

  sr = r->getProduct(0);
  fail_unless( sr               != NULL, NULL );
  fail_unless( sr->getSpecies() == "T" , NULL );

  kl = r->getKineticLaw();
  fail_unless( kl                     != NULL     , NULL );
  fail_unless( kl->getFormula()       == "k1 * X0", NULL );
  fail_unless( kl->getNumParameters() == 1        , NULL );

  p = kl->getParameter(0);
  fail_unless( p             != NULL, NULL );
  fail_unless( p->getId()    == "k1", NULL );
  fail_unless( p->getValue() == 0.1 , NULL );

  //
  // <reaction id="out">
  //   <listOfReactants>
  //     <speciesReference species="T"/>
  //   </listOfReactants>
  //   <listOfProducts>
  //     <speciesReference species="X1"/>
  //   </listOfProducts>
  //   <kineticLaw>
  //     <math xmlns="http://www.w3.org/1998/Math/MathML">
  //       <apply>
  //         <times/>
  //         <ci> k2 </ci>
  //         <ci> S2 </ci>
  //       </apply>
  //     </math>
  //     <listOfParameters>
  //       <parameter id="k2" value="0.15"/>
  //     </listOfParameters>
  //   </kineticLaw>
  // </reaction>
  //
  r = m->getReaction(1);

  fail_unless( r          != NULL , NULL );
  fail_unless( r->getId() == "out", NULL );

  fail_unless( r->getNumReactants() == 1, NULL );
  fail_unless( r->getNumProducts () == 1, NULL );

  sr = r->getReactant(0);
  fail_unless( sr               != NULL, NULL );
  fail_unless( sr->getSpecies() == "T" , NULL );

  sr = r->getProduct(0);
  fail_unless( sr               != NULL, NULL );
  fail_unless( sr->getSpecies() == "X1", NULL );

  kl = r->getKineticLaw();
  fail_unless( kl                     != NULL     , NULL );
  fail_unless( kl->getFormula()       == "k2 * S2", NULL );
  fail_unless( kl->getNumParameters() == 1        , NULL );

  p = kl->getParameter(0);
  fail_unless( p             != NULL, NULL );
  fail_unless( p->getId()    == "k2", NULL );
  fail_unless( p->getValue() == 0.15, NULL );

  delete d;
}
END_TEST


Suite *
create_suite_TestReadFromFile5 (void)
{ 
  Suite *suite = suite_create("test-data/l2v1-assignment.xml");
  TCase *tcase = tcase_create("test-data/l2v1-assignment.xml");


  tcase_add_test(tcase, test_read_l2v1_assignment);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
