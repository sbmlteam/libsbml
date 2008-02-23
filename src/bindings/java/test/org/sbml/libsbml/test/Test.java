/**
 * @file    Test.java
 * @brief   Simple test of the libSBML Java module
 * @author  Ben Bornstein
 * @author  Akiya Jouraku 
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and Japan Science and
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

package org.sbml.libsbml.test;

import org.sbml.libsbml.*;
import java.io.File;

public class Test
{
  static String[] listValidSBML = { 
    "l1v1-branch.xml",
    "l1v1-branch-schema-error.xml",
    "l1v1-branch.xml",
    "l1v1-minimal.xml",
    "l1v1-rules.xml",
    "l1v1-units.xml",
    "l2v1-branch.xml",
    "l2v1-2D-compartments.xml",
    "l2v1-assignment.xml",
    "l2v1-boundary.xml",
    "l2v1-branch.xml",
    "l2v1-delay.xml",
    "l2v1-events.xml",
    "l2v1-functions.xml",
    "l2v1-mc-ode.xml",
    "l2v1-units.xml",
  };

  static String[] listInvalidSBML = {    
    "not-sbml.xml",
    "no-encoding.xml",
    "not-utf8.xml",
    "not-xml.txt",
    "unknown-encoding.xml",
    "unknown-sbml.xml"
  };

  static String[] listTestFormula = {
    "V10 * MAPK_P / (KK10 + MAPK_P)",
    "V1 * MKKK / ((1 + pow(MAPK_PP / Ki, n)) * (K1 + MKKK))",
    "and(gt(T1, 0.5), lt(T1, 0.55))"
  };

  public void testValidationValidSBML ()
  {
    SBMLReader r = new SBMLReader();

    for (int i=0; i < listValidSBML.length; i++) {
      SBMLDocument d = r.readSBML("../../sbml/test/test-data/" + listValidSBML[i]);
      d.checkConsistency();
      long n = d.getNumErrors();
      if (n > 0)
      {
        SBMLErrorLog log = d.getErrorLog();
        assertTrue(log.getNumFailsWithSeverity(libsbml.LIBSBML_SEV_ERROR) == 0);
      }
    }
  }

  public void testValidationInvalidSBML ()
  {
    SBMLReader r = new SBMLReader();
    for (int i=0; i < listInvalidSBML.length; i++){
      SBMLDocument d = r.readSBML("../../sbml/test/test-data/" + listInvalidSBML[i]);
      d.checkConsistency();
      assertTrue(d.getNumErrors() != 0);
    }
  }

/*
  public void testOStream ()
  {
    String filename_src = "l2v1-branch.xml";

    SBMLReader   r = new SBMLReader();
    SBMLDocument d = r.readSBML("../../sbml/test/test-data/" + filename_src);
    XMLOutputStream xos = new XMLOutputStream(libsbml.cout);
    d.write(xos);
    libsbml.cout.endl();
  }
*/

  public void testOFStream ()
  {
    String filename_src = "l2v1-branch.xml";
    String filename_dst = "test_write.xml";
    boolean is_append   = false;

    SBMLReader   r = new SBMLReader();
    SBMLWriter   w = new SBMLWriter();
    SBMLDocument d = r.readSBML("../../sbml/test/test-data/" + filename_src);
    String       s1,s2;
    OFStream     ofs;
    XMLOutputStream xos;

    s1  = w.writeToString(d);
    ofs = new OFStream(filename_dst,is_append);
    xos = new XMLOutputStream(ofs);
    d.write(xos);
    ofs.endl();

    d = libsbml.readSBML(filename_dst);
    assertTrue(d.getNumErrors() == 0);
    s2 = libsbml.writeSBMLToString(d);
    assertTrue(s1.equals(s2));
  }

  public void testOStringStream ()
  {
    String filename_src = "l2v1-branch.xml";
    String filename_dst = "test_write.xml";
    boolean is_append   = false;

    SBMLReader   r = new SBMLReader();
    SBMLWriter   w = new SBMLWriter();
    SBMLDocument d = r.readSBML("../../sbml/test/test-data/" + filename_src);
    String       s1,s2;
    XMLOutputStream xos;
    OStringStream   oss;

    s1  = w.writeToString(d);
    oss = new OStringStream();
    xos = new XMLOutputStream(oss);
    d.write(xos);
    oss.endl();
    s2 = oss.str();

    assertTrue(s1.equals(s2));
  }

  public void testImplicitDowncastRule()
  {
    Model m = new Model();
    
    m.addRule( new AssignmentRule("x", "1 + 1") );
    m.addRule( new RateRule("x", "1 + 1") );
    m.addRule( new AlgebraicRule("1 + 1") );
    assertTrue( m.getNumRules() == 3 );
    assertTrue( m.getRule(0) instanceof AssignmentRule );
    assertTrue( m.getRule(1) instanceof RateRule );
    assertTrue( m.getRule(2) instanceof AlgebraicRule );
  }
    
  public void testCloneObject()
  {
    checkCloneObject( new FunctionDefinition()       );
    checkCloneObject( new UnitDefinition()           );
    checkCloneObject( new CompartmentType()          );
    checkCloneObject( new SpeciesType()              );
    checkCloneObject( new Compartment()              );
    checkCloneObject( new Species()                  );
    checkCloneObject( new Parameter()                );
    checkCloneObject( new InitialAssignment()        );
    checkCloneObject( new AssignmentRule()           );
    checkCloneObject( new AlgebraicRule()            );
    checkCloneObject( new RateRule()                 );
    checkCloneObject( new Constraint()               );
    checkCloneObject( new Reaction()                 );
    checkCloneObject( new SpeciesReference()         );
    checkCloneObject( new ModifierSpeciesReference() );
    checkCloneObject( new Event()                    );
    checkCloneObject( new EventAssignment()          );
    checkCloneObject( new Unit()                     );
    checkCloneObject( new UnitDefinition()           );
    checkCloneObject( new Trigger()                  );
    checkCloneObject( new Delay()                    );
    checkCloneObject( new ListOf()                   );
    checkCloneObject( new ListOfCompartments()       );
    checkCloneObject( new ListOfCompartmentTypes()   );
    checkCloneObject( new ListOfConstraints()        );
    checkCloneObject( new ListOfEvents()             );
    checkCloneObject( new ListOfEventAssignments()   );
    checkCloneObject( new ListOfFunctionDefinitions());
    checkCloneObject( new ListOfInitialAssignments() );
    checkCloneObject( new ListOfParameters()         );
    checkCloneObject( new ListOfReactions()          );
    checkCloneObject( new ListOfRules()              );
    checkCloneObject( new ListOfSpecies()            );
    checkCloneObject( new ListOfSpeciesReferences()  );
    checkCloneObject( new ListOfSpeciesTypes()       );
    checkCloneObject( new ListOfUnits()              );
    checkCloneObject( new ListOfUnitDefinitions()    );
    checkCloneObject( new StoichiometryMath()        );
  }

  public void checkCloneObject(SBase sb) 
  {
    SBase clone = sb.cloneObject();
    String en1  = sb.getElementName();
    String en2  = clone.getElementName();
    assertTrue( en1.equals(en2) );
  }

  public void testImplicitDowncastSBase()
  {
    ListOf lo = new ListOf();

    lo.append( new FunctionDefinition()       );
    lo.append( new UnitDefinition()           );
    lo.append( new CompartmentType()          );
    lo.append( new SpeciesType()              );
    lo.append( new Compartment()              );
    lo.append( new Species()                  );
    lo.append( new Parameter()                );
    lo.append( new InitialAssignment()        );
    lo.append( new AssignmentRule()           );
    lo.append( new AlgebraicRule()            );
    lo.append( new RateRule()                 );
    lo.append( new Constraint()               );
    lo.append( new Reaction()                 );
    lo.append( new SpeciesReference()         );
    lo.append( new ModifierSpeciesReference() );
    lo.append( new Event()                    );
    lo.append( new EventAssignment()          );
    lo.append( new Unit()                     );
    lo.append( new UnitDefinition()           );
    lo.append( new Trigger()                  );
    lo.append( new Delay()                    );
    lo.append( new ListOf()                   );
    lo.append( new ListOfCompartments()       );
    lo.append( new ListOfCompartmentTypes()   );
    lo.append( new ListOfConstraints()        );
    lo.append( new ListOfEvents()             );
    lo.append( new ListOfEventAssignments()   );
    lo.append( new ListOfFunctionDefinitions());
    lo.append( new ListOfInitialAssignments() );
    lo.append( new ListOfParameters()         );
    lo.append( new ListOfReactions()          );
    lo.append( new ListOfRules()              );
    lo.append( new ListOfSpecies()            );
    lo.append( new ListOfSpeciesReferences()  );
    lo.append( new ListOfSpeciesTypes()       );
    lo.append( new ListOfUnits()              );
    lo.append( new ListOfUnitDefinitions()    );
    lo.append( new StoichiometryMath()        );

    /**
     * ListOf.get() returns an object of type SBase, so the result must be
     * downcast to an object of the appropriate type.
     */

    int i = 0;

    assertTrue( lo.get(i++) instanceof FunctionDefinition       );
    assertTrue( lo.get(i++) instanceof UnitDefinition           );
    assertTrue( lo.get(i++) instanceof CompartmentType          );
    assertTrue( lo.get(i++) instanceof SpeciesType              );
    assertTrue( lo.get(i++) instanceof Compartment              );
    assertTrue( lo.get(i++) instanceof Species                  );
    assertTrue( lo.get(i++) instanceof Parameter                );
    assertTrue( lo.get(i++) instanceof InitialAssignment        );
    assertTrue( lo.get(i++) instanceof AssignmentRule           );
    assertTrue( lo.get(i++) instanceof AlgebraicRule            );
    assertTrue( lo.get(i++) instanceof RateRule                 );
    assertTrue( lo.get(i++) instanceof Constraint               );
    assertTrue( lo.get(i++) instanceof Reaction                 );
    assertTrue( lo.get(i++) instanceof SpeciesReference         );
    assertTrue( lo.get(i++) instanceof ModifierSpeciesReference );
    assertTrue( lo.get(i++) instanceof Event                    );
    assertTrue( lo.get(i++) instanceof EventAssignment          );
    assertTrue( lo.get(i++) instanceof Unit                     );
    assertTrue( lo.get(i++) instanceof UnitDefinition           );
    assertTrue( lo.get(i++) instanceof Trigger                  );
    assertTrue( lo.get(i++) instanceof Delay                    );
    assertTrue( lo.get(i++) instanceof ListOf                   );
    assertTrue( lo.get(i++) instanceof ListOfCompartments       );
    assertTrue( lo.get(i++) instanceof ListOfCompartmentTypes   );
    assertTrue( lo.get(i++) instanceof ListOfConstraints        );
    assertTrue( lo.get(i++) instanceof ListOfEvents             );
    assertTrue( lo.get(i++) instanceof ListOfEventAssignments   );
    assertTrue( lo.get(i++) instanceof ListOfFunctionDefinitions);
    assertTrue( lo.get(i++) instanceof ListOfInitialAssignments );
    assertTrue( lo.get(i++) instanceof ListOfParameters         );
    assertTrue( lo.get(i++) instanceof ListOfReactions          );
    assertTrue( lo.get(i++) instanceof ListOfRules              );
    assertTrue( lo.get(i++) instanceof ListOfSpecies            );
    assertTrue( lo.get(i++) instanceof ListOfSpeciesReferences  );
    assertTrue( lo.get(i++) instanceof ListOfSpeciesTypes       );
    assertTrue( lo.get(i++) instanceof ListOfUnits              );
    assertTrue( lo.get(i++) instanceof ListOfUnitDefinitions    );
    assertTrue( lo.get(i++) instanceof StoichiometryMath        );    
  }


  /**
   * Reads the Branch model from the SBML Level 1 version 1 specification
   * and inspections ensures that every SBML object and field was read-in
   * correctly.
   *
   * This test was adapted from one of the first libSBML comprehensive
   * "unit" tests, ../../src/TestReadFromFile1.c.
   */
  public void testReadFromFileL1V1 ()
  {
    SBMLDocument     d;
    Model            m;
    Compartment      c;
    KineticLaw       kl;
    Parameter        p;
    Reaction         r;
    Species          s;
    SpeciesReference sr;

    SBMLReader reader   = new SBMLReader();
    String     filename = "../../sbml/test/test-data/l1v1-branch.xml";

    String xml =
        "<?xml version='1.0' encoding='UTF-8'?>"
      + "<sbml xmlns='http://www.sbml.org/sbml/level1' level='1' version='1'>"
      + "<model name='Branch'/>"
      + "</sbml>";

    d = reader.readSBML(filename);
    // d = reader.readSBMLFromString(xml);
    
    if (d == null)
    {
      System.err.println("readSBML('l1v1-branch.xml') returned null.");
      System.exit(1);
    }


    /**
     * <sbml level="1" version="1" ...>
     */
    assertTrue( d.getLevel  () == 1 );
    assertTrue( d.getVersion() == 1 );

    /**
     * <model name="Branch">
     */
    m = d.getModel();

    assertTrue( m.getName().equals("Branch") );


    /**
     * <listOfCompartments>
     *  <compartment name="compartmentOne" volume="1"/>
     * </listOfCompartments>
     */
    assertTrue( m.getNumCompartments() == 1 );

    c = m.getCompartment(0);
    assertTrue( c.getName().equals("compartmentOne") );
    assertTrue( c.getVolume() == 1.0 );


    /**
     * <listOfSpecies>
     *   <species name="S1" initialAmount="0" compartment="compartmentOne"
     *           boundaryCondition="false"/>
     *   <species name="X0" initialAmount="0" compartment="compartmentOne"
     *           boundaryCondition="true"/>
     *   <species name="X1" initialAmount="0" compartment="compartmentOne"
     *           boundaryCondition="true"/>
     *   <species name="X2" initialAmount="0" compartment="compartmentOne"
     *           boundaryCondition="true"/>
     * </listOfSpecies>
     */
    assertTrue( m.getNumSpecies() == 4 );

    s = m.getSpecies(0);
    assertTrue( s.getName().equals("S1") );
    assertTrue( s.getCompartment().equals("compartmentOne") );
    assertTrue( s.getInitialAmount    () == 0.0   );
    assertTrue( s.getBoundaryCondition() == false );

    s = m.getSpecies(1);
    assertTrue( s.getName().equals("X0") );
    assertTrue( s.getCompartment().equals("compartmentOne") );
    assertTrue( s.getInitialAmount    () == 0.0   );
    assertTrue( s.getBoundaryCondition() == true  );

    s = m.getSpecies(2);
    assertTrue( s.getName().equals("X1") );
    assertTrue( s.getCompartment().equals("compartmentOne") );
    assertTrue( s.getInitialAmount    () == 0.0  );
    assertTrue( s.getBoundaryCondition() == true );

    s = m.getSpecies(3);
    assertTrue( s.getName().equals("X2") );
    assertTrue( s.getCompartment().equals("compartmentOne") );
    assertTrue( s.getInitialAmount    () == 0.0  );
    assertTrue( s.getBoundaryCondition() == true );


    /**
     * <listOfReactions>
     *   <reaction name="reaction_1" reversible="false"> ... </reaction>
     *   <reaction name="reaction_2" reversible="false"> ... </reaction>
     *   <reaction name="reaction_3" reversible="false"> ... </reaction>
     * </listOfReactions>
     */
    assertTrue( m.getNumReactions() == 3 );

    r = m.getReaction(0);
    assertTrue( r.getName().equals("reaction_1") );
    assertTrue( r.getReversible() == false );
    assertTrue( r.getFast      () == false );

    r = m.getReaction(1);
    assertTrue( r.getName().equals("reaction_2") );
    assertTrue( r.getReversible() == false );
    assertTrue( r.getFast      () == false );

    r = m.getReaction(2);
    assertTrue( r.getName().equals("reaction_3") );
    assertTrue( r.getReversible() == false );
    assertTrue( r.getFast      () == false );

    /**
     * <reaction name="reaction_1" reversible="false">
     *   <listOfReactants>
     *     <specieReference specie="X0" stoichiometry="1"/>
     *   </listOfReactants>
     *   <listOfProducts>
     *     <specieReference specie="S1" stoichiometry="1"/>
     *   </listOfProducts>
     *   <kineticLaw formula="k1 * X0">
     *     <listOfParameters>
     *       <parameter name="k1" value="0"/>
     *     </listOfParameters>
     *   </kineticLaw>
     * </reaction>
     */
    r = m.getReaction(0);

    assertTrue( r.getNumReactants() == 1 );
    assertTrue( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    assertTrue( sr.getSpecies().equals("X0") );
    assertTrue( sr.getStoichiometry() == 1 );
    assertTrue( sr.getDenominator  () == 1 );

    sr = r.getProduct(0);
    assertTrue( sr.getSpecies().equals("S1") );
    assertTrue( sr.getStoichiometry() == 1 );
    assertTrue( sr.getDenominator  () == 1 );

    kl = r.getKineticLaw();
    assertTrue( kl.getFormula().equals("k1 * X0") );
    assertTrue( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    assertTrue( p.getName().equals("k1") );
    assertTrue( p.getValue() == 0 );

    /**
     * <reaction name="reaction_2" reversible="false">
     *   <listOfReactants>
     *     <specieReference specie="S1" stoichiometry="1"/>
     *   </listOfReactants>
     *   <listOfProducts>
     *     <specieReference specie="X1" stoichiometry="1"/>
     *   </listOfProducts>
     *   <kineticLaw formula="k2 * S1">
     *     <listOfParameters>
     *       <parameter name="k2" value="0"/>
     *     </listOfParameters>
     *   </kineticLaw>
     * </reaction>
     */
    r = m.getReaction(1);
    assertTrue( r.getNumReactants() == 1 );
    assertTrue( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    assertTrue( sr.getSpecies().equals("S1") );
    assertTrue( sr.getStoichiometry() == 1 );
    assertTrue( sr.getDenominator  () == 1 );

    sr = r.getProduct(0);
    assertTrue( sr.getSpecies().equals("X1") );
    assertTrue( sr.getStoichiometry() == 1 );
    assertTrue( sr.getDenominator  () == 1 );

    kl = r.getKineticLaw();
    assertTrue( kl.getFormula().equals("k2 * S1") );
    assertTrue( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    assertTrue( p.getName().equals("k2") );
    assertTrue( p.getValue() == 0 );


    /**
     * <reaction name="reaction_3" reversible="false">
     *   <listOfReactants>
     *     <specieReference specie="S1" stoichiometry="1"/>
     *   </listOfReactants>
     *   <listOfProducts>
     *     <specieReference specie="X2" stoichiometry="1"/>
     *   </listOfProducts>
     *   <kineticLaw formula="k3 * S1">
     *     <listOfParameters>
     *       <parameter name="k3" value="0"/>
     *     </listOfParameters>
     *   </kineticLaw>
     * </reaction>
     */
    r = m.getReaction(2);
    assertTrue( r.getNumReactants() == 1 );
    assertTrue( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    assertTrue( sr.getSpecies().equals("S1") );
    assertTrue( sr.getStoichiometry() == 1 );
    assertTrue( sr.getDenominator  () == 1 );

    sr = r.getProduct(0);
    assertTrue( sr.getSpecies().equals("X2") );
    assertTrue( sr.getStoichiometry() == 1 );
    assertTrue( sr.getDenominator  () == 1 );

    kl = r.getKineticLaw();
    assertTrue( kl.getFormula().equals("k3 * S1") );
    assertTrue( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    assertTrue( p.getName().equals("k3") );
    assertTrue( p.getValue() == 0 );
  }

  /**
   * Reads the Branch model from the SBML Level 1 version 1 specification
   * and inspections ensures that every SBML object and field was read-in
   * correctly.
   *
   * This test was adapted from one of the first libSBML comprehensive
   * "unit" tests, ../../src/TestReadFromFile1.c.
   */
  public void testReadFromFileL2V1 ()
  {
    SBMLDocument     d;
    Model            m;
    Compartment      c;
    KineticLaw       kl;
    Parameter        p;
    Reaction         r;
    Species          s;
    SpeciesReference sr;

    SBMLReader reader   = new SBMLReader();
    String     filename = "../../sbml/test/test-data/l2v1-branch.xml";

    d = reader.readSBML(filename);
    // d = reader.readSBMLFromString(xml);

    if (d == null)
    {
      System.err.println("readSBML('l2v1-branch.xml') returned null.");
      System.exit(1);
    }


    /**
     * <sbml level="2" version="1" ...>
     */
    assertTrue( d.getLevel  () == 2 );
    assertTrue( d.getVersion() == 1 );

    /**
     * <model name="Branch">
     */
    m = d.getModel();

    assertTrue( m.getId().equals("Branch") );


    /**
     * <listOfCompartments>
     *  <compartment id="compartmentOne" volume="1"/>
     * </listOfCompartments>
     */
    assertTrue( m.getNumCompartments() == 1 );

    c = m.getCompartment(0);
    assertTrue( c.getId().equals("compartmentOne") );
    assertTrue( c.getVolume() == 1.0 );


    /**
     * <listOfSpecies>
     *   <species id="S1" initialAmount="0" compartment="compartmentOne"
     *           boundaryCondition="false"/>
     *   <species id="X0" initialAmount="0" compartment="compartmentOne"
     *           boundaryCondition="true"/>
     *   <species id="X1" initialAmount="0" compartment="compartmentOne"
     *           boundaryCondition="true"/>
     *   <species id="X2" initialAmount="0" compartment="compartmentOne"
     *           boundaryCondition="true"/>
     * </listOfSpecies>
     */
    assertTrue( m.getNumSpecies() == 4 );

    s = m.getSpecies(0);
    assertTrue( s.getId().equals("S1") );
    assertTrue( s.getCompartment().equals("compartmentOne") );
    assertTrue( s.getInitialAmount    () == 0.0   );
    assertTrue( s.getBoundaryCondition() == false );

    s = m.getSpecies(1);
    assertTrue( s.getId().equals("X0") );
    assertTrue( s.getCompartment().equals("compartmentOne") );
    assertTrue( s.getInitialAmount    () == 0.0   );
    assertTrue( s.getBoundaryCondition() == true  );

    s = m.getSpecies(2);
    assertTrue( s.getId().equals("X1") );
    assertTrue( s.getCompartment().equals("compartmentOne") );
    assertTrue( s.getInitialAmount    () == 0.0  );
    assertTrue( s.getBoundaryCondition() == true );

    s = m.getSpecies(3);
    assertTrue( s.getId().equals("X2") );
    assertTrue( s.getCompartment().equals("compartmentOne") );
    assertTrue( s.getInitialAmount    () == 0.0  );
    assertTrue( s.getBoundaryCondition() == true );

    /**
     * <listOfReactions>
     *   <reaction id="reaction_1" reversible="false"> ... </reaction>
     *   <reaction id="reaction_2" reversible="false"> ... </reaction>
     *   <reaction id="reaction_3" reversible="false"> ... </reaction>
     * </listOfReactions>
     */
    assertTrue( m.getNumReactions() == 3 );

    r = m.getReaction(0);
    assertTrue( r.getId().equals("reaction_1") );
    assertTrue( r.getReversible() == false );
    assertTrue( r.getFast      () == false );

    r = m.getReaction(1);
    assertTrue( r.getId().equals("reaction_2") );
    assertTrue( r.getReversible() == false );
    assertTrue( r.getFast      () == false );

    r = m.getReaction(2);
    assertTrue( r.getId().equals("reaction_3") );
    assertTrue( r.getReversible() == false );
    assertTrue( r.getFast      () == false );

    /**
     * <reaction id="reaction_1" reversible="false">
     *   <listOfReactants>
     *     <speciesReference species="X0" stoichiometry="1"/>
     *   </listOfReactants>
     *   <listOfProducts>
     *     <speciesReference species="S1" stoichiometry="1"/>
     *   </listOfProducts>
     *   <kineticLaw>
     *      <math xmlns="http://www.w3.org/1998/Math/MathML">
     *          <apply>
     *              <times/>
     *              <ci> k1 </ci>
     *              <ci> X0 </ci>
     *          </apply>
     *      </math>
     *     <listOfParameters>
     *       <parameter name="k1" value="0"/>
     *     </listOfParameters>
     *   </kineticLaw>
     * </reaction>
     */
    r = m.getReaction(0);

    assertTrue( r.getNumReactants() == 1 );
    assertTrue( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    assertTrue( sr.getSpecies().equals("X0") );
    assertTrue( sr.getStoichiometry() == 1 );
    assertTrue( sr.getDenominator()   == 1 );

    sr = r.getProduct(0);
    assertTrue( sr.getSpecies().equals("S1") );
    assertTrue( sr.getStoichiometry() == 1 );
    assertTrue( sr.getDenominator  () == 1 );

    kl = r.getKineticLaw();
    assertTrue( kl.getFormula().equals("k1 * X0") );
    assertTrue( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    assertTrue( p.getId().equals("k1") );
    assertTrue( p.getValue() == 0 );

    /**
     * <reaction id="reaction_2" reversible="false">
     *   <listOfReactants>
     *     <specieReference species="S1" stoichiometry="1"/>
     *   </listOfReactants>
     *   <listOfProducts>
     *     <specieReference species="X1" stoichiometry="1"/>
     *   </listOfProducts>
     *   <kineticLaw>
     *      <math xmlns="http://www.w3.org/1998/Math/MathML">
     *          <apply>
     *              <times/>
     *              <ci> k2 </ci>
     *              <ci> S1 </ci>
     *          </apply>
     *      </math>
     *     <listOfParameters>
     *       <parameter name="k2" value="0"/>
     *     </listOfParameters>
     *   </kineticLaw>
     * </reaction>
     */
    r = m.getReaction(1);
    assertTrue( r.getNumReactants() == 1 );
    assertTrue( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    assertTrue( sr.getSpecies().equals("S1") );
    assertTrue( sr.getStoichiometry() == 1 );

    sr = r.getProduct(0);
    assertTrue( sr.getSpecies().equals("X1") );
    assertTrue( sr.getStoichiometry() == 1 );

    kl = r.getKineticLaw();
    assertTrue( kl.getFormula().equals("k2 * S1") );
    assertTrue( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    assertTrue( p.getId().equals("k2") );
    assertTrue( p.getValue() == 0 );


    /**
     * <reaction id="reaction_3" reversible="false">
     *   <listOfReactants>
     *     <speciesReference species="S1" stoichiometry="1"/>
     *   </listOfReactants>
     *   <listOfProducts>
     *     <speciesReference species="X2" stoichiometry="1"/>
     *   </listOfProducts>
     *   <kineticLaw>
     *      <math xmlns="http://www.w3.org/1998/Math/MathML">
     *          <apply>
     *              <times/>
     *              <ci> k3 </ci>
     *              <ci> S1 </ci>
     *          </apply>
     *      </math>
     *     <listOfParameters>
     *       <parameter name="k3" value="0"/>
     *     </listOfParameters>
     *   </kineticLaw>
     * </reaction>
     */
    r = m.getReaction(2);
    assertTrue( r.getNumReactants() == 1 );
    assertTrue( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    assertTrue( sr.getSpecies().equals("S1") );
    assertTrue( sr.getStoichiometry() == 1 );

    sr = r.getProduct(0);
    assertTrue( sr.getSpecies().equals("X2") );
    assertTrue( sr.getStoichiometry() == 1 );

    kl = r.getKineticLaw();
    assertTrue( kl.getFormula().equals("k3 * S1") );
    assertTrue( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    assertTrue( p.getId().equals("k3") );
    assertTrue( p.getValue() == 0 );
  }

  public void testReadWriteSBML()
  {
    for(int i=0; i < listValidSBML.length; i++){
      String filename_dst1 = "test_copy1.xml";
      String filename_src = "../../sbml/test/test-data/" + listValidSBML[i];

      SBMLReader    r = new SBMLReader();
      SBMLWriter    w = new SBMLWriter();
      SBMLDocument  d = r.readSBML(filename_src);
      String       s1 = w.writeToString(d);
      String       s2;

      libsbml.writeSBML(d,filename_dst1);
      d  = libsbml.readSBML(filename_dst1);
      s2 = libsbml.writeSBMLToString(d);
      assertTrue( s1.equals(s2) );
    }
  }

  public void testSetAnnotation()
  {
    String s1;
    String s2;
    Species   s = new Species();
    XMLOutputStream xos;
    OStringStream   oss;


    XMLNode node = XMLNode.convertStringToXMLNode("<myApp:dim xmlns:myApp='http://www.mysim.org/'>sometext</myApp:dim>");
    s.setAnnotation(node);
    oss = new OStringStream();
    xos = new XMLOutputStream(oss);

    s.write(xos);
    s1 = oss.str();

    s2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    s2 = s2.concat("<species>\n");
    s2 = s2.concat("  <annotation>\n");
    s2 = s2.concat("    <myApp:dim xmlns:myApp=\"http://www.mysim.org/\">");
    s2 = s2.concat("sometext</myApp:dim>\n");
    s2 = s2.concat("  </annotation>\n");
    s2 = s2.concat("</species>");

    assertTrue( s1.equals(s2) );
  }
  
  public void testSetAnnotationAsString()
  {
    String s1;
    String s2;
    Species   s = new Species();
    XMLOutputStream xos;
    OStringStream   oss;



    s.setAnnotation("<myApp:dim xmlns:myApp='http://www.mysim.org/'>sometext</myApp:dim>");
    oss = new OStringStream();
    xos = new XMLOutputStream(oss);

    s.write(xos);
    s1 = oss.str();

    s2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    s2 = s2.concat("<species>\n");
    s2 = s2.concat("  <annotation>\n");
    s2 = s2.concat("    <myApp:dim xmlns:myApp=\"http://www.mysim.org/\">");
    s2 = s2.concat("sometext</myApp:dim>\n");
    s2 = s2.concat("  </annotation>\n");
    s2 = s2.concat("</species>");

    assertTrue( s1.equals(s2) );
  }
  
  public void testParseFormula()
  {
    for(int i=0; i < listTestFormula.length; i++){
      String f1 = listTestFormula[i];
      String f2;
      ASTNode math;

      math = libsbml.parseFormula(f1);
      f2 = libsbml.formulaToString(math);
      assertTrue( f2.equals(f1) );
    }
  }

  public void testCreateSBML()
  {

    SBMLDocument d1,d2;
    Model        m;
    Compartment  c;
    Species      s;
    Parameter    p;
    RateRule    rr;
    Event        e;
    EventAssignment ea;
    Trigger      t1,t2;

    String       str;
    int level   = 2;
    int version = 3;

    /**
     *
     *  CreateSBML
     *
     */

    /* --- SBMLDocument --- */

    d1 = new SBMLDocument();
    d1.setLevelAndVersion(level,version);

    /* --- Model --- */

    m = d1.createModel();

    /* --- Compartment --- */

    c = m.createCompartment();
    c.setId("cell");
    c.setSize(1);

    /* --- Species --- */

    s = m.createSpecies();
    s.setId("P1");
    s.setCompartment("cell");
    s.setInitialConcentration(0);

    s = m.createSpecies();
    s.setId("P2");
    s.setCompartment("cell");
    s.setInitialConcentration(0);

    /* --- Parameters --- */

    p = m.createParameter();
    p.setId("k1");
    p.setValue(1);
    p.setConstant(false);

    p = m.createParameter();
    p.setId("k2");
    p.setValue(0);
    p.setConstant(false);

    p = m.createParameter();
    p.setId("tau");
    p.setValue(0.25);

    /* --- RateRules --- */

    rr = m.createRateRule();
    rr.setVariable("P1");
    rr.setMath(libsbml.parseFormula("k1 - P1"));

    rr = m.createRateRule();
    rr.setVariable("P2");
    rr.setMath(libsbml.parseFormula("k2 - P2"));

    /* --- Event --- */

    e = m.createEvent();
    t1 = new Trigger(libsbml.parseFormula("gt(P1, tau)"));
    e.setTrigger(t1);
    ea = e.createEventAssignment();
    ea.setVariable("k2");
    ea.setMath(libsbml.parseFormula("1"));

    e = m.createEvent();
    t2 = new Trigger(libsbml.parseFormula("leq(P1,tau)"));
    e.setTrigger(t2);
    ea = e.createEventAssignment();
    ea.setVariable("k2");
    ea.setMath(libsbml.parseFormula("0"));

    /**
     *
     *  assertTrueion
     *
     */


    /* --- write SBML --- */

    str = libsbml.writeSBMLToString(d1);
    d2  = libsbml.readSBMLFromString(str);
    m   = d2.getModel();

    /* --- SBMLDocument --- */

    assertTrue( d1.getLevel()   == 2 );
    assertTrue( d1.getVersion() == 3 );

    /* --- Compartment --- */

    c = m.getCompartment(0);
    assertTrue( c.getId().equals("cell") );
    assertTrue( c.getSize() == 1 );

    /* --- Species --- */

    s = m.getSpecies(0);
    assertTrue( s.getId().equals("P1") );
    assertTrue( s.getCompartment().equals("cell") );
    assertTrue( s.getInitialConcentration() == 0 );

    s = m.getSpecies(1);
    assertTrue( s.getId().equals("P2") );
    assertTrue( s.getCompartment().equals("cell") );
    assertTrue( s.getInitialConcentration() == 0 );

    /* --- Parameters --- */

    p = m.getParameter(0);
    assertTrue( p.getId().equals("k1") );
    assertTrue( p.getValue() == 1 );
    assertTrue( p.getConstant() == false );

    p = m.getParameter(1);
    assertTrue( p.getId().equals("k2") );
    assertTrue( p.getValue() == 0 );
    assertTrue( p.getConstant() == false );

    p = m.getParameter(2);
    assertTrue( p.getId().equals("tau") );
    assertTrue( p.getValue() == 0.25 );

    /* --- RateRules --- */

    rr = (RateRule)m.getRule(0);
    assertTrue( rr.getVariable().equals("P1") );
    assertTrue( libsbml.formulaToString(rr.getMath()).equals("k1 - P1"));

    rr =(RateRule)m.getRule(1);
    assertTrue( rr.getVariable().equals("P2") );
    assertTrue( libsbml.formulaToString(rr.getMath()).equals("k2 - P2"));

    /* --- Event --- */

    e = m.getEvent(0);
    ea = e.getEventAssignment(0);
    assertTrue( libsbml.formulaToString(e.getTrigger().getMath()).equals("gt(P1, tau)") );
    assertTrue( ea.getVariable().equals("k2") );
    assertTrue( libsbml.formulaToString(ea.getMath()).equals("1") );

    e = m.getEvent(1);
    ea = e.getEventAssignment(0);
    assertTrue( libsbml.formulaToString(e.getTrigger().getMath()).equals("leq(P1, tau)") );
    assertTrue( ea.getVariable().equals("k2") );
    assertTrue( libsbml.formulaToString(ea.getMath()).equals("0") );
   }

  static void assertTrue(boolean condition) throws AssertionError
  {
    if (condition == true)
    {
      return;
    }
    throw new AssertionError();
  }

  static void assertEquals(Object a, Object b) throws AssertionError
  {
    if ( (a == null) && (b == null) )
    {
      return;
    }
    else if (a.equals(b))
    {
      return;
    }

    throw new AssertionError();
  }

  static void assertNotEquals(Object a, Object b) throws AssertionError
  {
    if ( (a == null) && (b == null) )
    {
      throw new AssertionError();
    }
    else if (a.equals(b))
    {
      throw new AssertionError();
    }
  }

  /**
   * Loads the SWIG-generated libSBML Java module when this class is
   * loaded, or reports a sensible diagnostic message about why it failed.
   */
  static
  {
    String varname;
    String shlibname;

    if (System.getProperty("mrj.version") != null)
    {
      varname = "DYLD_LIBRARY_PATH";    // We're on a Mac.
      shlibname = "libsbmlj.jnilib and/or libsbml.dylib";
    }
    else
    {
      varname = "LD_LIBRARY_PATH";      // We're not on a Mac.
      shlibname = "libsbmlj.so and/or libsbml.so";
    }

    try
    {
      System.loadLibrary("sbmlj");
      // For extra safety, check that the jar file is in the classpath.
      Class.forName("org.sbml.libsbml.libsbml");
    }
    catch (SecurityException e)
    {
      e.printStackTrace();
      System.err.println("Could not load the libSBML library files due to a"+
                         " security exception.\n");
      System.exit(1);
    }
    catch (UnsatisfiedLinkError e)
    {
      e.printStackTrace();
      System.err.println("Error: could not link with the libSBML library files."+
                         " It is likely\nyour " + varname +
                         " environment variable does not include the directories\n"+
                         "containing the " + shlibname + " library files.\n");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      e.printStackTrace();
      System.err.println("Error: unable to load the file libsbmlj.jar."+
                         " It is likely\nyour -classpath option and CLASSPATH" +
                         " environment variable\n"+
                         "do not include the path to libsbmlj.jar.\n");
      System.exit(1);
    }
  }
}
