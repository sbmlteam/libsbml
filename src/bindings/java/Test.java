/**
 * \file    Test.java
 * \brief   Simple test of the libSBML Java module
 * \author  Ben Bornstein
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


import org.sbml.libsbml.*;

import java.io.File;

public class Test
{
  static int NumTest = 0;
  static int NumNG   = 0;
  static int NumPass = 0;

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

  public static void main (String args[])
  {
    testImplicitDowncastRule();
    testImplicitDowncastSBase();
    testReadFromFileL1V1();
    testReadFromFileL2V1();
    testReadWriteSBML();
    testParseFormula();
    testOFStream();
    testOStringStream();
    testValidationInvalidSBML();
    testValidationValidSBML();
    testCreateSBML();
    testCloneObject();
    testSetAnnotation();
    testSetAnnotationAsString();

    System.gc();

    System.out.println("\n==========> RESULT <==========");
    if(NumNG == 0){
      System.out.println("All Tests Passed.");
    }
    else{
      System.out.println("NG " + NumNG + " PASS " + NumPass);
    }
  }

  static void testValidationValidSBML ()
  {
    System.out.println(">>>>> testValidationValidSBML <<<<<");
    SBMLReader r = new SBMLReader();

    for (int i=0; i < listValidSBML.length; i++) {
      SBMLDocument d = r.readSBML("../../sbml/test/test-data/" + listValidSBML[i]);
      d.checkConsistency();
      long n = d.getNumErrors();
      if (n > 0)
      {
        SBMLErrorLog log = d.getErrorLog();
        Assert(log.getNumFailsWithSeverity(SBMLError.Error) == 0);
        if (log.getNumFailsWithSeverity(SBMLError.Error) > 0) 
        {
          System.out.println(listValidSBML[i] + " is judged as invalid");
          d.printErrors(libsbml.cerr);
        }
      }
    }
  }

  static void testValidationInvalidSBML ()
  {
    System.out.println(">>>>> testValidationInvalidSBML <<<<<");

    SBMLReader r = new SBMLReader();
    for (int i=0; i < listInvalidSBML.length; i++){
      SBMLDocument d = r.readSBML("../../sbml/test/test-data/" + listInvalidSBML[i]);
      d.checkConsistency();
      Assert(d.getNumErrors() != 0);
    }
  }

  static void testOStream ()
  {
    String filename_src = "l2v1-branch.xml";

    System.out.println(">>>>> testOStream <<<<<");
    try{
      SBMLReader   r = new SBMLReader();
      SBMLDocument d = r.readSBML("../../sbml/test/test-data/" + filename_src);
      XMLOutputStream xos = new XMLOutputStream(libsbml.cout);
      d.write(xos);
      libsbml.cout.endl();
    }
    catch(Exception e){
      e.printStackTrace();
    }
  }

  static void testOFStream ()
  {
    String filename_src = "l2v1-branch.xml";
    String filename_dst = "test_write.xml";
    boolean is_append   = false;

    System.out.println(">>>>> testOFStream <<<<<");
    try{
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
      Assert(d.getNumErrors() == 0);
      s2 = libsbml.writeSBMLToString(d);
      Assert(s1.equals(s2));
    }
    catch(Exception e){
      e.printStackTrace();
    }
  }

  static void testOStringStream ()
  {
    String filename_src = "l2v1-branch.xml";
    String filename_dst = "test_write.xml";
    boolean is_append   = false;

    System.out.println(">>>>> testOStringStream <<<<<");
    try{
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

     //System.out.println("writeToString :\n" + s1);
     //System.out.println("OStringStream :\n" + s2);

      Assert(s1.equals(s2));
    }
    catch(Exception e){
        e.printStackTrace();
    }
  }

  static void testImplicitDowncastRule()
  {
    Model m = new Model();
    
    System.out.println(">>>>> testImplicitDowncastRule <<<<<");

    m.addRule( new AssignmentRule("x", "1 + 1") );
    m.addRule( new RateRule("x", "1 + 1") );
    m.addRule( new AlgebraicRule("1 + 1") );
    Assert( m.getNumRules() == 3 );
    Assert( m.getRule(0) instanceof AssignmentRule );
    Assert( m.getRule(1) instanceof RateRule );
    Assert( m.getRule(2) instanceof AlgebraicRule );
  }
    
  static void testCloneObject()
  {
    System.out.println(">>>>> testCloneObject <<<<<");

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
    checkCloneObject( new FormulaUnitsData()         );
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
    checkCloneObject( new ListFormulaUnitsData()     );
    checkCloneObject( new StoichiometryMath()        );
  }

  static void checkCloneObject(SBase sb) 
  {
    SBase clone = sb.cloneObject();
    String en1  = sb.getElementName();
    String en2  = clone.getElementName();
    //System.out.println("org " + en1 + " clone " + en2);
    Assert( en1.equals(en2) );
  }

  static void testImplicitDowncastSBase()
  {
    ListOf lo = new ListOf();

    System.out.println(">>>>> testImplicitDowncastSBase <<<<<");

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
    lo.append( new FormulaUnitsData()         );
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
    lo.append( new ListFormulaUnitsData()     );
    lo.append( new StoichiometryMath()        );

    /**
     * ListOf.get() returns an object of type SBase, so the result must be
     * downcast to an object of the appropriate type.
     */

    int i = 0;

    Assert( lo.get(i++) instanceof FunctionDefinition       );
    Assert( lo.get(i++) instanceof UnitDefinition           );
    Assert( lo.get(i++) instanceof CompartmentType          );
    Assert( lo.get(i++) instanceof SpeciesType              );
    Assert( lo.get(i++) instanceof Compartment              );
    Assert( lo.get(i++) instanceof Species                  );
    Assert( lo.get(i++) instanceof Parameter                );
    Assert( lo.get(i++) instanceof InitialAssignment        );
    Assert( lo.get(i++) instanceof AssignmentRule           );
    Assert( lo.get(i++) instanceof AlgebraicRule            );
    Assert( lo.get(i++) instanceof RateRule                 );
    Assert( lo.get(i++) instanceof Constraint               );
    Assert( lo.get(i++) instanceof Reaction                 );
    Assert( lo.get(i++) instanceof SpeciesReference         );
    Assert( lo.get(i++) instanceof ModifierSpeciesReference );
    Assert( lo.get(i++) instanceof Event                    );
    Assert( lo.get(i++) instanceof EventAssignment          );
    Assert( lo.get(i++) instanceof Unit                     );
    Assert( lo.get(i++) instanceof UnitDefinition           );
    Assert( lo.get(i++) instanceof FormulaUnitsData         );
    Assert( lo.get(i++) instanceof Trigger                  );
    Assert( lo.get(i++) instanceof Delay                    );
    Assert( lo.get(i++) instanceof ListOf                   );
    Assert( lo.get(i++) instanceof ListOfCompartments       );
    Assert( lo.get(i++) instanceof ListOfCompartmentTypes   );
    Assert( lo.get(i++) instanceof ListOfConstraints        );
    Assert( lo.get(i++) instanceof ListOfEvents             );
    Assert( lo.get(i++) instanceof ListOfEventAssignments   );
    Assert( lo.get(i++) instanceof ListOfFunctionDefinitions);
    Assert( lo.get(i++) instanceof ListOfInitialAssignments );
    Assert( lo.get(i++) instanceof ListOfParameters         );
    Assert( lo.get(i++) instanceof ListOfReactions          );
    Assert( lo.get(i++) instanceof ListOfRules              );
    Assert( lo.get(i++) instanceof ListOfSpecies            );
    Assert( lo.get(i++) instanceof ListOfSpeciesReferences  );
    Assert( lo.get(i++) instanceof ListOfSpeciesTypes       );
    Assert( lo.get(i++) instanceof ListOfUnits              );
    Assert( lo.get(i++) instanceof ListOfUnitDefinitions    );
    Assert( lo.get(i++) instanceof ListFormulaUnitsData     );
    Assert( lo.get(i++) instanceof StoichiometryMath        );
  }


  /**
   * Reads the Branch model from the SBML Level 1 version 1 specification
   * and inspections ensures that every SBML object and field was read-in
   * correctly.
   *
   * This test was adapted from one of the first libSBML comprehensive
   * "unit" tests, ../../src/TestReadFromFile1.c.
   */
  static void testReadFromFileL1V1 ()
  {
    SBMLDocument     d;
    Model            m;
    Compartment      c;
    KineticLaw       kl;
    Parameter        p;
    Reaction         r;
    Species          s;
    SpeciesReference sr;

    System.out.println(">>>>> testReadFromFileL1V1 <<<<<");

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
      System.exit(0);
    }


    /**
     * <sbml level="1" version="1" ...>
     */
    Assert( d.getLevel  () == 1 );
    Assert( d.getVersion() == 1 );

    /**
     * <model name="Branch">
     */
    m = d.getModel();

    Assert( m.getName().equals("Branch") );


    /**
     * <listOfCompartments>
     *  <compartment name="compartmentOne" volume="1"/>
     * </listOfCompartments>
     */
    Assert( m.getNumCompartments() == 1 );

    c = m.getCompartment(0);
    Assert( c.getName().equals("compartmentOne") );
    Assert( c.getVolume() == 1.0 );


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
    Assert( m.getNumSpecies() == 4 );

    s = m.getSpecies(0);
    Assert( s.getName().equals("S1") );
    Assert( s.getCompartment().equals("compartmentOne") );
    Assert( s.getInitialAmount    () == 0.0   );
    Assert( s.getBoundaryCondition() == false );

    s = m.getSpecies(1);
    Assert( s.getName().equals("X0") );
    Assert( s.getCompartment().equals("compartmentOne") );
    Assert( s.getInitialAmount    () == 0.0   );
    Assert( s.getBoundaryCondition() == true  );

    s = m.getSpecies(2);
    Assert( s.getName().equals("X1") );
    Assert( s.getCompartment().equals("compartmentOne") );
    Assert( s.getInitialAmount    () == 0.0  );
    Assert( s.getBoundaryCondition() == true );

    s = m.getSpecies(3);
    Assert( s.getName().equals("X2") );
    Assert( s.getCompartment().equals("compartmentOne") );
    Assert( s.getInitialAmount    () == 0.0  );
    Assert( s.getBoundaryCondition() == true );


    /**
     * <listOfReactions>
     *   <reaction name="reaction_1" reversible="false"> ... </reaction>
     *   <reaction name="reaction_2" reversible="false"> ... </reaction>
     *   <reaction name="reaction_3" reversible="false"> ... </reaction>
     * </listOfReactions>
     */
    Assert( m.getNumReactions() == 3 );

    r = m.getReaction(0);
    Assert( r.getName().equals("reaction_1") );
    Assert( r.getReversible() == false );
    Assert( r.getFast      () == false );

    r = m.getReaction(1);
    Assert( r.getName().equals("reaction_2") );
    Assert( r.getReversible() == false );
    Assert( r.getFast      () == false );

    r = m.getReaction(2);
    Assert( r.getName().equals("reaction_3") );
    Assert( r.getReversible() == false );
    Assert( r.getFast      () == false );

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

    Assert( r.getNumReactants() == 1 );
    Assert( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    Assert( sr.getSpecies().equals("X0") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator  () == 1 );

    sr = r.getProduct(0);
    Assert( sr.getSpecies().equals("S1") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator  () == 1 );

    kl = r.getKineticLaw();
    Assert( kl.getFormula().equals("k1 * X0") );
    Assert( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    Assert( p.getName().equals("k1") );
    Assert( p.getValue() == 0 );

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
    Assert( r.getNumReactants() == 1 );
    Assert( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    Assert( sr.getSpecies().equals("S1") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator  () == 1 );

    sr = r.getProduct(0);
    Assert( sr.getSpecies().equals("X1") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator  () == 1 );

    kl = r.getKineticLaw();
    Assert( kl.getFormula().equals("k2 * S1") );
    Assert( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    Assert( p.getName().equals("k2") );
    Assert( p.getValue() == 0 );


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
    Assert( r.getNumReactants() == 1 );
    Assert( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    Assert( sr.getSpecies().equals("S1") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator  () == 1 );

    sr = r.getProduct(0);
    Assert( sr.getSpecies().equals("X2") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator  () == 1 );

    kl = r.getKineticLaw();
    Assert( kl.getFormula().equals("k3 * S1") );
    Assert( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    Assert( p.getName().equals("k3") );
    Assert( p.getValue() == 0 );
  }

  /**
   * Reads the Branch model from the SBML Level 1 version 1 specification
   * and inspections ensures that every SBML object and field was read-in
   * correctly.
   *
   * This test was adapted from one of the first libSBML comprehensive
   * "unit" tests, ../../src/TestReadFromFile1.c.
   */
  static void testReadFromFileL2V1 ()
  {
    SBMLDocument     d;
    Model            m;
    Compartment      c;
    KineticLaw       kl;
    Parameter        p;
    Reaction         r;
    Species          s;
    SpeciesReference sr;

    System.out.println(">>>>> testReadFromFileL2V1 <<<<<");

    SBMLReader reader   = new SBMLReader();
    String     filename = "../../sbml/test/test-data/l2v1-branch.xml";

    d = reader.readSBML(filename);
    // d = reader.readSBMLFromString(xml);

    if (d == null)
    {
      System.err.println("readSBML('l2v1-branch.xml') returned null.");
      System.exit(0);
    }


    /**
     * <sbml level="2" version="1" ...>
     */
    Assert( d.getLevel  () == 2 );
    Assert( d.getVersion() == 1 );

    /**
     * <model name="Branch">
     */
    m = d.getModel();

    Assert( m.getId().equals("Branch") );


    /**
     * <listOfCompartments>
     *  <compartment id="compartmentOne" volume="1"/>
     * </listOfCompartments>
     */
    Assert( m.getNumCompartments() == 1 );

    c = m.getCompartment(0);
    Assert( c.getId().equals("compartmentOne") );
    Assert( c.getVolume() == 1.0 );


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
    Assert( m.getNumSpecies() == 4 );

    s = m.getSpecies(0);
    Assert( s.getId().equals("S1") );
    Assert( s.getCompartment().equals("compartmentOne") );
    Assert( s.getInitialAmount    () == 0.0   );
    Assert( s.getBoundaryCondition() == false );

    s = m.getSpecies(1);
    Assert( s.getId().equals("X0") );
    Assert( s.getCompartment().equals("compartmentOne") );
    Assert( s.getInitialAmount    () == 0.0   );
    Assert( s.getBoundaryCondition() == true  );

    s = m.getSpecies(2);
    Assert( s.getId().equals("X1") );
    Assert( s.getCompartment().equals("compartmentOne") );
    Assert( s.getInitialAmount    () == 0.0  );
    Assert( s.getBoundaryCondition() == true );

    s = m.getSpecies(3);
    Assert( s.getId().equals("X2") );
    Assert( s.getCompartment().equals("compartmentOne") );
    Assert( s.getInitialAmount    () == 0.0  );
    Assert( s.getBoundaryCondition() == true );

    /**
     * <listOfReactions>
     *   <reaction id="reaction_1" reversible="false"> ... </reaction>
     *   <reaction id="reaction_2" reversible="false"> ... </reaction>
     *   <reaction id="reaction_3" reversible="false"> ... </reaction>
     * </listOfReactions>
     */
    Assert( m.getNumReactions() == 3 );

    r = m.getReaction(0);
    Assert( r.getId().equals("reaction_1") );
    Assert( r.getReversible() == false );
    Assert( r.getFast      () == false );

    r = m.getReaction(1);
    Assert( r.getId().equals("reaction_2") );
    Assert( r.getReversible() == false );
    Assert( r.getFast      () == false );

    r = m.getReaction(2);
    Assert( r.getId().equals("reaction_3") );
    Assert( r.getReversible() == false );
    Assert( r.getFast      () == false );

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

    Assert( r.getNumReactants() == 1 );
    Assert( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    Assert( sr.getSpecies().equals("X0") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator()   == 1 );

    sr = r.getProduct(0);
    Assert( sr.getSpecies().equals("S1") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator  () == 1 );

    kl = r.getKineticLaw();
    Assert( kl.getFormula().equals("k1 * X0") );
    Assert( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    Assert( p.getId().equals("k1") );
    Assert( p.getValue() == 0 );

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
    Assert( r.getNumReactants() == 1 );
    Assert( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    Assert( sr.getSpecies().equals("S1") );
    Assert( sr.getStoichiometry() == 1 );

    sr = r.getProduct(0);
    Assert( sr.getSpecies().equals("X1") );
    Assert( sr.getStoichiometry() == 1 );

    kl = r.getKineticLaw();
    Assert( kl.getFormula().equals("k2 * S1") );
    Assert( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    Assert( p.getId().equals("k2") );
    Assert( p.getValue() == 0 );


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
    Assert( r.getNumReactants() == 1 );
    Assert( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    Assert( sr.getSpecies().equals("S1") );
    Assert( sr.getStoichiometry() == 1 );

    sr = r.getProduct(0);
    Assert( sr.getSpecies().equals("X2") );
    Assert( sr.getStoichiometry() == 1 );

    kl = r.getKineticLaw();
    Assert( kl.getFormula().equals("k3 * S1") );
    Assert( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    Assert( p.getId().equals("k3") );
    Assert( p.getValue() == 0 );
  }

  public static void testReadWriteSBML()
  {
    System.out.println(">>>>> testReadWriteSBML <<<<<");
    for(int i=0; i < listValidSBML.length; i++){
      testReadWriteSBML("../../sbml/test/test-data/" + listValidSBML[i]);
    }
  }

  public static void testReadWriteSBML(String filename_src)
  {
    String filename_dst1 = "test_copy1.xml";

    SBMLReader    r = new SBMLReader();
    SBMLWriter    w = new SBMLWriter();
    SBMLDocument  d = r.readSBML(filename_src);
    String       s1 = w.writeToString(d);
    String       s2;

    libsbml.writeSBML(d,filename_dst1);
    d  = libsbml.readSBML(filename_dst1);
    s2 = libsbml.writeSBMLToString(d);
    //System.out.println("SBML String :\n" + s1);
    Assert( s1.equals(s2) );
  }

  public static void testSetAnnotation()
  {
    System.out.println(">>>>> testSetAnnotation <<<<<");

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

//    System.out.println("SBML String :\n" + s1);
//    System.out.println("SBML String :\n" + s2);
    Assert( s1.equals(s2) );
  }
  
  public static void testSetAnnotationAsString()
  {
    System.out.println(">>>>> testSetAnnotationAsString <<<<<");

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

    //    System.out.println("SBML String :\n" + s1);
    //    System.out.println("SBML String :\n" + s2);
    Assert( s1.equals(s2) );
  }
  
  public static void testParseFormula()
  {
    System.out.println(">>>>> testParseFormula <<<<<");
    for(int i=0; i < listTestFormula.length; i++){
      testParseFormula(listTestFormula[i]);
    }
  }

  public static void testParseFormula(String f1)
  {
    String f2;
    ASTNode math;

    math = libsbml.parseFormula(f1);
    f2 = libsbml.formulaToString(math);
    //System.out.println("formulaToString : " + f2);
    Assert( f2.equals(f1) );
  }

  public static void testCreateSBML()
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
     *  Assertion
     *
     */

    System.out.println(">>>>> testCreateSBML <<<<<");

    /* --- write SBML --- */

    str = libsbml.writeSBMLToString(d1);
    //System.out.println(str);
    d2  = libsbml.readSBMLFromString(str);
    m   = d2.getModel();

    /* --- SBMLDocument --- */

    Assert( d1.getLevel()   == 2 );
    Assert( d1.getVersion() == 3 );

    /* --- Compartment --- */

    c = m.getCompartment(0);
    Assert( c.getId().equals("cell") );
    Assert( c.getSize() == 1 );

    /* --- Species --- */

    s = m.getSpecies(0);
    Assert( s.getId().equals("P1") );
    Assert( s.getCompartment().equals("cell") );
    Assert( s.getInitialConcentration() == 0 );

    s = m.getSpecies(1);
    Assert( s.getId().equals("P2") );
    Assert( s.getCompartment().equals("cell") );
    Assert( s.getInitialConcentration() == 0 );

    /* --- Parameters --- */

    p = m.getParameter(0);
    Assert( p.getId().equals("k1") );
    Assert( p.getValue() == 1 );
    Assert( p.getConstant() == false );

    p = m.getParameter(1);
    Assert( p.getId().equals("k2") );
    Assert( p.getValue() == 0 );
    Assert( p.getConstant() == false );

    p = m.getParameter(2);
    Assert( p.getId().equals("tau") );
    Assert( p.getValue() == 0.25 );

    /* --- RateRules --- */

    rr = (RateRule)m.getRule(0);
    Assert( rr.getVariable().equals("P1") );
    Assert( libsbml.formulaToString(rr.getMath()).equals("k1 - P1"));

    rr =(RateRule)m.getRule(1);
    Assert( rr.getVariable().equals("P2") );
    Assert( libsbml.formulaToString(rr.getMath()).equals("k2 - P2"));

    /* --- Event --- */

    e = m.getEvent(0);
    ea = e.getEventAssignment(0);
    Assert( libsbml.formulaToString(e.getTrigger().getMath()).equals("gt(P1, tau)") );
    Assert( ea.getVariable().equals("k2") );
    Assert( libsbml.formulaToString(ea.getMath()).equals("1") );

    e = m.getEvent(1);
    ea = e.getEventAssignment(0);
    Assert( libsbml.formulaToString(e.getTrigger().getMath()).equals("leq(P1, tau)") );
    Assert( ea.getVariable().equals("k2") );
    Assert( libsbml.formulaToString(ea.getMath()).equals("0") );
   }

  /**
   *  Print assertion result 
   */
  static void Assert(boolean condition)
  {
    ++NumTest;
    if (condition == false)
    {
      ++NumNG;
      System.out.println("Test " + NumTest + " <NG>");
      //System.err.println("Assertion failed!");
      //throw new RuntimeException();
    }
    else{
      ++NumPass;
      System.out.println("Test " + NumTest + " <PASS>");
    }
  }

  
  /**
   * Loads the SWIG generated libsbml Java module when this class is
   * loaded.
   */
  static
  {
    System.loadLibrary("sbmlj");
  }
}
