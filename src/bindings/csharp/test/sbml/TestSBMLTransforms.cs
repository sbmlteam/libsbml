/// 
///  @file    TestSBMLTransforms.cs
///  @brief   SBMLTransforms unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestSBMLTransforms.cpp
///  with the help of conversion sciprt (ctest_converter.pl).
/// 
/// <!---------------------------------------------------------------------------
///  This file is part of libSBML.  Please visit http://sbml.org for more
///  information about SBML, and the latest version of libSBML.
/// 
///  Copyright 2005-2010 California Institute of Technology.
///  Copyright 2002-2005 California Institute of Technology and
///                      Japan Science and Technology Corporation.
///  
///  This library is free software; you can redistribute it and/or modify it
///  under the terms of the GNU Lesser General Public License as published by
///  the Free Software Foundation.  A copy of the license agreement is provided
///  in the file named "LICENSE.txt" included with this software distribution
///  and also available online as http://sbml.org/software/libsbml/license.html
/// --------------------------------------------------------------------------->*/


namespace LibSBMLCSTest {

  using libsbml;

  using  System.IO;

  public class TestSBMLTransforms {
    public class AssertionError : System.Exception 
    {
      public AssertionError() : base()
      {
        
      }
    }


    static void assertTrue(bool condition)
    {
      if (condition == true)
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertEquals(object a, object b)
    {
      if ( (a == null) && (b == null) )
      {
        return;
      }
      else if ( (a == null) || (b == null) )
      {
        throw new AssertionError();
      }
      else if (a.Equals(b))
      {
        return;
      }
  
      throw new AssertionError();
    }

    static void assertNotEquals(object a, object b)
    {
      if ( (a == null) && (b == null) )
      {
        throw new AssertionError();
      }
      else if ( (a == null) || (b == null) )
      {
        return;
      }
      else if (a.Equals(b))
      {
        throw new AssertionError();
      }
    }

    static void assertEquals(bool a, bool b)
    {
      if ( a == b )
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertNotEquals(bool a, bool b)
    {
      if ( a != b )
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertEquals(int a, int b)
    {
      if ( a == b )
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertNotEquals(int a, int b)
    {
      if ( a != b )
      {
        return;
      }
      throw new AssertionError();
    }


    public void test_SBMLTransforms_replaceFD()
    {
      SBMLReader reader = new SBMLReader();
      SBMLDocument d;
      Model m;
      ASTNode ast;
      FunctionDefinition fd;
      ListOfFunctionDefinitions lofd;
      string filename =  "../../sbml/test/test-data/";
      filename += "multiple-functions.xml";
      d = reader.readSBML(filename);
      if (d == null);
      {
      }
      m = d.getModel();
      assertTrue( m.getNumFunctionDefinitions() == 2 );
      ast = m.getReaction(2).getKineticLaw().getMath();
      assertTrue((  "f(S1, p) * compartmentOne / t" == libsbml.formulaToString(ast) ));
      fd = m.getFunctionDefinition(0);
      SBMLTransforms.replaceFD(((ASTNode) ast),fd);
      assertTrue((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ));
      ast = m.getReaction(1).getKineticLaw().getMath();
      assertTrue((  "f(f(S1, p), compartmentOne) / t" == libsbml.formulaToString(ast) ));
      SBMLTransforms.replaceFD(((ASTNode) ast),fd);
      assertTrue((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ));
      ast = m.getReaction(0).getKineticLaw().getMath();
      assertTrue((  "g(f(S1, p), compartmentOne) / t" == libsbml.formulaToString(ast) ));
      SBMLTransforms.replaceFD(((ASTNode) ast),fd);
      assertTrue((  "g(S1 * p, compartmentOne) / t" == libsbml.formulaToString(ast) ));
      fd = m.getFunctionDefinition(1);
      SBMLTransforms.replaceFD(((ASTNode) ast),fd);
      assertTrue((  "f(S1 * p, compartmentOne) / t" == libsbml.formulaToString(ast) ));
      ast = m.getReaction(0).getKineticLaw().getMath();
      lofd = m.getListOfFunctionDefinitions();
      SBMLTransforms.replaceFD(((ASTNode) ast),lofd);
      assertTrue((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ));
      d.expandFunctionDefinitions();
      assertTrue( d.getModel().getNumFunctionDefinitions() == 0 );
      ast = d.getModel().getReaction(0).getKineticLaw().getMath();
      assertTrue((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ));
      ast = d.getModel().getReaction(1).getKineticLaw().getMath();
      assertTrue((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ));
      ast = d.getModel().getReaction(2).getKineticLaw().getMath();
      assertTrue((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ));
    }

  }
}
