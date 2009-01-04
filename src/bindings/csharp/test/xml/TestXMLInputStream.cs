/// 
///  @file    TestXMLInputStream.cs
///  @brief   XMLInputStream unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id:$
///  $HeadURL:$
/// 
///  This test file was converted from src/sbml/test/TestXMLInputStream.c
///  with the help of conversion sciprt (ctest_converter.pl).
/// 
/// <!---------------------------------------------------------------------------
///  This file is part of libSBML.  Please visit http://sbml.org for more
///  information about SBML, and the latest version of libSBML.
/// 
///  Copyright 2005-2009 California Institute of Technology.
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

  public class TestXMLInputStream {
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


    public string LV_L1v1()
    {
      return "level=\"1\" version=\"1\">\n";
    }

    public string LV_L1v2()
    {
      return "level=\"1\" version=\"2\">\n";
    }

    public string LV_L2v1()
    {
      return "level=\"2\" version=\"1\">\n";
    }

    public string LV_L2v2()
    {
      return "level=\"2\" version=\"2\">\n";
    }

    public string NS_L1()
    {
      return "xmlns=\"http://www.sbml.org/sbml/level1\" ";
    }

    public string NS_L2v1()
    {
      return "xmlns=\"http://www.sbml.org/sbml/level2\" ";
    }

    public string NS_L2v2()
    {
      return "xmlns=\"http://www.sbml.org/sbml/level2/version2\" ";
    }

    public string SBML_END()
    {
      return "</sbml>\n";
    }

    public string SBML_START()
    {
      return "<sbml ";
    }

    public string XML_START()
    {
      return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    }

    public string wrapSBML_L1v1(string s)
    {
      string r = XML_START();
      r += SBML_START();
      r += NS_L1();
      r += LV_L1v1();
      r += s;
      r += SBML_END();
      return r;
    }

    public string wrapSBML_L1v2(string s)
    {
      string r = XML_START();
      r += SBML_START();
      r += NS_L1();
      r += LV_L1v2();
      r += s;
      r += SBML_END();
      return r;
    }

    public string wrapSBML_L2v1(string s)
    {
      string r = XML_START();
      r += SBML_START();
      r += NS_L2v1();
      r += LV_L2v1();
      r += s;
      r += SBML_END();
      return r;
    }

    public string wrapSBML_L2v2(string s)
    {
      string r = XML_START();
      r += SBML_START();
      r += NS_L2v2();
      r += LV_L2v2();
      r += s;
      r += SBML_END();
      return r;
    }

    public string wrapXML(string s)
    {
      string r = XML_START();
      r += s;
      return r;
    }

    public void test_XMLInputStream_create()
    {
      string text = wrapSBML_L2v1("  <model id=\"Branch\"/>\n");
      XMLInputStream stream = new  XMLInputStream(text,false, "");
      assertTrue( stream != null );
      assertTrue( stream.isEOF() == false );
      assertTrue( stream.isGood() == true );
      assertTrue( stream.isError() == false );
      stream.next();
      assertTrue( (  "UTF-8" != stream.getEncoding() ) == false );
      stream = null;
    }

    public void test_XMLInputStream_next_peek()
    {
      string text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
    "<sbml " + 
    "xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\">\n" + 
    "  <model id=\"Branch\"/>\n" + 
    "</sbml>";
      XMLInputStream stream = new  XMLInputStream(text,false, "");
      XMLToken next = stream.peek();
      assertTrue( stream != null );
      assertTrue( (  "sbml" != next.getName() ) == false );
      XMLToken next1 = stream.next();
      assertTrue( (  "sbml" != next1.getName() ) == false );
      stream = null;
    }

    public void test_XMLInputStream_setErrorLog()
    {
      string text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
    "<sbml " + 
    "xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\">\n" + 
    "<listOfFunctionDefinitions>\n" + 
    "<notes>My Functions</notes>\n" + 
    "<functionDefinition/>\n" + 
    "</listOfFunctionDefinitions>\n" + 
    "<listOfUnitDefinitions>\n" + 
    "<notes>My Units</notes>\n" + 
    "<unitDefinition/>\n" + 
    "</listOfUnitDefinitions>\n" + 
    "</sbml>";
      XMLInputStream stream = new  XMLInputStream(text,false, "");
      assertTrue( stream != null );
      XMLErrorLog log = new  XMLErrorLog();
      stream.setErrorLog(log);
      assertTrue( stream.getErrorLog() == log );
    }

    public void test_XMLInputStream_skip()
    {
      string text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
    "<sbml " + 
    "xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\">\n" + 
    "<listOfFunctionDefinitions>\n" + 
    "<notes>My Functions</notes>\n" + 
    "<functionDefinition/>\n" + 
    "</listOfFunctionDefinitions>\n" + 
    "<listOfUnitDefinitions>\n" + 
    "<notes>My Units</notes>\n" + 
    "<unitDefinition/>\n" + 
    "</listOfUnitDefinitions>\n" + 
    "</sbml>";
      XMLInputStream stream = new  XMLInputStream(text,false, "");
      assertTrue( stream != null );
      XMLToken next = stream.next();
      stream.skipText();
      stream.skipPastEnd(stream.next());
      stream.skipText();
      next = stream.next();
      assertTrue( (  "listOfUnitDefinitions" != next.getName() ) == false );
      stream = null;
    }

  }
}
