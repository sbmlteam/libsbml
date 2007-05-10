/**
 * @file    translateMath.java
 * @brief   Translates infix formulas into MathML and vice-versa
 * @author  Nicolas Rodriguez (translated from libSBML C++ examples)
 * @author  Ben Bornstein
 * @author  Michael Hucka
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


import java.io.IOException;

import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;
import org.sbml.libsbml.libsbml;


public class translateMath
{
  public static void main (String[] args)
  {
    if (args.length != 0)
    {
      println("Usage: java translateMath\n");
      System.exit(1);
    }

    println("This program translates infix formulas into MathML and" +
            " vice-versa. \nCtrl-D or Ctrl-Z quits and triggers" +
            " translation.\n");
        
    char c;
    int  i = 0;

    String mathStr = new String();

    try
    {
      // Hit CTRL-Z on PC's to send EOF, CTRL-D on Unix
      while (i != -1)
      {
        // Read a character from keyboard.
        i  = System.in.read();

	// System.in.read() returns everything, including things
	// like return, backspace, etc.  Filter those out.
	if (i < 32 || i > 127)
	  continue;

        // 1 byte character is returned as an int, so cast it to a char.
        c = (char) i;
        mathStr += c;
      }
    }
    catch (IOException ioe)
    {
      println( "IO error:" + ioe );
    }

    // If the input starts with '<', assume it's XML content.
    String result = (mathStr.charAt(0) == '<') ?
                    translateMathML(mathStr) : translateInfix(mathStr);

    println("Result:\n");
    println(result);
  }


  /**
   * Translates the given infix formula into MathML.
   *
   * @return the MathML as a string.
   */
  static String translateInfix (String formula)
  {
    return libsbml.writeMathMLToString( libsbml.parseFormula(formula) );
  }


  /**
   * Translates the given MathML into an infix formula.  The MathML must
   * contain no leading whitespace, but an XML header is optional.
   *
   * @return the infix formula as a string.
   */
  static String translateMathML (String xml)
  {
    return libsbml.formulaToString( libsbml.readMathMLFromString(xml) );
  }


  static void println (String msg)
  {
    System.out.println(msg);
  }


  /**
   * Loads the SWIG-generated libSBML Java module when this class is
   * loaded, or reports a sensible diagnostic message about why it failed.
   */
  static
  {
    String varname;

    if (System.getProperty("mrj.version") != null)
      varname = "DYLD_LIBRARY_PATH";	// We're on a Mac.
    else
      varname = "LD_LIBRARY_PATH";	// We're not on a Mac.

    try
    {
      System.loadLibrary("sbmlj");
      // For extra safety, check that the jar file is in the classpath.
      Class.forName("org.sbml.libsbml.libsbml");
    }
    catch (SecurityException e)
    {
      System.err.println("\nCould not load the libSBML library files due to a"+
			 " security exception.\n");
    }
    catch (UnsatisfiedLinkError e)
    {
      System.err.println("\nError: could not link with the libSBML library."+
			 "  It is likely\nyour " + varname +
			 " environment variable does not include\nthe"+
			 " directory containing the libsbml.dylib library"+
			 " file.\n");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      System.err.println("\nError: unable to load the file libsbmlj.jar."+
			 "  It is likely\nyour " + varname +
			 " environment variable does not include\nthe "+
			 " directory containing the libsbmlj.jar file.\n");
      System.exit(1);
    }
  }
}
