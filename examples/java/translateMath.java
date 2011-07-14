/**
 * @file    translateMath.java
 * @brief   Translates infix formulas into MathML and vice-versa
 * @author  Nicolas Rodriguez (translated from libSBML C++ examples)
 * @author  Ben Bornstein
 * @author  Michael Hucka
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
    String shlibname;

    if (System.getProperty("os.name").startsWith("Mac OS"))
    {
      varname = "DYLD_LIBRARY_PATH";    // We're on a Mac.
      shlibname = "'libsbmlj.jnilib'";
    }
    else
    {
      varname = "LD_LIBRARY_PATH";      // We're not on a Mac.
      shlibname = "'libsbmlj.so' and/or 'libsbml.so'";
    }

    try
    {
      System.loadLibrary("sbmlj");
      // For extra safety, check that the jar file is in the classpath.
      Class.forName("org.sbml.libsbml.libsbml");
    }
    catch (UnsatisfiedLinkError e)
    {
      System.err.println("Error encountered while attempting to load libSBML:");
      e.printStackTrace();
      System.err.println("Please check the value of your " + varname +
                         " environment variable and/or" +
                         " your 'java.library.path' system property" +
                         " (depending on which one you are using) to" +
                         " make sure it list the directories needed to" +
                         " find the " + shlibname + " library file and the" +
                         " libraries it depends upon (e.g., the XML parser).");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      System.err.println("Error: unable to load the file 'libsbmlj.jar'." +
                         " It is likely that your -classpath command line " +
                         " setting or your CLASSPATH environment variable " +
                         " do not include the file 'libsbmlj.jar'.");
      System.exit(1);
    }
    catch (SecurityException e)
    {
      System.err.println("Error encountered while attempting to load libSBML:");
      e.printStackTrace();
      System.err.println("Could not load the libSBML library files due to a"+
                         " security exception.\n");
      System.exit(1);
    }
  }
}

