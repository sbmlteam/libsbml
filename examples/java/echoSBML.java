/**
 * @file    echoSBML.java
 * @brief   Echos (and pretty prints) an SBML model.
 * @author  Akira Funahashi (translated from libSBML C++ examples (using other Java examples provided by Nicolas Rodriguez))
 * @author  Ben Bornstein
 * @author  Michael Hucka
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

import org.sbml.libsbml.SBMLReader;
import org.sbml.libsbml.SBMLWriter;

public class echoSBML
{
  public static void main (String[] args)
  {
    if (args.length != 2)
    {
      println("Usage: java echoSBML input-filename output-filename");
      System.exit(2);
    }

	SBMLReader reader     = new SBMLReader();
	SBMLWriter writer     = new SBMLWriter();
	writer.writeSBML(reader.readSBML(args[0]), args[1]);
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
    catch (UnsatisfiedLinkError e)
    {
      System.err.println("Error: could not link with the libSBML library."+
			 "  It is likely\nyour " + varname +
			 " environment variable does not include\nthe"+
			 " directory containing the libsbml library file.");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      System.err.println("Error: unable to load the file libsbmlj.jar."+
			 "  It is likely\nyour " + varname + " environment"+
			 " variable or CLASSPATH variable\ndoes not include"+
			 " the directory containing the libsbmlj.jar file.");
      System.exit(1);
    }
    catch (SecurityException e)
    {
      System.err.println("Could not load the libSBML library files due to a"+
			 " security exception.");
    }
  }
}
