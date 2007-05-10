/**
 * @file    readSBML.java
 * @brief   Similar to validateSBML, but without the validation
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


import java.io.File;

import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;
import org.sbml.libsbml.OstreamWrapper;
 

public class readSBML
{
  public static void main (String[] args)
  {
    if (args.length != 1)
    {
      System.out.println("Usage: java readSBML <filename>");
      System.exit(1);
    }

    String filename = args[0];
    OstreamWrapper stderr = new OstreamWrapper(OstreamWrapper.CERR);
    SBMLReader reader     = new SBMLReader();
    SBMLDocument document;
    long start, stop, size;

    start    = System.currentTimeMillis();
    document = reader.readSBML(filename);
    stop     = System.currentTimeMillis();

    size     = new File(args[0]).length();

    println("        filename: " + filename);
    println("       file size: " + size);
    println("  read time (ms): " + (stop - start));
    println("        error(s): " + document.getNumErrors());

    if (document.getNumErrors() > 0)
    {
      document.printErrors(stderr);
      println("Printing skipped.  Please correct the above problems first.");
      System.exit(1);
    }
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
