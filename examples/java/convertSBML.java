/**
 * @file    convertSBML.java
 * @brief   Converts SBML L1 documents (any version) to L2v3
 * @author  Michael Hucka
 * @author  Nicolas Rodriguez
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;
import org.sbml.libsbml.libsbml;
 

public class convertSBML
{
  public static void main (String[] args)
  {
    long latestLevel   = SBMLDocument.getDefaultLevel();
    long latestVersion = SBMLDocument.getDefaultVersion();

    if (args.length != 2)
    {
      println("Usage: convertSBML input-filename output-filename\n" +
	      "This program will attempt to convert an SBML model either to\n" +
	      "Level " + latestLevel + " " + "Version " + latestVersion + " " +
	      "(if the model is not already), or if the\n" +
	      "model is already at that Level and Version, this program\n" +
	      "will attempt to convert it to SBML Level 1 Version 2.");
      System.exit(1);
    }

    String inputFile      = args[0];
    String outputFile     = args[1];
    SBMLDocument document = libsbml.readSBML(inputFile);

    if (document.getNumErrors() > 0)
    {
      document.printErrors(libsbml.cerr);
      println("Conversion skipped.  Please correct the above problems first.");
      System.exit(1);
    }

    /**
     * If the given model is not already L2v3, assume that the user wants
     * to convert it to the latest release of SBML (which is L2v3
     * currently).  If the model is already L2v3, assume that the user
     * wants to attempt to convert it down to Level 1 (specifically L1v2).
     */

    if (document.getLevel() < latestLevel ||
        document.getVersion() < latestVersion)
    {
      document.setLevelAndVersion(latestLevel, latestVersion);
    }
    else
    {
      document.setLevelAndVersion(1, 2);
    }
      
    if (document.getNumErrors() > 0)
    {
      println("Conversion Error(s):");
      document.printErrors(libsbml.cerr);
      println("Conversion skipped.  Either libSBML does not (yet) have\n" +
              "the ability to convert this model, or (automatic) conversion"+
              "\nis not possible.\n");
      System.exit(1);
    }
    else
    {
      libsbml.writeSBML(document, outputFile);
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
      System.err.println("Could not load the libSBML library files due to a"+
			 " security exception.\n");
    }
    catch (UnsatisfiedLinkError e)
    {
      System.err.println("Error: could not link with the libSBML library."+
			 "  It is likely\nyour " + varname +
			 " environment variable does not include\nthe"+
			 " directory containing the libsbml.dylib library"+
			 " file.\n");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      System.err.println("Error: unable to load the file libsbmlj.jar."+
			 "  It is likely\nyour " + varname +
			 " environment variable does not include\nthe "+
			 " directory containing the libsbmlj.jar file.\n");
      System.exit(1);
    }
  }
}
