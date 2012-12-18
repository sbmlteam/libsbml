/**
 * @file    stripPackage.cs
 * @brief   Strips the given package from the given SBML file.
 * @author  Frank T. Bergmann
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

namespace LibSBMLCSExample
{
  using System;
  using System.IO;
  using libsbmlcs;

  public class stripPackage
  {
    public static void Main (string[] args)
    {
      if (args.Length != 3)
      {
        string myname = Path.GetFileName(Environment.GetCommandLineArgs()[0]);
        Console.WriteLine("Usage: {0} input-filename package-to-strip output-filename", myname);
        Environment.Exit(1);
      }

      string inputFile      = args[0];
      string packageToStrip = args[1];
      string outputFile     = args[2];

      if ( ! File.Exists(inputFile) )
      {
        Console.WriteLine("[Error] {0} : No such file.", inputFile);
        Environment.Exit(1);        
      }

      SBMLReader   reader  = new SBMLReader();
      SBMLWriter   writer  = new SBMLWriter();
      SBMLDocument sbmlDoc = reader.readSBML(inputFile);

      if ( sbmlDoc.getErrorLog().getNumFailsWithSeverity(libsbml.LIBSBML_SEV_ERROR) > 0)
      {
        sbmlDoc.printErrors(); 
        Console.WriteLine("[Error] Cannot read {0}", inputFile);
        Environment.Exit(1);        
      }

	  /* create a new conversion properties structure */
      ConversionProperties props = new ConversionProperties();
	  
	  /* add an option that we want to strip a given package */
	  props.addOption("stripPackage", true, "Strip SBML Level 3 package constructs from the model");
	  
	  /* add an option with the package we want to remove */
	  props.addOption("package", packageToStrip, "Name of the SBML Level 3 package to be stripped");
	  
	  /* perform the conversion */
	  if (sbmlDoc.convert(props) != libsbml.LIBSBML_OPERATION_SUCCESS)
	  {
	  	Console.WriteLine ("conversion failed ... ");
	  	Environment.Exit(3);   
	  }
	  
      writer.writeSBML(sbmlDoc, outputFile);

      Console.WriteLine("[OK] Stripped package '{0}' from {1} and wrote to {2}", packageToStrip, inputFile, outputFile);
    }
  }

}
