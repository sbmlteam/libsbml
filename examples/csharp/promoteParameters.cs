/**
 * @file    promoteParameters.cs
 * @brief   promotes all local to global paramters
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

  public class promoteParameters
  {
    public static void Main (string[] args)
    {
      if (args.Length != 2)
      {
        string myname = Path.GetFileName(Environment.GetCommandLineArgs()[0]);
        Console.WriteLine("Usage: {0} input-filenameoutput-filename", myname);
        Environment.Exit(1);
      }

      string inputFile      = args[0];
      string outputFile     = args[1];

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
	  
	  /* add an option that we want to promote parameters */
	  props.addOption("promoteLocalParameters", true, "Promotes all Local Parameters to Global ones");
	  
	  /* perform the conversion */
	  if (sbmlDoc.convert(props) != libsbml.LIBSBML_OPERATION_SUCCESS)
	  {
	  	Console.WriteLine ("conversion failed ... ");
	  	Environment.Exit(3);   
	  }
	  
      writer.writeSBML(sbmlDoc, outputFile);

      Console.WriteLine("[OK] promoted paramters from {0} and wrote to {1}", inputFile, outputFile);
    }
  }

}
