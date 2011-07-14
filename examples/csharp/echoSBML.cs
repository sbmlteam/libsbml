/**
 * @file    echoSBML.cs
 * @brief   Echos an SBML model.
 * @author  Akiya Jouraku (translated from libSBML C++ examples)
 * @author  Ben Bornstein
 * @author  Michael Hucka
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

namespace LibSBMLCSExample
{
  using System;
  using System.IO;
  using libsbmlcs;

  public class echoSBML
  {
    public static void Main (string[] args)
    {
      if (args.Length != 2)
      {
        string myname = Path.GetFileName(Environment.GetCommandLineArgs()[0]);
        Console.WriteLine("Usage: {0} input-filename output-filename", myname);
        Environment.Exit(1);
      }

      string inputFile  = args[0];
      string outputFile = args[1];

      if ( ! File.Exists(inputFile) )
      {
        Console.WriteLine("[Error] {0} : No such file.", inputFile);
        Environment.Exit(1);        
      }

      SBMLReader   reader  = new SBMLReader();
      SBMLWriter   writer  = new SBMLWriter();
      SBMLDocument sbmlDoc = reader.readSBML(inputFile);

      if ( sbmlDoc.getNumErrors() > 0)
      {
        sbmlDoc.printErrors(); 
        Console.WriteLine("[Error] Cannot read {0}", inputFile);
        Environment.Exit(1);        
      }

      writer.writeSBML(sbmlDoc, outputFile);

      Console.WriteLine("[OK] Echoed {0} to {1}", inputFile, outputFile);
    }
  }

}
