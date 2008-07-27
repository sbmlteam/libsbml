/**
 * @file   TestRW.cs
 * @brief  A very simple C# binding test program (only read/write SBML files)
 * @author Akiya Jouraku
 *
 * $Id:$
 * $HeadURL:$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2008 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *--------------------------------------------------------------------------->*/

namespace TestLibSBMLCSharp
{
    using System;
    using System.Text;
    using System.IO;
    using libsbml;

    class ReadAndWriteSBML
    {
        static int numErrors = 0;

        static void Main(string[] args)
        {
            // 
            // Checks arguments
            //
            if (args.Length < 1)
            {
                Console.Out.WriteLine("Need one or more argument: fileName or directoryname [...]");
                Environment.Exit(1);
            }

            // 
            // Run tests
            //
            Console.Out.WriteLine();
            for (int i = 0; i < args.Length; i++)
            {
                if (Directory.Exists(args[i]))
                {
                    string[] files = System.IO.Directory.GetFiles(args[i]);
                    for (int j = 0; j < files.Length; j++)
                    {
                        if (files[j].EndsWith(".xml"))
                        {
                          testReadAndWriteSBML(files[j]);
                        }
                    }
                }
                else
                {
                    testReadAndWriteSBML(args[i]);
                }
            }

            testCreateSBML();

            // 
            // Prints result
            //
            if ( numErrors > 0 )
            {
              if (numErrors > 1)
              {
                Console.Out.WriteLine("\n" + numErrors + " tests failed.\n");
              }
              else
              {
                Console.Out.WriteLine("\n" + numErrors + " test failed.\n");
              }
              Environment.Exit(1);
            }

            Console.Out.WriteLine("\nAll tests passed.\n");

            Environment.Exit(0);
        }


        static void testReadAndWriteSBML(string file)
        {
            string ofile = "test.xml";

            SBMLDocument d1 = testReadSBMLFromString(file);
            if ( d1 != null )
            {
              testWriteSBML(d1, ofile);
              testWriteCompressedSBML(d1, ofile);
            }

            SBMLDocument d2 = testReadSBMLFromFile(file);
            if ( d2 != null )
            {
              testWriteSBML(d2, ofile);
              testWriteCompressedSBML(d2, ofile);
            }
        }


        static SBMLDocument testReadSBMLFromString(string file)
        {
            if (!File.Exists(file))
            {
                ERR("[ReadSBMLFromString] Error: (" + file + ") : No such file or directory.");
                return null;
            }

            StreamReader oReader = new StreamReader(file);
            string       sSBML   = oReader.ReadToEnd();

            SBMLDocument d = libsbml.readSBMLFromString(sSBML);

            if (d == null)
            {
                ERR("[ReadSBMLFromString] Error: (" + file + ") SBMLDocument is null.");
                return null;
            }

            if (d.getModel() == null)
            {
                for (int i = 0; i < d.getNumErrors(); i++)
                {
                    ERR("[ReadSBMLFromString] Error: (" + file + ") : " + d.getError(i).getMessage());
                }
                return null;
            }
            else if ( d.getNumErrors() > 0 )
            {
                bool iserror = false;
                for (int i = 0; i < d.getNumErrors(); i++)
                {
                    long severity = d.getError(i).getSeverity();
                    if ( (severity == libsbml.LIBSBML_SEV_ERROR) ||
                         (severity == libsbml.LIBSBML_SEV_FATAL)
                       )
                    {
                      iserror = true;
                      ERR("[ReadSBMLFromString] Error: (" + file + ") : " + d.getError(i).getMessage());
                    }
                }
                if (iserror)
                {
                  return null;
                }
            }

            OK();

            return d;
        }


        static SBMLDocument testReadSBMLFromFile(string file)
        {
            if (!File.Exists(file))
            {
                ERR("[ReadSBMLFromFile] Error: (" + file + ") : No such file or directory.");
                return null;
            }

            SBMLDocument d = libsbml.readSBML(file);

            if (d.getModel() == null)
            {
                for (int i = 0; i < d.getNumErrors(); i++)
                {
                    ERR("[ReadSBMLFromFile] Error: (" + file + " : )" + d.getError(i).getMessage());
                }
                return null;
            }
            else if ( d.getNumErrors() > 0 )
            {
                bool iserror = false;
                for (int i = 0; i < d.getNumErrors(); i++)
                {
                    long severity = d.getError(i).getSeverity();
                    if ( (severity == libsbml.LIBSBML_SEV_ERROR) ||  
                         (severity == libsbml.LIBSBML_SEV_FATAL)
                       )
                    {
                      iserror = true; 
                      ERR("[ReadSBMLFromFile] Error: (" + file + " : )" + d.getError(i).getMessage());
                    }
                }
                if (iserror)
                {
                  return null;
                }
            }

            OK();

            return d;
        }


        static void testWriteSBML(SBMLDocument d, string file)
        {
            try
            {
                if ( libsbml.writeSBML(d, file) == 0)
                {
                    ERR("[WriteSBML] Error: cannot write " + file);
                }
                else
                {
                    OK();
                }
             }
             catch (Exception e)
             {
                 ERR("[WriteSBML] (" + file + ") Error: Exception thrown : " + e.Message);
             }

             try
             {
                 string sbmlstr = libsbml.writeSBMLToString(d);

                 if ( libsbml.writeSBML(libsbml.readSBMLFromString(sbmlstr), file) == 0)
                 {
                     ERR("[WriteSBML] Error: cannot write " + file);
                 }
                 else
                 {
                     OK();
                 }
              }
              catch (Exception e)
              {
                  ERR("[WriteSBML] (" + file + ") Error: Exception thrown : " + e.Message);
              }
        }


        static void testCreateSBML()
        {
            SBMLDocument d = new SBMLDocument();
            d.setLevelAndVersion(2, 3);

            Model m = d.createModel();
            m.setId("testmodel");

            Compartment c1 = m.createCompartment();
            Compartment c2 = m.createCompartment();
            c1.setId("c1");
            c2.setId("c2");

            Species s1 = m.createSpecies();
            Species s2 = m.createSpecies();

            string id1 = "s1";
            string id2 = "s2";

            // strings with non-ASCII characters (multibyte characters)
            string n1  = "γ-lyase";
            string n2  = "β-synthase";

            s1.setId(id1);
            s1.setName(n1);
            s1.setCompartment("c1");

            s2.setId(id2);
            s2.setName(n2);
            s2.setCompartment("c2");

            string file = "test2.xml";

            try
            {
                if ( libsbml.writeSBML(d, file) == 0)
                {
                    ERR("[CreateSBML] Error: cannot write " + file);
                }
                else
                {
                    OK();
                }
            }
            catch (Exception e)
            {
                ERR("[CreateSBML] (" + file + ") Error: Exception thrown : " + e.Message);
            }
        }


        static void testWriteCompressedSBML(SBMLDocument d, string file)
        {
            if ( SBMLWriter.hasZlib() )
            {
                //
                // write/read gzip file
                //
                string cfile = file + ".gz";
                try
                {
                    if ( libsbml.writeSBML(d, cfile) == 0)
                    {
                        ERR("[WriteCompressedSBML] Error: cannot write " + file + ".gz");
                    }
                    else
                    {
                        OK();
                    }
                 }
                 catch (Exception e)
                 {
                    ERR("[WriteCompressedSBML] (" + cfile + ") Error: Exception thrown : " + e.Message);
                 }

                 //
                 // write/read zip file
                 //
                 cfile = file + ".zip";
                 try
                 {
                     if ( libsbml.writeSBML(d, cfile) == 0)
                     {
                          ERR("[WriteCompressedSBML] Error: cannot write " + file + ".zip");
                      }
                      else
                      {
                          OK();
                      }
                  }
                  catch (Exception e)
                  {
                      ERR("[WriteCompressedSBML] (" + cfile + ") Error: Exception thrown : " + e.Message);
                  }
              }

              if ( SBMLWriter.hasBzip2() )
              {
                  //
                  // write/read bzip2 file
                  //
                  string cfile = file + ".bz2";
                  try
                  {
                      if (libsbml.writeSBML(d, cfile) == 0)
                      {
                          ERR("[WriteCompressedSBML] Error: cannot write " + cfile);
                      }
                      else
                      {
                          if ( libsbml.readSBML(cfile) == null)
                          {
                              ERR("[WriteCompressedSBML] Error: failed to read " + cfile);
                          }
                          else
                          {
                              OK();
                          }
                      }
                  }
                  catch (Exception e)
                  {
                      ERR("[WriteCompressedSBML] (" + cfile + ") Error: Exception thrown : " + e.Message);
                  }
              }
        }

        static void OK()
        {
            Console.Out.Write(".");
        }

        static void ERR(string message)
        {
            Console.Out.WriteLine(message);
            ++numErrors;
        }

    }

}
