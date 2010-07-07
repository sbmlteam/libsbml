/**
 * @file   TestRW.cs
 * @brief  A very simple C# binding test program (only read/write SBML files)
 * @author Akiya Jouraku
 *
 * $Id: TestRW.cs 10866 2010-01-29 19:52:27Z mhucka $
 * $URL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/bindings/csharp/TestRW.cs $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
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
        static int numErrors  = 0;
        static int defLevel   = 2;
        static int defVersion = 4;

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
            testCovariantReturnTypes();

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

        static void testCovariantReturnTypes()
        {
          //
          // test clone() methods 
          //
          testClone(new Compartment(defLevel,defVersion));
          testClone(new CompartmentType(defLevel,defVersion));
          testClone(new Constraint(defLevel,defVersion));
          testClone(new Delay(defLevel,defVersion));
          testClone(new Event(defLevel,defVersion));
          testClone(new EventAssignment(defLevel,defVersion));
          testClone(new FunctionDefinition(defLevel,defVersion));
          testClone(new InitialAssignment(defLevel,defVersion));
          testClone(new KineticLaw(defLevel,defVersion));
          testClone(new Model(defLevel,defVersion));
          testClone(new Parameter(defLevel,defVersion));
          testClone(new Reaction(defLevel,defVersion));
          testClone(new AlgebraicRule(defLevel,defVersion));
          testClone(new AssignmentRule(defLevel,defVersion));
          testClone(new RateRule(defLevel,defVersion));
          testClone(new SBMLDocument(defLevel,defVersion));
          testClone(new Species(defLevel,defVersion));
          testClone(new SpeciesReference(defLevel,defVersion));
          testClone(new SpeciesType(defLevel,defVersion));
          testClone(new StoichiometryMath(defLevel,defVersion));
          testClone(new Trigger(defLevel,defVersion));
          testClone(new Unit(defLevel,defVersion));
          testClone(new UnitDefinition(defLevel,defVersion));
          testClone(new ListOf());
          testClone(new ListOfCompartmentTypes());
          testClone(new ListOfCompartments());
          testClone(new ListOfConstraints());
          testClone(new ListOfEventAssignments());
          testClone(new ListOfEvents());
          testClone(new ListOfFunctionDefinitions());
          testClone(new ListOfInitialAssignments());
          testClone(new ListOfParameters());
          testClone(new ListOfReactions());
          testClone(new ListOfRules());
          testClone(new ListOfSpecies());
          testClone(new ListOfSpeciesReferences());
          testClone(new ListOfSpeciesTypes());
          testClone(new ListOfUnitDefinitions());
          testClone(new ListOfUnits());

          //
          // test ListOfXXX:get() methods 
          //
          testListOfGet(new ListOfCompartmentTypes(), new CompartmentType(defLevel,defVersion));
          testListOfGet(new ListOfCompartments(), new Compartment(defLevel,defVersion));
          testListOfGet(new ListOfConstraints(), new Constraint(defLevel,defVersion));
          testListOfGet(new ListOfEventAssignments(), new EventAssignment(defLevel,defVersion));
          testListOfGet(new ListOfEvents(), new Event(defLevel,defVersion));
          testListOfGet(new ListOfFunctionDefinitions(), new FunctionDefinition(defLevel,defVersion));
          testListOfGet(new ListOfInitialAssignments(), new InitialAssignment(defLevel,defVersion));
          testListOfGet(new ListOfParameters(), new Parameter(defLevel,defVersion));
          testListOfGet(new ListOfReactions(), new Reaction(defLevel,defVersion));
          testListOfGet(new ListOfRules(), new AssignmentRule(defLevel,defVersion));
          testListOfGet(new ListOfRules(), new AlgebraicRule(defLevel,defVersion));
          testListOfGet(new ListOfRules(), new RateRule(defLevel,defVersion));
          testListOfGet(new ListOfSpecies(), new Species(defLevel,defVersion));
          testListOfGet(new ListOfSpeciesReferences(), new SpeciesReference(defLevel,defVersion));
          testListOfGet(new ListOfSpeciesTypes(), new SpeciesType(defLevel,defVersion));
          testListOfGet(new ListOfUnitDefinitions(), new UnitDefinition(defLevel,defVersion));
          testListOfGet(new ListOfUnits(), new Unit(defLevel,defVersion));

          //
          // test ListOfXXX:remove() methods 
          //
          testListOfRemove(new ListOfCompartmentTypes(), new CompartmentType(defLevel,defVersion));
          testListOfRemove(new ListOfCompartments(), new Compartment(defLevel,defVersion));
          testListOfRemove(new ListOfConstraints(), new Constraint(defLevel,defVersion));
          testListOfRemove(new ListOfEventAssignments(), new EventAssignment(defLevel,defVersion));
          testListOfRemove(new ListOfEvents(), new Event(defLevel,defVersion));
          testListOfRemove(new ListOfFunctionDefinitions(), new FunctionDefinition(defLevel,defVersion));
          testListOfRemove(new ListOfInitialAssignments(), new InitialAssignment(defLevel,defVersion));
          testListOfRemove(new ListOfParameters(), new Parameter(defLevel,defVersion));
          testListOfRemove(new ListOfReactions(), new Reaction(defLevel,defVersion));
          testListOfRemove(new ListOfRules(), new AssignmentRule(defLevel,defVersion));
          testListOfRemove(new ListOfRules(), new AlgebraicRule(defLevel,defVersion));
          testListOfRemove(new ListOfRules(), new RateRule(defLevel,defVersion));
          testListOfRemove(new ListOfSpecies(), new Species(defLevel,defVersion));
          testListOfRemove(new ListOfSpeciesReferences(), new SpeciesReference(defLevel,defVersion));
          testListOfRemove(new ListOfSpeciesTypes(), new SpeciesType(defLevel,defVersion));
          testListOfRemove(new ListOfUnitDefinitions(), new UnitDefinition(defLevel,defVersion));
          testListOfRemove(new ListOfUnits(), new Unit(defLevel,defVersion));
        }

        static void testClone(SBase s)
        {
          string ename = s.getElementName();
          SBase c = s.clone();

          if ( c is Compartment ) { Compartment x = (s as Compartment).clone(); c = x; }
          else if ( c is CompartmentType ) { CompartmentType x = (s as CompartmentType).clone(); c = x; }
          else if ( c is Constraint ) { Constraint x = (s as Constraint).clone(); c = x; }
          else if ( c is Delay ) { Delay x = (s as Delay).clone(); c = x; }
          else if ( c is Event ) { Event x = (s as Event).clone(); c = x; }
          else if ( c is EventAssignment ) { EventAssignment x = (s as EventAssignment).clone(); c = x; }
          else if ( c is FunctionDefinition ) { FunctionDefinition x = (s as FunctionDefinition).clone(); c = x; }
          else if ( c is InitialAssignment ) { InitialAssignment x = (s as InitialAssignment).clone(); c = x; }
          else if ( c is KineticLaw ) { KineticLaw x = (s as KineticLaw).clone(); c = x; }
          // currently return type of ListOf::clone() is SBase
          else if ( c is ListOf ) { SBase x = (s as ListOf).clone(); c = x; }
          else if ( c is Model ) { Model x = (s as Model).clone(); c = x; }
          else if ( c is Parameter ) { Parameter x = (s as Parameter).clone(); c = x; }
          else if ( c is Reaction ) { Reaction x = (s as Reaction).clone(); c = x; }
          else if ( c is AlgebraicRule ) { AlgebraicRule x = (s as AlgebraicRule).clone(); c = x; }
          else if ( c is AssignmentRule ) { AssignmentRule x = (s as AssignmentRule).clone(); c = x; }
          else if ( c is RateRule ) { RateRule x = (s as RateRule).clone(); c = x; }
          else if ( c is SBMLDocument ) { SBMLDocument x = (s as SBMLDocument).clone(); c = x; }
          else if ( c is Species ) { Species x = (s as Species).clone(); c = x; }
          else if ( c is SpeciesReference ) { SpeciesReference x = (s as SpeciesReference).clone(); c = x; }
          else if ( c is SpeciesType ) { SpeciesType x = (s as SpeciesType).clone(); c = x; }
          else if ( c is SpeciesReference ) { SpeciesReference x = (s as SpeciesReference).clone(); c = x; }
          else if ( c is StoichiometryMath ) { StoichiometryMath x = (s as StoichiometryMath).clone(); c = x; }
          else if ( c is Trigger ) { Trigger x = (s as Trigger).clone(); c = x; }
          else if ( c is Unit ) { Unit x = (s as Unit).clone(); c = x; }
          else if ( c is UnitDefinition ) { UnitDefinition x = (s as UnitDefinition).clone(); c = x; }
          else if ( c is ListOfCompartmentTypes ) { ListOfCompartmentTypes x = (s as ListOfCompartmentTypes).clone(); c = x; }
          else if ( c is ListOfCompartments ) { ListOfCompartments x = (s as ListOfCompartments).clone(); c = x; }
          else if ( c is ListOfConstraints ) { ListOfConstraints x = (s as ListOfConstraints).clone(); c = x; }
          else if ( c is ListOfEventAssignments ) { ListOfEventAssignments x = (s as ListOfEventAssignments).clone(); c = x; }
          else if ( c is ListOfEvents ) { ListOfEvents x = (s as ListOfEvents).clone(); c = x; }
          else if ( c is ListOfFunctionDefinitions ) { ListOfFunctionDefinitions x = (s as ListOfFunctionDefinitions).clone(); c = x; }
          else if ( c is ListOfInitialAssignments ) { ListOfInitialAssignments x = (s as ListOfInitialAssignments).clone(); c = x; }
          else if ( c is ListOfParameters ) { ListOfParameters x = (s as ListOfParameters).clone(); c = x; }
          else if ( c is ListOfReactions ) { ListOfReactions x = (s as ListOfReactions).clone(); c = x; }
          else if ( c is ListOfRules ) { ListOfRules x = (s as ListOfRules).clone(); c = x; }
          else if ( c is ListOfSpecies ) { ListOfSpecies x = (s as ListOfSpecies).clone(); c = x; }
          else if ( c is ListOfSpeciesReferences ) { ListOfSpeciesReferences x = (s as ListOfSpeciesReferences).clone(); c = x; }
          else if ( c is ListOfSpeciesTypes ) { ListOfSpeciesTypes x = (s as ListOfSpeciesTypes).clone(); c = x; }
          else if ( c is ListOfUnitDefinitions ) { ListOfUnitDefinitions x = (s as ListOfUnitDefinitions).clone(); c = x; }
          else if ( c is ListOfUnits ) { ListOfUnits x = (s as ListOfUnits).clone(); c = x; }
          else
          {
            ERR("[testClone] Error: (" + ename + ") : clone() failed.");
            return;
          }

          if ( c == null)
          {
            ERR("[testClone] Error: (" + ename + ") : clone() failed.");
            return;
          }

          string enameClone = c.getElementName();

          if ( ename == enameClone )
          {
            //Console.Out.WriteLine("[testClone] OK: (" + ename + ") clone(" + enameClone + ") : type match.");
            OK();
          }
          else
          {
            ERR("[testClone] Error: (" + ename + ") clone(" + enameClone + ") : type mismatch.");
          } 

        }

        static void testListOfGet(ListOf lof, SBase s)
        {
          string ename = s.getElementName();

          lof.append(s);
          SBase c = lof.get(0);

          if ( c is CompartmentType ) { CompartmentType x = (lof as ListOfCompartmentTypes).get(0); c = x; }
          else if ( c is Compartment ) { Compartment x = (lof as ListOfCompartments).get(0); c = x; }
          else if ( c is Constraint )  { Constraint x = (lof as ListOfConstraints).get(0); c = x; }
          else if ( c is EventAssignment ) { EventAssignment x = (lof as ListOfEventAssignments).get(0); c = x; }
          else if ( c is Event ) { Event x = (lof as ListOfEvents).get(0); c = x; }
          else if ( c is FunctionDefinition ) { FunctionDefinition x = (lof as ListOfFunctionDefinitions).get(0); c = x; }
          else if ( c is InitialAssignment ) { InitialAssignment x = (lof as ListOfInitialAssignments).get(0); c = x; }
          else if ( c is Parameter ) { Parameter x = (lof as ListOfParameters).get(0); c = x; }
          else if ( c is Reaction ) { Reaction x = (lof as ListOfReactions).get(0); c = x; }
          else if ( c is Rule ) { Rule x = (lof as ListOfRules).get(0); c = x; }
          else if ( c is Species ) { Species x = (lof as ListOfSpecies).get(0); c = x; }
          else if ( c is SpeciesReference ) {SimpleSpeciesReference x = (lof as ListOfSpeciesReferences).get(0); c = x; }
          else if ( c is SpeciesType ) { SpeciesType x = (lof as ListOfSpeciesTypes).get(0); c = x; }
          else if ( c is UnitDefinition ) { UnitDefinition x = (lof as ListOfUnitDefinitions).get(0); c = x; }
          else if ( c is Unit ) { Unit x = (lof as ListOfUnits).get(0); c = x; }
          else
          {
            ERR("[testListOfGet] Error: (" + ename + ") : ListOfXXX::get() failed.");
            return;
          }

          if ( c == null)
          {
            ERR("[testListOfGet] Error: (" + ename + ") : ListOfXXX::get() failed.");
            return;
          }

          string enameGet = c.getElementName();

          if ( ename == enameGet )
          {
            //Console.Out.WriteLine("[testListOfGet] OK: (" + ename + ") get(" + enameGet + ") : type match.");
            OK();
          }
          else
          {
            ERR("[testListOfGet] Error: (" + ename + ") get(" + enameGet + ") : type mismatch.");
          }
        }

        static void testListOfRemove(ListOf lof, SBase s)
        {
          string ename = s.getElementName();

          lof.append(s);
          SBase c = lof.get(0);

          if ( c is CompartmentType ) { CompartmentType x = (lof as ListOfCompartmentTypes).remove(0); c = x; }
          else if ( c is Compartment ) { Compartment x = (lof as ListOfCompartments).remove(0); c = x; }
          else if ( c is Constraint )  { Constraint x = (lof as ListOfConstraints).remove(0); c = x; }
          else if ( c is EventAssignment ) { EventAssignment x = (lof as ListOfEventAssignments).remove(0); c = x; }
          else if ( c is Event ) { Event x = (lof as ListOfEvents).remove(0); c = x; }
          else if ( c is FunctionDefinition ) { FunctionDefinition x = (lof as ListOfFunctionDefinitions).remove(0); c = x; }
          else if ( c is InitialAssignment ) { InitialAssignment x = (lof as ListOfInitialAssignments).remove(0); c = x; }
          else if ( c is Parameter ) { Parameter x = (lof as ListOfParameters).remove(0); c = x; }
          else if ( c is Reaction ) { Reaction x = (lof as ListOfReactions).remove(0); c = x; }
          else if ( c is Rule ) { Rule x = (lof as ListOfRules).remove(0); c = x; }
          else if ( c is Species ) { Species x = (lof as ListOfSpecies).remove(0); c = x; }
          else if ( c is SpeciesReference ) {SimpleSpeciesReference x = (lof as ListOfSpeciesReferences).remove(0); c = x; }
          else if ( c is SpeciesType ) { SpeciesType x = (lof as ListOfSpeciesTypes).remove(0); c = x; }
          else if ( c is UnitDefinition ) { UnitDefinition x = (lof as ListOfUnitDefinitions).remove(0); c = x; }
          else if ( c is Unit ) { Unit x = (lof as ListOfUnits).remove(0); c = x; }
          else
          {
            ERR("[testListOfRemove] Error: (" + ename + ") : ListOfXXX::remove() failed.");
            return;
          }

          if ( c == null)
          {
            ERR("[testListOfRemove] Error: (" + ename + ") : ListOfXXX::remove() failed.");
            return;
          }

          string enameGet = c.getElementName();

          if ( ename == enameGet )
          {
            //Console.Out.WriteLine("[testListOfRemove] OK: (" + ename + ") remove(" + enameGet + ") : type match.");
            OK();
          }
          else
          {
            ERR("[testListOfRemove] Error: (" + ename + ") remove(" + enameGet + ") : type mismatch.");
          }
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
            SBMLDocument d = new SBMLDocument(defLevel,defVersion);

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

            testReadSBMLFromFile(file);
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
