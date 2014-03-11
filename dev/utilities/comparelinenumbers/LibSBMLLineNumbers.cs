using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using libsbmlcs;

namespace LibSBMLLineNumbers
{
    class Program
    {
        public static TextWriter Writer { get; set; }

        public static string XMLParser
        {
            get
            {
                try
                {
				    // we really ought to have a better way to detect the parser!
                    var stream = new XMLInputStream("<sbml/>", false, "expat", null);
                        if (!stream.isError())
                            return "expat";
                        stream = new XMLInputStream("<sbml/>", false, "libxml", null);
                        if (!stream.isError())
                            return "libxml";
                        stream = new XMLInputStream("<sbml/>", false, "xerces", null);
                        if (!stream.isError())
                            return "xerces-c";
                    
                }
                catch 
                {
                    
                }
                return "unknown";
            }
        }
        private static string GetLine(string sbml, long line)
        {
            var lines = sbml.Split('\n');
            if (line-1 < lines.Length)
                return lines[line - 1];
            return "invalid line";
        }
        private static void PrintLineNumbers(int level, int version, SBMLDocument doc, string message)
        {
            //doc.checkInternalConsistency();
            var sbml = libsbml.writeSBMLToString(doc);
            var doc2 = libsbml.readSBMLFromString(sbml);
            doc2.checkConsistency();
            for (int i = 0; i < doc2.getNumErrors(); i++)
            {
                var current = doc2.getError(i);
                if (current.getSeverity() == libsbml.LIBSBML_SEV_ERROR || 
                    current.getSeverity() == libsbml.LIBSBML_SEV_FATAL)
                {
                    if (current.getErrorId() == 10103)
                        continue;

                    Writer.WriteLine("{0}, {1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}", 
                        XMLParser, 
                        level, 
                        version, 
                        message, 
                        current.getLine(), 
                        current.getColumn(), 
                        current.getErrorId(),
                        current.getShortMessage(),
                        GetLine(sbml, current.getLine()));
                }
            }

            //libsbml.writeSBMLToFile(doc, string.Format("{0}-l{1}v{2}.xml", message, level, version));

        }
        private static void TestCompartment(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var comp = model.createCompartment();

            PrintLineNumbers(level, 
                version, 
                doc, "Compartment");
        }
        private static void TestCompartmentType(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var comp = model.createCompartmentType();

            PrintLineNumbers(level,
                version,
                doc, "CompartmentType");
        }
        private static void TestSpecies(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createSpecies();

            PrintLineNumbers(level,
                version,
                doc, "Species");
        }
        private static void TestSpeciesType(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createSpeciesType();

            PrintLineNumbers(level,
                version,
                doc, "SpeciesType");
        }
        private static void TestFunctionDefinition(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createFunctionDefinition();

            PrintLineNumbers(level,
                version,
                doc, "FunctionDefiniton"); 
        }
        private static void TestParameter(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createParameter();

            PrintLineNumbers(level,
                version,
                doc, "Parameter"); 
        }
        private static void TestAlgebraicRule(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createAlgebraicRule();

            PrintLineNumbers(level,
                version,
                doc, "AlgebraicRule"); 
        }
        private static void TestAssignmentRule(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createAssignmentRule();

            PrintLineNumbers(level,
                version,
                doc, "AssignmentRule"); 
        }
        private static void TestRateRule(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createRateRule();

            PrintLineNumbers(level,
                version,
                doc, "RateRule"); 
        }
        private static void TestReaction(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createReaction();

            PrintLineNumbers(level,
                version,
                doc, "Reaction"); 
        }
        private static void TestKineticLaw(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var reaction= model.createReaction();

            reaction.initDefaults();
            reaction.createKineticLaw();

            PrintLineNumbers(level,
                version,
                doc, "KineticLaw"); 
        }
        private static void TestLocalParameter(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var reaction = model.createReaction();

            reaction.initDefaults();
            var law = reaction.createKineticLaw();
            law.createParameter();

            PrintLineNumbers(level,
                version,
                doc, "LocalParameter"); 
        }
        private static void TestEvent(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createEvent();

            PrintLineNumbers(level,
                version,
                doc, "Event"); 
        }
        private static void TestEventAssignment(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createEvent();
            if (element == null) return;
            element.createEventAssignment();

            PrintLineNumbers(level,
                version,
                doc, "EventAssignment"); 
        }
        private static void TestTrigger(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createEvent();
            if (element == null) return;
            element.createTrigger();

            PrintLineNumbers(level,
                version,
                doc, "Trigger"); 
        }
        private static void TestDelay(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createEvent();
            if (element == null) return;
            element.createDelay();

            PrintLineNumbers(level,
                version,
                doc, "Delay"); 
        }
        private static void TestPriority(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createEvent();
            if (element == null) return;
            element.createPriority();

            PrintLineNumbers(level,
                version,
                doc, "Priority"); 
        }
        private static void TestConstraint(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createConstraint();

            PrintLineNumbers(level,
                version,
                doc, "Constraint"); 
        }
        private static void TestInitialAssignment(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createInitialAssignment();

            PrintLineNumbers(level,
                version,
                doc, "InitialAssignment"); 
        }
        private static void TestSpeciesReference(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var reaction = model.createReaction();

            reaction.initDefaults();
            var sr = reaction.createReactant();
            

            PrintLineNumbers(level,
                version,
                doc, "SpeciesReference");
        }
        private static void TestStoichiometryMath(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var reaction = model.createReaction();

            reaction.initDefaults();
            var sr = reaction.createReactant();
            sr.createStoichiometryMath();

            PrintLineNumbers(level,
                version,
                doc, "StoichiometryMath");
        }
        private static void TestUnit(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createUnitDefinition();
            var element1 = model.createUnit();

            PrintLineNumbers(level,
                version,
                doc, "Unit");
        }
        private static void TestUnitDefinition(int level, int version)
        {
            var doc = new SBMLDocument(level, version);
            var model = doc.createModel();
            var element = model.createUnitDefinition();

            PrintLineNumbers(level,
                version,
                doc, "UnitDefinition");
        }
        private static void TestSBMLLevelVersion(int level, int version)
        {
            TestCompartment(level, version);
            TestCompartmentType(level, version);
            TestSpecies(level, version);
            TestSpeciesType(level, version);
            TestFunctionDefinition(level, version);
            TestParameter(level, version);
            TestAlgebraicRule(level, version);
            TestAssignmentRule(level, version);
            TestRateRule(level, version);
            TestReaction(level, version);
            TestKineticLaw(level, version);
            TestLocalParameter(level, version);
            TestEvent(level, version);
            TestEventAssignment(level, version);
            TestTrigger(level, version);
            TestDelay(level, version);
            TestPriority(level, version);
            TestConstraint(level, version);
            TestInitialAssignment(level, version);
            TestSpeciesReference(level, version);
            TestStoichiometryMath(level, version);
            TestUnit(level, version);
            TestUnitDefinition(level, version);
        }

        static void Main(string[] args)
        {
            Writer = Console.Out;

            var versions = new List<int[]> { new [] {1, 1}
                , new [] {1, 1}
                , new [] {1, 2}
                , new [] {2, 1}
                , new [] {2, 2}
                , new [] {2, 3}
                , new [] {2, 4}
                , new [] {3, 1}
            };

            Writer.WriteLine("Testing libSBML {0} xml parser: {1}", libsbml.getLibSBMLDottedVersion(), XMLParser);
            Writer.WriteLine();
            Writer.WriteLine("Parser, Level, Version, Element, Line, Column, ErrorId, ShortMessage, XML Snippet");

            foreach (var item in versions)
            {
                TestSBMLLevelVersion(item[0], item[1]);
            }

            //Console.ReadKey();
        
        }
    }
}
