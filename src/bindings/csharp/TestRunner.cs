/**
 * @file   TestRunner.cs
 * @brief  Test Runner for C# test files.
 * @author Frank Bergmann (fbergman@u.washington.edu)
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

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Reflection;

namespace LibSBMLCSTestRunner
{
    /// <summary>
    /// <para>This test Programm takes a directory of C# files, compiles them and 
    /// then runs all test methods found. </para>
    /// 
    /// <para>- currently no support for test data</para>
    /// 
    /// <para>To use it simply invoke it with three arguments, for example: 
    /// 
    /// <c>LibSBMLCSTestRunner \\libsbml\\src\\sbml\\test \\libsbml\\src\\sbml\\test-data libsbmlCS.dll</c>
    /// </para>
    /// 
    /// author: Frank Bergmann (fbergman@u.washington.edu)
    /// 
    /// </summary>
    class TestRunner
    {
        private static void PrintUsageAndExit()
        {
            Console.WriteLine("Need three arguments:");
            Console.WriteLine("\t- Directory containing generated test files");
            Console.WriteLine("\t- Directory containing test-data");
            Console.WriteLine("\t- Full path to libsbml C# bindings to be used");
            Environment.Exit(-1);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("LibSBML C# Testrunner");
            Console.WriteLine("=====================");

            if (args.Length != 3)
            {
                PrintUsageAndExit();
            }

            string sSource = args[0];
            string sData = args[1];
            string sLibrary = args[2];

            if (!Directory.Exists(sSource))
            {
                Console.WriteLine("Source Directory does not exist" + Environment.NewLine);
                PrintUsageAndExit();
            }

            if (!Directory.Exists(sData))
            {
                Console.WriteLine("Data Directory does not exist" + Environment.NewLine);
                PrintUsageAndExit();
            }

            if (!File.Exists(sLibrary))
            {
                Console.WriteLine("libsbml C# binding assembly does not exist." + Environment.NewLine);
                PrintUsageAndExit();
            }

            // all seems well so let us run through the tests:
            Console.WriteLine("Running the tests with: ");
            Console.WriteLine("\tSource Directory: " + sSource);
            Console.WriteLine("\tData Directory:   " + sData);
            Console.WriteLine("\tC# binding:       " + sLibrary);
            Console.WriteLine();

            RunTests(sLibrary, sSource, sData);

        }

        private static void RunTests(string sLibrary, string sSource, string sData)
        {
            // add reference library to the compiler so that it will be referenced
            // by the test files
            Compiler.addAssembly(sLibrary);

            // then compile and run all C# files
            string[] testFiles = Directory.GetFiles(sSource, "*.cs");            

            nCompileErrors = 0;
            nSuccessSum    = 0;
            nFailureSum    = 0;
            nTestFunc      = 0;
            foreach (string testFile in testFiles)
            {
                RunTestFile(testFile, sData);
            }

            Console.WriteLine(String.Format("Encountered {0} compile errors (invalid tests)", nCompileErrors));
            Console.WriteLine(String.Format("Total Number of Test files {0}, Tests {1}, failures {2}", 
                                             testFiles.Length, nTestFunc, nFailureSum));
            if (nFailureSum == 0 && nCompileErrors == 0 )
            {
              Console.WriteLine("\nAll tests passed.");
              Environment.Exit(0); 
            }
            Environment.Exit(1); 
        }

        static int nCompileErrors;
        static int nSuccessSum;
        static int nFailureSum;
        static int nTestFunc;

        private static void RunTestFile(string testFile, string sData)
        {
            Console.WriteLine("Runing test file: '" + new FileInfo(testFile).Name + "'");
            Console.WriteLine("----------------------------------------------------------------");

            // read C# code
            StreamReader oReader = new StreamReader(testFile);
            string source = oReader.ReadToEnd(); oReader.Close();
            Assembly oTestClass = null;

            // compile the test file and create an assembly
            oTestClass = Compiler.GetAssembly(source);            

            if (oTestClass == null)
            {
                Console.WriteLine("Error compiling the test class (details on std::error) ");
                Console.Error.WriteLine(Compiler.getLastErrors());
                nCompileErrors++;
                Console.WriteLine();
                return;
            }

            // test compiled so now we can run the tests
            RunTestsInAssembly(oTestClass, sData);

            Console.WriteLine(); Console.WriteLine();
        }

        private static void RunTestsInAssembly(Assembly oTestClass, string sData)
        {
            // get all classes, we know that all test-classes begin with Test
            Type[] types = oTestClass.GetExportedTypes();
            foreach (Type type in types)
            {
                if (type.Name.StartsWith("Test"))
                {
                    // we have a test class: 
                    RunTestsInType(oTestClass, type, sData);
                }
            }            
        }

        private static void RunTestsInType(Assembly oTestClass, Type type, string sData)
        {
            // counting successes and failures
            int nSuccess = 0;
            int nFailure = 0;

            try
            {
                // get all methods
                MemberInfo[] members = type.GetMembers();

                foreach (MemberInfo member in members)
                {
                    
                    // test methods begin with test_
                    if (member.Name.StartsWith("test_"))
                    {
                        ++nTestFunc;
                        // set up the class
                        object testClass = SetupTestClass(oTestClass, type);

                        // run the test
                        try
                        {
                            type.InvokeMember(member.Name, BindingFlags.InvokeMethod |
                                        BindingFlags.Default, null, testClass, null);
                        }
                        catch (TargetInvocationException ex)
                        {
                            Console.WriteLine("Calling '" + member.Name + "' failed: " + ex.InnerException.Message
                                + Environment.NewLine + ex.InnerException.StackTrace);
                            nFailure++;
                            continue;
                        }
                        catch (Exception ex)
                        {
                            Console.WriteLine("Calling '" + member.Name + "' failed: " + ex.Message);
                            nFailure++;
                            continue;
                        }

                        // if we are still here the test was successful
                        Console.WriteLine("Calling '" + member.Name + "' succeeded");
                        nSuccess++;

                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error running tests: " + ex.Message);
                return;
            }

            Console.WriteLine();
            Console.WriteLine(String.Format("Testing completed: Pass:{0}, Fail:{1}, (Total:{2})", nSuccess, nFailure, nSuccess+nFailure));
            nSuccessSum += nSuccess;
            nFailureSum += nFailure;

        }

        private static object SetupTestClass(Assembly oTestClass, Type type)
        {

            object oClass = Activator.CreateInstance(type);
            try
            {
                type.InvokeMember("setUp", BindingFlags.InvokeMethod | BindingFlags.Default, null, oClass, null);
            }
            catch (Exception)
            {
                Console.WriteLine("Could not setUp class ... ");
            }
            return oClass;
        }
    }
}
