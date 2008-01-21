/*
 * @file    AutoTestRunner.java
 * @brief   Test Runner for Java test scripts
 * @author  Akiya Jouraku
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

import java.io.File;
import java.io.FilenameFilter;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.lang.reflect.*;

import org.sbml.libsbml.test.*;
import org.sbml.libsbml.*;

public class AutoTestRunner
{
  static String pkgName     = "org.sbml.libsbml.test.";
  static String testDir     = "test/org/sbml/libsbml/test/";
  static String fileRegex   = "Test.*\\.java";
  static String methodRegex = "^test.*";

  public static File[] getTestFileNames ()
  {
    File fd = new File(testDir);
    return fd.listFiles( new TestFilenameFilter(fileRegex) );
  }

  public static void test()
  {
    File[] testFiles = getTestFileNames();
    int testnum = 0;
    int failnum = 0;
    int filenum = testFiles.length;

    while ( --filenum >= 0 )
    {
      String clsName = pkgName + testFiles[filenum].getName().replaceFirst(".java$","");
      Class  cls = null;
      Object obj = null;
      Method[] listMethods = null;
      Method setup    = null;
      Method teardown = null;

      try {
       cls = Class.forName(clsName);
       obj = cls.newInstance(); 
       listMethods = cls.getDeclaredMethods();
      }
      catch (ClassNotFoundException e)
      {
        e.printStackTrace();
        continue;
      }
      catch ( InstantiationException e)
      {
        e.printStackTrace();
        continue;
      }
      catch ( IllegalAccessException e)
      {
        e.printStackTrace();
        continue;
      }

      try {
        setup = cls.getDeclaredMethod("setUp",(Class[])null);
        setup.setAccessible(true);
      }
      catch ( NoSuchMethodException e) {}

      try {
        teardown = cls.getDeclaredMethod("tearDown",(Class[])null);
        teardown.setAccessible(true);
      }
      catch ( NoSuchMethodException e) {}

      for (int i=0; i < listMethods.length; i++)
      {
        Pattern re_method  = Pattern.compile(methodRegex);
        String method_name = listMethods[i].getName();
        Matcher m = re_method.matcher(method_name);

        if ( m.matches()) {

          ++testnum;

          /**
           *
           * setup()
           *
           */
          try {
            if ( setup != null)
            {
              setup.invoke(obj,(Object[])null); 
            }
          }
          catch ( IllegalAccessException e)
          {
            ++failnum;
            System.err.println("F");
            e.printStackTrace();
            continue;
          } 
          catch ( IllegalArgumentException e)
          {
            ++failnum;
            System.err.println("F");
             e.printStackTrace();
            continue;
          } 
          catch ( InvocationTargetException e)
          {
            ++failnum;
            System.err.println("F");
            e.getCause().printStackTrace();  
            continue;
          } 
          catch ( NullPointerException e)
          {
            ++failnum;
            System.err.println("F");
            e.printStackTrace();  
            continue;
          } 

          /**
           *
           * test*()
           *
           */

          try {
            listMethods[i].invoke(obj,(Object[])null);
            System.err.print(".");
          }
          catch ( IllegalAccessException e)
          {
            ++failnum;
            System.err.println("F");
            e.printStackTrace();
          }
          catch ( IllegalArgumentException e)
          {
            ++failnum;
            System.err.println("F");
            e.printStackTrace();
          }
          catch ( InvocationTargetException e)
          {
            ++failnum;
            System.err.println("F");
            e.getCause().printStackTrace();
          }
          catch ( NullPointerException e)
          {
            ++failnum;
            System.err.println("F");
            e.printStackTrace();
          }
  
          /**
           *
           * tearDown()
           *
           */
           try{
             if ( teardown != null)
             {
               teardown.invoke(obj,(Object[])null); 
             }
           }
           catch ( IllegalAccessException e)
           {
             ++failnum;
             e.printStackTrace();
           }
           catch ( IllegalArgumentException e)
           {
             ++failnum;
             e.printStackTrace();
           }
           catch ( InvocationTargetException e)
           {
             ++failnum;
             e.getCause().printStackTrace();
           }
           catch ( NullPointerException e)
           {
             ++failnum;
             e.printStackTrace();
           }
        }
      }
    }

    System.err.println("\n" + testnum + " tests, " + failnum + " failures ");
    if ( failnum == 0 )
    {
      System.err.println("All tests passed");
      System.exit(0);
    }
    else
    {
      System.exit(1);
    }
  }

  public static void main (String argv[])
  {
    test();
  }

}

class TestFilenameFilter implements FilenameFilter
{
  String fileRegex = null;
  
  TestFilenameFilter(String re)
  {
    fileRegex = re;
  }

  public boolean accept(File dir, String name) 
  {
    Pattern re_tfile = Pattern.compile(fileRegex);
    Matcher m = re_tfile.matcher(name);
    if ( m.matches()) {
      return true;
    }
    return false;
  }
}

