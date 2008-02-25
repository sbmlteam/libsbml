/**
 * @file    ExcludeDoclet.java
 * @brief   Allow classes to be excluded from JavaDoc runs
 * @author  Jamie Ho, Sun Microsystems, Inc.
 *
 * $Id$
 * $Source$
 *
 *<!-- ------------------------------------------------------------------------
 *  @(#)ExcludeDoclet.java	1.1 04/08/31
 *
 * Copyright 2004 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 * 
 *  - Redistribution of source code must retain the above copyright notice, 
 *    this list of conditions and the following disclaimer.
 *
 *  - Redistribution in binary form must reproduce the above copyright notice, 
 *    this list of conditions and the following disclaimer in the documentation 
 *    and/or other materials provided with the distribution.
 *
 * Neither the name of Sun Microsystems, Inc. nor the names of contributors may be 
 * used to endorse or promote products derived from this software without specific 
 * prior written permission.
 *  
 * This software is provided "AS IS," without a warranty of any kind. ALL EXPRESS 
 * OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING ANY IMPLIED 
 * WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT, 
 * ARE HEREBY EXCLUDED. SUN MICROSYSTEMS, INC. ("SUN") AND ITS LICENSORS SHALL NOT 
 * BE LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING 
 * OR DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES. IN NO EVENT WILL SUN OR ITS 
 * LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DATA, OR FOR DIRECT, INDIRECT, 
 * SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER CAUSED AND 
 * REGARDLESS OF THE THEORY OF LIABILITY, ARISING OUT OF THE USE OF OR INABILITY 
 * TO USE THIS SOFTWARE, EVEN IF SUN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH 
 * DAMAGES.
 *  
 * You acknowledge that this software is not designed, licensed or intended for 
 * use in the design, construction, operation or maintenance of any nuclear facility. 
 *
 *------------------------------------------------------------------------- -->
 *
 * This source code was originally obtained on 24 Feb. 2008 from
 * http://java.sun.com/developer/JDCTechTips/2004/tt1214.html
 *
 * The following documentation was copied from that page.
 *
 * java -classpath <path to doclet and path to tools.jar>
 *     ExcludeDoclet  -excludefile <path to exclude file>  <javadoc options>
 *
 * In response to the command, the validOptions method of the Doclet class
 * looks for the -excludefile option. If it finds it, the method reads the
 * contents of the exclude file -- these are the set of classes and
 * packages to ignore. Then the start method is called. As each class or
 * package is processed, the method throws away the classes and packages in
 * the exclude set. The doclet includes the optionLength method, this
 * allows the doclet to run under both J2SE 1.4 and 5.0.
 *
 * Compile the doclet as follows:
 *
 *   javac -classpath tools.jar ExcludeDoclet.java
 *
 * Replace tools.jar with the appropriate location of your JDK
 * installation. For example, if you're running in the Windows environment
 * and your JDK is installed in the c:\jdk1.5.0 directory, specify
 * c:\jdk1.5.0\lib\tools.jar.
 *
 * Next, create a file such as skip.txt to identify which classes to
 * skip. For this example, run ExcludeDoclet with the standard JDK
 * classes, and ignore a set in the java.lang package:
 *
 *    java.lang.Math
 *    java.lang.Long
 *    java.lang.InternalError 
 *    java.lang.InterruptedException 
 *    java.lang.Iterable 
 *    java.lang.LinkageError
 *
 * Then run the following command (on one line):
 *
 * java -classpath .;c:\jdk1.5.0\lib\tools.jar ExcludeDoclet 
 *    -d docs -excludefile skip.txt -sourcepath c:\jdk1.5.0\src 
 *    -source 1.5 java.lang
 *
 * The command will generate the javadoc for the java.lang package,
 * excluding the six classes and interfaces identified in skip.txt.
 *
 * Additional notes (M. Hucka):
 * - "tools.jar" is called "classes.jar" on MacOS and it's located in
 *   /System/Library/Frameworks/JavaVM.framework/Classes/classes.jar
 *   See http://lists.apple.com/archives/java-dev/2002/Jun/msg00901.html

 * - 2008-02-25 I made a small tweak to the diagnostic msgs printed by start()
 */

import java.io.*;
import java.util.*;
import com.sun.tools.javadoc.Main;
import com.sun.javadoc.*;

/**
 * A wrapper for Javadoc.  Accepts an additional option called "-excludefile",
 * which specifies which classes and packages should be excluded from the output.
 * 
 * @author Jamie Ho
 */
public class ExcludeDoclet extends Doclet {
    private static List m_args = new ArrayList();
    private static Set m_excludeSet = new HashSet();
    
    /**
     * Iterate through the documented classes and remove the ones that should
     * be excluded.
     * 
     * @param root the initial RootDoc (before filtering).
     */
    public static boolean start(RootDoc root){
        root.printNotice("ExcludeDoclet: removing excluded source files...");
        ClassDoc[] classes = root.classes();
        for (int i = 0; i < classes.length; i++) {
            if (m_excludeSet.contains(classes[i].qualifiedName()) ||
                m_excludeSet.contains(classes[i].containingPackage().name())) {
                root.printNotice("Excluding " + classes[i].qualifiedName());
                continue;
                
            }
            m_args.add(classes[i].position().file().getPath());   
        }
        return true;
        
    }
    
    /**
     * Let every option be valid.  The real validation happens in the standard
     * doclet, not here.  Remove the "-excludefile" and "-subpackages" options
     * because they are not needed by the standard doclet.
     * 
     * @param options   the options from the command line
     * @param reporter  the error reporter
     */
    public static boolean validOptions(String[][] options,
                                       DocErrorReporter reporter) {
        for (int i = 0; i < options.length; i++) {
            if (options[i][0].equalsIgnoreCase("-excludefile")) {
                try {
                    readExcludeFile(options[i][1]);
                } catch (Exception e) {
                    e.printStackTrace();   
                }
                continue;
            }
            if (options[i][0].equals("-subpackages")) {
                continue;   
            }
            for (int j = 0; j < options[i].length; j++) {
                m_args.add(options[i][j]);   
            }
        }
        return true;
    }
    
    /**
     * Parse the file that specifies which classes and packages to exclude from
     * the output. You can write comments in this file by starting the line with
     * a '#' character.
     * 
     * @param filePath the path to the exclude file.
     */
    private static void readExcludeFile(String filePath)
        throws Exception {
        LineNumberReader reader = new LineNumberReader(new FileReader(filePath));
        String line;
        while ((line = reader.readLine()) != null) {
            if (line.trim().startsWith("#"))
                continue;
            m_excludeSet.add(line.trim());
        }
    }
    
    /**
     * Method required to validate the length of the given option.  This is a
     * bit ugly but the options must be hard coded here.  Otherwise, Javadoc
     * will throw errors when parsing options.  We could delegate to the 
     * Standard doclet when computing option lengths, but then this doclet would
     * be dependent on the version of J2SE used.  Prefer to hard code options
     * here so that this doclet can be used with 1.4.x or 1.5.x .
     * 
     * @param option  the option to compute the length for
     */
    public static int optionLength(String option) {
        
        if (option.equalsIgnoreCase("-excludefile")) {
            return 2;   
        }
        
        /*1.4 Options Begin Here*/
        
        
        /*1.5 Options Begin Here*/
        
        //General options
        if (option.equals("-author") ||
            option.equals("-docfilessubdirs") ||
            option.equals("-keywords") ||
            option.equals("-linksource") ||
            option.equals("-nocomment") ||
            option.equals("-nodeprecated") ||
            option.equals("-nosince") ||
            option.equals("-notimestamp") ||
            option.equals("-quiet") ||
            option.equals("-xnodate") ||
            option.equals("-version")) {
            return 1;
        } else if (option.equals("-d") ||
                   option.equals("-docencoding") ||
                   option.equals("-encoding") ||
                   option.equals("-excludedocfilessubdir") ||
                   option.equals("-link") ||
                   option.equals("-sourcetab") ||
                   option.equals("-noqualifier") ||
                   option.equals("-output") ||
                   option.equals("-sourcepath") ||
                   option.equals("-tag") ||
                   option.equals("-taglet") ||
                   option.equals("-tagletpath")) {
            return 2;
        } else if (option.equals("-group") ||
                   option.equals("-linkoffline")) {
            return 3;
        } 
        
        //Standard doclet options
        option = option.toLowerCase();
        if (option.equals("-nodeprecatedlist") ||
            option.equals("-noindex") ||
            option.equals("-notree") ||
            option.equals("-nohelp") ||
            option.equals("-splitindex") ||
            option.equals("-serialwarn") ||
            option.equals("-use") ||
            option.equals("-nonavbar") ||
            option.equals("-nooverview")) {
            return 1;
        } else if (option.equals("-footer") ||
                   option.equals("-header") ||
                   option.equals("-packagesheader") ||
                   option.equals("-doctitle") ||
                   option.equals("-windowtitle") ||
                   option.equals("-bottom") ||
                   option.equals("-helpfile") ||
                   option.equals("-stylesheetfile") ||
                   option.equals("-charset") ||
                   option.equals("-overview")) {
            return 2;
        } else {
            return 0;
        }
    }
     
    /**
     * Execute this doclet to filter out the unwanted classes and packages.  
     * Then execute the standard doclet.
     * 
     * @param args  the Javadoc arguments from the command line
     */
    public static void main(String[] args) {
        String name = ExcludeDoclet.class.getName();
        Main.execute(name, name, args);
        Main.execute((String[]) m_args.toArray(new String[] {}));
    }
}
