/**
 * @file    printNotes.java
 * @brief   Prints notes strings for each element
 * @author  Akira Funahashi (translated from libSBML C++ examples (using other Java examples provided by Nicolas Rodriguez))
 * @author  Akiya Jouraku
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


import org.sbml.libsbml.Compartment;
import org.sbml.libsbml.Constraint;
import org.sbml.libsbml.Event;
import org.sbml.libsbml.Delay;
import org.sbml.libsbml.InitialAssignment;
import org.sbml.libsbml.ModifierSpeciesReference;
import org.sbml.libsbml.Parameter;
import org.sbml.libsbml.SBase;
import org.sbml.libsbml.Species;
import org.sbml.libsbml.SpeciesReference;
import org.sbml.libsbml.SpeciesType;
import org.sbml.libsbml.Trigger;
import org.sbml.libsbml.EventAssignment;
import org.sbml.libsbml.FunctionDefinition;
import org.sbml.libsbml.KineticLaw;
import org.sbml.libsbml.Model;
import org.sbml.libsbml.Reaction;
import org.sbml.libsbml.Rule;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;
import org.sbml.libsbml.UnitDefinition;
 

public class printNotes
{
  public static void main (String[] args)
  {        
    if (args.length != 1)
    {
      println("Usage: java printNotes filename");
      System.exit(1);
    }

    String filename       = args[0];
    SBMLDocument document;
    SBMLReader reader     = new SBMLReader();

    document = reader.readSBML(filename);
    long errors = document.getNumErrors();
    println("\nfilename: " + filename + "\n");
    if (errors > 0)
    {
      document.printErrors();
      System.exit((int)errors);
    }

    /* Model */
    Model m = document.getModel();
    printNotes(m);

    for(int i=0; i < m.getNumReactions(); i++)
    {
      Reaction re = m.getReaction(i);
      printNotes(re);

      /* SpeciesReference (Reactant) */
      for(int j=0; j < re.getNumReactants(); j++)
      { 
        SpeciesReference rt = re.getReactant(j);
        if (rt.isSetNotes()) print("   ");
        printNotes(rt, (rt.isSetSpecies() ? rt.getSpecies() : ""));
      }

      /* SpeciesReference (Product) */
      for(int j=0; j < re.getNumProducts(); j++)
      { 
        SpeciesReference rt = re.getProduct(j);
        if (rt.isSetNotes()) print("   ");
        printNotes(rt, (rt.isSetSpecies() ? rt.getSpecies() : "") );
      }

      /* ModifierSpeciesReference (Modifiers) */
      for(int j=0; j < re.getNumModifiers(); j++)
      { 
        ModifierSpeciesReference md = re.getModifier(j);
        if (md.isSetNotes()) print("   ");
        printNotes(md, (md.isSetSpecies() ? md.getSpecies() : "") );
      }

      /* KineticLaw */
      if(re.isSetKineticLaw())
      { 
        KineticLaw kl = re.getKineticLaw();
        if (kl.isSetNotes()) print("   ");
        printNotes(kl);

        /* Parameter */
        for(int j=0; j < kl.getNumParameters(); j++)
        { 
          Parameter pa = kl.getParameter(j);
          if (pa.isSetNotes()) print("      ");
          printNotes(pa);
        }
      }
    }
    
    /* Species */
    for(int i=0; i < m.getNumSpecies(); i++)
    {
      Species sp = m.getSpecies(i);
      printNotes(sp);
    }   
    
    /* Compartments */
    for(int i=0; i < m.getNumCompartments(); i++)
    { 
      Compartment sp = m.getCompartment(i);
      printNotes(sp);
    }   
    
    /* FunctionDefinition */
    for(int i=0; i < m.getNumFunctionDefinitions(); i++)
    {
      FunctionDefinition sp = m.getFunctionDefinition(i);
      printNotes(sp);
    }

    /* UnitDefinition */
    for(int i=0; i < m.getNumUnitDefinitions(); i++)
    {
      UnitDefinition sp = m.getUnitDefinition(i);
      printNotes(sp);
    }

    /* Parameter */
    for(int i=0; i < m.getNumParameters(); i++)
    {
      Parameter sp = m.getParameter(i);
      printNotes(sp);
    }

    /* Rule */
    for(int i=0; i < m.getNumRules(); i++)
    {
      Rule sp = m.getRule(i);
      printNotes(sp);
    }
    
    /* InitialAssignment */
    for(int i=0; i < m.getNumInitialAssignments(); i++)
    {
      InitialAssignment sp = m.getInitialAssignment(i);
      printNotes(sp);
    }

    /* Event */
    for(int i=0; i < m.getNumEvents(); i++)
    { 
      Event sp = m.getEvent(i);
      printNotes(sp);

      /* Trigger */
      if(sp.isSetTrigger())
      {
        Trigger tg = sp.getTrigger();
        if (tg.isSetNotes()) print("   ");
        printNotes(tg);
      }

      /* Delay */
      if(sp.isSetDelay())
      {
        Delay dl = sp.getDelay();
        if (dl.isSetNotes()) print("   ");
        printNotes(dl);
      }

      /* EventAssignment */
      for(int j=0; j < sp.getNumEventAssignments(); j++)
      {
        EventAssignment ea = sp.getEventAssignment(j);
        if (ea.isSetNotes()) print("   ");
        printNotes(ea);
      }
    }
    
    /* SpeciesType */
    for(int i=0; i < m.getNumSpeciesTypes(); i++)
    {
      SpeciesType sp = m.getSpeciesType(i);
      printNotes(sp);
    }

    /* Constraints */
    for(int i=0; i < m.getNumConstraints(); i++)
    {
      Constraint sp = m.getConstraint(i);
      printNotes(sp);
    }

    System.exit((int)errors);
  }

  static void printNotes(SBase sb, String id)
  {
    if (!sb.isSetNotes()) return;

    String pid = id;

    if (pid == "" && sb.isSetId())
	{
	  pid = sb.getId();
	}
    println("----- " + sb.getElementName() + " (" + pid
	     + ") notes -----");
	println(sb.getNotesString());
	println("");
  }

  static void printNotes(SBase sb)
  {
    printNotes(sb, "");
  }

  static void print (String msg)
  {
    System.out.print(msg);
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
    catch (UnsatisfiedLinkError e)
    {
      System.err.println("Error: could not link with the libSBML library."+
			 "  It is likely\nyour " + varname +
			 " environment variable does not include\nthe"+
			 " directory containing the libsbml library file.");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      System.err.println("Error: unable to load the file libsbmlj.jar."+
			 "  It is likely\nyour " + varname + " environment"+
			 " variable or CLASSPATH variable\ndoes not include"+
			 " the directory containing the libsbmlj.jar file.");
      System.exit(1);
    }
    catch (SecurityException e)
    {
      System.err.println("Could not load the libSBML library files due to a"+
			 " security exception.");
    }
  }
}
