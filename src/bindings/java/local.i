/**
 * \file    local.i
 * \brief   Java-specific SWIG directives for wrapping libSBML API
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


%include "javadoc.i"
%include "std_string.i"

%{
#include <iostream>
#include <fstream>
#include <sstream>
%}


/**
 * Renames *::clone() to *::cloneObject().
 * In JDK 1.4.2, libsbml's *::clone() methods can't override 
 * "Object Java.lang.Object.clone()" because JDK 1.4.2 doesn't
 * allow override with different return type. 
 */

%rename(cloneObject) *::clone;

/**
 * Ignores XMLToken::clone() in order to use XMLNode::clone().
 * (XMLNode is a derived class of XMLToken)
 * In JDK 1.4.2, "XMLNode XMLNode::clone()" can't override 
 * "XMLToken XMLToken::clone()" because JDK 1.4.2 doesn't
 * allow override with different return type.
 */

%javamethodmodifiers       XMLToken::clone "private"

/**
 * Ignores XMLErrorLog::getError(unsigned int) in order to use
 * SBMLErrorLog::getError(unsigned int).
 * (XMLErrorLog is a derived class of SBMLErrorLog)
 * In JDK 1.4.2, "SBMLError* SBMLErrorLog::getError(unsigned int)"
 * can't override "XMLError* XMLErrorLog::getError(unsigned int)"
 * due to the above mentioned reason.
 */

%javamethodmodifiers       XMLErrorLog::getError "private"

/**
 * Turns off object destruction.  For testing purposes only.
 *
 * FIXME: Disable for the final 3.0 release.
 */
/*
%typemap (javafinalize) SWIGTYPE %{ %}
*/


/**
 * Make most libSBML constants (e.g. SBMLTypecodes) Java compile-time
 * constants so they may be used in switch statements.
 */
%include "enumsimple.swg"
%javaconst(1);


/**
 * A bug in Swig prevents these four ASTNode constants being generated
 * as Java compile-time constants.  Swig does not parse the following
 * enum correctly:
 *
 *   typedef enum
 *   {
 *       AST_PLUS    = '+'
 *     , AST_MINUS   = '-'
 *     , AST_TIMES   = '*'
 *     , AST_DIVIDE  = '/'
 *     , AST_POWER   = '^'
 *
 *
 * The generated Java code does not like the tick marks (').  To fix
 * this, we need to be explicit about (and duplicate) the value of
 * the constants here.
 */

%javaconstvalue("'+'") AST_PLUS;
%javaconstvalue("'-'") AST_MINUS;
%javaconstvalue("'*'") AST_TIMES;
%javaconstvalue("'/'") AST_DIVIDE;
%javaconstvalue("'^'") AST_POWER;

/*
 * SWIG can't wrap a enum value such as 'XMLError::Info'.
 * The following java code is added to wrap 'enum SBMLSeverity' 
 * and 'enum SBMLCategory' in SBMLError class.
 */

%ignore SBMLError::SBMLSeverity;
%ignore SBMLError::SBMLCategory;
%typemap("javacode") SBMLError
%{
  // SBMLSeverity
  public final static int Info    = XMLError.Info;  
  public final static int Warning = XMLError.Warning;  
  public final static int Error   = XMLError.Error;  
  public final static int Fatal   = XMLError.Fatal;  

  // SBMLCategory
  public final static int Internal                  = XMLError.Internal;  
  public final static int System                    = XMLError.System;  
  public final static int XML                       = XMLError.XML;  
  public final static int SBML                      = 3;
  public final static int SBMLL1Compatibility       = 4;
  public final static int SBMLL2v1Compatibility     = 5;
  public final static int SBMLL2v2Compatibility     = 6;
  public final static int SBMLConsistency           = 7;
  public final static int SBMLConsistencyIdentifier = 8;
  public final static int SBMLConsistencyUnits      = 9;
  public final static int SBMLConsistencyMathML     = 10;
  public final static int SBMLConsistencySBO        = 11;
  public final static int SBMLOverdetermined        = 12;
  public final static int SBMLL2v3Compatibility     = 13;
%}


#ifndef USE_LAYOUT

/**
 * @return the most specific Java object possible for the given SBase
 * object.
 */
%pragma(java) modulecode =
%{
  public static SBase DowncastSBase(long cPtr, boolean owner)
  {
    if (cPtr == 0) return null;

    SBase sb = new SBase(cPtr,false);
    switch( sb.getTypeCode() )
    {
      case libsbmlConstants.SBML_COMPARTMENT:
        return new Compartment(cPtr, owner);

      case libsbmlConstants.SBML_COMPARTMENT_TYPE:
        return new CompartmentType(cPtr, owner);

      case libsbmlConstants.SBML_CONSTRAINT:
        return new Constraint(cPtr, owner);

      case libsbmlConstants.SBML_DOCUMENT:
        return new SBMLDocument(cPtr, owner);

      case libsbmlConstants.SBML_DELAY:
        return new Delay(cPtr, owner);

      case libsbmlConstants.SBML_EVENT:
        return new Event(cPtr, owner);

      case libsbmlConstants.SBML_EVENT_ASSIGNMENT:
        return new EventAssignment(cPtr, owner);

      case libsbmlConstants.SBML_FUNCTION_DEFINITION:
        return new FunctionDefinition(cPtr, owner);

      case libsbmlConstants.SBML_INITIAL_ASSIGNMENT:
        return new InitialAssignment(cPtr, owner);

      case libsbmlConstants.SBML_KINETIC_LAW:
        return new KineticLaw(cPtr, owner);

      case libsbmlConstants.SBML_LIST_OF:
	String name = sb.getElementName();
        if(name.equals("listOf")){
          return new ListOf(cPtr, owner);
        }
	else if(name.equals("listOfCompartments")){
          return new ListOfCompartments(cPtr, owner);
	}
	else if(name.equals("listOfCompartmentTypes")){
          return new ListOfCompartmentTypes(cPtr, owner);
	}
	else if(name.equals("listOfConstraints")){
          return new ListOfConstraints(cPtr, owner);
	}
        else if(name.equals("listOfEvents")){
          return new ListOfEvents(cPtr, owner);
        }
        else if(name.equals("listOfEventAssignments")){
          return new ListOfEventAssignments(cPtr, owner);
        }
        else if(name.equals("listOfFunctionDefinitions")){
          return new ListOfFunctionDefinitions(cPtr, owner);
        }
        else if(name.equals("listOfInitialAssignments")){
          return new ListOfInitialAssignments(cPtr, owner);
        }
        else if(name.equals("listOfParameters")){
          return new ListOfParameters(cPtr, owner);
        }
	else if(name.equals("listOfReactions")){
          return new ListOfReactions(cPtr, owner);
	}
        else if(name.equals("listOfRules")){
          return new ListOfRules(cPtr, owner);
        }
        else if(name.equals("listOfSpecies")){
          return new ListOfSpecies(cPtr, owner);
        }
        else if(name.equals("listOfUnknowns")){
          return new ListOfSpeciesReferences(cPtr, owner);
        }
        else if(name.equals("listOfReactants")){
          return new ListOfSpeciesReferences(cPtr, owner);
        }
        else if(name.equals("listOfProducts")){
          return new ListOfSpeciesReferences(cPtr, owner);
        }
        else if(name.equals("listOfModifiers")){
          return new ListOfSpeciesReferences(cPtr, owner);
        }
        else if(name.equals("listOfSpeciesTypes")){
          return new ListOfSpeciesTypes(cPtr, owner);
        }
        else if(name.equals("listOfUnits")){
          return new ListOfUnits(cPtr, owner);
        }
        else if(name.equals("listOfUnitDefinitions")){
          return new ListOfUnitDefinitions(cPtr, owner);
        }
        return new ListOf(cPtr, owner);

      case libsbmlConstants.SBML_MODEL:
        return new Model(cPtr, owner);

      case libsbmlConstants.SBML_PARAMETER:
        return new Parameter(cPtr, owner);

      case libsbmlConstants.SBML_REACTION:
        return new Reaction(cPtr, owner);

      case libsbmlConstants.SBML_SPECIES:
        return new Species(cPtr, owner);

      case libsbmlConstants.SBML_SPECIES_REFERENCE:
        return new SpeciesReference(cPtr, owner);

      case libsbmlConstants.SBML_MODIFIER_SPECIES_REFERENCE:
        return new ModifierSpeciesReference(cPtr, owner);

      case libsbmlConstants.SBML_SPECIES_TYPE:
        return new SpeciesType(cPtr, owner);

      case libsbmlConstants.SBML_TRIGGER:
        return new Trigger(cPtr, owner);

      case libsbmlConstants.SBML_UNIT_DEFINITION:
        return new UnitDefinition(cPtr, owner);

      case libsbmlConstants.SBML_UNIT:
        return new Unit(cPtr, owner);

      case libsbmlConstants.SBML_FORMULA_UNITS_DATA:
        return new FormulaUnitsData(cPtr, owner);

      case libsbmlConstants.SBML_LIST_FORMULA_UNITS_DATA:
        return new ListFormulaUnitsData(cPtr, owner);

      case libsbmlConstants.SBML_ALGEBRAIC_RULE:
        return new AlgebraicRule(cPtr, owner);

      case libsbmlConstants.SBML_ASSIGNMENT_RULE:
        return new AssignmentRule(cPtr, owner);

      case libsbmlConstants.SBML_RATE_RULE:
        return new RateRule(cPtr, owner);

      case libsbmlConstants.SBML_STOICHIOMETRY_MATH:
        return new StoichiometryMath(cPtr, owner);

      default:
        return new SBase(cPtr, owner);
    }
  }
%}

#else

/**
 * @return the most specific Java object possible for the given SBase
 * object.
 */
%pragma(java) modulecode =
%{
  public static SBase DowncastSBase(long cPtr, boolean owner)
  {
    if (cPtr == 0) return null;

    SBase sb = new SBase(cPtr,false);
    switch( sb.getTypeCode() )
    {
      case libsbmlConstants.SBML_COMPARTMENT:
        return new Compartment(cPtr, owner);

      case libsbmlConstants.SBML_COMPARTMENT_TYPE:
        return new CompartmentType(cPtr, owner);

      case libsbmlConstants.SBML_CONSTRAINT:
        return new Constraint(cPtr, owner);

      case libsbmlConstants.SBML_DOCUMENT:
        return new SBMLDocument(cPtr, owner);

      case libsbmlConstants.SBML_DELAY:
        return new Delay(cPtr, owner);

      case libsbmlConstants.SBML_EVENT:
        return new Event(cPtr, owner);

      case libsbmlConstants.SBML_EVENT_ASSIGNMENT:
        return new EventAssignment(cPtr, owner);

      case libsbmlConstants.SBML_FUNCTION_DEFINITION:
        return new FunctionDefinition(cPtr, owner);

      case libsbmlConstants.SBML_INITIAL_ASSIGNMENT:
        return new InitialAssignment(cPtr, owner);

      case libsbmlConstants.SBML_KINETIC_LAW:
        return new KineticLaw(cPtr, owner);

      case libsbmlConstants.SBML_LIST_OF:
        String name = sb.getElementName();
        if(name.equals("listOf")){
          return new ListOf(cPtr, owner);
        }
        else if(name.equals("listOfCompartments")){
          return new ListOfCompartments(cPtr, owner);
        }
        else if(name.equals("listOfCompartmentTypes")){
          return new ListOfCompartmentTypes(cPtr, owner);
        }
        else if(name.equals("listOfConstraints")){
          return new ListOfConstraints(cPtr, owner);
        }
        else if(name.equals("listOfEvents")){
          return new ListOfEvents(cPtr, owner);
        }
        else if(name.equals("listOfEventAssignments")){
          return new ListOfEventAssignments(cPtr, owner);
        }
        else if(name.equals("listOfFunctionDefinitions")){
          return new ListOfFunctionDefinitions(cPtr, owner);
        }
        else if(name.equals("listOfInitialAssignments")){
          return new ListOfInitialAssignments(cPtr, owner);
        }
        else if(name.equals("listOfParameters")){
          return new ListOfParameters(cPtr, owner);
        }
        else if(name.equals("listOfReactions")){
          return new ListOfReactions(cPtr, owner);
        }
        else if(name.equals("listOfRules")){
          return new ListOfRules(cPtr, owner);
        }
        else if(name.equals("listOfSpecies")){
          return new ListOfSpecies(cPtr, owner);
        }
        else if(name.equals("listOfUnknowns")){
          return new ListOfSpeciesReferences(cPtr, owner);
        }
        else if(name.equals("listOfReactants")){
          return new ListOfSpeciesReferences(cPtr, owner);
        }
        else if(name.equals("listOfProducts")){
          return new ListOfSpeciesReferences(cPtr, owner);
        }
        else if(name.equals("listOfModifiers")){
          return new ListOfSpeciesReferences(cPtr, owner);
        }
        else if(name.equals("listOfSpeciesTypes")){
          return new ListOfSpeciesTypes(cPtr, owner);
        }
        else if(name.equals("listOfUnits")){
          return new ListOfUnits(cPtr, owner);
        }
        else if(name.equals("listOfUnitDefinitions")){
          return new ListOfUnitDefinitions(cPtr, owner);
        }
        else if(name.equals("listOfCompartmentGlyphs")){
          return new ListOfCompartmentGlyphs(cPtr, owner);
        }
        else if(name.equals("listOfAdditionalGraphicalObjects")){
          return new ListOfGraphicalObjects(cPtr, owner);
        }
        else if(name.equals("listOfLayouts")){
          return new ListOfLayouts(cPtr, owner);
        }
        else if(name.equals("listOfCurveSegments")){
          return new ListOfLineSegments(cPtr, owner);
        }
        else if(name.equals("listOfSpeciesGlyphs")){
          return new ListOfSpeciesGlyphs(cPtr, owner);
        }
        else if(name.equals("listOfSpeciesReferenceGlyphs")){
          return new ListOfSpeciesReferenceGlyphs(cPtr, owner);
        }
        else if(name.equals("listOfReactionGlyphs")){
          return new ListOfReactionGlyphs(cPtr, owner);
        }
        else if(name.equals("listOfTextGlyphs")){
          return new ListOfTextGlyphs(cPtr, owner);
        }
        return new ListOf(cPtr, owner);

      case libsbmlConstants.SBML_MODEL:
        return new Model(cPtr, owner);

      case libsbmlConstants.SBML_PARAMETER:
        return new Parameter(cPtr, owner);

      case libsbmlConstants.SBML_REACTION:
        return new Reaction(cPtr, owner);

      case libsbmlConstants.SBML_SPECIES:
        return new Species(cPtr, owner);

      case libsbmlConstants.SBML_SPECIES_REFERENCE:
        return new SpeciesReference(cPtr, owner);

      case libsbmlConstants.SBML_MODIFIER_SPECIES_REFERENCE:
        return new ModifierSpeciesReference(cPtr, owner);

      case libsbmlConstants.SBML_SPECIES_TYPE:
        return new SpeciesType(cPtr, owner);

      case libsbmlConstants.SBML_TRIGGER:
        return new Trigger(cPtr, owner);

      case libsbmlConstants.SBML_UNIT_DEFINITION:
        return new UnitDefinition(cPtr, owner);

      case libsbmlConstants.SBML_UNIT:
        return new Unit(cPtr, owner);

      case libsbmlConstants.SBML_FORMULA_UNITS_DATA:
        return new FormulaUnitsData(cPtr, owner);

      case libsbmlConstants.SBML_LIST_FORMULA_UNITS_DATA:
        return new ListFormulaUnitsData(cPtr, owner);

      case libsbmlConstants.SBML_ALGEBRAIC_RULE:
        return new AlgebraicRule(cPtr, owner);

      case libsbmlConstants.SBML_ASSIGNMENT_RULE:
        return new AssignmentRule(cPtr, owner);

      case libsbmlConstants.SBML_RATE_RULE:
        return new RateRule(cPtr, owner);

      case libsbmlConstants.SBML_STOICHIOMETRY_MATH:
        return new StoichiometryMath(cPtr, owner);

      case libsbmlConstants.SBML_LAYOUT_BOUNDINGBOX:
        return new BoundingBox(cPtr, owner);
        
      case libsbmlConstants.SBML_LAYOUT_COMPARTMENTGLYPH:
        return new CompartmentGlyph(cPtr, owner);
        
      case libsbmlConstants.SBML_LAYOUT_CUBICBEZIER:
        return new CubicBezier(cPtr, owner);
        
      case libsbmlConstants.SBML_LAYOUT_CURVE:
        return new Curve(cPtr, owner);
        
      case libsbmlConstants.SBML_LAYOUT_DIMENSIONS:
        return new Dimensions(cPtr, owner);
        
      case libsbmlConstants.SBML_LAYOUT_GRAPHICALOBJECT:
        return new GraphicalObject(cPtr, owner);
        
      case libsbmlConstants.SBML_LAYOUT_LAYOUT:
        return new Layout(cPtr, owner);
        
      case libsbmlConstants.SBML_LAYOUT_LINESEGMENT:
        return new LineSegment(cPtr, owner);
        
      case libsbmlConstants.SBML_LAYOUT_POINT:
        return new Point(cPtr, owner);
        
      case libsbmlConstants.SBML_LAYOUT_REACTIONGLYPH:
        return new ReactionGlyph(cPtr, owner);
        
      case libsbmlConstants.SBML_LAYOUT_SPECIESGLYPH:
        return new SpeciesGlyph(cPtr, owner);
        
      case libsbmlConstants.SBML_LAYOUT_SPECIESREFERENCEGLYPH:
        return new SpeciesReferenceGlyph(cPtr, owner);
        
      case libsbmlConstants.SBML_LAYOUT_TEXTGLYPH:
        return new TextGlyph(cPtr, owner);
       default:
        return new SBase(cPtr, owner);
    }
  }
%}

#endif /* ! USE_LAYOUT */

/**
 * Convert SBase objects into the most specific object possible.
 */
%typemap("javaout") SBase*
{
  return libsbml.DowncastSBase($jnicall, $owner);
}

/**
 * Convert Rule objects into the most specific object possible.
 */
%typemap("javaout") Rule*
{
  return (Rule) libsbml.DowncastSBase($jnicall, $owner);
}

#ifdef USE_LAYOUT
/**
 * Convert LineSegment objects into the most specific object possible.
 */
%typemap("javaout") LineSegment*
{
  return (LineSegment) libsbml.DowncastSBase($jnicall, $owner);
}

/**
 * Convert LineSegment objects into the most specific object possible.
 */
%typemap("javaout") GraphicalObject*
{
  return (GraphicalObject) libsbml.DowncastSBase($jnicall, $owner);
}

#endif /* USE_LAYOUT */


/**
 * getCPtrAndDisown() is like getCPtr() but it also sets the SWIG memory
 * ownsership flag to false.
 *
 * We used to use %typemap(javagetcptr), but this has been deprecated
 * in SWIG 1.3.24.  Instead we add getCPtrAndDisown() from the incantation
 * below (taken from the SWIG 1.3.24 CHANGES file).
 */

/* Utility macro for manipulating the Java body code method attributes */
%define SWIGJAVA_ATTRIBS(TYPENAME, CTOR_ATTRIB, GETCPTR_ATTRIB)

%typemap(javabody) TYPENAME
%{
   private long swigCPtr;
   protected boolean swigCMemOwn;

   CTOR_ATTRIB $javaclassname(long cPtr, boolean cMemoryOwn)
   {
     swigCMemOwn = cMemoryOwn;
     swigCPtr    = cPtr;
   }

   GETCPTR_ATTRIB static long getCPtr($javaclassname obj)
   {
     return (obj == null) ? 0 : obj.swigCPtr;
   }

   GETCPTR_ATTRIB static long getCPtrAndDisown ($javaclassname obj)
   {
     long ptr = 0;

     if (obj != null)
     {
       ptr             = obj.swigCPtr;
       obj.swigCMemOwn = false;
     }

     return ptr;
   }
%}


%typemap(javabody_derived) TYPENAME
%{
   private long swigCPtr;

   CTOR_ATTRIB $javaclassname(long cPtr, boolean cMemoryOwn)
   {
     super($moduleJNI.SWIG$javaclassnameUpcast(cPtr), cMemoryOwn);
     swigCPtr = cPtr;
   }

   GETCPTR_ATTRIB static long getCPtr($javaclassname obj)
   {
     return (obj == null) ? 0 : obj.swigCPtr;
   }

   GETCPTR_ATTRIB static long getCPtrAndDisown ($javaclassname obj)
   {
     long ptr = 0;

     if (obj != null)
     {
       ptr             = obj.swigCPtr;
       obj.swigCMemOwn = false;
     }

     return ptr;
   }
%}

%enddef

/* The default is protected getCPtr, protected constructor */
SWIGJAVA_ATTRIBS(SWIGTYPE, protected, protected)

/* Public getCPtr method, protected constructor */
%define PUBLIC_GETCPTR(TYPENAME)
SWIGJAVA_ATTRIBS(TYPENAME, protected, public)
%enddef

/* Public getCPtr method, public constructor */
%define PUBLIC_BODYMETHODS(TYPENAME)
SWIGJAVA_ATTRIBS(TYPENAME, public, public)
%enddef


/**
 * Part of libSBML methods takeover ownership of passed-in objects, so we need
 * to make sure SWIG disowns the object.
 * (Most libSBML methods don't takeover ownership since 3.0.0)
 *
 */

/**
 * takeover ownership
 *
 * - void ListOf::appendAndOwn(SBase* item)
 */
%typemap(javain) SBase*       item "SBase.getCPtrAndDisown($javainput)";
%typemap(javain) const SBase* item "SBase.getCPtr($javainput)";

/**
 * takeover ownership
 *
 * - void ASTNode::addChild (ASTNode* child)
 * - void ASTNode::prependChild (ASTNode* child)
 */
%typemap(javain) ASTNode*       child "ASTNode.getCPtrAndDisown($javainput)";
%typemap(javain) const ASTNode* child "ASTNode.getCPtr($javainput)";

/**
 * takeover ownership
 *
 * - void FormulaUnitsData::setUnitDefinition(UnitDefinition * ud)
 * - void FormulaUnitsData::setPerTimeUnitDefinition(UnitDefinition * ud)
 * - void FormulaUnitsData::setEventTimeUnitDefinition(UnitDefinition * ud)
 */
%typemap(javain) UnitDefinition*       ud "UnitDefinition.getCPtrAndDisown($javainput)";
%typemap(javain) const UnitDefinition* ud "UnitDefinition.getCPtr($javainput)";


/**
 * Some combinations of platforms and underlying XML parsers *require*
 * an absolute path to a filename while others do not.  It's best to
 * hide this from the end-user by making SBMLReader.readSBML() and
 * readSBML() always compute an absolute path and filename.
 */

%typemap("javaimports") SBMLReader "
import java.io.File;
"

%javamethodmodifiers       SBMLReader::readSBML "private";
%rename(readSBMLInternal)  SBMLReader::readSBML;

%typemap("javacode") SBMLReader
%{
  public SBMLDocument readSBML (String filename)
  {
    File   file    = new java.io.File(filename);
    String abspath = file.getAbsolutePath();
    SBMLReader reader = new SBMLReader();
    return reader.readSBMLInternal(abspath);
  }
%}

%javamethodmodifiers       readSBML "private"
%rename(readSBMLInternal)  readSBML;

%pragma(java) modulecode =
%{
  public static SBMLDocument readSBML (String filename)
  {
    SBMLReader reader = new SBMLReader();
    return reader.readSBML(filename);
  }
%}

/**
 * Wraps std::ostream by implementing three simple wrapper classes.
 *
 * 1) OStream wraps std::cout, std::cerr, and std::clog.
 *    The following public final static variables are provied in
 *    libsbml class like in C++.
 *
 *    1. public final static OStream cout;
 *    2. public final static OStream cerr;
 *    3. public final static OStream clog;
 *
 * 2) OFStream (derived class of OStream) wraps std::ofstream 
 *    with ios_base::cout (default) or ios_base::app flag. 
 *
 * 3) OStringStream (derived class of OStream) wraps std::ostringstream.
 *
 * These wrapper classes provide only the minimum functions.
 *
 * (sample code) -----------------------------------------------------
 *
 * 1. wraps std::cout
 *
 *    XMLOutputStream xos = new XMLOutputStream(libsbml.cout);
 *
 * 2. wraps std::cerr
 *
 *    SBMLDocument d = libsbml.readSBML("foo.xml");
 *    if ( d.getNumErrors() > 0) {
 *       d.printErrors(libsbml.cerr);
 *    }
 *
 * 3. wraps std::ofstream (write to file "foo.xml")
 *
 *    OFStream   ofs = new OFStream("foo.xml");
 *    SBMLDocument d = libsbml.readSBML("foo.xml");
 *    SBMLWriter   w = new SBMLWriter();
 *    w.writeSBML(d,ofs);
 *
 * 4. wraps std::ofstream (write to file "foo.xml" with append mode)
 *
 *    OFStream ofs = new OFStream("foo.xml",true);
 *    XMLOutputStream xos = new XMLOutputStream(ofs);
 *
 * 5. wraps std::ostringstream 
 *
 *    OStringStream   oss = new OStringStream();
 *    XMLOutputStream xos = new XMLOutputStream(oss);
 *    ...
 *    oss.endl();
 *    String s = oss.str();
 *
 */

%typemap(jstype) std::ostream& "OStream"
%typemap(javain) std::ostream& "SWIGTYPE_p_std__ostream.getCPtr($javainput.get_ostream())";

%inline 
%{

  class LIBLAX_EXTERN OStream 
  {
  protected:
    std::ostream* Stream;

  public:
    enum StdOSType {COUT,CERR,CLOG};

    /**
     * Creates a new OStream object with one of standard output stream objects.
     */
    OStream (StdOSType sot = COUT) 
    {
      switch (sot) {
        case COUT:
          Stream = &std::cout;
          break;
        case CERR:
          Stream = &std::cerr;
          break;
        case CLOG:
          Stream = &std::clog;
          break;
        default:
          Stream = &std::cout;
      }
    }

    virtual ~OStream () 
    {
    }
  
    /**
     * Returns stream object.
     */
    virtual std::ostream* get_ostream ()  
    { 
      return Stream;
    }

    void endl ()
    {
      std::endl(*Stream);
    }
  
  };
  
  class LIBLAX_EXTERN OFStream : public OStream 
  {
  public:
    /**
     * Creates a new OFStream object and opens a given file with 
     * append flag (default is false) and associates its content 
     * with stream object (Stream).
     */
    OFStream (const std::string& filename, bool is_append = false) 
    {
      if (is_append) {
        Stream = new std::ofstream(filename.c_str(),ios_base::app);
      }
      else {
        Stream = new std::ofstream(filename.c_str(),ios_base::out);
      }
    }
  
    /**
     * Opens a given file with append flag (default is false) and associates
     * its content with existing stream object (Stream).
     */
    void open (const std::string& filename, bool is_append = false) 
    { 
      if (is_append) {
        static_cast<std::ofstream*>(Stream)->open(filename.c_str(),ios_base::app);
      }
      else {
        static_cast<std::ofstream*>(Stream)->open(filename.c_str(),ios_base::out);
      }
    }  
  
    /**
     * Closes the file currently associated with stream object.
     */
    void close ()
    {
      static_cast<std::ofstream*>(Stream)->close();
    }
  
    /**
     * Returns true if stream object is currently associated with a file
     */
    bool is_open () 
    { 
      return static_cast<std::ofstream*>(Stream)->is_open(); 
    }
  
    virtual ~OFStream ()
    {
      delete Stream;
    }
  
  };
  
  class LIBLAX_EXTERN OStringStream : public OStream 
  {
  public:
    /**
     * Creates a new OStringStream object
     */
    OStringStream () 
    {
      Stream = new std::ostringstream();
    }
  
    /**
     * Returns the copy of the string object currently assosiated 
     * with the stream buffer.
     */
    std::string str () 
    {
      return static_cast<std::ostringstream*>(Stream)->str();
    }
  
    /**
     * Sets string s to the string object currently assosiated with 
     * the stream buffer.
     */
    void str (const std::string& s)
    {
      static_cast<std::ostringstream*>(Stream)->str(s.c_str());
    }
  
    virtual ~OStringStream () 
    {
      delete Stream;
    }
  
  };
%}
  
%pragma(java) modulecode =
%{
  public final static OStream cout;
  public final static OStream cerr;
  public final static OStream clog;

  static {
    cout = new OStream(OStream.COUT); 
    cerr = new OStream(OStream.CERR); 
    clog = new OStream(OStream.CLOG); 
  }
%}

