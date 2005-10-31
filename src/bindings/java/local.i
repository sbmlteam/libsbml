/**
 * \file    local.i
 * \brief   Java-specific SWIG directives for wrapping libSBML API
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein and Ben Kovitz
 *     
 *     The SBML Team
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


%include "javadoc.i"


/**
 * Turns off object destruction.  For testing purposes only.
 */
/* %typemap (javafinalize) SWIGTYPE %{ %} */


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


/**
 * Both ASTNode::setValue(int) and ASTNode::setValue(long) are defined
 * in the C API.  But Swig maps C/C++ longs to Java ints, so this
 * resulting in a duplicate method definition.
 */
%ignore ASTNode::setValue(long);

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

    switch( libsbmlJNI.SBase_getTypeCode(cPtr) )
    {
      case libsbmlConstants.SBML_COMPARTMENT:
        return new Compartment(cPtr, owner);

      case libsbmlConstants.SBML_DOCUMENT:
        return new SBMLDocument(cPtr, owner);

      case libsbmlConstants.SBML_EVENT:
        return new Event(cPtr, owner);

      case libsbmlConstants.SBML_EVENT_ASSIGNMENT:
        return new EventAssignment(cPtr, owner);

      case libsbmlConstants.SBML_FUNCTION_DEFINITION:
        return new FunctionDefinition(cPtr, owner);

      case libsbmlConstants.SBML_KINETIC_LAW:
        return new KineticLaw(cPtr, owner);

      case libsbmlConstants.SBML_LIST_OF:
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

      case libsbmlConstants.SBML_UNIT_DEFINITION:
        return new UnitDefinition(cPtr, owner);

      case libsbmlConstants.SBML_UNIT:
        return new Unit(cPtr, owner);

      case libsbmlConstants.SBML_ALGEBRAIC_RULE:
        return new AlgebraicRule(cPtr, owner);

      case libsbmlConstants.SBML_ASSIGNMENT_RULE:
        return new AssignmentRule(cPtr, owner);

      case libsbmlConstants.SBML_RATE_RULE:
        return new RateRule(cPtr, owner);

      case libsbmlConstants.SBML_SPECIES_CONCENTRATION_RULE:
        return new SpeciesConcentrationRule(cPtr, owner);

      case libsbmlConstants.SBML_COMPARTMENT_VOLUME_RULE:
        return new CompartmentVolumeRule(cPtr, owner);

      case libsbmlConstants.SBML_PARAMETER_RULE:
        return new ParameterRule(cPtr, owner);

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

    switch( libsbmlJNI.SBase_getTypeCode(cPtr) )
    {
      case libsbmlConstants.SBML_COMPARTMENT:
        return new Compartment(cPtr, owner);

      case libsbmlConstants.SBML_DOCUMENT:
        return new SBMLDocument(cPtr, owner);

      case libsbmlConstants.SBML_EVENT:
        return new Event(cPtr, owner);

      case libsbmlConstants.SBML_EVENT_ASSIGNMENT:
        return new EventAssignment(cPtr, owner);

      case libsbmlConstants.SBML_FUNCTION_DEFINITION:
        return new FunctionDefinition(cPtr, owner);

      case libsbmlConstants.SBML_KINETIC_LAW:
        return new KineticLaw(cPtr, owner);

      case libsbmlConstants.SBML_LIST_OF:
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

      case libsbmlConstants.SBML_UNIT_DEFINITION:
        return new UnitDefinition(cPtr, owner);

      case libsbmlConstants.SBML_UNIT:
        return new Unit(cPtr, owner);

      case libsbmlConstants.SBML_ALGEBRAIC_RULE:
        return new AlgebraicRule(cPtr, owner);

      case libsbmlConstants.SBML_ASSIGNMENT_RULE:
        return new AssignmentRule(cPtr, owner);

      case libsbmlConstants.SBML_RATE_RULE:
        return new RateRule(cPtr, owner);

      case libsbmlConstants.SBML_SPECIES_CONCENTRATION_RULE:
        return new SpeciesConcentrationRule(cPtr, owner);

      case libsbmlConstants.SBML_COMPARTMENT_VOLUME_RULE:
        return new CompartmentVolumeRule(cPtr, owner);

      case libsbmlConstants.SBML_PARAMETER_RULE:
        return new ParameterRule(cPtr, owner);

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
 * Most libSBML methods takeover ownership of passed-in objects, so we need
 * to make sure SWIG disowns the object.
 */
%typemap(javain) SWIGTYPE *, SWIGTYPE &
                 "$javaclassname.getCPtrAndDisown($javainput)";

/**
 * Of course, there are some exceptions to the above rule.  These typemaps
 * cover the following functions:
 *
 *  - writeSBML()
 *  - writeSBMLToString()
 *  - writeMathML()
 *  - writeMathMLToString()
 *
 * Which take either an SBMLDocument or MathMLDocument as input.
 */
%typemap(javain) SBMLDocument   * "SBMLDocument.getCPtr($javainput)";
%typemap(javain) MathMLDocument * "MathMLDocument.getCPtr($javainput)";


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
    long   cPtr    = libsbmlJNI.SBMLReader_readSBMLInternal(swigCPtr, abspath);
    return (cPtr == 0) ? null : new SBMLDocument(cPtr, true);
  }
%}


%javamethodmodifiers       readSBML "private";
%rename(readSBMLInternal)  readSBML;

%pragma(java) modulecode =
%{
  public static SBMLDocument readSBML (String filename)
  {
    SBMLReader reader = new SBMLReader();
    return reader.readSBML(filename);
  }
%}
