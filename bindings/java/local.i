/**
 * Filename    : local.i
 * Description : Java-specific SWIG directives for wrapping libSBML API
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2004-04-02
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2004 California Institute of Technology and
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
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


/**
 * Turns off object destruction.  For testing purposes only.
 */
/* %typemap (javafinalize) SWIGTYPE %{ %} */


/**
 * Make most libSBML constants (e.g. SBMLTypecodes) Java compile-time
 * constants so they may be used in switch statements.
 */
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
 * The generated Java code does not the tick marks (').
 */
%javaconst(0) AST_PLUS;
%javaconst(0) AST_MINUS;
%javaconst(0) AST_TIMES;
%javaconst(0) AST_DIVIDE;
%javaconst(0) AST_POWER;


/**
 * Both ASTNode::setValue(int) and ASTNode::setValue(long) are defined
 * in the C API.  But Swig maps C/C++ longs to Java ints, so this
 * resulting in a duplicate method definition.
 */
%ignore ASTNode::setValue(long);


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


/**
 * getCPtrAndDisown() is like getCPtr() but it also sets the SWIG memory
 * ownsership flag to false.
 *
 * NOTE: getCPtr() is already defined by SWIG, but if we wish to add a
 * custom variant of this method, we have to duplicate the original,
 * otherwise it will be clobbered.
 */
%typemap(javagetcptr) SWIGTYPE, SWIGTYPE *, SWIGTYPE &
%{
  public static long getCPtr ($javaclassname obj)
  {
    return (obj == null) ? 0 : obj.swigCPtr;
  }


  public static long getCPtrAndDisown ($javaclassname obj)
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
 * SWIG does not generate a no argument Java constructor if all C++
 * constructors of a class have default arguments.  By attaching a default
 * constructor to classes at the SWIG level (with %extend) we are forcing
 * SWIG to generate a no argument Java constructor.
 */

%extend SBMLDocument
{
  SBMLDocument() { return new SBMLDocument(); }
}


%extend Model
{
  Model() { return new Model(); }
}

%typemap("javacode") Model
%{
  public Model (String id)
  {
    this(id, "");
  }
%}


%extend FunctionDefinition
{
  FunctionDefinition() { return new FunctionDefinition(); }
}

%typemap("javacode") FunctionDefinition
%{
  public FunctionDefinition (String id)
  {
    this(id, "");
  }
%}


%extend Unit
{
  Unit() { return new Unit(); }
}

%typemap("javacode") Unit
%{
  public Unit (int kind, int exponent, int scale, double multiplier)
  {
    this(kind, exponent, scale, multiplier, 0.0);
  }

  public Unit (int kind, int exponent, int scale)
  {
    this(kind, exponent, scale, 1.0, 0.0);
  }
  
  public Unit (int kind, int exponent)
  {
    this(kind, exponent, 0, 1.0, 0.0);
  }

  public Unit (int kind)
  {
    this(kind, 1, 0, 1.0, 0.0);
  }

  public Unit (String kind, int exponent, int scale, double multiplier)
  {
    this(kind, exponent, scale, multiplier, 0.0);
  }

  public Unit (String kind, int exponent, int scale)
  {
    this(kind, exponent, scale, 1.0, 0.0);
  }
  
  public Unit (String kind, int exponent)
  {
    this(kind, exponent, 0, 1.0, 0.0);
  }

  public Unit (String kind)
  {
    this(kind, 1, 0, 1.0, 0.0);
  }
%}


%extend UnitDefinition
{
  UnitDefinition() { return new UnitDefinition(); }
}

%typemap("javacode") UnitDefinition
%{
  public UnitDefinition (String id)
  {
    this(id, "");
  }
%}


%extend Compartment
{
  Compartment() { return new Compartment(); }
}


%extend Species
{
  Species() { return new Species(); }
}


%extend Parameter
{
  Parameter() { return new Parameter(); }
}

%typemap("javacode") Parameter
%{
  public Parameter (String id, double value, String units)
  {
    this(id, value, units, true);
  }

  public Parameter (String id, double value)
  {
    this(id, value, "", true);
  }
%}


%extend Rule
{
  Rule() { return new Rule(); }
}


%extend AlgebraicRule
{
  AlgebraicRule() { return new AlgebraicRule(); }
}


%typemap("javacode") AssignmentRule
%{
  public AssignmentRule (String variable, String formula)
  {
    this(variable, formula, libsbml.RULE_TYPE_SCALAR);
  }

  public AssignmentRule (String variable, ASTNode math)
  {
    this(variable, math, libsbml.RULE_TYPE_SCALAR);
  }
%}


%typemap("javacode") SpeciesConcentrationRule
%{
  public SpeciesConcentrationRule (String species, String formula)
  {
    this(species, formula, libsbml.RULE_TYPE_SCALAR);
  }
%}


%typemap("javacode") CompartmentVolumeRule
%{
  public CompartmentVolumeRule (String compartment, String formula)
  {
    this(compartment, formula, libsbml.RULE_TYPE_SCALAR);
  }
%}


%typemap("javacode") CompartmentVolumeRule
%{
  public CompartmentVolumeRule (String compartment, String formula)
  {
    this(compartment, formula, libsbml.RULE_TYPE_SCALAR);
  }
%}


%typemap("javacode") ParameterRule
%{
  public ParameterRule (String name, String formula)
  {
    this(name, formula, libsbml.RULE_TYPE_SCALAR);
  }
%}


%extend RateRule
{
  RateRule() { return new RateRule(); }
}


%extend Reaction
{
  Reaction() { return new Reaction(); }
}

%typemap("javacode") Reaction
%{
  public Reaction (String sid, KineticLaw kl)
  {
    this(sid, kl, true);
  }

  public Reaction (String sid)
  {
    this(sid, null, true);
  }
%}


%extend SimpleSpeciesReference
{
  SimpleSpeciesReference() { return new SimpleSpeciesReference(); }
}


%extend ModifierSpeciesReference
{
  ModifierSpeciesReference() { return new ModifierSpeciesReference(); }
}


%extend SpeciesReference
{
  SpeciesReference() { return new SpeciesReference(); }
}

%typemap("javacode") SpeciesReference
%{
  SpeciesReference (String species, double stoichiometry)
  {
    this(species, stoichiometry, 1);
  }

  SpeciesReference (String species)
  {
    this(species, 1.0, 1);
  }
%}


%extend KineticLaw
{
  KineticLaw() { return new KineticLaw(); }
}

%typemap("javacode") KineticLaw
%{
  KineticLaw (String formula, String timeUnits)
  {
    this(formula, timeUnits, "");
  }

  KineticLaw (String formula)
  {
    this(formula, "", "");
  }
%}


%extend Event
{
  Event() { return new Event(); }
}

%typemap("javacode") Event
%{
  Event (String id, String trigger)
  {
    this(id, trigger, "");
  }

  Event (String id)
  {
    this(id, "", "");
  }

  Event (String id, ASTNode trigger)
  {
    this(id, trigger, (ASTNode) null);
  }
%}


%extend EventAssignment
{
  EventAssignment() { return new EventAssignment(); }
}


%extend SBMLReader
{
  SBMLReader() { return new SBMLReader(); }
}


%extend ParseMessage
{
  ParseMessage() { return new ParseMessage(); }
}

%typemap("javacode") ParseMessage
%{
  ParseMessage (String message, long line)
  {
    this(message, line, 0);
  }

  ParseMessage (String message)
  {
    this(message, 0, 0);
  }
%}
