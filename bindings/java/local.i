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
 * The directives below override the default SWIG generated
 * code for certain methods.  The idea is to tell SWIG to disown the
 * passed-in object.  The containing object will takeover ownership
 * and delete the object as appropriate.  This avoids a deadly
 * double-delete which can result in a segmentation fault.  For
 * example, each SBase that is appended to a ListOf is subsequently
 * owned by that ListOf.
 *
 * This is handled slightly more elegantly in the Python SWIG module.
 * Python SWIG supports the feature("shadow") directive.  Java does
 * not, hence the hack below.
 *
 * For a given method XXX(), the idea is to rename it to XXXInternal()
 * and make it private (so no one can call it).  Simply using %ignore
 * does not work, as SWIG will generate absolutely no wrapper code
 * (e.g. libsbmlJNI.XXX()) for the method.  The method is
 * then replaced using a javacode typemap.  If this is at all
 * confusing, look at the method definitions for append() and 
 * appendInternal() in src/ListOf.java.  The former method is defined
 * below, the latter is automatically generated by SWIG but renamed and
 * declard private by us.
 */


%javamethodmodifiers    ListOf::append "private";
%rename(appendInternal) ListOf::append;

%typemap("javacode") ListOf
%{
  public void append (SBase item)
  {
    libsbmlJNI.ListOf_appendInternal(swigCPtr, SBase.getCPtr(item));
    item.swigCMemOwn = false;
  }
%}


%javamethodmodifiers      SBMLDocument::setModel "private";
%rename(setModelInternal) SBMLDocument::setModel;

%typemap("javacode") SBMLDocument
%{
  public void setModel (Model m)
  {
    libsbmlJNI.SBMLDocument_setModelInternal(swigCPtr, Model.getCPtr(m));
    m.swigCMemOwn = false;
  }
%}


%javamethodmodifiers Model::addFunctionDefinition "private";
%javamethodmodifiers Model::addUnitDefinition     "private";
%javamethodmodifiers Model::addCompartment        "private";
%javamethodmodifiers Model::addSpecies            "private";
%javamethodmodifiers Model::addParameter          "private";
%javamethodmodifiers Model::addRule               "private";
%javamethodmodifiers Model::addReaction           "private";
%javamethodmodifiers Model::addEvent              "private";

%rename(addFunctionDefinitionInternal) Model::addFunctionDefinition;
%rename(addUnitDefinitionInternal)     Model::addUnitDefinition;
%rename(addCompartmentInternal)        Model::addCompartment;
%rename(addSpeciesInternal)            Model::addSpecies;
%rename(addParameterInternal)          Model::addParameter;
%rename(addRuleInternal)               Model::addRule;
%rename(addReactionInternal)           Model::addReaction;
%rename(addEventInternal)              Model::addEvent;

%typemap("javacode") Model
%{
  public void addFunctionDefinition (FunctionDefinition fd)
  {
    libsbmlJNI.Model_addFunctionDefinitionInternal
      (swigCPtr, FunctionDefinition.getCPtr(fd));
    fd.swigCMemOwn = false;
  }

  public void addUnitDefinition (UnitDefinition ud)
  {
    libsbmlJNI.Model_addUnitDefinitionInternal
      (swigCPtr, UnitDefinition.getCPtr(ud));
    ud.swigCMemOwn = false;
  }

  public void addCompartment (Compartment c)
  {
    libsbmlJNI.Model_addCompartmentInternal(swigCPtr, Compartment.getCPtr(c));
    c.swigCMemOwn = false;
  }

  public void addSpecies (Species s)
  {
    libsbmlJNI.Model_addSpeciesInternal(swigCPtr, Species.getCPtr(s));
    s.swigCMemOwn = false;
  }

  public void addParameter (Parameter p)
  {
    libsbmlJNI.Model_addParameterInternal(swigCPtr, Parameter.getCPtr(p));
    p.swigCMemOwn = false;
  }

  public void addRule (Rule r)
  {
    libsbmlJNI.Model_addRuleInternal(swigCPtr, Rule.getCPtr(r));
    r.swigCMemOwn = false;
  }

  public void addReaction (Reaction r)
  {
    libsbmlJNI.Model_addReactionInternal(swigCPtr, Reaction.getCPtr(r));
    r.swigCMemOwn = false;
  }

  public void addEvent (Event e)
  {
    libsbmlJNI.Model_addEventInternal(swigCPtr, Event.getCPtr(e));
    e.swigCMemOwn = false;
  }
%}


%javamethodmodifiers     FunctionDefinition::setMath "private";
%rename(setMathInternal) FunctionDefinition::setMath;

%typemap("javacode") FunctionDefinition
%{
  public void setMath (ASTNode math)
  {
    libsbmlJNI.
      FunctionDefinition_setMathInternal(swigCPtr, ASTNode.getCPtr(math));
    math.swigCMemOwn = false;
  }
%}


%javamethodmodifiers     UnitDefinition::addUnit "private";
%rename(addUnitInternal) UnitDefinition::addUnit;

%typemap("javacode") UnitDefinition
%{
  public void addUnit (Unit u)
  {
    libsbmlJNI.UnitDefinition_addUnitInternal(swigCPtr, Unit.getCPtr(u));
    u.swigCMemOwn = false;
  }
%}


%javamethodmodifiers     Rule::setMath "private";
%rename(setMathInternal) Rule::setMath;

%typemap("javacode") Rule
%{
  public void setMath (ASTNode math)
  {
    libsbmlJNI.Rule_setMathInternal(swigCPtr, ASTNode.getCPtr(math));
    math.swigCMemOwn = false;
  }
%}


%javamethodmodifiers Reaction::addReactant   "private";
%javamethodmodifiers Reaction::addProduct    "private";
%javamethodmodifiers Reaction::addModifier   "private";
%javamethodmodifiers Reaction::setKineticLaw "private";

%rename(addReactantInternal)   Reaction::addReactant;
%rename(addProductInternal)    Reaction::addProduct;
%rename(addModifierInternal)   Reaction::addModifier;
%rename(setKineticLawInternal) Reaction::setKineticLaw;

%typemap("javacode") Reaction
%{
  public void addReactant (SpeciesReference sr)
  {
    libsbmlJNI.
      Reaction_addReactantInternal(swigCPtr, SpeciesReference.getCPtr(sr));
    sr.swigCMemOwn = false;
  }

  public void addProduct (SpeciesReference sr)
  {
    libsbmlJNI.
      Reaction_addProductInternal(swigCPtr, SpeciesReference.getCPtr(sr));
    sr.swigCMemOwn = false;
  }

  public void addModifier (SpeciesReference sr)
  {
    libsbmlJNI.
      Reaction_addModifierInternal(swigCPtr, SpeciesReference.getCPtr(sr));
    sr.swigCMemOwn = false;
  }

  public void setKineticLaw (KineticLaw kl)
  {
    libsbmlJNI.
      Reaction_setKineticLawInternal(swigCPtr, KineticLaw.getCPtr(kl));
    kl.swigCMemOwn = false;
  }
%}


%javamethodmodifiers SpeciesReference::setStoichiometryMath "private";
%rename(setStoichiometryMathInternal) SpeciesReference::setStoichiometryMath;

%typemap("javacode") SpeciesReference
%{
  public void setStoichiometryMath (ASTNode math)
  {
    libsbmlJNI.SpeciesReference_setStoichiometryMathInternal__SWIG_0
      (swigCPtr, ASTNode.getCPtr(math));
    math.swigCMemOwn = false;
  }
%}


%javamethodmodifiers Event::setTrigger         "private";
%javamethodmodifiers Event::setDelay           "private";
%javamethodmodifiers Event::addEventAssignment "private";

%rename(setTriggerInternal)         Event::setTrigger;
%rename(setDelayInternal)           Event::setDelay;
%rename(addEventAssignmentInternal) Event::addEventAssignment;


%typemap("javacode") Event
%{
  public void setTrigger (ASTNode math)
  {
    libsbmlJNI.Event_setTriggerInternal(swigCPtr, ASTNode.getCPtr(math));
    math.swigCMemOwn = false;
  }

  public void setDelay (ASTNode math)
  {
    libsbmlJNI.Event_setDelayInternal(swigCPtr, ASTNode.getCPtr(math));
    math.swigCMemOwn = false;
  }

  public void addEventAssignment (EventAssignment ea)
  {
    libsbmlJNI.Event_addEventAssignmentInternal
      (swigCPtr, EventAssignment.getCPtr(ea));
    ea.swigCMemOwn = false;
  }
%}


%javamethodmodifiers     EventAssignment::setMath "private";
%rename(setMathInternal) EventAssignment::setMath;

%typemap("javacode") EventAssignment
%{
  public void setMath (ASTNode math)
  {
    libsbmlJNI.
      EventAssignment_setMathInternal(swigCPtr, ASTNode.getCPtr(math));
    math.swigCMemOwn = false;
  }
%}
