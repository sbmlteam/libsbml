/**
 * Filename    : local.i
 * Description : Python-specific SWIG directives for wrapping libSBML API
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
 * Allows ListOf objects:
 *
 *   - To be indexed and sliced, e.g. lst[0].
 */
%extend ListOf
{
  SBase* __getitem__(int index)
  {
    return self->get(index);
  }
}



/**
 * Convert SBase objects into the most specific type possible.
 */
%typemap(out) SBase*
{
  $result = SWIG_NewPointerObj($1, GetDowncastSwigType($1), 0);
}

/**
 * Convert Rule objects into the most specific type possible.
 */
%typemap(out) Rule*
{
  $result = SWIG_NewPointerObj($1, GetDowncastSwigType($1), 0);
}


/**
 * Add an equality operator to SBase.  All subclasses of SBase
 * will inherit this method.
 *
 * The %feature("shadow") rewrites __cmp__ such that two objects of
 * disimilar type can be compared without throwing a TypeError.  For
 * example: the following will return false and not throw an exception:
 *
 *   c = libsbml.Compartment()
 *   n = 5
 *   c == n
 *
 * The %extend forces the generation of a Python shadow class method
 * named __cmp__.  For some strange reason, this is order dependent.
 * The directive %feature must occur before %extend.  If not, the
 * %feature directive seems to be ignored.  If anything, it seems like
 * it should be the reverse.
 *
 * For more information, see testEquality() in accept.py.
 */
%feature("shadow") SBase::__cmp__
{
  def __cmp__(self, rhs):
    if hasattr(self, 'this') and hasattr(rhs, 'this'):
      if self.this == rhs.this: return 0
    return 1
}

%extend SBase
{
  bool __cmp__(const SBase& rhs) { return self != &rhs; }
}


/**
 * The features directives below override the default SWIG generated
 * code for certain methods.  The idea is to tell SWIG to disown the
 * passed-in object.  The containing object will takeover ownership
 * and delete the object as appropriate.  This avoids a deadly
 * double-delete which can result in a segmentation fault.  For
 * example, each SBase that is appended to a ListOf is subsequently
 * owned by that ListOf.
 */


%feature("shadow") SpeciesReference::setStoichiometryMath
%{
  def setStoichiometryMath(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.SpeciesReference_setStoichiometryMath(*args)
%}

%feature("shadow") ListOf::append(SBase*)
%{
  def append(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.ListOf_append(*args)
%}


%feature("shadow") ListOf::remove(unsigned int)
%{
  def remove(*args):
    result = _libsbml.ListOf_remove(*args)
    if result is not None: result.thisown = 1
    return result
%}


%feature("shadow") SBMLDocument::setModel(Model*)
%{
  def setModel(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.SBMLDocument_setModel(*args)
%}


%feature("shadow") FunctionDefinition::setMath(ASTNode*)
%{
  def setMath(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.FunctionDefinition_setMath(*args)
%}


%feature("shadow") Event::setTrigger(ASTNode*)
%{
  def setTrigger(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Event_setTrigger(*args)
%}


%feature("shadow") Event::setDelay(ASTNode*)
%{
  def setDelay(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Event_setDelay(*args)
%}


%feature("shadow") Event::addEventAssignment(EventAssignment&)
%{
  def addEventAssignment(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Event_addEventAssignment(*args)
%}


%feature("shadow") EventAssignment::setMath(ASTNode*)
%{
  def setMath(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.EventAssignment_setMath(*args)
%}


%feature("shadow") Rule::setMath(ASTNode*)
%{
  def setMath(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Rule_setMath(*args)
%}


%feature("shadow") UnitDefinition::addUnit(Unit&)
%{
  def addUnit(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.UnitDefinition_addUnit(*args)
%}


%feature("shadow") Model::addRule(Rule&)
%{
  def addRule(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Model_addRule(*args)
%}


%feature("shadow") Reaction::Reaction(const std::string&, KineticLaw*, bool)
%{
  def __init__(self, *args):
    _swig_setattr(self, Reaction, 'this', _libsbml.new_Reaction(*args))
    _swig_setattr(self, Reaction, 'thisown', 1)
    if args[1] is not None: args[1].thisown = 0
%}


%feature("shadow") Reaction::setKineticLaw(KineticLaw& kl)
%{
  def setKineticLaw(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Reaction_setKineticLaw(*args)
%}


%feature("shadow") Reaction::addReactant(SpeciesReference& sr)
%{
  def addReactant(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Reaction_addReactant(*args)
%}


%feature("shadow") Reaction::addProduct(SpeciesReference& sr)
%{
  def addProduct(*args):
    if args[1] is not None:
      args[1].thisown = 0
    return _libsbml.Reaction_addProduct(*args)
%}


%feature("shadow") Reaction::addModifier(ModifierSpeciesReference& sr)
%{
  def addModifier(*args):
    if args[1] is not None:
      args[1].thisown = 0
    return _libsbml.Reaction_addModifier(*args)
%}
