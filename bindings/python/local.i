/**
 * Filename    : local.i
 * Description : Python-specific SWIG directives for wrapping libSBML API
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
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

/**
 * Allows ListOf objects:
 *
 *   - To be indexed and sliced, e.g. lst[0].
 */

%extend SBase {

  %pythoncode {

    def __str__(self):
      clas = self._getClass()

      result = clas.__name__
      if result.endswith("Ptr"):
        result = result[:-3]
      result += ":\n"

      getters = [
        (self._memberName(memberName), clas.__dict__[memberName])
          for memberName in clas.__dict__.keys()
            if memberName.startswith("get")
      ]
      getters.sort(lambda a, b: cmp(a[0], b[0]))
      memberColumnWidth = max([len(getter[0]) for getter in getters]) + 2
      fmt = "   %%-%ds %%s" % memberColumnWidth

      result += "\n".join([
        line for line in self._memberLines(fmt, getters) if line is not None
      ])
      
      return result;


    def _memberName(self, getterName):
      return getterName[3].lower() + getterName[4:]


    def _memberLines(self, fmt, getters):
      return [self._memberline(fmt, getter) for getter in getters]


    def _memberline(self, fmt, getter):
        try:
          return fmt % (getter[0] + ":", repr(getter[1](self)))
        except TypeError:
          return None


    def _getClass(self):
      if self.__class__ in self.__dict__:
        # find the actual SWIG shadow class if we are a Ptr
        return self.__dict__[self.__class__]
      else:
        return self.__class__
  }
}


%extend ListOf {
  int __len__()
  {
    return self->getNumItems();
  }

  %pythoncode {

    def __getitem__(self, key):

      try:
         keyIsSlice = isinstance(key, slice)
      except:
         keyIsSlice = 0

      if keyIsSlice:
        start = key.start
        if start is None:
          start = 0
        stop = key.stop
        if stop is None:
          stop = self.getNumItems()
        return [self[i] for i in range(
          self._fixNegativeIndex(start), self._fixNegativeIndex(stop)
        )]

      key = self._fixNegativeIndex(key)
      if key < 0 or key >= self.getNumItems():
        raise IndexError(key)
      return self.get(key)


    def _fixNegativeIndex(self, index):
      if index < 0:
        return index + self.getNumItems()
      else:
        return index


    def __iter__(self):
      for i in range(self.getNumItems()):
        yield self[i]


    def __repr__(self):
      return "[" + ", ".join([repr(self[i]) for i in range(len(self))]) + "]"


    def __str__(self):
      return repr(self)
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


%feature("shadow") ListOf::prepend(SBase*)
%{
  def prepend(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.ListOf_prepend(*args)
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


%feature("shadow") FunctionDefinition::FunctionDefinition
%{
  def __init__(self, *args):
    _swig_setattr(self, FunctionDefinition, 'this', _libsbml.new_FunctionDefinition(*args))
    _swig_setattr(self, FunctionDefinition, 'thisown', 1)
    try:
      if args[1] is not None: args[1].thisown = 0
    except (IndexError, AttributeError):
      pass
%}


%feature("shadow") FunctionDefinition::setMath(ASTNode*)
%{
  def setMath(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.FunctionDefinition_setMath(*args)
%}


%feature("shadow") Event::Event(const std::string&, ASTNode*, ASTNode*)
%{
  def __init__(self, *args):
    _swig_setattr(self, Event, 'this', _libsbml.new_Event(*args))
    _swig_setattr(self, Event, 'thisown', 1)
    for index in [1, 2]:
      try:
        if args[index] is not None: args[index].thisown = 0
      except (IndexError, AttributeError):
        pass
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


%feature("shadow") EventAssignment::EventAssignment(const std::string&, ASTNode*)
%{
  def __init__(self, *args):
    _swig_setattr(self, EventAssignment, 'this', _libsbml.new_EventAssignment(*args))
    _swig_setattr(self, EventAssignment, 'thisown', 1)
    try:
      if args[1] is not None: args[1].thisown = 0
    except (IndexError, AttributeError):
      pass
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


%feature("shadow") AssignmentRule::AssignmentRule
%{
  def __init__(self, *args):
    _swig_setattr(self, AssignmentRule, 'this', _libsbml.new_AssignmentRule(*args))
    _swig_setattr(self, AssignmentRule, 'thisown', 1)
    try:
      if args[1] is not None: args[1].thisown = 0
    except (IndexError, AttributeError):
      pass
%}


%feature("shadow") AlgebraicRule::AlgebraicRule
%{
  def __init__(self, *args):
    _swig_setattr(self, AlgebraicRule, 'this', _libsbml.new_AlgebraicRule(*args))
    _swig_setattr(self, AlgebraicRule, 'thisown', 1)
    try:
      if args[0] is not None: args[0].thisown = 0
    except (IndexError, AttributeError):
      pass
%}


%feature("shadow") RateRule::RateRule
%{
  def __init__(self, *args):
    _swig_setattr(self, RateRule, 'this', _libsbml.new_RateRule(*args))
    _swig_setattr(self, RateRule, 'thisown', 1)
    try:
      if args[1] is not None: args[1].thisown = 0
    except (IndexError, AttributeError):
      pass
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


%feature("shadow") Model::addFunctionDefinition(FunctionDefinition&)
%{
  def addFunctionDefinition(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Model_addFunctionDefinition(*args)
%}


%feature("shadow") Model::addUnitDefinition(UnitDefinition&)
%{
  def addUnitDefinition(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Model_addUnitDefinition(*args)
%}


%feature("shadow") Model::addCompartment(Compartment&)
%{
  def addCompartment(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Model_addCompartment(*args)
%}


%feature("shadow") Model::addSpecies(Species&)
%{
  def addSpecies(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Model_addSpecies(*args)
%}


%feature("shadow") Model::addParameter(Parameter&)
%{
  def addParameter(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Model_addParameter(*args)
%}


%feature("shadow") Model::addReaction(Reaction&)
%{
  def addReaction(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Model_addReaction(*args)
%}


%feature("shadow") Model::addEvent(Event&)
%{
  def addEvent(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.Model_addEvent(*args)
%}


%feature("shadow") KineticLaw::setMath(ASTNode*)
%{
  def setMath(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.KineticLaw_setMath(*args)
%}


%feature("shadow") KineticLaw::addParameter(Parameter&)
%{
  def addParameter(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.KineticLaw_addParameter(*args)
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


%feature("shadow") ASTNode::addChild(ASTNode*)
%{
  def addChild(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.ASTNode_addChild(*args)
%}


%feature("shadow") ASTNode::prependChild(ASTNode*)
%{
  def prependChild(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.ASTNode_prependChild(*args)
%}


%feature("shadow") MathMLDocument::setMath(ASTNode*)
%{
  def setMath(*args):
    if args[1] is not None: args[1].thisown = 0
    return _libsbml.MathMLDocument_setMath(*args)
%}
