/**
 * \file    local.i
 * \brief   Perl-specific SWIG directives for wrapping libSBML API
 * \author  TBI {xtof,raim}@tbi.univie.ac.at
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 TBI
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
 *     Christoph Flamm and Rainer Machne
 *
 * Contributor(s):
 */


/**
 * Convert SBase and Rule objects into the most specific type possible.
 */
%typemap(out) SBase*, Rule*
{
  ST(argvi) = sv_newmortal();
  SWIG_MakePtr(ST(argvi++), (void*)$1, GetDowncastSwigType($1), SWIG_SHADOW|0);
}

/**
 * Unfortunately SWIG Version 1.3.21 supports %feature only for python and
 * java therefore we need to patch LibSBML.pm by ourself
 * (do not remove the following empty line)
 */

%feature("shadow") SpeciesReference::setStoichiometryMath
%{
  sub setStoichiometryMath {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::SpeciesReference_setStoichiometryMath(@_);
  }
%}

%feature("shadow") ListOf::append(SBase*)
%{
  sub append {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::ListOf_append(@_);
  }
%}

%feature("shadow") ListOf::prepend(SBase*)
%{
  sub prepend {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::ListOf_prepend(@_);
  }
%}

%feature("shadow") ListOf::remove(unsigned int)
%{
  sub remove {
    my $obj = LibSBMLc::ListOf_remove(@_);
    $obj->DISOWN() if defined $obj;
    return $obj;
  }
%}

%feature("shadow") SBMLDocument::setModel(Model*)
%{
  sub setModel {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::SBMLDocument_setModel(@_);
  }
%}

%feature("shadow") FunctionDefinition::setMath(ASTNode*)
%{
  sub setMath {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::FunctionDefinition_setMath(@_);
  }
%}

%feature("shadow") Event::setTrigger(ASTNode*)
%{
  sub setTrigger {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Event_setTrigger(@_);
  }
%}

%feature("shadow") Event::setDelay(ASTNode*)
%{
  sub setDelay {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Event_setDelay(@_);
  }
%}

%feature("shadow") Event::addEventAssignment(EventAssignment&)
%{
  sub addEventAssignment {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Event_addEventAssignment(@_);
  }
%}

%feature("shadow") EventAssignment::setMath(ASTNode*)
%{
  sub setMath {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::EventAssignment_setMath(@_);
  }
%}

%feature("shadow") Rule::setMath(ASTNode*)
%{
  sub setMath {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Rule_setMath(@_);
  }
%}

%feature("shadow") UnitDefinition::addUnit(Unit&)
%{
  sub addUnit {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::UnitDefinition_addUnit(@_);
  }
%}

%feature("shadow") Model::addRule(Rule&)
%{
  sub addRule {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Model_addRule(@_);
  }
%}

%feature("shadow") Model::addFunctionDefinition(FunctionDefinition&)
%{
  sub addFunctionDefinition {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Model_addFunctionDefinition(@_);
  }
%}

%feature("shadow") Model::addUnitDefinition(UnitDefinition&)
%{
  sub addUnitDefinition {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Model_addUnitDefinition(@_);
  }
%}

%feature("shadow") Model::addCompartment(Compartment&)
%{
  sub addCompartment {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Model_addCompartment(@_);
  }
%}

%feature("shadow") Model::addSpecies(Species&)
%{
  sub addSpecies {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Model_addSpecies(@_);
  }
%}

%feature("shadow") Model::addParameter(Parameter&)
%{
  sub addParameter {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Model_addParameter(@_);
  }
%}

%feature("shadow") Model::addReaction(Reaction&)
%{
  sub addReaction {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Model_addReaction(@_);
  }
%}

%feature("shadow") Model::addEvent(Event&)
%{
  sub addEvent {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Model_addEvent(@_);
  }
%}

%feature("shadow") KineticLaw::setMath(ASTNode*)
%{
  sub setMath {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::KineticLaw_setMath(@_);
  }
%}

%feature("shadow") KineticLaw::addParameter(Parameter&)
%{
  sub addParameter {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::KineticLaw_addParameter(@_);
  }
%}

%feature("shadow") Reaction::setKineticLaw(KineticLaw& kl)
%{
  sub setKineticLaw {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Reaction_setKineticLaw(@_);
  }
%}

%feature("shadow") Reaction::addReactant(SpeciesReference& sr)
%{
  sub addReactant {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Reaction_addReactant(@_);
  }
%}

%feature("shadow") Reaction::addProduct(SpeciesReference& sr)
%{
  sub addProduct {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Reaction_addProduct(@_);
  }
%}

%feature("shadow") Reaction::addModifier(ModifierSpeciesReference& sr)
%{
  sub addModifier {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::Reaction_addModifier(@_);
  }
%}

%feature("shadow") ASTNode::addChild(ASTNode*)
%{
  sub addChild {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::ASTNode_addChild(@_);
  }
%}

%feature("shadow") ASTNode::prependChild(ASTNode*)
%{
  sub prependChild {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::ASTNode_prependChild(@_);
  }
%}

%feature("shadow") MathMLDocument::setMath(ASTNode*)
%{
  sub setMath {
    $_[1]->DISOWN() if defined $_[1];
    return LibSBMLc::MathMLDocument_setMath(@_);
  }
%}

%feature("shadow") AssignmentRule::AssignmentRule
%{
  sub new {
    my $pkg = shift;
    $_[1]->DISOWN() if defined $_[1] && ref($_[1]) eq 'LibSBML::ASTNode';
    my $self = LibSBMLc::new_AssignmentRule(@_);
    bless $self, $pkg if defined($self);
  }
%}

%feature("shadow") FunctionDefinition::FunctionDefinition
%{
  sub new {
    my $pkg = shift;
    $_[1]->DISOWN() if defined $_[1] && ref($_[1]) eq 'LibSBML::ASTNode';
    my $self = LibSBMLc::new_FunctionDefinition(@_);
    bless $self, $pkg if defined($self);
  }
%}

%feature("shadow") AlgebraicRule::AlgebraicRule
%{
  sub new {
    my $pkg = shift;
    $_[0]->DISOWN() if defined $_[0] && ref($_[0]) eq 'LibSBML::ASTNode';
    my $self = LibSBMLc::new_AlgebraicRule(@_);
    bless $self, $pkg if defined($self);
  }
%}

%feature("shadow") EventAssignment::EventAssignment
%{
  sub new {
    my $pkg = shift;
    $_[1]->DISOWN() if defined $_[1] && ref($_[1]) eq 'LibSBML::ASTNode';
    my $self = LibSBMLc::new_EventAssignment(@_);
    bless $self, $pkg if defined($self);
  }
%}

%feature("shadow") Event::Event
%{
  sub new {
    my $pkg = shift;
    $_[1]->DISOWN() if defined $_[1] && ref($_[1]) eq 'LibSBML::ASTNode';
    my $self = LibSBMLc::new_Event(@_);
    bless $self, $pkg if defined($self);
  }
%}

