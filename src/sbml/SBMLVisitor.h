/**
 * \file   SBMLVisitor.h
 * \brief  Visitor Design Pattern for the SBML object tree
 * \author Ben Bornstein
 * 
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and
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
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef SBMLVisitor_h
#define SBMLVisitor_h


#ifdef __cplusplus


#include "SBMLTypeCodes.h"


/**
 * Forward class name declarations avoid cyclic dependencies.
 */

class SBase;

class SBMLDocument;
class Model;
class FunctionDefinition;
class Unit;
class UnitDefinition;
class Compartment;
class Species;
class Parameter;

class Rule;
class AlgebraicRule;
class AssignmentRule;
class SpeciesConcentrationRule;
class CompartmentVolumeRule;
class ParameterRule;
class RateRule;

class Reaction;
class SimpleSpeciesReference;
class ModifierSpeciesReference;
class SpeciesReference;
class KineticLaw;

class Event;
class EventAssignment;

class ListOf;


/**
 * The Visitor Pattern (Design Patterns, Gamma et al.) allows you to add
 * operations to an established class hierarchy without actually modifying
 * the classes in heirarchy.  For computer science types, C++
 * implementations of Visitor are a form of double-disptach.
 *
 * For convenience, an SBMLVisitor couples the notion of visitation with
 * SBML object tree traversal.
 */
class SBMLVisitor
{
public:

  virtual ~SBMLVisitor ();

  virtual void visit (const SBMLDocument &x);
  virtual void visit (const Model        &x);
  virtual void visit (const KineticLaw   &x);
  virtual void visit (const ListOf       &x, SBMLTypeCode_t type);

  virtual bool visit (const SBase                    &x);
  virtual bool visit (const FunctionDefinition       &x);
  virtual bool visit (const UnitDefinition           &x);
  virtual bool visit (const Unit                     &x);
  virtual bool visit (const Compartment              &x);
  virtual bool visit (const Species                  &x);
  virtual bool visit (const Parameter                &x);
  virtual bool visit (const Rule                     &x);
  virtual bool visit (const AlgebraicRule            &x);
  virtual bool visit (const AssignmentRule           &x);
  virtual bool visit (const SpeciesConcentrationRule &x);
  virtual bool visit (const CompartmentVolumeRule    &x);
  virtual bool visit (const ParameterRule            &x);
  virtual bool visit (const RateRule                 &x);
  virtual bool visit (const Reaction                 &x);
  virtual bool visit (const SimpleSpeciesReference   &x);
  virtual bool visit (const SpeciesReference         &x);
  virtual bool visit (const ModifierSpeciesReference &x);
  virtual bool visit (const Event                    &x);
  virtual bool visit (const EventAssignment          &x);

  virtual void leave (const SBMLDocument &x);
  virtual void leave (const Model        &x);
  virtual void leave (const KineticLaw   &x);
  virtual void leave (const Reaction     &x);
  virtual void leave (const ListOf &x, SBMLTypeCode_t type);
};


#endif  /* __cplusplus */
#endif  /* SBMLVisitor_h */
