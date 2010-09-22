/**
 * @file   SBMLVisitor.h
 * @brief  Visitor Design Pattern for the SBML object tree
 * @author Ben Bornstein
 * 
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->
 *
 * @class SBMLVisitor
 * @brief Support class for performing operations on SBML objects.
 * 
 * <em style='color: #555'>This class of objects is defined by libSBML only
 * and has no direct equivalent in terms of SBML components.</em>
 * 
 * This is a class that supports the use of the <a target="_blank"
 * href="http://en.wikipedia.org/wiki/Design_pattern_(computer_science)"><i>Visitor
 * Pattern</i></a> to perform operations on SBML objects.  The LibSBML
 * implementation of the Visitor Pattern couples the notion of visitation
 * with the traversal of an SBML model oject hierarchy.
 *
 * This class (SBMLVisitor) does not provide stand-alone functionality;
 * rather, it defines the interface that user visitor classes must
 * implement.  All of the individual SBML object classes have methods named
 * <code>accept</code> that are used for invoking an object of class
 * SBMLVisitor.  An example of its use is in the SBML validation system,
 * which is internally implemented using this Visitor Pattern facility.
 */

#ifndef SBMLVisitor_h
#define SBMLVisitor_h


#ifdef __cplusplus


#include <sbml/SBMLTypeCodes.h>

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * Forward class name declarations avoid cyclic dependencies.
 */

class SBase;

class SBMLDocument;
class Model;
class FunctionDefinition;
class Unit;
class UnitDefinition;
class CompartmentType;
class SpeciesType;
class Compartment;
class Species;
class Parameter;
class InitialAssignment;

class Rule;
class AlgebraicRule;
class AssignmentRule;
class RateRule;

class Constraint;

class Reaction;
class SimpleSpeciesReference;
class ModifierSpeciesReference;
class SpeciesReference;
class KineticLaw;

class Event;
class EventAssignment;

class Trigger;
class Delay;
class Priority;

class ListOf;


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
  virtual bool visit (const CompartmentType          &x);
  virtual bool visit (const SpeciesType              &x);
  virtual bool visit (const Compartment              &x);
  virtual bool visit (const Species                  &x);
  virtual bool visit (const Parameter                &x);
  virtual bool visit (const InitialAssignment        &x);
  virtual bool visit (const Rule                     &x);
  virtual bool visit (const AlgebraicRule            &x);
  virtual bool visit (const AssignmentRule           &x);
  virtual bool visit (const RateRule                 &x);
  virtual bool visit (const Constraint               &x);
  virtual bool visit (const Reaction                 &x);
  virtual bool visit (const SimpleSpeciesReference   &x);
  virtual bool visit (const SpeciesReference         &x);
  virtual bool visit (const ModifierSpeciesReference &x);
  virtual bool visit (const Event                    &x);
  virtual bool visit (const EventAssignment          &x);
  virtual bool visit (const Trigger                  &x);
  virtual bool visit (const Delay                    &x);
  virtual void visit (const Priority                 &x);

  virtual void leave (const SBMLDocument &x);
  virtual void leave (const Model        &x);
  virtual void leave (const KineticLaw   &x);
  virtual void leave (const Priority     &x);
  virtual void leave (const Reaction     &x);
  virtual void leave (const ListOf &x, SBMLTypeCode_t type);
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* SBMLVisitor_h */
