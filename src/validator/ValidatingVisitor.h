/**
 * \file    ValidatingVisitor.h
 * \brief   Visits (and validates) each object in an SBML object tree
 * \author  Ben Bornstein
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
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef ValidatingVisitor_h
#define ValidatingVisitor_h


#ifdef __cplusplus


#include "sbml/SBMLVisitor.h"


class Validator;


/**
 * An SBMLVisitor visits each object in an SBML object tree, calling the
 * appropriate visit() method for the object visited.
 *
 * A ValidatingVisitor overrides each visit method to validate the given
 * SBML object.
 */
class ValidatingVisitor: public SBMLVisitor
{
public:

   ValidatingVisitor (Validator& v, const Model& m);
  ~ValidatingVisitor ();


  void visit (const SBMLDocument &x);
  void visit (const Model        &x);
  void visit (const KineticLaw   &x);

  bool visit (const FunctionDefinition       &x);
  bool visit (const UnitDefinition           &x);
  bool visit (const Unit                     &x);
  bool visit (const Compartment              &x);
  bool visit (const Species                  &x);
  bool visit (const Parameter                &x);
  bool visit (const AlgebraicRule            &x);
  bool visit (const AssignmentRule           &x);
  bool visit (const SpeciesConcentrationRule &x);
  bool visit (const CompartmentVolumeRule    &x);
  bool visit (const ParameterRule            &x);
  bool visit (const RateRule                 &x);
  bool visit (const Reaction                 &x);
  bool visit (const SpeciesReference         &x);
  bool visit (const ModifierSpeciesReference &x);
  bool visit (const Event                    &x);
  bool visit (const EventAssignment          &x);


protected:

  Validator&   mValidator;
  const Model& mModel;
};


#endif  /* __cplusplus */
#endif  /* ValidatingVisitor_h */
