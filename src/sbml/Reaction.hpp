/**
 * Filename    : Reaction.hpp
 * Description : SBML Reaction
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-25
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef Reaction_hpp
#define Reaction_hpp


#include <string>

#include "extern.h"

#include "SBase.hpp"
#include "ListOf.hpp"
#include "SpeciesReference.hpp"
#include "ModifierSpeciesReference.hpp"
#include "KineticLaw.hpp"


class Reaction : public SBase
{
public:

  /**
   * Creates a new Reaction, optionally with its id, KineticLaw, and
   * reversible attributes set.
   */
  LIBSBML_EXTERN
  Reaction (   const std::string&  sid        = ""
             , KineticLaw*         kl         = NULL
             , bool                reversible = true );

  /**
   * Destroys this Reaction.
   */
  LIBSBML_EXTERN
  virtual ~Reaction ();


  /**
   * Initializes the fields of this Reaction to their defaults:
   *
   *   - reversible = true
   *   - fast       = false  (L1 only)
   */
  LIBSBML_EXTERN
  void initDefaults ();

  /**
   * @return the id of this Reaction.
   */
  LIBSBML_EXTERN
  const std::string& getId () const;

  /**
   * @return the name of this Reaction.
   */
  LIBSBML_EXTERN
  const std::string& getName () const;

  /**
   * @return the KineticLaw of this Reaction.
   */
  LIBSBML_EXTERN
  KineticLaw* getKineticLaw () const;

  /**
   * @return the reversible status of this Reaction.
   */
  LIBSBML_EXTERN
  bool getReversible () const;

  /**
   * @return the fast status of this Reaction.
   */
  LIBSBML_EXTERN
  bool getFast () const;

  /**
   * @return true if the id of this Reaction has been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetId () const;

  /**
   * @return true if the name of this Reaction has been set, false
   * otherwise.
   *
   * In SBML L1, a Reaction name is required and therefore <b>should always
   * be set</b>.  In L2, name is optional and as such may or may not be
   * set.
   */
  LIBSBML_EXTERN
  bool isSetName () const;

  /**
   * @return true if the KineticLaw of this Reaction has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetKineticLaw () const;

  /**
   * @return true if the fast status of this Reation has been set, false
   * otherwise.
   *
   * In L1, fast is optional with a default of false, which means it is
   * effectively always set.  In L2, however, fast is optional with no
   * default value, so it may or may not be set to a specific value.
   */
  LIBSBML_EXTERN
  bool isSetFast () const;

  /**
   * Sets the id of this Reaction to a copy of sid.
   */
  LIBSBML_EXTERN
  void setId (const std::string& sid);

  /**
   * Sets the name of this Reaction to a copy of string (SName in L1).
   */
  LIBSBML_EXTERN
  void setName (const std::string& str);

  /**
   * Sets the KineticLaw of this Reaction to the given KineticLaw.
   */
  LIBSBML_EXTERN
  void setKineticLaw (KineticLaw& kl);

  /**
   * Sets the reversible status of this Reaction to value.
   */
  LIBSBML_EXTERN
  void setReversible (bool value);

  /**
   * Sets the fast status of this Reaction to value.
   */
  LIBSBML_EXTERN
  void setFast (bool value);

  /**
   * Adds the given reactant (SpeciesReference) to this Reaction.
   */
  LIBSBML_EXTERN
  void addReactant (SpeciesReference& sr);

  /**
   * Adds the given product (SpeciesReference) to this Reaction.
   */
  LIBSBML_EXTERN
  void addProduct (SpeciesReference& sr);

  /**
   * Adds the given modifier (ModifierSpeciesReference) to this Reaction.
   */
  LIBSBML_EXTERN
  void addModifier (ModifierSpeciesReference& msr);

  /**
   * @return the list of Reactants for this Reaction.
   */
  LIBSBML_EXTERN
  ListOf& getListOfReactants ();

  /**
   * @return the list of Products for this Reaction.
   */
  LIBSBML_EXTERN
  ListOf& getListOfProducts ();

  /**
   * @return the list of Modifiers for this Reaction.
   */
  LIBSBML_EXTERN
  ListOf& getListOfModifiers ();

  /**
   * @return the nth reactant (SpeciesReference) of this Reaction.
   */
  LIBSBML_EXTERN
  SpeciesReference* getReactant (unsigned int n) const;

  /**
   * @return the reactant (SpeciesReference) in this Reaction with the
   * given id or NULL if no such reactant exists.
   */
  LIBSBML_EXTERN
  SpeciesReference* getReactantById (const std::string& sid) const;

  /**
   * @return the nth product (SpeciesReference) of this Reaction.
   */
  LIBSBML_EXTERN
  SpeciesReference* getProduct (unsigned int n) const;

  /**
   * @return the product (SpeciesReference) in this Reaction with the given
   * id or NULL if no such product exists.
   */
  LIBSBML_EXTERN
  SpeciesReference* getProductById (const std::string& sid) const;

  /**
   * @return the nth modifier (ModifierSpeciesReference) of this Reaction.
   */
  LIBSBML_EXTERN
  ModifierSpeciesReference* getModifier (unsigned int n) const;

  /**
   * @return the modifier (ModifierSpeciesReference) in this Reaction with
   * the given id or NULL if no such modifier exists.
   */
  LIBSBML_EXTERN
  ModifierSpeciesReference* getModifierById (const std::string& sid) const;

  /**
   * @return the number of reactants (SpeciesReferences) in this Reaction.
   */
  LIBSBML_EXTERN
  unsigned int getNumReactants () const;

  /**
   * @return the number of products (SpeciesReferences) in this Reaction.
   */
  LIBSBML_EXTERN
  unsigned int getNumProducts () const;

  /**
   * @return the number of modifiers (ModifierSpeciesReferences) in this
   * Reaction.
   */
  LIBSBML_EXTERN
  unsigned int getNumModifiers () const;

  /**
   * Unsets the name of this Reaction.
   *
   * In SBML L1, a Reaction name is required and therefore <b>should always
   * be set</b>.  In L2, name is optional and as such may or may not be
   * set.
   */
  LIBSBML_EXTERN
  void unsetName ();

  /**
   * Unsets the KineticLaw of this Reaction.
   */
  LIBSBML_EXTERN
  void unsetKineticLaw ();

  /**
   * Unsets the fast status of this Reation.
   *
   * In L1, fast is optional with a default of false, which means it is
   * effectively always set.  In L2, however, fast is optional with no
   * default value, so it may or may not be set to a specific value.
   */
  LIBSBML_EXTERN
  void unsetFast ();


protected:

  std::string  id;
  std::string  name;
  ListOf       reactant;
  ListOf       product;
  ListOf       modifier;
  KineticLaw*  kineticLaw;
  bool         reversible;
  bool         fast;

  struct
  {
    unsigned int fast:1;
  }
  isSet;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // Reaction_hpp
