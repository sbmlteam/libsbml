/**
 * \file    ModifierSpeciesReference.h
 * \brief   SBML ModifierSpeciesReference
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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


#ifndef ModifierSpeciesReference_h
#define ModifierSpeciesReference_h


#include "common/extern.h"


#ifdef __cplusplus


#include <string>
#include "SimpleSpeciesReference.h"


class SBMLVisitor;


class ModifierSpeciesReference : public SimpleSpeciesReference
{
public:

  /**
   * Creates a new ModifierSpeciesReference, optionally with its species
   * attribute set.
   */
  LIBSBML_EXTERN
  ModifierSpeciesReference (const std::string& species = "");

  /**
   * Destroys this ModifierSpeciesReference.
   */
  LIBSBML_EXTERN
  virtual ~ModifierSpeciesReference();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Reaction's next
   * SimpleSpeciesReference (if available).
   */
  LIBSBML_EXTERN
  virtual bool accept (SBMLVisitor& v) const;


protected:

  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Creates a new ModifierSpeciesReference and returns a pointer to it.
 */
LIBSBML_EXTERN
ModifierSpeciesReference_t *
ModifierSpeciesReference_create (void);

/**
 * Creates a new ModifierSpeciesReference with the given species and
 * returns a pointer to it.  This convenience function is functionally
 * equivalent to:
 *
 *   ModifierSpeciesReference_t *msr = ModiferSpeciesReference_create();
 *   ModifierSpeciesReference_setSpecies(msr, species);
 */
LIBSBML_EXTERN
ModifierSpeciesReference_t *
ModifierSpeciesReference_createWith (const char *species);

/**
 * Frees the given ModifierSpeciesReference.
 */
LIBSBML_EXTERN
void
ModifierSpeciesReference_free (ModifierSpeciesReference_t *msr);


/**
 * @return the species for this ModifierSpeciesReference.
 */
LIBSBML_EXTERN
const char *
ModifierSpeciesReference_getSpecies (const ModifierSpeciesReference_t *msr);

/**
 * @return 1 if the species for this ModifierSpeciesReference has been set,
 * 0 otherwise.
 */
LIBSBML_EXTERN
int
ModifierSpeciesReference_isSetSpecies (const ModifierSpeciesReference_t *msr);

/**
 * Sets the species of this ModifierSpeciesReference to a copy of sid.
 */
LIBSBML_EXTERN
void
ModifierSpeciesReference_setSpecies ( ModifierSpeciesReference_t *msr,
                                      const char *sid );


END_C_DECLS


#endif  /* !SWIG */
#endif  /* ModifierSpeciesReference_h */
