/**
 * Filename    : SpeciesReference.cpp
 * Description : SBML SpeciesReference
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-22
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


#include "sbml/FormulaParser.h"

#include "sbml/SpeciesReference.h"
#include "sbml/SpeciesReference.hpp"


/**
 * Creates a new SpeciesReference, optionally with its species,
 * stoichiometry, and denominator attributes set.
 */
LIBSBML_EXTERN
SpeciesReference::SpeciesReference (  const std::string& _species
                                    , double             _stoichiometry
                                    , int                _denominator ) :
    SimpleSpeciesReference( _species       )
  , stoichiometry         ( _stoichiometry )
  , denominator           ( _denominator   )
  , stoichiometryMath     ( NULL           )
{
  init(SBML_SPECIES_REFERENCE);
}


/**
 * Destroys this SpeciesReference.
 */
LIBSBML_EXTERN
SpeciesReference::~SpeciesReference ()
{
  delete stoichiometryMath;
}


/**
 * Initializes the fields of this SpeciesReference to their defaults:
 *
 *   - stoichiometry = 1
 *   - denominator   = 1
 */
LIBSBML_EXTERN
void
SpeciesReference::initDefaults ()
{
  stoichiometry = 1.0;
  denominator   = 1;
}


/**
 * @return the stoichiometry of this SpeciesReference.
 */
LIBSBML_EXTERN
double
SpeciesReference::getStoichiometry () const
{
  return stoichiometry;
}


/**
 * @return the stoichiometryMath of this SpeciesReference.
 */
LIBSBML_EXTERN
const ASTNode*
SpeciesReference::getStoichiometryMath () const
{
  return stoichiometryMath;
}


/**
 * @return the denominator of this SpeciesReference.
 */
LIBSBML_EXTERN
int
SpeciesReference::getDenominator () const
{
  return denominator;
}


/**
 * @return true if the stoichiometryMath of this SpeciesReference has been
 * set, false otherwise.
 */
LIBSBML_EXTERN
bool
SpeciesReference::isSetStoichiometryMath () const
{
  return (stoichiometryMath != NULL);
}


/**
 * Sets the stoichiometry of this SpeciesReference to value.
 */
LIBSBML_EXTERN
void
SpeciesReference::setStoichiometry (double value)
{
  stoichiometry = value;
}


/**
 * Sets the stoichiometryMath of this SpeciesReference to the given
 * ASTNode.
 *
 * The node <b>is not copied</b> and this SpeciesReference <b>takes
 * ownership</b> of it; i.e. subsequent calls to this function or a call to
 * SpeciesReference_free() will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
SpeciesReference::setStoichiometryMath (ASTNode* math)
{
  if (stoichiometryMath == math) return;


  delete stoichiometryMath;
  stoichiometryMath = math;
}


/**
 * Sets the stoichiometryMath of this SpeciesReference to the given
 * formula string.
 */
LIBSBML_EXTERN
void
SpeciesReference::setStoichiometryMath (const std::string& formula)
{
  if ( !formula.empty() )
  {
    setStoichiometryMath( (ASTNode*) SBML_parseFormula( formula.c_str() ) );
  }
}


/**
 * Sets the denominator of this SpeciesReference to value.
 */
LIBSBML_EXTERN
void
SpeciesReference::setDenominator (int value)
{
  denominator = value;
}




/**
 * Creates a new SpeciesReference and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_create (void)
{
  return new(std::nothrow) SpeciesReference;
}


/**
 * Creates a new SpeciesReference with the given species, stoichiometry and
 * denominator and returns a pointer to it.  This convenience function is
 * functionally equivalent to:
 *
 *   SpeciesReference_t *r = SpeciesReference_create();
 *   SpeciesReference_setSpecies(r, species);
 *   r->stoichiometry = stoichiometry;
 *   ...;
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_createWith (   const char *species
                              , double     stoichiometry
                              , int        denominator   )
{
  const char *s = species ? species : "";
  return new(std::nothrow) SpeciesReference(s, stoichiometry, denominator);
}


/**
 * Frees the given SpeciesReference.
 */
LIBSBML_EXTERN
void
SpeciesReference_free (SpeciesReference_t *sr)
{
  delete static_cast<SpeciesReference*>(sr);
}


/**
 * Initializes the fields of this SpeciesReference to their defaults:
 *
 *   - stoichiometry = 1
 *   - denominator   = 1
 */
LIBSBML_EXTERN
void
SpeciesReference_initDefaults (SpeciesReference_t *sr)
{
  static_cast<SpeciesReference*>(sr)->initDefaults();
}


/**
 * @return the species of this SpeciesReference.
 */
LIBSBML_EXTERN
const char *
SpeciesReference_getSpecies (const SpeciesReference_t *sr)
{
  const SpeciesReference* x = static_cast<const SpeciesReference*>(sr);


  return x->isSetSpecies() ? x->getSpecies().c_str() : NULL;
}


/**
 * @return the stoichiometry of this SpeciesReference.
 */
LIBSBML_EXTERN
double
SpeciesReference_getStoichiometry (const SpeciesReference_t *sr)
{
  return static_cast<const SpeciesReference*>(sr)->getStoichiometry();
}


/**
 * @return the stoichiometryMath of this SpeciesReference.
 */
LIBSBML_EXTERN
const ASTNode_t *
SpeciesReference_getStoichiometryMath (const SpeciesReference_t *sr)
{
  return static_cast<const SpeciesReference*>(sr)->getStoichiometryMath();
}


/**
 * @return the denominator of this SpeciesReference.
 */
LIBSBML_EXTERN
int
SpeciesReference_getDenominator (const SpeciesReference_t *sr)
{
  return static_cast<const SpeciesReference*>(sr)->getDenominator();
}


/**
 * @return 1 if the species of this SpeciesReference has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetSpecies (const SpeciesReference_t *sr)
{
  return (int) static_cast<const SpeciesReference*>(sr)->isSetSpecies();
}


/**
 * @return 1 if the stoichiometryMath of this SpeciesReference has been
 * set, 0 otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetStoichiometryMath (const SpeciesReference_t *sr)
{
  return (int)
  static_cast<const SpeciesReference*>(sr)->isSetStoichiometryMath();
}


/**
 * Sets the species of this SpeciesReference to a copy of sname.
 */
LIBSBML_EXTERN
void
SpeciesReference_setSpecies (SpeciesReference_t *sr, const char *sname)
{
  static_cast<SpeciesReference*>(sr)->setSpecies(sname ? sname : "");
}


/**
 * Sets the stoichiometry of this SpeciesReference to value.
 */
LIBSBML_EXTERN
void
SpeciesReference_setStoichiometry (SpeciesReference_t *sr, double value)
{
  static_cast<SpeciesReference*>(sr)->setStoichiometry(value);
}


/**
 * Sets the stoichiometryMath of this SpeciesReference to the given
 * ASTNode.
 *
 * The node <b>is not copied</b> and this SpeciesReference <b>takes
 * ownership</b> of it; i.e. subsequent calls to this function or a call to
 * SpeciesReference_free() will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
SpeciesReference_setStoichiometryMath (SpeciesReference_t *sr, ASTNode_t *math)
{
  ASTNode* x = static_cast<ASTNode*>(math);
  static_cast<SpeciesReference*>(sr)->setStoichiometryMath(x);
}


/**
 * Sets the denominator of this SpeciesReference to value.
 */
LIBSBML_EXTERN
void
SpeciesReference_setDenominator (SpeciesReference_t *sr, int value)
{
  static_cast<SpeciesReference*>(sr)->setDenominator(value);
}
