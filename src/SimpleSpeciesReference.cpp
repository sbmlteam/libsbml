/**
 * \file    SimpleSpeciesReference.cpp
 * \brief   SBML SimpleSpeciesReference
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


#include "util.h"
#include "SBMLVisitor.h"

#include "SimpleSpeciesReference.h"


/**
 * Creates a new SimpleSpeciesReference, optionally with its species
 * attribute set.
 */
LIBSBML_EXTERN
SimpleSpeciesReference::SimpleSpeciesReference (const std::string& s) :
  SBase(), species(s)
{
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Reaction's next
 * SimpleSpeciesReference (if available).
 */
LIBSBML_EXTERN
bool
SimpleSpeciesReference::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return the species for this SimpleSpeciesReference.
 */
LIBSBML_EXTERN
const std::string&
SimpleSpeciesReference::getSpecies () const
{
  return species;
}


/**
 * @return true if the species for this SimpleSpeciesReference has been
 * set, false otherwise.
 */
LIBSBML_EXTERN
bool
SimpleSpeciesReference::isSetSpecies () const
{
  return ! species.empty();
}


/**
 * Sets the species of this SimpleSpeciesReference to a copy of sid.
 */
LIBSBML_EXTERN
void
SimpleSpeciesReference::setSpecies (const std::string& sid)
{
  species = sid;
}




/**
 * @return the species for this SimpleSpeciesReference.
 */
LIBSBML_EXTERN
const char *
SimpleSpeciesReference_getSpecies (const SimpleSpeciesReference_t *ssr)
{
  const SimpleSpeciesReference* x =
    static_cast<const SimpleSpeciesReference*>(ssr);


  return x->isSetSpecies() ? x->getSpecies().c_str() : NULL;
}


/**
 * @return 1 if the species for this SimpleSpeciesReference has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
SimpleSpeciesReference_isSetSpecies (const SimpleSpeciesReference_t *ssr)
{
  return (int) static_cast<const SimpleSpeciesReference*>(ssr)->isSetSpecies();
}


/**
 * Sets the species of this SimpleSpeciesReference to a copy of sid.
 */
LIBSBML_EXTERN
void
SimpleSpeciesReference_setSpecies ( SimpleSpeciesReference_t *ssr,
                                    const char *sid )
{
  static_cast<SimpleSpeciesReference*>(ssr)->setSpecies(sid ? sid : "");
}


/**
 * The SimpleSpeciesReferenceCmp function compares the string sid to
 * ssr->species.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than
 * ssr->species.  Returns -1 if either sid or ssr->species is NULL.
 */
LIBSBML_EXTERN
int
SimpleSpeciesReferenceCmp(const char *sid, const SimpleSpeciesReference_t *ssr)
{
  int result = -1;


  if (sid != NULL && SimpleSpeciesReference_isSetSpecies(ssr))
  {
    result = strcmp(sid, SimpleSpeciesReference_getSpecies(ssr));
  }

  return result;
}
