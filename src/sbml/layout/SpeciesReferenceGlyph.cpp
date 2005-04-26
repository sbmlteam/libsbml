/**
 * Filename    : SpeciesReferenceGlyph.cpp
 * Description : SBML Layout SpeciesReferenceGlyph source
 * Organization: European Media Laboratories Research gGmbH
 * Created     : 2004-07-15
 *
 * Copyright 2004 European Media Laboratories Research gGmbH
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
 * European Media Laboratories Research gGmbH have no obligations to
 * provide maintenance, support, updates, enhancements or modifications.
 * In no event shall the European Media Laboratories Research gGmbH be
 * liable to any party for direct, indirect, special, incidental or
 * consequential damages, including lost profits, arising out of the use of
 * this software and its documentation, even if the European Media
 * Laboratories Research gGmbH have been advised of the possibility of such
 * damage.  See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ralph Gauges
 *     Bioinformatics Group
 *     European Media Laboratories Research gGmbH
 *     Schloss-Wolfsbrunnenweg 31c
 *     69118 Heidelberg
 *     Germany
 *
 *     http://www.eml-research.de/english/Research/BCB/
 *     mailto:ralph.gauges@eml-r.villa-bosch.de
 *
 * Contributor(s):
 */


#include "common/common.h"
#include "SpeciesReferenceGlyph.h"


/**
 * Creates a new SpeciesReferenceGlyph.  The id if the associated species
 * reference and the id of the associated species glyph are set to the
 * empty string.  The role is set to SPECIES_ROLE_UNDEFINED.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph::SpeciesReferenceGlyph () :
    role  ( SPECIES_ROLE_UNDEFINED )
  , curve ( new Curve()            )
{
  init(SBML_LAYOUT_SPECIESREFERENCEGLYPH);
}


/**
 * Creates a new SpeciesReferenceGlyph.  The id is given as the first
 * argument, the id of the associated species reference is given as the
 * second argument.  The third argument is the id of the associated species
 * glpyh and the fourth argument is the role.
 */ 
LIBSBML_EXTERN
SpeciesReferenceGlyph::SpeciesReferenceGlyph
(
  const std::string& sid,
  const std::string& speciesGlyphId,
  const std::string& speciesReferenceId,
  SpeciesReferenceRole_t role
) :
    GraphicalObject ( sid                )
  , speciesReference( speciesReferenceId )
  , speciesGlyph    ( speciesGlyphId     )
  , role            ( role               )
  , curve           ( new Curve()        )
{
  init(SBML_LAYOUT_SPECIESREFERENCEGLYPH);
}


/**
 * Destructor.
 */ 
LIBSBML_EXTERN
SpeciesReferenceGlyph::~SpeciesReferenceGlyph ()
{
  delete this->curve;
}


/**
 * Returns the id of the associated SpeciesGlyph.
 */ 
LIBSBML_EXTERN
const std::string&
SpeciesReferenceGlyph::getSpeciesGlyphId () const
{
  return this->speciesGlyph;
}


/**
 * Sets the id of the associated species glyph.
 */ 
LIBSBML_EXTERN
void
SpeciesReferenceGlyph::setSpeciesGlyphId (const std::string& speciesGlyphId)
{
  this->speciesGlyph = speciesGlyphId;
}


/**
 * Returns the id of the associated species reference.
 */ 
LIBSBML_EXTERN
const std::string&
SpeciesReferenceGlyph::getSpeciesReferenceId () const
{
  return this->speciesReference;
}


/**
 * Sets the id of the associated species reference.
 */ 
LIBSBML_EXTERN
void
SpeciesReferenceGlyph::setSpeciesReferenceId (const std::string& id)
{
  this->speciesReference=id;
}


/**
 * Returns the role.
 */ 
LIBSBML_EXTERN
SpeciesReferenceRole_t
SpeciesReferenceGlyph::getRole() const
{
  return this->role;
}


/**
 * Sets the role based on a string.
 * The String can be one of
 * SUBSTRATE
 * PRODUCT
 * SIDESUBSTRATE
 * SIDEPRODUCT
 * MODIFIER
 * ACTIVATOR
 * INHIBITOR    
 */ 
LIBSBML_EXTERN
void
SpeciesReferenceGlyph::setRole (const std::string& role)
{
       if ( role == "SUBSTRATE"     ) this->role = SPECIES_ROLE_SUBSTRATE;
  else if ( role == "PRODUCT"       ) this->role = SPECIES_ROLE_PRODUCT;
  else if ( role == "SIDESUBSTRATE" ) this->role = SPECIES_ROLE_SIDESUBSTRATE;
  else if ( role == "SIDEPRODUCT"   ) this->role = SPECIES_ROLE_SIDEPRODUCT;
  else if ( role == "MODIFIER"      ) this->role = SPECIES_ROLE_MODIFIER;
  else if ( role == "ACTIVATOR"     ) this->role = SPECIES_ROLE_ACTIVATOR;
  else if ( role == "INHIBITOR"     ) this->role = SPECIES_ROLE_INHIBITOR;
  else                                this->role = SPECIES_ROLE_UNDEFINED;
}


/**
 * Sets the role.
 */ 
LIBSBML_EXTERN
void
SpeciesReferenceGlyph::setRole (SpeciesReferenceRole_t role)
{
  this->role=role;
}


/**
 * Returns the curve object for the species reference glyph
 */ 
LIBSBML_EXTERN
Curve* SpeciesReferenceGlyph::getCurve() const
{
  return this->curve;
}


/**
 * Sets the curve object for the species reference glyph.
 */ 
LIBSBML_EXTERN
void
SpeciesReferenceGlyph::setCurve (Curve* curve)
{
  if(!curve) return;

  delete this->curve;
  this->curve = curve;
}


/**
 * Returns true if the curve consists of one or more segments.
 */ 
LIBSBML_EXTERN
bool
SpeciesReferenceGlyph::isSetCurve () const
{
  return this->curve->getNumCurveSegments() > 0;
}


/**
 * Returns true if the id of the associated species glpyh is not the empty
 * string.
 */ 
LIBSBML_EXTERN
bool
SpeciesReferenceGlyph::isSetSpeciesGlyphId () const
{
  return ! this->speciesGlyph.empty();
}


/**
 * Returns true if the id of the associated species reference is not the
 * empty string.
 */ 
LIBSBML_EXTERN
bool
SpeciesReferenceGlyph::isSetSpeciesReferenceId () const
{
  return ! this->speciesReference.empty();
}


/**
 * Returns true of role is different from SPECIES_ROLE_UNDEFINED.
 */ 
LIBSBML_EXTERN
bool SpeciesReferenceGlyph::isSetRole () const
{
  return ! (this->role == SPECIES_ROLE_UNDEFINED);
}


/**
 * Calls initDefaults on GraphicalObject and sets role to
 * SPECIES_ROLE_UNDEFINED.
 */ 
LIBSBML_EXTERN
void
SpeciesReferenceGlyph::initDefaults ()
{
    GraphicalObject::initDefaults();
    this->role = SPECIES_ROLE_UNDEFINED;
}


/**
 * Creates a new LineSegment object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
LIBSBML_EXTERN
LineSegment&
SpeciesReferenceGlyph::createLineSegment ()
{
  return this->curve->createLineSegment();
}


/**
 * Creates a new CubicBezier object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
LIBSBML_EXTERN
CubicBezier&
SpeciesReferenceGlyph::createCubicBezier ()
{
  return this->curve->createCubicBezier();
}


/**
 * Creates a new SpeciesReferenceGlyph object and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t *
SpeciesReferenceGlyph_create()
{
  return new(std::nothrow) SpeciesReferenceGlyph;
}


/**
 * Creates a new SpeciesReferenceGlyph from a template.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t *
SpeciesReferenceGlyph_createFrom (const SpeciesReferenceGlyph_t *temp)
{
  return new(std::nothrow) SpeciesReferenceGlyph(*temp);
}


/**
 * Creates a new SpeciesReferenceGlyph object with the given id and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t *
SpeciesReferenceGlyph_createWith (const char *sid,
                                  const char *speciesReferenceId,
                                  const char *speciesGlyphId,
                                  SpeciesReferenceRole_t role)
{
  return new(std::nothrow)
    SpeciesReferenceGlyph(sid, speciesReferenceId, speciesGlyphId, role);
}


/**
 * Frees the memory for the SpeciesReferenceGlyph
 */
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_free(SpeciesReferenceGlyph_t *srg)
{
  delete srg;
}


/**
 * Sets the reference species for the species glyph.
 */
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_setSpeciesReferenceId (SpeciesReferenceGlyph_t *srg,
                                             const char *id)
{
  srg->setSpeciesReferenceId(id);
}


/**
 * Gets the reference species id for the given species glyph.
 */
LIBSBML_EXTERN
const char *
SpeciesReferenceGlyph_getSpeciesReferenceId (const SpeciesReferenceGlyph_t *srg)
{
  return srg->getSpeciesReferenceId().c_str();
}


/**
 * Returns 0 if the reference species reference has not been set for this
 * glyph and 1 otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReferenceGlyph_isSetSpeciesReferenceId
  (const SpeciesReferenceGlyph_t *srg)
{
  return static_cast<int>( srg->isSetSpeciesReferenceId() );
}


/**
 * Sets the species glyph reference for the species glyph.
 */
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_setSpeciesGlyphId (SpeciesReferenceGlyph_t *srg,
                                         const char *id)
{
    static_cast<SpeciesReferenceGlyph*>(srg)->setSpeciesGlyphId(id);
}


/**
 * Gets the reference speciess id for the given species glyph.
 */
LIBSBML_EXTERN
const char *
SpeciesReferenceGlyph_getSpeciesGlyphId (const SpeciesReferenceGlyph_t *srg)
{
  return srg->getSpeciesGlyphId().c_str();
}


/**
 * Returns 0 if  the reference species reference has not  been set for this
 * glyph and 1 otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReferenceGlyph_isSetSpeciesGlyphId (const SpeciesReferenceGlyph_t *srg)
{
  return static_cast<int>( srg->isSetSpeciesGlyphId() );
}


/**
 * Sets the curve for the species reference glyph.
 */
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_setCurve(SpeciesReferenceGlyph_t *srg, Curve_t *c)
{
  srg->setCurve(c);
}


/**
 * Gets the Curve for the given species reference glyph.
 */
LIBSBML_EXTERN
Curve_t *
SpeciesReferenceGlyph_getCurve (SpeciesReferenceGlyph_t *srg)
{
  return srg->getCurve();
}


/**
 * Returns true if the Curve has one or more LineSegment.
 */
LIBSBML_EXTERN
int
SpeciesReferenceGlyph_isSetCurve (SpeciesReferenceGlyph_t *srg)
{
  static_cast<int>( srg->isSetCurve() );
}


/**
 * Sets the role of the species reference glyph based on the string.  The
 * string can be one of UNDEFINED, SUBSTRATE, PRODUCT, SIDESUBSTRATE,
 * SIDEPRODUCT, MODIFIER, INHIBITOR or ACTIVATOR.  If it is none of those,
 * the role is set to SPECIES_ROLE_UNDEFINED.
 */
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_setRole (SpeciesReferenceGlyph_t *srg,
                               SpeciesReferenceRole_t   role)
{
  srg->setRole(role);
}


/**
 * Returns the role of the species reference.
 */ 
LIBSBML_EXTERN
SpeciesReferenceRole_t
SpeciesReferenceGlyph_getRole (const SpeciesReferenceGlyph_t *srg)
{
  return srg->getRole();
}


/**
 * Returns true if the role is not SPECIES_ROLE_UNDEFINED.
 */ 
LIBSBML_EXTERN
int
SpeciesReferenceGlyph_isSetRole (const SpeciesReferenceGlyph_t *srg)
{
  return static_cast<int>( srg->isSetRole() );
}


/**
 * Calls initDefaults on GraphicalObject and sets role to
 * SPECIES_ROLE_UNDEFINED.
 */ 
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_initDefaults (SpeciesReferenceGlyph_t *srg)
{
  srg->initDefaults();
}


/**
 * Creates a new LineSegment object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
LIBSBML_EXTERN
LineSegment_t *
SpeciesReferenceGlyph_createLineSegment (SpeciesReferenceGlyph_t *srg)
{
  return & srg->getCurve()->createLineSegment();
}  


/**
 * Creates a new CubicBezier object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
LIBSBML_EXTERN
CubicBezier_t *
SpeciesReferenceGlyph_createCubicBezier (SpeciesReferenceGlyph_t *srg)
{
  return & srg->getCurve()->createCubicBezier();
}
