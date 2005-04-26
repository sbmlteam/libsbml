/**
 * Filename    : SpeciesGlyph.cpp
 * Description : SBML Layout SpeciesGlyph source
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
#include "SpeciesGlyph.h"


/**
 * Creates a new SpeciesGlyph with the id of the associated species set to
 * the empty string.
 */        
LIBSBML_EXTERN
SpeciesGlyph::SpeciesGlyph ()
{
  init(SBML_LAYOUT_SPECIESGLYPH);
}


/**
 * Creates a new SpeciesGlyph with the given id and the id of the
 * associated species object set to the second argument.
 */ 
LIBSBML_EXTERN
SpeciesGlyph::SpeciesGlyph (const std::string& sid,
                            const std::string& speciesId) : 
    GraphicalObject( id )
  , species        ( speciesId )
{
  init(SBML_LAYOUT_SPECIESGLYPH);
}


/**
 * Destructor.
 */ 
LIBSBML_EXTERN
SpeciesGlyph::~SpeciesGlyph ()
{
} 


/**
 * Returns the id of the associated species object.
 */ 
LIBSBML_EXTERN
const std::string&
SpeciesGlyph::getSpeciesId () const
{
  return this->species;
}


/**
 * Sets the id of the associated species object.
 */ 
LIBSBML_EXTERN
void
SpeciesGlyph::setSpeciesId (const std::string& id)
{
  this->species=id;
} 


/**
 * Returns true if the id of the associated species object is not the empty
 * string.
 */ 
LIBSBML_EXTERN
bool
SpeciesGlyph::isSetSpeciesId () const
{
  return ! this->species.empty();
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
LIBSBML_EXTERN
void SpeciesGlyph::initDefaults ()
{
  GraphicalObject::initDefaults();
}


/**
 * Creates a new SpeciesGlyph and returns the pointer to it.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
SpeciesGlyph_create ()
{
  return new(std::nothrow) SpeciesGlyph;
}


/**
 * Create a new SpeciesGlyph object from a template.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
SpeciesGlyph_createFrom (const SpeciesGlyph_t *temp)
{
  return new(std::nothrow) SpeciesGlyph(*temp);
}


/**
 * Creates a new SpeciesGlyph with the given id
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
SpeciesGlyph_createWith (const char *id)
{
  return new(std::nothrow) SpeciesGlyph(id, "");
}


/**
 * Creates a new SpeciesGlyph referencing with the give species id.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
SpeciesGlyph_createWithSpeciesId (const char *sid, const char *speciesId)
{
  return new(std::nothrow) SpeciesGlyph(sid, speciesId);
}


/**
 * Frees the memory taken by the given compartment glyph.
 */
LIBSBML_EXTERN
void
SpeciesGlyph_free (SpeciesGlyph_t *sg)
{
  delete sg;
}


/**
 * Sets the associated species id. 
 */
LIBSBML_EXTERN
void
SpeciesGlyph_setSpeciesId (SpeciesGlyph_t *sg, const char *id)
{
  sg->setSpeciesId(id);
}


/**
 * Gets the the id of the associated species.
 */
LIBSBML_EXTERN
const char *
SpeciesGlyph_getSpeciesId (const SpeciesGlyph_t *sg)
{
  return sg->getSpeciesId().c_str();
}



/**
 * Returns 0 if the  id of the associated species is the empty string.
 * otherwise.
 */
LIBSBML_EXTERN
int
SpeciesGlyph_isSetSpeciesId (const SpeciesGlyph_t *sg)
{
  return static_cast<int>( sg->isSetSpeciesId() );
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
LIBSBML_EXTERN
void
SpeciesGlyph_initDefaults (SpeciesGlyph_t *sg)
{
  sg->initDefaults();
}
