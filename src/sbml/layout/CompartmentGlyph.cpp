/**
 * Filename    : CompartmentGlyph.cpp
 * Description : SBML Layout CompartmentGlyph source
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
#include "CompartmentGlyph.h"


/**
 * Default Constructor which creates a new CompartmentGlyph.  Id and
 * associated compartment id are unset.
 */
LIBSBML_EXTERN
CompartmentGlyph::CompartmentGlyph ()
{
	init(SBML_LAYOUT_COMPARTMENTGLYPH);
}


/**
 * Constructor which creates a new CompartmentGlyph.  Id and associated
 * compartment id are set to copies of the values given as arguments.
 */
LIBSBML_EXTERN
CompartmentGlyph::CompartmentGlyph (const std::string& id,
                                    const std::string& compId) : 
  GraphicalObject(id), compartment(compId)
{
  init(SBML_LAYOUT_COMPARTMENTGLYPH);
}


/**
 * Destructor.
 */        
LIBSBML_EXTERN
CompartmentGlyph::~CompartmentGlyph ()
{
} 


/**
 * Returns the id of the associated compartment.
 */        
LIBSBML_EXTERN
const std::string&
CompartmentGlyph::getCompartmentId () const
{
  return this->compartment;
}


/**
 * Sets the id of the associated compartment.
 */ 
LIBSBML_EXTERN
void
CompartmentGlyph::setCompartmentId (const std::string& id)
{
  this->compartment = id;
}


/**
 * Returns true if the id of the associated compartment is not the empty
 * string.
 */  
LIBSBML_EXTERN
bool 
CompartmentGlyph::isSetCompartmentId () const
{
  return ! this->compartment.empty();
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
LIBSBML_EXTERN
void CompartmentGlyph::initDefaults ()
{
  GraphicalObject::initDefaults();
}




/**
 * Creates a new CompartmentGlyph and returns the pointer to it.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_create(void)
{
  return new(std::nothrow) CompartmentGlyph;
}


/**
 * Creates a new CompartmentGlyph from a template.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createFrom (const CompartmentGlyph_t *temp)
{
  return new(std::nothrow) CompartmentGlyph(*temp);
}


/**
 * Creates a new CompartmentGlyph with the given id
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createWith (const char *id)
{
  return new(std::nothrow) CompartmentGlyph(id ? id : "", "");
}


/**
 * Creates a new CompartmentGlyph with the given id
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createWithCompartmentId (const char *sid, const char *compId)
{
  return new(std::nothrow) CompartmentGlyph(sid ? sid : "", compId ? compId : "");
}


/**
 * Frees the memory taken by the given compartment glyph.
 */
LIBSBML_EXTERN
void
CompartmentGlyph_free (CompartmentGlyph_t *cg)
{
  delete cg;
}


/**
 * Sets the reference compartment for the compartment glyph.
 */
LIBSBML_EXTERN
void
CompartmentGlyph_setCompartmentId (CompartmentGlyph_t *cg, const char* id)
{
    static_cast<CompartmentGlyph*>(cg)->setCompartmentId( id ? id : "" );
}


/**
 * Gets the reference compartments id for the given compartment glyph.
 */
LIBSBML_EXTERN
const char *
CompartmentGlyph_getCompartmentId (const CompartmentGlyph_t *cg)
{
    return cg->isSetCompartmentId() ? cg->getCompartmentId().c_str() : NULL;
}


/**
 * Returns 0 if the reference compartment has not been set for this glyph
 * and 1 otherwise.
 */
LIBSBML_EXTERN
int
CompartmentGlyph_isSetCompartmentId (const CompartmentGlyph_t *cg)
{
  return static_cast<int>( cg->isSetCompartmentId() );
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
LIBSBML_EXTERN
void
CompartmentGlyph_initDefaults (CompartmentGlyph_t *cg)
{
  cg->initDefaults();
}
