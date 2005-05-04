/**
 * Filename    : BoundingBox.cpp
 * Description : SBML Layout BoundingBox source
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
#include "BoundingBox.h"


/**
 * Default Constructor set position and dimensions to (0.0,0.0,0.0) and the
 * id to an empty string.
 */ 
LIBSBML_EXTERN
BoundingBox::BoundingBox() : SBase()
{
  init(SBML_LAYOUT_BOUNDINGBOX);
}


/**
 * Constructor set position and dimensions to (0.0,0.0,0.0) and the id to a
 * copy of the given string.
 */ 
LIBSBML_EXTERN
BoundingBox::BoundingBox (const std::string id) : SBase(), id(id)
{
  init(SBML_LAYOUT_BOUNDINGBOX);
}


/**
 * Constructor which sets the id, the coordinates and the dimensions to the
 * given 2D values.
 */ 
LIBSBML_EXTERN
BoundingBox::BoundingBox (const std::string id,
                          double x, double y,
                          double width, double height)
  : SBase     ()
  , id        ( id )
  , position  ( Point(x, y, 0.0)               )
  , dimensions( Dimensions(width, height, 0.0) )
{
  init(SBML_LAYOUT_BOUNDINGBOX);
}


/**
 * Constructor which sets the id, the coordinates and the dimensions to the
 * given 3D values.
 */ 
LIBSBML_EXTERN
BoundingBox::BoundingBox (const std::string id,
                          double x, double y, double z,
                          double width, double height, double depth)
  : SBase()
  , id        ( id )
  , position  ( Point(x, y, z)                   )
  , dimensions( Dimensions(width, height, depth) )
{
  init(SBML_LAYOUT_BOUNDINGBOX);
}

        
/**
 * Constructor which sets the id, the coordinates and the dimensions to the
 * given 3D values.
 */ 
LIBSBML_EXTERN
BoundingBox::BoundingBox (const std::string id,
                          const Point&      p,
                          const Dimensions& d)
  : SBase     ()
  , id        ( id )
  , position  ( p  )
  , dimensions( d  )
{
  init(SBML_LAYOUT_BOUNDINGBOX);
}


/**
 * Destructor which does nothing.
 */ 
LIBSBML_EXTERN
BoundingBox::~BoundingBox ()
{
}


/**
 * Does nothing since no defaults are defined for a BundingBox.
 */ 
LIBSBML_EXTERN
void BoundingBox::initDefaults ()
{
}


/**
 * Sets the id to a copy of the given string.
 */  
LIBSBML_EXTERN
void BoundingBox::setId (const std::string& id)
{
  this->id = id;
}


/**
 * Returns the id of the BOundingBox.
 */ 
LIBSBML_EXTERN
const std::string
BoundingBox::getId () const
{
  return this->id;
}


/**
 * Returns true if the id is not the empty string.
 */ 
LIBSBML_EXTERN
bool
BoundingBox::isSetId () const
{
  return ! this->id.empty();
}


/**
 * Returns the position of the BoundingBox as const referece to a Point
 * object.
 */ 
LIBSBML_EXTERN
const Point&
BoundingBox::getPosition () const
{
  return this->position;
}


/**
 * Returns the dimensions of the BoundingBox as const referece to a
 * Dimensions object.
 */ 
LIBSBML_EXTERN
const Dimensions&
BoundingBox::getDimensions () const
{
  return this->dimensions;
}


/**
 * Returns the position of the BoundingBox as referece to a Point object.
 */ 
LIBSBML_EXTERN
Point&
BoundingBox::getPosition ()
{
  return this->position;
}


/**
 * Returns the dimensions of the BoundingBox as referece to a Dimensions
 * object.
 */ 
LIBSBML_EXTERN
Dimensions&
BoundingBox::getDimensions ()
{
  return this->dimensions;
}


/**
 * Sets the position to a copy of the Point object given.
 */ 
LIBSBML_EXTERN
void BoundingBox::setPosition (const Point& p)
{
  this->position = Point(p);
}


/**
 * Sets the dimensions to a copy of the Dimensions object given.
 */ 
LIBSBML_EXTERN
void
BoundingBox::setDimensions (const Dimensions& d)
{
  this->dimensions = Dimensions(d);
}


/**
 * Function that creates a BoundingBox_t object with position set to
 * (0.0,0.0,0.0) and dimensions set to (0.0,0.0,0.0). The id is set to the
 * empty string.
 */
LIBSBML_EXTERN
BoundingBox_t *
BoundingBox_create (void)
{
  return new(std::nothrow) BoundingBox;
}


/**
 * Function that creates a BoundingBox_t object with position set to
 * (0.0,0.0,0.0) and dimensions set to (0.0,0.0,0.0).  The id is set to the
 * given string.
 */
LIBSBML_EXTERN
BoundingBox_t *
BoundingBox_createWith (const char *id)
{
  return new(std::nothrow) BoundingBox(id ? id : "");
}


/**
 * Function that creates a BoundingBox_t object with the coordinates and
 * sizes given as arguments. The id is set to the empty string.
 */ 
LIBSBML_EXTERN
BoundingBox_t *
BoundingBox_createWithCoordinates (const char *id,
                                   double x, double y, double z,
                                   double width, double height, double depth)
{
  return new(std::nothrow) BoundingBox(id ? id : "" , x, y, z, width, height, depth);
}


/**
 * Frees all memory taken by the given BoundingBox_t object.
 */ 
LIBSBML_EXTERN
void
BoundingBox_free (BoundingBox_t *bb)
{
  delete bb;
}


/**
 * Does nothing since no defaults are defined for BoundingBox.
 */
LIBSBML_EXTERN
void
BoundingBox_initDefaults (BoundingBox_t *bb)
{
  bb->initDefaults();
}


/**
 * Sets the id of the BoundingBox_t object to the id given as second
 * argument.
 */ 
LIBSBML_EXTERN
void
BoundingBox_setId (BoundingBox_t *bb, const char* id)
{
    static_cast<BoundingBox*>(bb)->setId( id ? id : "" );
}


/**
 * Returns the id
 */ 
LIBSBML_EXTERN
const char *
BoundingBox_getId (const BoundingBox_t *bb)
{
    return bb->isSetId() ? bb->getId().c_str() : NULL;
}


/**
 * Returns true if the id is set, that is if the id is not the empty
 * string.
 */ 
LIBSBML_EXTERN
int
BoundingBox_isSetId (const BoundingBox_t *bb)
{
  return static_cast<int>( bb->isSetId() );
}


/**
 * Returns the position as a Point_t object.
 */ 
LIBSBML_EXTERN
Point_t *
BoundingBox_getPosition (BoundingBox_t *bb)
{
  return & bb->getPosition();
}


/**
 * Returns the dimensions as a Dimensions_t object.
 */ 
LIBSBML_EXTERN
Dimensions_t *
BoundingBox_getDimensions (BoundingBox_t *bb)
{
  return & bb->getDimensions();
}


/**
 * Sets the position to a copy of the Point_t object given as argument.
 */
LIBSBML_EXTERN
void
BoundingBox_setPosition (BoundingBox_t *bb, const Point_t *p)
{
  bb->setPosition(p ? *p : Point());
}


/**
 * Sets the dimensions to a copy of the Dimensions_t object given.
 */ 
LIBSBML_EXTERN
void
BoundingBox_setDimensions (BoundingBox_t *bb, const Dimensions_t *d)
{
  bb->setDimensions(d ? *d : Dimensions());
}
