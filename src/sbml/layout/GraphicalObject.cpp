/**
 * Filename    : GraphicalObject.cpp
 * Description : SBML Layout GraphicalObject source
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
#include "GraphicalObject.h"


/**
 * Creates a new GraphicalObject.
 */
LIBSBML_EXTERN
GraphicalObject::GraphicalObject() : SBase ()
{
  init(SBML_LAYOUT_GRAPHICALOBJECT);
}


/**
 * Creates a new GraphicalObject with the given id.
 */
LIBSBML_EXTERN
GraphicalObject::GraphicalObject (const std::string& id) : 
    SBase()
  , id   (id)
{
  init(SBML_LAYOUT_GRAPHICALOBJECT);
}


/**
 * Creates a new GraphicalObject with the given id and 2D coordinates for
 * the bounding box.
 */
LIBSBML_EXTERN
GraphicalObject::GraphicalObject (const std::string& id,
                                  double x, double y, double w, double h) :
    SBase      ()
  , id         ( id )
  , boundingBox( BoundingBox("", x, y, 0.0, w, h, 0.0) )
{
  init(SBML_LAYOUT_GRAPHICALOBJECT);
}


/**
 * Creates a new GraphicalObject with the given id and 3D coordinates for
 * the bounding box.
 */
LIBSBML_EXTERN
GraphicalObject::GraphicalObject (const std::string& id,
                                  double x, double y, double z,
                                  double w, double h, double d) :
    SBase      ()
  , id         ( id )
  , boundingBox( BoundingBox("", x, y, z, w, h, d) )
{
  init(SBML_LAYOUT_GRAPHICALOBJECT);
}


/**
 * Creates a new GraphicalObject with the given id and 3D coordinates for
 * the bounding box.
 */
LIBSBML_EXTERN
GraphicalObject::GraphicalObject (const std::string& id,
                                  const Point&       p,
                                  const Dimensions&  d) : 
    SBase      ()
  , id         ( id )
  , boundingBox( BoundingBox("", p, d) )
{
  init(SBML_LAYOUT_GRAPHICALOBJECT);
}


/**
 * Creates a new GraphicalObject with the given id and 3D coordinates for
 * the bounding box.
 */
LIBSBML_EXTERN
GraphicalObject::GraphicalObject (const std::string& id, const BoundingBox& bb)
  : SBase      ()
  , id         ( id )
  , boundingBox( bb )
{
  init(SBML_LAYOUT_GRAPHICALOBJECT);
}


/**
 * Destructor.
 */ 
LIBSBML_EXTERN
GraphicalObject::~GraphicalObject ()
{
}


/**
 * Gets the id for the GraphicalObject.
 */
LIBSBML_EXTERN
const std::string&
GraphicalObject::getId () const
{
  return this->id;
}


/**
 * Sets the id for the GraphicalObject.
 */
LIBSBML_EXTERN
void
GraphicalObject::setId (const std::string& id)
{
  this->id = id;
}


/**
 * Sets the boundingbox for the GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject::setBoundingBox (const BoundingBox& bb)
{
  this->boundingBox = bb;
}


/**
 * Returns the bounding box for the GraphicalObject.
 */ 
LIBSBML_EXTERN
const BoundingBox&
GraphicalObject::getBoundingBox () const
{
  return this->boundingBox;
} 


/**
 * Returns the bounding box for the GraphicalObject.
 */ 
LIBSBML_EXTERN
BoundingBox&
GraphicalObject::getBoundingBox ()
{
  return this->boundingBox;
}


/**
 * Does nothing. No defaults are defined for GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject::initDefaults ()
{
}


/**
 * Creates a new GraphicalObject.
 */
LIBSBML_EXTERN
GraphicalObject_t *
GraphicalObject_create ()
{
  return new(std::nothrow) GraphicalObject;
}


/**
 * Creates a GraphicalObject from a template.
 */
LIBSBML_EXTERN
GraphicalObject_t *
GraphicalObject_createFrom (const GraphicalObject_t *temp)
{
  return new(std::nothrow) GraphicalObject(*temp);
}

/**
 * Frees all memory taken up by the GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject_free (GraphicalObject_t *go)
{
  delete go;
}


/**
 * Sets the id for the GraphicalObject.
 */
LIBSBML_EXTERN
void
GraphicalObject_setId (GraphicalObject_t *go, const char *id)
{
  go->setId(id);
}


/**
 * Gets the id for the given GraphicalObject.
 */
LIBSBML_EXTERN
const char *
GraphicalObject_getId (const GraphicalObject_t *go)
{
  return go->getId().c_str();
}


/**
 * Sets the boundingbox for the GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject_setBoundingBox (GraphicalObject_t *go, const BoundingBox_t *bb)
{
  go->setBoundingBox(*bb);
}


/**
 * Returns the bounding box for the GraphicalObject.
 */ 
LIBSBML_EXTERN
BoundingBox_t *
GraphicalObject_getBoundingBox (GraphicalObject_t *go)
{
  return & go->getBoundingBox();
}


/**
 * Does nothing. No defaults are defined for GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject_initDefaults (GraphicalObject_t *go)
{
  go->initDefaults();
}
