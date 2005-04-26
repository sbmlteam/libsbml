/**
 * Filename    : LineSegment.cpp
 * Description : SBML Layout LineSegment source
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
#include "LineSegment.h"


/**
 * Creates a line segment with both points set to (0.0,0.0,0.0)
 */ 
LIBSBML_EXTERN
LineSegment::LineSegment () :
    SBase     ()
  , startPoint( Point(0.0, 0.0, 0.0) )
  , endPoint  ( Point(0.0, 0.0, 0.0) )
{
  init(SBML_LAYOUT_LINESEGMENT);
}


/**
 * Creates a new line segment with the given 2D coordinates.
 */ 
LIBSBML_EXTERN
LineSegment::LineSegment (double x1, double y1, double x2, double y2) :
    SBase     ()
  , startPoint( Point(x1, y1, 0.0) )
  , endPoint  ( Point(x2, y2, 0.0) )
{
  init(SBML_LAYOUT_LINESEGMENT);
}


/**
 * Creates a new line segment with the given 3D coordinates.
 */ 
LIBSBML_EXTERN
LineSegment::LineSegment (double x1, double y1, double z1,
                          double x2, double y2, double z2) :
    SBase()
  , startPoint( Point(x1, y1, z1) )
  , endPoint  ( Point(x2, y2, z2))
{
  init(SBML_LAYOUT_LINESEGMENT);
}


/**
 * Creates a new line segment with the two given points.
 */ 
LIBSBML_EXTERN
LineSegment::LineSegment (const Point& start, const Point& end) : 
    SBase     ()
  , startPoint( start )
  , endPoint  ( end   )
{
  init(SBML_LAYOUT_LINESEGMENT);
}


/**
 * Destructor.
 */ 
LIBSBML_EXTERN
LineSegment::~LineSegment ()
{
}


/**
 * Does nothing since no defaults are defined for LineSegment.
 */ 
LIBSBML_EXTERN
void LineSegment::initDefaults ()
{
}


/**
 * Sets the id to a copy of the given string.
 */     
LIBSBML_EXTERN
void
LineSegment::setId (const std::string& id)
{
  this->id = id;
}


/**
 * Returns the id.
 */ 
LIBSBML_EXTERN
const std::string&
LineSegment::getId () const
{
  return this->id;
}


/**
 * Returns the start point of the line.
 */ 
LIBSBML_EXTERN
const Point&
LineSegment::getStart () const
{
  return this->startPoint;
}


/**
 * Returns the start point of the line.
 */ 
LIBSBML_EXTERN
Point&
LineSegment::getStart()
{
  return this->startPoint;
}


/**
 * Initializes the start point with a copy of the given Point object.
 */
LIBSBML_EXTERN
void
LineSegment::setStart (const Point& start)
{
  this->startPoint = start;
}


/**
 * Initializes the start point with the given coordinates.
 */
LIBSBML_EXTERN
void
LineSegment::setStart (double x, double y, double z)
{
  this->startPoint.setOffsets(x, y, z);
}


/**
 * Returns the end point of the line.
 */ 
LIBSBML_EXTERN
const Point&
LineSegment::getEnd () const
{
  return this->endPoint;
}


/**
 * Returns the end point of the line.
 */ 
LIBSBML_EXTERN
Point&
LineSegment::getEnd ()
{
  return this->endPoint;
}


/**
 * Initializes the end point with a copy of the given Point object.
 */
LIBSBML_EXTERN
void
LineSegment::setEnd (const Point& end)
{
  this->endPoint = end;
}


/**
 * Initializes the end point with the given coordinates.
 */
LIBSBML_EXTERN
void
LineSegment::setEnd (double x, double y, double z)
{
  this->endPoint.setOffsets(x, y, z);
}




/**
 * Creates a LineSegment and returns the pointer.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_create (void)
{
  return new(std::nothrow) LineSegment;
}


/**
 * Creates a LineSegment from a template.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_createFrom (const LineSegment_t *temp)
{
  return new(std::nothrow) LineSegment(*temp);
}


/**
 * Creates a LineSegment with the given points and returns the pointer.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_createWithPoints (const Point_t *start, const Point_t *end)
{
  return new(std::nothrow) LineSegment (*start, *end);
}


/**
 * Creates a LineSegment with the given coordinates and returns the pointer.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_createWithCoordinates (double x1, double y1, double z1,
                                   double x2, double y2, double z2)
{
  return new(std::nothrow) LineSegment(x1, y1, z1, x2, y2, z2);
}


/**
 * Frees the memory for the line segment.
 */
LIBSBML_EXTERN
void
LineSegment_free (LineSegment_t *ls)
{
  delete ls;
}


/**
 * Initializes the start point with a copy of the given Point object.
 */
LIBSBML_EXTERN
void
LineSegment_setStart (LineSegment_t *ls, const Point_t *start)
{
  ls->setStart(*start);
}


/**
 * Initializes the end point with a copy of the given Point object.
 */
LIBSBML_EXTERN
void
LineSegment_setEnd (LineSegment_t *ls, const Point_t *end)
{
  ls->setEnd(*end);
}


/**
 * Returns the start point of the line.
 */ 
LIBSBML_EXTERN
Point_t *
LineSegment_getStart (LineSegment_t *ls)
{
  return & ls->getStart();
}


/**
 * Returns the end point of the line.
 */ 
LIBSBML_EXTERN
Point_t *
LineSegment_getEnd (LineSegment_t *ls)
{
  return & ls->getEnd();
}


/**
 * Does nothing since no defaults are defined for LineSegment.
 */ 
LIBSBML_EXTERN
void
LineSegment_initDefaults (LineSegment_t *ls)
{
  ls->initDefaults();
}
