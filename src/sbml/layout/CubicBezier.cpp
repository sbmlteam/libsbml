/**
 * Filename    : CubicBezier.cpp
 * Description : SBML Layout CubicBezier source
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
#include "CubicBezier.h"


/**
 * Creates a CubicBezier and returns the pointer.
 */
LIBSBML_EXTERN
CubicBezier::CubicBezier()
{
  init(SBML_LAYOUT_CUBICBEZIER);
}


/**
 * Creates a CubicBezier with the given 2D coordinates and returns the
 * pointer.
 */
LIBSBML_EXTERN
CubicBezier::CubicBezier (double x1, double y1, double x2, double y2)
  : LineSegment( Point(x1, y1, 0.0), Point(x2, y2, 0.0) )
{
  init(SBML_LAYOUT_CUBICBEZIER);
  this->straighten();
}


/**
 * Creates a CubicBezier with the given 3D coordinates and returns the
 * pointer.
 */
LIBSBML_EXTERN
CubicBezier::CubicBezier (double x1, double y1, double z1,
                          double x2, double y2, double z2)
  : LineSegment( Point(x1, y1, z1), Point(x2, y2, z2) )
{
  init(SBML_LAYOUT_CUBICBEZIER);
  this->straighten();
}


/**
 * Makes a line from a CubicBezier by setting both base points into the
 * middle between the start and the end point.
 */
LIBSBML_EXTERN
void CubicBezier::straighten ()
{
  double x = (this->endPoint.getXOffset()+this->startPoint.getXOffset()) / 2.0;
  double y = (this->endPoint.getYOffset()+this->startPoint.getYOffset()) / 2.0;
  double z = (this->endPoint.getZOffset()+this->startPoint.getZOffset()) / 2.0;

  this->basePoint1.setOffsets(x, y, z);
  this->basePoint2.setOffsets(x, y, z);
}


/**
 * Creates a CubicBezier with the given points and returns the pointer.
 */
LIBSBML_EXTERN
CubicBezier::CubicBezier (const Point& start, const Point& end)
  : LineSegment(start, end)
{
  init(SBML_LAYOUT_CUBICBEZIER); 
  this->straighten();
}


/**
 * Creates a CubicBezier with the given points and returns the pointer.
 */
LIBSBML_EXTERN
CubicBezier::CubicBezier (const Point& start, const Point& base1,
                          const Point& base2, const Point& end)
  : LineSegment( start ,end )
  , basePoint1 ( base1 )
  , basePoint2 ( base2 )
{
  init(SBML_LAYOUT_CUBICBEZIER);
}


/**
 * Destructor.
 */ 
LIBSBML_EXTERN
CubicBezier::~CubicBezier ()
{
}


/**
 * Calls initDefaults from LineSegment.
 */ 
LIBSBML_EXTERN
void
CubicBezier::initDefaults()
{
  LineSegment::initDefaults();
}


/**
 * Returns the first base point of the curve (the one closer to the
 * starting point).
 */ 
LIBSBML_EXTERN
const Point&
CubicBezier::getBasePoint1() const
{
  return this->basePoint1;
}


/**
 * Returns the first base point of the curve (the one closer to the
 * starting point).
 */ 
LIBSBML_EXTERN
Point&
CubicBezier::getBasePoint1 ()
{
  return this->basePoint1;
}


/**
 * Initializes first base point with a copy of the given point.
 */
LIBSBML_EXTERN
void
CubicBezier::setBasePoint1 (const Point& p)
{
  this->basePoint1 = Point(p);
}


/**
 * Initializes first base point with the given ccordinates.
 */
LIBSBML_EXTERN
void
CubicBezier::setBasePoint1 (double x, double y, double z)
{
  this->basePoint1.setOffsets(x, y ,z);
}


/**
 * Returns the second base point of the curve (the one closer to the
 * starting point).
 */ 
LIBSBML_EXTERN
const Point&
CubicBezier::getBasePoint2 () const
{
  return this->basePoint2;
}


/**
 * Returns the second base point of the curve (the one closer to the
 * starting point).
 */ 
LIBSBML_EXTERN
Point&
CubicBezier::getBasePoint2 ()
{
  return this->basePoint2;
}


/**
 * Initializes second base point with a copy of the given point.
 */
LIBSBML_EXTERN
void CubicBezier::setBasePoint2 (const Point& p)
{
  this->basePoint2 = Point(p);
}


/**
 * Initializes second base point with the given ccordinates.
 */
LIBSBML_EXTERN
void
CubicBezier::setBasePoint2 (double x, double y, double z)
{
  this->basePoint2.setOffsets(x, y, z);
}




/**
 * Creates a CubicBezier and returns the pointer.
 */
LIBSBML_EXTERN
CubicBezier_t *
CubicBezier_create (void)
{
  return new(std::nothrow) CubicBezier;
}


/**
 * Creates a CubicBezier with the given points and returns the pointer.
 */
LIBSBML_EXTERN
CubicBezier_t *
CubicBezier_createWithPoints (const Point_t *start, const Point_t *base1,
                              const Point_t *base2, const Point_t *end)
{
  return new(std::nothrow)CubicBezier(*start, *base1, *base2, *end);
}


/**
 * Creates a CubicBezier with the given coordinates and returns the
 * pointer.
 */
LIBSBML_EXTERN
CubicBezier_t *
CubicBezier_createWithCoordinates (double x1, double y1, double z1,
                                   double x2, double y2, double z2,
                                   double x3, double y3, double z3,
                                   double x4, double y4, double z4)
{
  return new(std::nothrow)
    CubicBezier( Point(x1,y1,z1), Point(x2,y2,z2),
                 Point(x3,y3,z3), Point(x4,y4,z4) );
}


/**
 * Creates a CubicBezier object from a template.
 */
LIBSBML_EXTERN
CubicBezier_t *
CubicBezier_createFrom (const CubicBezier_t *temp)
{
  return new(std::nothrow) CubicBezier(*temp);
}


/**
 * Frees the memory for the cubic bezier.
 */
LIBSBML_EXTERN
void
CubicBezier_free (CubicBezier_t *cb)
{
  delete cb;
}


/**
 * Initializes start point with a copy of the given point.
 */
LIBSBML_EXTERN
void
CubicBezier_setStart (CubicBezier_t *cb, const Point_t *start)
{
  LineSegment_setStart(cb, start);
}


/**
 * Returns the starting point of the curve.
 */ 
LIBSBML_EXTERN
Point_t *
CubicBezier_getStart (CubicBezier_t *cb)
{
  return LineSegment_getStart(cb);
}


/**
 * Initializes end point with a copy of the given point.
 */
LIBSBML_EXTERN
void
CubicBezier_setEnd (CubicBezier_t *cb, const Point_t *end)
{
  LineSegment_setEnd(cb, end);
}


/**
 * Returns the end point of the curve.
 */ 
LIBSBML_EXTERN
Point_t *
CubicBezier_getEnd (CubicBezier_t *cb)
{
  return LineSegment_getEnd(cb);
}


/**
 * Initializes the first base point with a copy of the given point.
 */
LIBSBML_EXTERN
void
CubicBezier_setBasePoint1 (CubicBezier_t *cb, const Point_t *point)
{
  cb->setBasePoint1(*point);
}


/**
 * Returns the first base point of the curve (the one closer to the
 * starting point).
 */ 
LIBSBML_EXTERN
Point_t *
CubicBezier_getBasePoint1 (CubicBezier_t *cb)
{
  return & cb->getBasePoint1();
}


/**
 * Initializes the second base point with a copy of the given point.
 */
LIBSBML_EXTERN
void
CubicBezier_setBasePoint2 (CubicBezier_t *cb, const Point_t *point)
{
  cb->setBasePoint2(*point);
}


/**
 * Returns the second base point of the curve (the one closer to the
 * starting point).
 */ 
LIBSBML_EXTERN
Point_t *
CubicBezier_getBasePoint2 (CubicBezier_t *cb)
{
  return & cb->getBasePoint2();
}


/**
 * Calls initDefaults from LineSegment.
 */ 
LIBSBML_EXTERN
void
CubicBezier_initDefaults (CubicBezier_t *cb)
{
  cb->initDefaults();
}
