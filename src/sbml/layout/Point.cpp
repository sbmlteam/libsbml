/**
 * Filename    : Point.cpp
 * Description : SBML Layout Point source
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
#include "Point.h"


/**
 * Creates a new point with x,y and z set  to 0.0.
 */ 
LIBSBML_EXTERN
Point::Point() : SBase(), xOffset(0.0), yOffset(0.0), zOffset(0.0)
{
  init(SBML_LAYOUT_POINT);
}


/**
 * Creates a new point with the given ccordinates.
 */ 
LIBSBML_EXTERN
Point::Point(double x, double y, double z) :
    SBase  ()
  , xOffset(x)
  , yOffset(y)
  , zOffset(z)
{
  init(SBML_LAYOUT_POINT);
}


/**
 * Sets the Z offset to 0.0.
 */
LIBSBML_EXTERN
void Point::initDefaults ()
{
  this->setZOffset(0.0);
}


/**
 * Destructor.
 */ 
LIBSBML_EXTERN
Point::~Point()
{
}


/**
 * Sets the coordinates to the given values.
 */ 
LIBSBML_EXTERN
void
Point::setOffsets (double x, double y, double z)
{
  this->setXOffset(x);
  this->setYOffset(y);
  this->setZOffset(z);
}


/**
 * Sets the x offset.
 */ 
LIBSBML_EXTERN
void
Point::setXOffset (double x)
{
  this->setX(x);
}


/**
 * Sets the y offset.
 */ 
LIBSBML_EXTERN
void
Point::setYOffset (double y)
{
  this->setY(y);
}


/**
 * Sets the z offset.
 */ 
LIBSBML_EXTERN
void
Point::setZOffset (double z)
{
  this->setZ(z);
}


/**
 * Sets the x offset.
 */ 
LIBSBML_EXTERN
void
Point::setX (double x)
{
  this->xOffset = x;
}


/**
 * Sets the y offset.
 */ 
LIBSBML_EXTERN
void
Point::setY (double y)
{
  this->yOffset = y;
}


/**
 * Sets the z offset.
 */ 
LIBSBML_EXTERN
void
Point::setZ (double z)
{
  this->zOffset = z;
}


/**
 * Returns the x offset.
 */ 
LIBSBML_EXTERN
double
Point::getXOffset () const
{
  return this->x();
}


/**
 * Returns the y offset.
 */ 
LIBSBML_EXTERN
double
Point::getYOffset () const
{
  return this->y();
}


/**
 * Returns the z offset.
 */ 
LIBSBML_EXTERN
double
Point::getZOffset () const
{
  return this->z();
}

/**
 * Returns the x offset.
 */ 
LIBSBML_EXTERN
double
Point::x () const
{
  return this->xOffset;
}


/**
 * Returns the y offset.
 */ 
LIBSBML_EXTERN
double
Point::y () const
{
  return this->yOffset;
}


/**
 * Returns the z offset.
 */ 
LIBSBML_EXTERN
double
Point::z () const
{
  return this->zOffset;
}





/**
 * Creates a new point with the coordinates (0.0,0.0,0.0).
 */ 
LIBSBML_EXTERN
Point_t *
Point_create (void)
{
  return new(std::nothrow) Point; 
}


/**
 * Creates a new Point with the given coordinates.
 */ 
LIBSBML_EXTERN
Point_t *
Point_createWithCoordinates (double x, double y, double z)
{
  return new(std::nothrow) Point(x, y, z);
}


/**
 * Frees all memory for the Point.
 */ 
LIBSBML_EXTERN
void
Point_free (Point_t *p)
{
  delete p;
}


/**
 * Sets the Z offset to 0.0
 */ 
LIBSBML_EXTERN
void
Point_initDefaults (Point_t *p)
{
  p->initDefaults();
}


/**
 * Sets the coordinates to the given values.
 */ 
LIBSBML_EXTERN
void
Point_setOffsets (Point_t *p, double x, double y, double z)
{
  p->setOffsets(x, y, z);
}


/**
 * Sets the x offset.
 */ 
LIBSBML_EXTERN
void
Point_setXOffset (Point_t *p, double x)
{
  p->setX(x);
}


/**
 * Sets the y offset.
 */ 
LIBSBML_EXTERN
void
Point_setYOffset (Point_t *p, double y)
{
  p->setY(y);
}


/**
 * Sets the z offset.
 */ 
LIBSBML_EXTERN
void
Point_setZOffset (Point_t *p, double z)
{
  p->setZ(z);
}


/**
 * Gets the x offset.
 */ 
LIBSBML_EXTERN
double
Point_getXOffset (const Point_t *p)
{
  return p->x();
}


/**
 * Gets the y offset.
 */ 
LIBSBML_EXTERN
double
Point_getYOffset (const Point_t *p)
{
  return p->y();
}


/**
 * Gets the z offset.
 */ 
LIBSBML_EXTERN
double
Point_getZOffset (const Point_t *p)
{
  return p->z();
}


/**
 * Sets the x offset.
 */ 
LIBSBML_EXTERN
void
Point_setX (Point_t *p, double x)
{
  p->setX(x);
}


/**
 * Sets the y offset.
 */ 
LIBSBML_EXTERN
void
Point_setY (Point_t *p, double y)
{
  p->setY(y);
}


/**
 * Sets the z offset.
 */ 
LIBSBML_EXTERN
void
Point_setZ (Point_t *p, double z)
{
  p->setZ(z);
}


/**
 * Gets the x offset.
 */ 
LIBSBML_EXTERN
double
Point_x (const Point_t *p)
{
  return p->x();
}


/**
 * Gets the y offset.
 */ 
LIBSBML_EXTERN
double
Point_y (const Point_t *p)
{
  return p->y();
}


/**
 * Gets the z offset.
 */ 
LIBSBML_EXTERN
double
Point_z (const Point_t *p)
{
  return p->z();
}


