/**
 * Filename    : Curve.cpp
 * Description : SBML Layout Curve source
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

#include "Curve.h"
#include "CubicBezier.h"


/**
 * Creates a curve with an empty list of segments.
 */ 
LIBSBML_EXTERN
Curve::Curve () : SBase ()
{
  init(SBML_LAYOUT_CURVE);
}


/**
 * Destructor.
 */ 
LIBSBML_EXTERN
Curve::~Curve ()
{
}


/**
 * Does nothing since no defaults are defined for Curve.
 */ 
LIBSBML_EXTERN
void Curve::initDefaults ()
{
}


/**
 * Returns a reference to the ListOf object that holds all the curve
 * segments.
 */
LIBSBML_EXTERN
const ListOf&
Curve::getListOfCurveSegments () const
{
  return this->curveSegments;
}


/**
 * Returns a reference to the ListOf object that holds all the curve
 * segments.
 */
LIBSBML_EXTERN
ListOf&
Curve::getListOfCurveSegments ()
{
  return this->curveSegments;
}


/**
 * Returns a pointer to the curve segment with the given index.  If the
 * index is invalid, NULL is returned.
 */  
LIBSBML_EXTERN
LineSegment*
Curve::getCurveSegment (unsigned int index) const
{
  return static_cast<LineSegment*>( this->curveSegments.get(index) );
}


/**
 * Adds a new CurveSegment to the end of the list.
 */ 
LIBSBML_EXTERN
void
Curve::addCurveSegment (LineSegment& segment)
{
  this->curveSegments.append(&segment);
}


/**
 * Returns the number of curve segments.
 */ 
LIBSBML_EXTERN
unsigned int
Curve::getNumCurveSegments () const
{
  return this->curveSegments.getNumItems();
}


/**
 * Creates a new LineSegment and adds it to the end of the list.  A
 * reference to the new LineSegment object is returned.
 */
LIBSBML_EXTERN
LineSegment&
Curve::createLineSegment ()
{
  LineSegment* ls = new LineSegment();

  this->addCurveSegment(*ls);
  return *ls;
}


/**
 * Creates a new CubicBezier and adds it to the end of the list.  A
 * reference to the new CubicBezier object is returned.
 */
LIBSBML_EXTERN
CubicBezier& Curve::createCubicBezier ()
{
  CubicBezier* cb = new CubicBezier();

  this->addCurveSegment(*cb);
  return *cb;
}




/**
 * Creates a new curve and returns the pointer to it.
 */
LIBSBML_EXTERN
Curve_t *
Curve_create (void)
{
  return new(std::nothrow) Curve;
}


/**
 * Creates a new Curve object from a template.
 */
LIBSBML_EXTERN
Curve_t *
Curve_createFrom (const Curve_t *temp)
{
  return new(std::nothrow) Curve(temp ? *temp : Curve());
}


/**
 * Frees the memory taken by the Curve.
 */
LIBSBML_EXTERN
void
Curve_free (Curve_t *c)
{
  delete c;
}


/**
 * Adds a LineSegment.
 */
LIBSBML_EXTERN
void
Curve_addCurveSegment (Curve_t *c, LineSegment_t *ls)
{
  c->addCurveSegment(ls ? *ls : *(new LineSegment()));
}


/**
 * Returns the number of line segments.
 */
LIBSBML_EXTERN
unsigned int
Curve_getNumCurveSegments (const Curve_t *c)
{
  return c->getNumCurveSegments();
}


/**
 * Returns the line segment with the given index.
 */
LIBSBML_EXTERN
LineSegment_t *
Curve_getCurveSegment (const Curve_t *c, unsigned int index)
{
  return c->getCurveSegment(index);
}


/**
 * Returns the ListOf object that holds all the curve segments.
 */ 
LIBSBML_EXTERN
ListOf_t *
Curve_getListOfCurveSegments (Curve_t *c)
{
  return & c->getListOfCurveSegments();
}


/**
 * Does nothing since no defaults are defined for Curve.
 */ 
LIBSBML_EXTERN
void
Curve_initDefaults (Curve_t *c)
{
  c->initDefaults();
}


/**
 * Creates a new LineSegment and adds it to the end of the list.  A pointer
 * to the new LineSegment object is returned.
 */
LIBSBML_EXTERN
LineSegment_t *
Curve_createLineSegment (Curve_t *c)
{
  return & c->createLineSegment();
}


/**
 * Creates a new CubicBezier and adds it to the end of the list.  A pointer
 * to the new CubicBezier object is returned.
 */
LIBSBML_EXTERN
CubicBezier_t *
Curve_createCubicBezier (Curve_t *c)
{
  return & c->createCubicBezier();
}
