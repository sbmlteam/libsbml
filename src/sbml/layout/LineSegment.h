/**
 * Filename    : LineSegment.h
 * Description : SBML Layout LineSegment C++ Header
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


#ifndef LineSegment_H__
#define LineSegment_H__


#include "common/extern.h"


#ifdef __cplusplus


#include <string>

#include "sbml/SBase.h"
#include "Point.h"


class LineSegment : public SBase
{
protected:

  std::string id;
  Point startPoint;
  Point endPoint;


public:

  /**
   * Creates a line segment with both points set to (0.0,0.0,0.0)
   */ 
  LIBSBML_EXTERN
  LineSegment ();

  /**
   * Creates a new line segment with the given 2D coordinates.
   */ 
  LIBSBML_EXTERN
  LineSegment (double x1, double y1, double x2, double y2);


  /**
   * Creates a new line segment with the given 3D coordinates.
   */ 
  LIBSBML_EXTERN
  LineSegment(double x1, double y1, double z1, double x2, double y2, double z2);

  /**
   * Creates a new line segment with the two given points.
   */ 
  LIBSBML_EXTERN
  LineSegment (const Point& start, const Point& end);

  /**
   * Destructor.
   */ 
  LIBSBML_EXTERN
  virtual ~LineSegment ();


  /**
   * Sets the id to a copy of the given string.
   */     
  LIBSBML_EXTERN
  void setId (const std::string& id);

  /**
   * Returns the id.
   */ 
  LIBSBML_EXTERN
  const std::string& getId () const;

  /**
   * Returns the start point of the line.
   */ 
  LIBSBML_EXTERN
  const Point& getStart () const;

  /**
   * Returns the start point of the line.
   */ 
  LIBSBML_EXTERN
  Point& getStart ();

  /**
   * Initializes the start point with a copy of the given Point object.
   */
  LIBSBML_EXTERN
  void setStart (const Point& start);

  /**
   * Initializes the start point with the given coordinates.
   */
  LIBSBML_EXTERN
  void setStart (double x, double y, double z = 0.0);

  /**
   * Returns the end point of the line.
   */ 
  LIBSBML_EXTERN
  const Point& getEnd () const;

  /**
   * Returns the end point of the line.
   */ 
  LIBSBML_EXTERN
  Point& getEnd ();

  /**
   * Initializes the end point with a copy of the given Point object.
   */
  LIBSBML_EXTERN
  void setEnd (const Point& end);

  /**
   * Initializes the end point with the given coordinates.
   */
  LIBSBML_EXTERN
  void setEnd (double x, double y, double z = 0.0);

  /**
   * Does noting since no defaults are defined for LineSegment.
   */ 
  LIBSBML_EXTERN
  void initDefaults ();
};


#endif /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Creates a LineSegment and returns the pointer.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_create (void);


/**
 * Creates a LineSegment from a template.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_createFrom (const LineSegment_t *temp);

/**
 * Creates a LineSegment with the given points and returns the pointer.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_createWithPoints (const Point_t *start, const Point_t *end);

/**
 * Creates a LineSegment with the given coordinates and returns the
 * pointer.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_createWithCoordinates (double x1, double y1, double z1,
                                   double x2, double y2, double z2);

/**
 * Frees the memory for the line segment.
 */
LIBSBML_EXTERN
void
LineSegment_free (LineSegment_t *ls);


/**
 * Initializes the start point with a copy of the given Point object.
 */
LIBSBML_EXTERN
void 
LineSegment_setStart (LineSegment_t *ls, const Point_t *start);

/**
 * Initializes the end point with a copy of the given Point object.
 */
LIBSBML_EXTERN
void 
LineSegment_setEnd (LineSegment_t *ls, const Point_t *end);


/**
 * Returns the start point of the line.
 */ 
LIBSBML_EXTERN
Point_t *
LineSegment_getStart (LineSegment_t *ls);

/**
 * Returns the end point of the line.
 */ 
LIBSBML_EXTERN
Point_t *
LineSegment_getEnd (LineSegment_t *ls);

/**
 * Does noting since no defaults are defined for LineSegment.
 */ 
LIBSBML_EXTERN
void
LineSegment_initDefaults (LineSegment_t *ls);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* LineSegment_H__ */
