/**
 * Filename    : Point.h
 * Description : SBML Layout Point C++ Header
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


#ifndef Point_H__
#define Point_H__


#include "common/extern.h"


#ifdef __cplusplus


#include "sbml/SBase.h"


class Point : public SBase
{
protected:

  double xOffset;
  double yOffset;
  double zOffset;


public:

  /**
   * Creates a new point with x,y and z set to 0.0.
   */ 
  LIBSBML_EXTERN
  Point ();
        
  /**
   * Creates a new point with the given ccordinates.
   */ 
  LIBSBML_EXTERN
  Point (double x, double y, double z = 0.0);
        
  /**
   * Destructor.
   */ 
  LIBSBML_EXTERN
  virtual ~Point ();


  /**
   * Returns the x offset.
   */ 
  LIBSBML_EXTERN
  double getXOffset () const;
        
  /**
   * Returns the y offset.
   */ 
  LIBSBML_EXTERN
  double getYOffset () const;
        
  /**
   * Returns the z offset.
   */ 
  LIBSBML_EXTERN
  double getZOffset () const;
        
  /**
   * Sets the x offset.
   */ 
  LIBSBML_EXTERN
  void setXOffset (double x);
        
  /**
   * Sets the y offset.
   */ 
  LIBSBML_EXTERN
  void setYOffset (double y);
        
  /**
   * Sets the z offset.
   */ 
  LIBSBML_EXTERN
  void setZOffset (double z);
        
  /**
   * Sets the coordinates to the given values.
   */ 
  LIBSBML_EXTERN
  void setOffsets (double x, double y, double z = 0.0);
        
  /**
   * Sets the Z offset to 0.0.
   */ 
  LIBSBML_EXTERN
  void initDefaults ();
};


#endif /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Creates a new point with the coordinates (0.0,0.0,0.0).
 */ 
LIBSBML_EXTERN
Point_t *
Point_create (void);

/**
 * Creates a new Point with the given coordinates.
 */ 
LIBSBML_EXTERN
Point_t *
Point_createWithCoordinates (double x, double y, double z);

/**
 * Frees all memory for the Point.
 */ 
LIBSBML_EXTERN
void
Point_free (Point_t *p);

/**
 * Sets the Z offset to 0.0
 */ 
LIBSBML_EXTERN
void
Point_initDefaults (Point_t *p);

/**
 * Sets the coordinates to the given values.
 */ 
LIBSBML_EXTERN
void
Point_setOffsets (Point_t *p, double x, double y, double z);

/**
 * Sets the x offset.
 */ 
LIBSBML_EXTERN
void
Point_setXOffset (Point_t *p, double x);

/**
 * Sets the y offset.
 */ 
LIBSBML_EXTERN
void
Point_setYOffset (Point_t *p, double y);

/**
 * Sets the z offset.
 */ 
LIBSBML_EXTERN
void
Point_setZOffset (Point_t *p, double z);

/**
 * Gets the x offset.
 */ 
LIBSBML_EXTERN
double
Point_getXOffset (const Point_t *p);

/**
 * Gets the y offset.
 */ 
LIBSBML_EXTERN
double
Point_getYOffset (const Point_t *p);

/**
 * Gets the z offset.
 */ 
LIBSBML_EXTERN
double
Point_getZOffset (const Point_t *p);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* Point_H__ */
