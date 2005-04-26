/**
 * Filename    : BoundingBox.h
 * Description : SBML Layout BoundingBox C++ Header
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


#ifndef BoundingBox_H__
#define BoundingBox_H__


#include "common/extern.h"


#ifdef __cplusplus


#include <string>

#include "sbml/SBase.h"

#include "Point.h"
#include "Dimensions.h"


class BoundingBox : public SBase
{
protected:

  std::string id;
  Point position;
  Dimensions dimensions;


public:
        
  /**
   * Default Constructor set position and dimensions to (0.0,0.0,0.0) and
   * the id to an empty string.
   */ 
  LIBSBML_EXTERN
  BoundingBox ();
        
  /**
   * Constructor set position and dimensions to (0.0,0.0,0.0) and the id to
   * a copy of the given string.
   */ 
  LIBSBML_EXTERN
  BoundingBox (const std::string id);
        
  /**
   * Constructor which sets the id, the coordinates and the dimensions to
   * the given 2D values.
   */ 
  LIBSBML_EXTERN
  BoundingBox (const std::string id, double x, double y,
               double width, double height);
        
  /**
   * Constructor which sets the id, the coordinates and the dimensions to
   * the given 3D values.
   */ 
  LIBSBML_EXTERN
  BoundingBox (const std::string id, double x, double y, double z,
               double width, double height, double depth);
  
  /**
   * Constructor which sets the id, the coordinates and the dimensions to
   * the given values.
   */ 
  LIBSBML_EXTERN
  BoundingBox (const std::string id, const Point& p, const Dimensions& d);

  /**
   * Destructor which does nothing.
   */ 
  LIBSBML_EXTERN
  virtual ~BoundingBox ();
        
  /**
   * Sets the id to a copy of the given string.
   */  
  LIBSBML_EXTERN
  void setId (const std::string& id);
        
  /**
   * Returns the id of the BoundingBox.
   */ 
  LIBSBML_EXTERN
  const std::string getId () const;
        
  /**
   * Returns true if the id is not the empty string.
   */ 
  LIBSBML_EXTERN
  bool isSetId () const;    

  /**
   * Returns the position of the BoundingBox as const referece to a Point
   * object.
   */ 
  LIBSBML_EXTERN
  const Point& getPosition () const;

  /**
   * Returns the dimensions of the BoundingBox as const referece to a
   * Dimensions object.
   */ 
  LIBSBML_EXTERN
  const Dimensions& getDimensions () const;
        
  /**
   * Returns the position of the BoundingBox as referece to a Point object.
   */ 
  LIBSBML_EXTERN
  Point& getPosition ();
        
  /**
   * Returns the dimensions of the BoundingBox as referece to a Dimensions
   * object.
   */ 
  LIBSBML_EXTERN
  Dimensions& getDimensions ();
        
  /**
   * Sets the position to a copy of the Point object given.
   */ 
  LIBSBML_EXTERN
  void setPosition (const Point& p);
        
  /**
   * Sets the dimensions to a copy of the Dimensions object given.
   */ 
  LIBSBML_EXTERN
  void setDimensions (const Dimensions& d);  
        
  /**
   * Does nothing yet since there are no defaults fo a BoundingBox. 
   */ 
  LIBSBML_EXTERN
  void initDefaults ();
};


#endif /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Function that creates a BoundingBox_t object with position set to
 * (0.0,0.0,0.0) and dimensions set to (0.0,0.0,0.0). The id is set to the
 * empty string.
 */
LIBSBML_EXTERN
BoundingBox_t *
BoundingBox_create (void);

/**
 * ZFunction that creates a BoundingBox_t object with position set to
 * (0.0,0.0,0.0) and dimensions set to (0.0,0.0,0.0).  The id is set to the
 * given string.
 */
LIBSBML_EXTERN
BoundingBox_t *
BoundingBox_createWith (const char *id);

/**
 * Function that creates a BoundingBox_t object with the coordinates and
 * sizes given as arguments. The id is set to the empty string.
 */ 
LIBSBML_EXTERN
BoundingBox_t *
BoundingBox_createWithCoordinates (const char *id, double x, double y, double z,
                                   double width, double height, double depth);

/**
 * Frees all memory taken by the given BoundingBox_t object.
 */ 
LIBSBML_EXTERN
void
BoundingBox_free (BoundingBox_t *bb);

/**
 * Does nothing since no defaults are defined for BoundingBox.
  */
LIBSBML_EXTERN
void
BoundingBox_initDefaults (BoundingBox_t *bb);

/**
 * Sets the id of the BoundingBox_t object to the id given as second
 * argument.
 */ 
LIBSBML_EXTERN
void
BoundingBox_setId (BoundingBox_t *bb, const char *id);

/**
 * Returns the id
 */ 
LIBSBML_EXTERN
const char * 
BoundingBox_getId (const BoundingBox_t *bb);

/**
 * Returns true if the id is set, that is if the id is not the empty
 * string.
 */ 
LIBSBML_EXTERN
int
BoundingBox_isSetId (const BoundingBox_t *bb);

/**
 * Returns the position as a Point_t object.
 */ 
LIBSBML_EXTERN
Point_t *
BoundingBox_getPosition (BoundingBox_t *bb);

/**
 * Returns the dimensions as a Dimensions_t object.
 */ 
LIBSBML_EXTERN
Dimensions_t *
BoundingBox_getDimensions (BoundingBox_t *bb);

/**
 * Sets the position to a copy of the Point_t object given as argument.
  */
LIBSBML_EXTERN
void
BoundingBox_setPosition (BoundingBox_t *bb, const Point_t *p);

/**
 * Sets the dimensions to a copy of the Dimensions_t object given.
 */ 
LIBSBML_EXTERN
void
BoundingBox_setDimensions (BoundingBox_t *bb, const Dimensions_t *d);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* BoundingBox_H__ */
