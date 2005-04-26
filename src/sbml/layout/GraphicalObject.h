/**
 * Filename    : GraphicalObject.h
 * Description : SBML Layout GraphicalObject C++ Header
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


#ifndef GraphicalObject_H__
#define GraphicalObject_H__


#include "common/extern.h"


#ifdef __cplusplus


#include <string>

#include "sbml/SBase.h"
#include "BoundingBox.h"


class GraphicalObject : public SBase
{
protected:

  std::string id;
  BoundingBox boundingBox;

  friend class LayoutHandler;
        

public:

  /**
   * Creates a new GraphicalObject.
   */
  LIBSBML_EXTERN
  GraphicalObject ();

  /**
   * Creates a new GraphicalObject with the given id.
   */
  LIBSBML_EXTERN
  GraphicalObject (const std::string& id);

  /**
   * Creates a new GraphicalObject with the given id and 2D coordinates for
   * the bounding box.
   */
  LIBSBML_EXTERN
  GraphicalObject (const std::string& id,
                   double x, double y, double w, double h);

  /**
   * Creates a new GraphicalObject with the given id and 3D coordinates for
   * the bounding box.
   */
  LIBSBML_EXTERN
  GraphicalObject (const std::string& id,
                   double x, double y, double z,
                   double w, double h, double d);

  /**
   * Creates a new GraphicalObject with the given id and 3D coordinates for
   * the bounding box.
   */
  LIBSBML_EXTERN
  GraphicalObject (const std::string& id, const Point& p, const Dimensions& d);

  /**
   * Creates a new GraphicalObject with the given id and 3D coordinates for
   * the bounding box.
   */
  LIBSBML_EXTERN
  GraphicalObject (const std::string& id, const BoundingBox& bb);

  /**
   * Destructor.
   */ 
  LIBSBML_EXTERN
  virtual ~GraphicalObject ();

  /**
   * Does nothing. No defaults are defined for GraphicalObject.
   */ 
  LIBSBML_EXTERN
  void initDefaults ();

  /**
   * Gets the id for the GraphicalObject.
   */
  LIBSBML_EXTERN
  const std::string& getId () const;

  /**
   * Sets the id for the GraphicalObject.
   */
  LIBSBML_EXTERN
  void setId (const std::string& id);

  /**
   * Returns true if the id is not the empty string.
   */
  LIBSBML_EXTERN
  bool isSetId () const;

  /**
   * Sets the boundingbox for the GraphicalObject.
   */ 
  LIBSBML_EXTERN
  void setBoundingBox (const BoundingBox& bb);

  /**
   * Returns the bounding box for the GraphicalObject.
   */ 
  LIBSBML_EXTERN
  BoundingBox& getBoundingBox ();

  /**
   * Returns the bounding box for the GraphicalObject.
   */ 
  LIBSBML_EXTERN
  const BoundingBox& getBoundingBox() const;
};


#endif /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Creates a new GraphicalObject.
 */
LIBSBML_EXTERN
GraphicalObject_t *
GraphicalObject_create (void);


/**
 * Creates a GraphicalObject from a template.
 */
LIBSBML_EXTERN
GraphicalObject_t *
GraphicalObject_createFrom (const GraphicalObject_t *temp);

/**
 * Frees all memory taken up by the GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject_free (GraphicalObject_t *go);

/**
 * Sets the id for the GraphicalObject.
 */
LIBSBML_EXTERN
void
GraphicalObject_setId (GraphicalObject_t *go, const char *id);

/**
 * Gets the id for the given GraphicalObject.
 */
LIBSBML_EXTERN
const char *
GraphicalObject_getId (const GraphicalObject_t *go);


/**
 * Sets the boundingbox for the GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject_setBoundingBox (GraphicalObject_t *go, const BoundingBox_t *bb);

/**
 * Returns the bounding box for the GraphicalObject.
 */ 
LIBSBML_EXTERN
BoundingBox_t *
GraphicalObject_getBoundingBox (GraphicalObject_t *go);

/**
 * Does nothing. No defaults are defined for GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject_initDefaults (GraphicalObject_t *go);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* GraphicalObject_H__ */
