/**
 * Filename    : Dimensions.cpp
 * Description : SBML Layout Dimensions source
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
#include "Dimensions.h"


/**
 * Creates a new Dimensions object with all sizes set to 0.0.
 */ 
LIBSBML_EXTERN
Dimensions::Dimensions (): SBase(), w(0.0), h(0.0), d(0.0)
{
  init(SBML_LAYOUT_DIMENSIONS);
}


/**
 * Creates a new Dimensions object with the given sizes.
 */ 
LIBSBML_EXTERN
Dimensions::Dimensions (double width, double height, double depth) :
  SBase(), w(width), h(height), d(depth)
{
  init(SBML_LAYOUT_DIMENSIONS);
}


/**
 * Frees memory taken up by the Dimensions object.
 */ 
LIBSBML_EXTERN
Dimensions::~Dimensions ()
{
}


/**
 * Returns the width.
 */
LIBSBML_EXTERN
double
Dimensions::width() const
{
  return this->w;
}


/**
 * Returns the height.
 */
LIBSBML_EXTERN
double
Dimensions::height() const
{
  return this->h;
}


/**
 * Returns the depth.
 */
LIBSBML_EXTERN
double
Dimensions::depth () const
{
  return this->d;
}


/**
 * Returns the width.
 */
LIBSBML_EXTERN
double
Dimensions::getWidth() const
{
  return this->width();
}


/**
 * Returns the height.
 */
LIBSBML_EXTERN
double
Dimensions::getHeight() const
{
  return this->height();
}


/**
 * Returns the depth.
 */
LIBSBML_EXTERN
double
Dimensions::getDepth () const
{
  return this->depth();
}


/**
 * Sets the width to the given value.
 */ 
LIBSBML_EXTERN
void
Dimensions::setWidth (double width)
{
  this->w = width;
}


/**
 * Sets the height to the given value.
 */ 
LIBSBML_EXTERN
void
Dimensions::setHeight (double height)
{
  this->h = height;
}


/**
 * Sets the depth to the given value.
 */ 
LIBSBML_EXTERN
void Dimensions::setDepth (double depth)
{
  this->d = depth;
}


/**
 * Sets all sizes of the Dimensions object to the given values.
 */ 
LIBSBML_EXTERN
void
Dimensions::setBounds (double w, double h, double d)
{
  this->setWidth (w);
  this->setHeight(h);
  this->setDepth (d);
}


/**
 * Sets the depth to 0.0
 */ 
void Dimensions::initDefaults ()
{
  this->setDepth(0.0);
}




/**
 * Creates a new Dimensions object with all sizes set to 0.0.
 */ 
LIBSBML_EXTERN
Dimensions_t *
Dimensions_create (void)
{
  return new(std::nothrow) Dimensions;
}

/**
 * Creates a new Dimensions object with the given sizes.
 */ 
LIBSBML_EXTERN
Dimensions_t *
Dimensions_createWithSize (double w, double h, double d)
{
  return new(std::nothrow) Dimensions(w, h, d);
}


/**
 * Frees memory taken up by the Dimensions object.
 */ 
LIBSBML_EXTERN
void
Dimensions_free (Dimensions_t *d)
{
  delete d;
}


/**
 * Sets the depth to 0.0
 */ 
LIBSBML_EXTERN
void
Dimensions_initDefaults (Dimensions_t *d)
{
  d->initDefaults();
}


/**
 * Sets all sizes of the Dimensions object to the given values.
 */ 
LIBSBML_EXTERN
void
Dimensions_setBounds (Dimensions_t *dim, double w, double h, double d)
{
  dim->setBounds(w, h, d);
}


/**
 * Sets the width to the given value.
 */ 
LIBSBML_EXTERN
void
Dimensions_setWidth (Dimensions_t *d, double w)
{
  d->setWidth(w);
}


/**
 * Sets the height to the given value.
 */ 
LIBSBML_EXTERN
void
Dimensions_setHeight (Dimensions_t *d, double h)
{
  d->setHeight(h);
}


/**
 * Sets the depth to the given value.
 */ 
LIBSBML_EXTERN
void
Dimensions_setDepth (Dimensions_t *dim, double d)
{
  dim->setDepth(d);
}


/**
 * Returns the width.
 */
LIBSBML_EXTERN
double
Dimensions_width (const Dimensions_t *d)
{
  return d->width();
}


/**
 * Returns the height.
 */
LIBSBML_EXTERN
double
Dimensions_height(const Dimensions_t *d)
{
  return d->height();
}

/**
 * Returns the depth.
 */
LIBSBML_EXTERN
double
Dimensions_depth (const Dimensions_t *d)
{
  return d->depth();
}

/**
 * Returns the width.
 */
LIBSBML_EXTERN
double
Dimensions_getWidth (const Dimensions_t *d)
{
  return d->width();
}


/**
 * Returns the height.
 */
LIBSBML_EXTERN
double
Dimensions_getHeight(const Dimensions_t *d)
{
  return d->height();
}

/**
 * Returns the depth.
 */
LIBSBML_EXTERN
double
Dimensions_getDepth (const Dimensions_t *d)
{
  return d->depth();
}
