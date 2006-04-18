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


#include "Point.h"
#include <sbml/SBMLErrorLog.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>


/**
 * Creates a new point with x,y and z set  to 0.0.
 */ 
Point::Point() : SBase(), mXOffset(0.0), mYOffset(0.0), mZOffset(0.0),mElementName("point")
{
}


/**
 * Creates a new point with the given ccordinates.
 */ 
Point::Point(double x, double y, double z) :
    SBase  ()
  , mXOffset(x)
  , mYOffset(y)
  , mZOffset(z)
  , mElementName("point")  
{
}


/**
 * Sets the Z offset to 0.0.
 */
void Point::initDefaults ()
{
  this->setZOffset(0.0);
}


/**
 * Destructor.
 */ 
Point::~Point()
{
}


/**
 * Sets the coordinates to the given values.
 */ 
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
void
Point::setXOffset (double x)
{
  this->setX(x);
}


/**
 * Sets the y offset.
 */ 
void
Point::setYOffset (double y)
{
  this->setY(y);
}


/**
 * Sets the z offset.
 */ 
void
Point::setZOffset (double z)
{
  this->setZ(z);
}


/**
 * Sets the x offset.
 */ 
void
Point::setX (double x)
{
  this->mXOffset = x;
}


/**
 * Sets the y offset.
 */ 
void
Point::setY (double y)
{
  this->mYOffset = y;
}


/**
 * Sets the z offset.
 */ 
void
Point::setZ (double z)
{
  this->mZOffset = z;
}


/**
 * Returns the x offset.
 */ 
double
Point::getXOffset () const
{
  return this->x();
}


/**
 * Returns the y offset.
 */ 
double
Point::getYOffset () const
{
  return this->y();
}


/**
 * Returns the z offset.
 */ 
double
Point::getZOffset () const
{
  return this->z();
}

/**
 * Returns the x offset.
 */ 
double
Point::x () const
{
  return this->mXOffset;
}


/**
 * Returns the y offset.
 */ 
double
Point::y () const
{
  return this->mYOffset;
}


/**
 * Returns the z offset.
 */ 
double
Point::z () const
{
  return this->mZOffset;
}


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.  For example:
 *
 *   SBase::writeElements(stream);
 *   mReactans.write(stream);
 *   mProducts.write(stream);
 *   ...
 */
void Point::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
}

/**
 * Sets the element name to be returned by getElementName.
 */
void Point::setElementName(const std::string& name)
{
    this->mElementName=name;
}
 
/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string& Point::getElementName () const 
{
  return this->mElementName;
}

/**
 * @return a (deep) copy of this Model.
 */
SBase* 
Point::clone () const
{
    return new Point(*this);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
Point::createObject (XMLInputStream& stream)
{
  SBase*        object = 0;

  object=SBase::createObject(stream);
  
  return object;
}

/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */

void Point::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  attributes.readInto("x", mXOffset,this->getErrorLog(),true);
  attributes.readInto("y", mYOffset,this->getErrorLog(),true);
  if(!attributes.readInto("z", mZOffset))
  {
      this->mZOffset=0.0;
  }
}

/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.  For example:
 *
 *   SBase::writeAttributes(stream);
 *   stream.writeAttribute( "id"  , mId   );
 *   stream.writeAttribute( "name", mName );
 *   ...
 */
void Point::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);
  stream.writeAttribute("x", mXOffset);
  stream.writeAttribute("y", mYOffset);
  if(this->mZOffset!=0.0)
  {
    stream.writeAttribute("z", mZOffset);
  }
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Point::getTypeCode () const
{
  return SBML_LAYOUT_POINT;
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

/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
Point_t *
Point_clone (const Point_t *m)
{
  return static_cast<Point*>( m->clone() );
}


