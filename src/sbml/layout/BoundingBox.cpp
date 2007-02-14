/**
 * Filename    : BoundingBox.cpp
 * Description : SBML Layout BoundingBox source
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


#include "BoundingBox.h"
#include "LayoutUtilities.h"

#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>



/**
 * Default Constructor set position and dimensions to (0.0,0.0,0.0) and the
 * id to an empty string.
 */ 
BoundingBox::BoundingBox() : SBase()
{
}


/**
 * Constructor set position and dimensions to (0.0,0.0,0.0) and the id to a
 * copy of the given string.
 */ 
BoundingBox::BoundingBox (const std::string id) : SBase(), mId(id)
{
}


/**
 * Constructor which sets the id, the coordinates and the dimensions to the
 * given 2D values.
 */ 
BoundingBox::BoundingBox (const std::string id,
                          double x, double y,
                          double width, double height)
  : SBase     ()
  , mId        ( id )
  , mPosition  ( x, y, 0.0               )
  , mDimensions( width, height, 0.0 )
{
}


/**
 * Constructor which sets the id, the coordinates and the dimensions to the
 * given 3D values.
 */ 
BoundingBox::BoundingBox (const std::string id,
                          double x, double y, double z,
                          double width, double height, double depth)
  : SBase()
  , mId        ( id )
  , mPosition  (x, y, z)                   
  , mDimensions( width, height, depth )
{
}

        
/**
 * Constructor which sets the id, the coordinates and the dimensions to the
 * given 3D values.
 */ 
BoundingBox::BoundingBox (const std::string id,
                          const Point*      p,
                          const Dimensions* d)
  : SBase     ()
  , mId        ( id )
{
    if(p)
    {
        this->mPosition=*p;   
    }
    if(d)
    {
        this->mDimensions=*d;   
    }
}

/**
 * Creates a new BoundingBox from the given XMLNode
 */
BoundingBox::BoundingBox(const XMLNode& node)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
    this->readAttributes(attributes);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="position")
        {
            this->mPosition=Point(*child);
        }
        else if(childName=="dimensions")
        {
            this->mDimensions=Dimensions(*child);
        }
        else if(childName=="annotation")
        {
            this->mAnnotation=new XMLNode(node);
        }
        else if(childName=="notes")
        {
            this->mNotes=new XMLNode(node);
        }
        else
        {
            //throw;
        }
    }    
}


/**
 * Destructor which does nothing.
 */ 
BoundingBox::~BoundingBox ()
{
}


/**
 * Does nothing since no defaults are defined for a BundingBox.
 */ 
void BoundingBox::initDefaults ()
{
}


/**
 * Sets the id to a copy of the given string.
 */  
void BoundingBox::setId (const std::string& id)
{
  this->mId = id;
}


/**
 * Returns the id of the BOundingBox.
 */ 
const std::string
BoundingBox::getId () const
{
  return this->mId;
}


/**
 * Returns true if the id is not the empty string.
 */ 
bool
BoundingBox::isSetId () const
{
  return ! this->mId.empty();
}


/**
 * Returns the position of the BoundingBox as const referece to a Point
 * object.
 */ 
const Point*
BoundingBox::getPosition () const
{
  return &this->mPosition;
}


/**
 * Returns the dimensions of the BoundingBox as const referece to a
 * Dimensions object.
 */ 
const Dimensions*
BoundingBox::getDimensions () const
{
  return &this->mDimensions;
}


/**
 * Returns the position of the BoundingBox as referece to a Point object.
 */ 
Point*
BoundingBox::getPosition ()
{
  return &this->mPosition;
}


/**
 * Returns the dimensions of the BoundingBox as referece to a Dimensions
 * object.
 */ 
Dimensions*
BoundingBox::getDimensions ()
{
  return &this->mDimensions;
}


/**
 * Sets the position to a copy of the Point object given.
 */ 
void BoundingBox::setPosition (const Point* p)
{
    if(!p) return;  
    this->mPosition = Point(*p);
}


/**
 * Sets the dimensions to a copy of the Dimensions object given.
 */ 
void
BoundingBox::setDimensions (const Dimensions* d)
{
  if(!d) return;
  this->mDimensions = Dimensions(*d);
}


/**
 * Sets the x offset of the BoundingBox.
 */
void
BoundingBox::setX(double x)
{
  this->mPosition.setX(x);
}


/**
 * Sets the y offset of the BoundingBox.
 */
void
BoundingBox::setY(double y)
{
  this->mPosition.setY(y);
}


/**
 * Sets the z offset of the BoundingBox.
 */
void
BoundingBox::setZ(double z)
{
  this->mPosition.setZ(z);
}


/**
 * Sets the width of the BoundingBox.
 */
void
BoundingBox::setWidth(double width)
{
  this->mDimensions.setWidth(width);
}


/**
 * Sets the height of the BoundingBox.
 */
void
BoundingBox::setHeight(double height)
{
  this->mDimensions.setHeight(height);
}


/**
 * Sets the depth of the BoundingBox.
 */
void
BoundingBox::setDepth(double depth)
{
  this->mDimensions.setDepth(depth);
}

/**
 * Returns the x offset of the bounding box.
 */
double
BoundingBox::x() const
{
  return this->mPosition.x();
}

/**
 * Returns the y offset of the bounding box.
 */
double
BoundingBox::y() const
{
  return this->mPosition.y();
}

/**
 * Returns the z offset of the bounding box.
 */
double
BoundingBox::z() const
{
  return this->mPosition.z();
}

/**
 * Returns the width of the bounding box.
 */
double
BoundingBox::width() const
{
  return this->mDimensions.width();
}

/**
 * Returns the height of the bounding box.
 */
double
BoundingBox::height() const
{
  return this->mDimensions.height();
}

/**
 * Returns the depth of the bounding box.
 */
double
BoundingBox::depth() const
{
  return this->mDimensions.depth();
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
void BoundingBox::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  this->mPosition.write(stream);
  this->mDimensions.write(stream);
}

/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string& BoundingBox::getElementName () const 
{
  static const std::string name = "boundingBox";
  return name;
}

/**
 * @return a (deep) copy of this Model.
 */
SBase* 
BoundingBox::clone () const
{
    return new BoundingBox(*this);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
BoundingBox::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "dimensions")
  {
    object = &mDimensions;
  }

  else if ( name == "position"    )
  {
      object = &mPosition;
  }

  return object;
}

/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */

void BoundingBox::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  attributes.readInto("id", mId);
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
void BoundingBox::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);
  stream.writeAttribute("id", mId);
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
BoundingBox::getTypeCode () const
{
  return SBML_LAYOUT_BOUNDINGBOX;
}


/**
 * Accepts the given SBMLVisitor.
 */
bool
BoundingBox::accept (SBMLVisitor& v) const
{
  /*
  bool result=v.visit(*this);
  mPosition.accept(v);
  mDimensions.accept(v);
  v.leave(*this);*/
  return false;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode BoundingBox::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  xmlns.add("http://projects.eml.org/bcb/sbml/level2", "");
  XMLTriple triple = XMLTriple("dimensions", "", "");
  XMLAttributes att = XMLAttributes();
  // add the SBase Ids
  addSBaseAttributes(*this,att);
  if(this->isSetId())
  {
    att.add("id",this->mId);
  }
  XMLToken token = XMLToken(triple, att, xmlns); 
  XMLNode node(token);
  // add the notes and annotations
  node.addChild(*this->mNotes);
  node.addChild(*this->mAnnotation);
  // add position
  node.addChild(this->mPosition.toXML("position"));
  // add dimensions
  node.addChild(this->mDimensions.toXML());
  return node;
}



/**
 * Function that creates a BoundingBox_t object with position set to
 * (0.0,0.0,0.0) and dimensions set to (0.0,0.0,0.0). The id is set to the
 * empty string.
 */
LIBSBML_EXTERN
BoundingBox_t *
BoundingBox_create (void)
{
  return new(std::nothrow) BoundingBox;
}


/**
 * Function that creates a BoundingBox_t object with position set to
 * (0.0,0.0,0.0) and dimensions set to (0.0,0.0,0.0).  The id is set to the
 * given string.
 */
LIBSBML_EXTERN
BoundingBox_t *
BoundingBox_createWith (const char *id)
{
  return new(std::nothrow) BoundingBox(id ? id : "");
}


/**
 * Function that creates a BoundingBox_t object with the coordinates and
 * sizes given as arguments. The id is set to the empty string.
 */ 
LIBSBML_EXTERN
BoundingBox_t *
BoundingBox_createWithCoordinates (const char *id,
                                   double x, double y, double z,
                                   double width, double height, double depth)
{
  return new(std::nothrow) BoundingBox(id ? id : "" , x, y, z, width, height, depth);
}


/**
 * Frees all memory taken by the given BoundingBox_t object.
 */ 
LIBSBML_EXTERN
void
BoundingBox_free (BoundingBox_t *bb)
{
  delete bb;
}


/**
 * Does nothing since no defaults are defined for BoundingBox.
 */
LIBSBML_EXTERN
void
BoundingBox_initDefaults (BoundingBox_t *bb)
{
  bb->initDefaults();
}


/**
 * Sets the id of the BoundingBox_t object to the id given as second
 * argument.
 */ 
LIBSBML_EXTERN
void
BoundingBox_setId (BoundingBox_t *bb, const char* id)
{
    static_cast<BoundingBox*>(bb)->setId( id ? id : "" );
}


/**
 * Returns the id
 */ 
LIBSBML_EXTERN
const char *
BoundingBox_getId (const BoundingBox_t *bb)
{
    return bb->isSetId() ? bb->getId().c_str() : NULL;
}


/**
 * Returns true if the id is set, that is if the id is not the empty
 * string.
 */ 
LIBSBML_EXTERN
int
BoundingBox_isSetId (const BoundingBox_t *bb)
{
  return static_cast<int>( bb->isSetId() );
}


/**
 * Returns the position as a Point_t object.
 */ 
LIBSBML_EXTERN
Point_t *
BoundingBox_getPosition (BoundingBox_t *bb)
{
  return bb->getPosition();
}


/**
 * Returns the dimensions as a Dimensions_t object.
 */ 
LIBSBML_EXTERN
Dimensions_t *
BoundingBox_getDimensions (BoundingBox_t *bb)
{
  return bb->getDimensions();
}


/**
 * Sets the position to a copy of the Point_t object given as argument.
 */
LIBSBML_EXTERN
void
BoundingBox_setPosition (BoundingBox_t *bb, const Point_t *p)
{
   bb->setPosition(p);
}


/**
 * Sets the dimensions to a copy of the Dimensions_t object given.
 */ 
LIBSBML_EXTERN
void
BoundingBox_setDimensions (BoundingBox_t *bb, const Dimensions_t *d)
{
  bb->setDimensions(d);
}

/**
 * Sets the x offset of the bounding box.
 */
LIBSBML_EXTERN
void
BoundingBox_setX(BoundingBox_t* bb,double x)
{
    bb->setX(x);
}

/**
 * Sets the y offset of the bounding box.
 */
LIBSBML_EXTERN
void
BoundingBox_setY(BoundingBox_t* bb,double y)
{
    bb->setY(y);
}


/**
 * Sets the z offset of the bounding box.
 */
LIBSBML_EXTERN
void
BoundingBox_setZ(BoundingBox_t* bb,double z)
{
    bb->setZ(z);
}


/**
 * Sets the width of the bounding box.
 */
LIBSBML_EXTERN
void
BoundingBox_setWidth(BoundingBox_t* bb,double width)
{
    bb->setWidth(width);
}


/**
 * Sets the height of the bounding box.
 */
LIBSBML_EXTERN
void
BoundingBox_setHeight(BoundingBox_t* bb,double height)
{
    bb->setHeight(height);
}


/**
 * Sets the depth of the bounding box.
 */
LIBSBML_EXTERN
void
BoundingBox_setDepth(BoundingBox_t* bb,double depth)
{
    bb->setDepth(depth);
}

/**
 * Returns the x offset of the bounding box
 */
LIBSBML_EXTERN
double
BoundingBox_x(BoundingBox_t* bb)
{
    return bb->x();
}

/**
 * Returns the y offset of the bounding box
 */
LIBSBML_EXTERN
double
BoundingBox_y(BoundingBox_t* bb)
{
    return bb->y();
}

/**
 * Returns the z offset of the bounding box
 */
LIBSBML_EXTERN
double
BoundingBox_z(BoundingBox_t* bb)
{
    return bb->z();
}

/**
 * Returns the width of the bounding box
 */
LIBSBML_EXTERN
double
BoundingBox_width(BoundingBox_t* bb)
{
    return bb->width();
}

/**
 * Returns the height of the bounding box
 */
LIBSBML_EXTERN
double
BoundingBox_height(BoundingBox_t* bb)
{
    return bb->height();
}

/**
 * Returns the depth of the bounding box
 */
LIBSBML_EXTERN
double
BoundingBox_depth(BoundingBox_t* bb)
{
    return bb->depth();
}

/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
BoundingBox_t *
BoundingBox_clone (const BoundingBox_t *m)
{
  return static_cast<BoundingBox*>( m->clone() );
}

