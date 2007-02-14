/**
 * Filename    : GraphicalObject.cpp
 * Description : SBML Layout GraphicalObject source
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


#include "GraphicalObject.h"
#include <sbml/SBMLErrorLog.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>


/**
 * Creates a new GraphicalObject.
 */
GraphicalObject::GraphicalObject() : SBase ()
{
}


/**
 * Creates a new GraphicalObject with the given id.
 */
GraphicalObject::GraphicalObject (const std::string& id) : 
    SBase()
  , mId   (id)
{
}


/**
 * Creates a new GraphicalObject with the given id and 2D coordinates for
 * the bounding box.
 */
GraphicalObject::GraphicalObject (const std::string& id,
                                  double x, double y, double w, double h) :
    SBase      ()
  , mId         ( id )
  , mBoundingBox( BoundingBox("", x, y, 0.0, w, h, 0.0) )
{
}


/**
 * Creates a new GraphicalObject with the given id and 3D coordinates for
 * the bounding box.
 */
GraphicalObject::GraphicalObject (const std::string& id,
                                  double x, double y, double z,
                                  double w, double h, double d) :
    SBase      ()
  , mId         ( id )
  , mBoundingBox( BoundingBox("", x, y, z, w, h, d) )
{
}


/**
 * Creates a new GraphicalObject with the given id and 3D coordinates for
 * the bounding box.
 */
GraphicalObject::GraphicalObject (const std::string& id,
                                  const Point*       p,
                                  const Dimensions*  d) : 
    SBase      ()
  , mId         ( id )
  , mBoundingBox( BoundingBox("", p, d) )
{
}


/**
 * Creates a new GraphicalObject with the given id and 3D coordinates for
 * the bounding box.
 */
GraphicalObject::GraphicalObject (const std::string& id, const BoundingBox* bb)
  : SBase      ()
  , mId         ( id )
{
    if(bb)
    {
        this->mBoundingBox=*bb;
    }
}

/**
 * Creates a new GraphicalObject from the given XMLNode
 */
GraphicalObject::GraphicalObject(const XMLNode& node)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
    this->readAttributes(attributes);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="boundingBox")
        {
            this->mBoundingBox=BoundingBox(*child);
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
 * Destructor.
 */ 
GraphicalObject::~GraphicalObject ()
{
}


/**
 * Gets the id for the GraphicalObject.
 */
const std::string&
GraphicalObject::getId () const
{
  return this->mId;
}

/**
 * returns true if the id is not the empty string
 */
bool GraphicalObject::isSetId() const{
    return !this->mId.empty();
}


/**
 * Sets the id for the GraphicalObject.
 */
void
GraphicalObject::setId (const std::string& id)
{
  this->mId = id;
}


/**
 * Sets the boundingbox for the GraphicalObject.
 */ 
void
GraphicalObject::setBoundingBox (const BoundingBox* bb)
{
  if(bb==NULL) return;  
  this->mBoundingBox = *bb;
}


/**
 * Returns the bounding box for the GraphicalObject.
 */ 
const BoundingBox*
GraphicalObject::getBoundingBox () const
{
  return &this->mBoundingBox;
} 


/**
 * Returns the bounding box for the GraphicalObject.
 */ 
BoundingBox*
GraphicalObject::getBoundingBox ()
{
  return &this->mBoundingBox;
}


/**
 * Does nothing. No defaults are defined for GraphicalObject.
 */ 
void
GraphicalObject::initDefaults ()
{
}

/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string& GraphicalObject::getElementName () const 
{
  static const std::string name = "graphicalObject";
  return name;
}

/**
 * @return a (deep) copy of this Model.
 */
SBase* 
GraphicalObject::clone () const
{
    return new GraphicalObject(*this);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
GraphicalObject::createObject (XMLInputStream& stream)
{

  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "boundingBox")
  {
    object = &mBoundingBox;
  }

  return object;
}

/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */

void GraphicalObject::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  attributes.readInto("id", mId,this->getErrorLog(),true);
}

/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
GraphicalObject::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  mBoundingBox.write(stream);

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
void GraphicalObject::writeAttributes (XMLOutputStream& stream) const
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
GraphicalObject::getTypeCode () const
{
  return SBML_LAYOUT_GRAPHICALOBJECT;
}

/**
 * Accepts the given SBMLVisitor.
 */
bool
GraphicalObject::accept (SBMLVisitor& v) const
{
  /*  
  bool result=v.visit(*this);
  this->mBoundingBox.accept(v);
  v.leave(*this);
  */
  return false;
}



/**
 * Creates a new GraphicalObject.
 */
LIBSBML_EXTERN
GraphicalObject_t *
GraphicalObject_create (void)
{
  return new(std::nothrow) GraphicalObject;
}


/**
 * Creates a GraphicalObject from a template.
 */
LIBSBML_EXTERN
GraphicalObject_t *
GraphicalObject_createFrom (const GraphicalObject_t *temp)
{
  return new(std::nothrow) GraphicalObject(*temp);
}

/**
 * Frees all memory taken up by the GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject_free (GraphicalObject_t *go)
{
  delete go;
}


/**
 * Sets the id for the GraphicalObject.
 */
LIBSBML_EXTERN
void
GraphicalObject_setId (GraphicalObject_t *go, const char *id)
{
    go->setId( id ? id : "" );
}


/**
 * Gets the id for the given GraphicalObject.
 */
LIBSBML_EXTERN
const char *
GraphicalObject_getId (const GraphicalObject_t *go)
{
    return go->isSetId() ? go->getId().c_str() : NULL;
}


/**
 * Sets the boundingbox for the GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject_setBoundingBox (GraphicalObject_t *go, const BoundingBox_t *bb)
{
  go->setBoundingBox(bb);
}


/**
 * Returns the bounding box for the GraphicalObject.
 */ 
LIBSBML_EXTERN
BoundingBox_t *
GraphicalObject_getBoundingBox (GraphicalObject_t *go)
{
  return go->getBoundingBox();
}


/**
 * Does nothing. No defaults are defined for GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject_initDefaults (GraphicalObject_t *go)
{
  go->initDefaults();
}

/**
 * Returns zero if the id is the empty string
 */
LIBSBML_EXTERN
int
GraphicalObject_isSetId(GraphicalObject_t* go){
    return (int)go->isSetId();
}

/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
GraphicalObject_t *
GraphicalObject_clone (const GraphicalObject_t *m)
{
  return static_cast<GraphicalObject*>( m->clone() );
}


