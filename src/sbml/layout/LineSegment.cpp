/**
 * Filename    : LineSegment.cpp
 * Description : SBML Layout LineSegment source
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


#include "LineSegment.h"
#include "LayoutUtilities.h"
#include <sbml/SBMLNamespaces.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * Creates a line segment with both points set to (0.0,0.0,0.0)
 */ 
LineSegment::LineSegment () :
    SBase     ("","",-1)
  , mStartPoint( 0.0, 0.0, 0.0 )
  , mEndPoint  ( 0.0, 0.0, 0.0 )
{
  this->mStartPoint.setElementName("start");
  this->mEndPoint.setElementName("end");
}

LineSegment::LineSegment (unsigned int level, unsigned int version):
   SBase (level , version)
  , mStartPoint( 0.0, 0.0, 0.0 )
  , mEndPoint  ( 0.0, 0.0, 0.0 )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
  this->mStartPoint.setElementName("start");
  this->mEndPoint.setElementName("end");
}

                          
LineSegment::LineSegment (SBMLNamespaces *sbmlns) :
   SBase (sbmlns)
  , mStartPoint( 0.0, 0.0, 0.0 )
  , mEndPoint  ( 0.0, 0.0, 0.0 )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
  this->mStartPoint.setElementName("start");
  this->mEndPoint.setElementName("end");
}

/**
 * Creates a new line segment with the given 2D coordinates.
 */ 
LineSegment::LineSegment (double x1, double y1, double x2, double y2) :
    SBase     ()
  , mStartPoint(x1, y1, 0.0 )
  , mEndPoint  ( x2, y2, 0.0 )
{
  this->mStartPoint.setElementName("start");
  this->mEndPoint.setElementName("end");
}


/**
 * Creates a new line segment with the given 3D coordinates.
 */ 
LineSegment::LineSegment (double x1, double y1, double z1,
                          double x2, double y2, double z2) :
    SBase()
  , mStartPoint( x1, y1, z1)
  , mEndPoint  ( x2, y2, z2)
{
  this->mStartPoint.setElementName("start");
  this->mEndPoint.setElementName("end");
}

/**
 * Copy constructor.
 */
LineSegment::LineSegment(const LineSegment& orig):SBase(orig)
{
    this->mStartPoint=orig.mStartPoint;
    this->mEndPoint=orig.mEndPoint;
  this->mId = orig.mId;
}


/**
 * Assignment operator.
 */
LineSegment& LineSegment::operator=(const LineSegment& orig)
{
  if(&orig!=this)
  {
    this->SBase::operator=(orig);
    this->mId = orig.mId;
    this->mStartPoint=orig.mStartPoint;
    this->mEndPoint=orig.mEndPoint;
  }
  
  return *this;
}


/**
 * Creates a new line segment with the two given points.
 */ 
LineSegment::LineSegment (const Point* start, const Point* end) : 
    SBase     ()
{
  if(start && end)
  {  
    this->mStartPoint=*start;  
    this->mStartPoint.setElementName("start");
    this->mEndPoint=*end;  
    this->mEndPoint.setElementName("end");
  }
}

/**
 * Creates a new LineSegment from the given XMLNode
 */
LineSegment::LineSegment(const XMLNode& node)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
    this->readAttributes(attributes);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="start")
        {
            this->mStartPoint=Point(*child);
        }
        else if(childName=="end")
        {
            this->mEndPoint=Point(*child);
        }
        else if(childName=="annotation")
        {
            this->mAnnotation=new XMLNode(*child);
        }
        else if(childName=="notes")
        {
            this->mNotes=new XMLNode(*child);
        }
        else
        {
            //throw;
        }
        ++n;
    }    
}


/**
 * Destructor.
 */ 
LineSegment::~LineSegment ()
{
}


/**
 * Does nothing since no defaults are defined for LineSegment.
 */ 
void LineSegment::initDefaults ()
{
}


/**
  * Returns the value of the "id" attribute of this BoundingBox.
  */
const std::string& LineSegment::getId () const
{
  return mId;
}


/**
  * Predicate returning @c true or @c false depending on whether this
  * BoundingBox's "id" attribute has been set.
  */
bool LineSegment::isSetId () const
{
  return (mId.empty() == false);
}

/**
  * Sets the value of the "id" attribute of this BoundingBox.
  */
int LineSegment::setId (const std::string& id)
{
  if (!(SyntaxChecker::isValidSBMLSId(id)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mId = id;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
  * Unsets the value of the "id" attribute of this BoundingBox.
  */
void LineSegment::unsetId ()
{
  mId.erase();
}


/**
 * Returns the start point of the line.
 */ 
const Point*
LineSegment::getStart () const
{
  return &this->mStartPoint;
}


/**
 * Returns the start point of the line.
 */ 
Point*
LineSegment::getStart()
{
  return &this->mStartPoint;
}


/**
 * Initializes the start point with a copy of the given Point object.
 */
void
LineSegment::setStart (const Point* start)
{
  if(start)
  {  
    this->mStartPoint=*start;
    this->mStartPoint.setElementName("start");
  }
}


/**
 * Initializes the start point with the given coordinates.
 */
void
LineSegment::setStart (double x, double y, double z)
{
  this->mStartPoint.setOffsets(x, y, z);
}


/**
 * Returns the end point of the line.
 */ 
const Point*
LineSegment::getEnd () const
{
  return &this->mEndPoint;
}


/**
 * Returns the end point of the line.
 */ 
Point*
LineSegment::getEnd ()
{
  return &this->mEndPoint;
}


/**
 * Initializes the end point with a copy of the given Point object.
 */
void
LineSegment::setEnd (const Point* end)
{
  if(end)
  {  
    this->mEndPoint = *end;
    this->mEndPoint.setElementName("end");
  }
}


/**
 * Initializes the end point with the given coordinates.
 */
void
LineSegment::setEnd (double x, double y, double z)
{
  this->mEndPoint.setOffsets(x, y, z);
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string& LineSegment::getElementName () const 
{
  static const std::string name = "curveSegment";
  return name;
}

/**
 * @return a (deep) copy of this Model.
 */
SBase* 
LineSegment::clone () const
{
    return new LineSegment(*this);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
LineSegment::createObject (XMLInputStream& stream)
{

  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "start")
  {
    object = &mStartPoint;
  }
  else if(name == "end")
  {
    object = &mEndPoint;
  }

 
  return object;
}

/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */

void LineSegment::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

}

/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
LineSegment::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  mStartPoint.write(stream);
  mEndPoint.write(stream);
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
void LineSegment::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);
  stream.writeAttribute("xsi:type", "LineSegment");
}

/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
LineSegment::getTypeCode () const
{
  return SBML_LAYOUT_LINESEGMENT;
}

/**
 * Accepts the given SBMLVisitor.
 */
bool
LineSegment::accept (SBMLVisitor& v) const
{
   /*
  bool result=v.visit(*this);
  this->mStartPoint.accept(v);
  this->mEndPoint.accept(v);
  v.leave(*this);*/
  return false;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode LineSegment::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("curveSegment", "", "");
  XMLAttributes att = XMLAttributes();
  // add the SBase Ids
  addSBaseAttributes(*this,att);
  att.add("type","LineSegment","http://www.w3.org/2001/XMLSchema-instance","xsi");
  XMLToken token = XMLToken(triple, att, xmlns); 
  XMLNode node(token);
  // add the notes and annotations
  if(this->mNotes) node.addChild(*this->mNotes);
  if(this->mAnnotation) node.addChild(*this->mAnnotation);
  // add start point
  node.addChild(this->mStartPoint.toXML("start"));
  // add end point
  node.addChild(this->mEndPoint.toXML("end"));
  return node;
}



/**
 * Creates a LineSegment and returns the pointer.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_create (void)
{
  return new(std::nothrow) LineSegment;
}

/** @cond doxygen-libsbml-internal */
/**
 * Creates a new LineSegment_t structure using the given SBML @p 
 * level and @p version values and a set of XMLNamespaces.
 *
 * @param level an unsigned int, the SBML Level to assign to this 
 * LineSegment
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * LineSegment
 * 
 * @param xmlns XMLNamespaces, a pointer to an array of XMLNamespaces to
 * assign to this LineSegment
 *
 * @return a pointer to the newly created LineSegment_t structure.
 *
 * @note Once a LineSegment has been added to an SBMLDocument, the @p 
 * level, @p version and @p xmlns namespaces for the document @em override 
 * those used to create the Reaction.  Despite this, the ability 
 * to supply the values at creation time is an important aid to creating 
 * valid SBML.  Knowledge of the intended SBML Level and Version 
 * determine whether it is valid to assign a particular value to an 
 * attribute, or whether it is valid to add an object to an existing 
 * SBMLDocument.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_createWithLevelVersionAndNamespaces (unsigned int level,
              unsigned int version)
{
  return new(std::nothrow) LineSegment(level, version);
}
/** @endcond */


/**
 * Creates a LineSegment from a template.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_createFrom (const LineSegment_t *temp)
{
  return new(std::nothrow) LineSegment(temp ? *temp : LineSegment());
}


/**
 * Creates a LineSegment with the given points and returns the pointer.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_createWithPoints (const Point_t *start, const Point_t *end)
{
  return new(std::nothrow) LineSegment (start, end );
}


/**
 * Creates a LineSegment with the given coordinates and returns the pointer.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_createWithCoordinates (double x1, double y1, double z1,
                                   double x2, double y2, double z2)
{
  return new(std::nothrow) LineSegment(x1, y1, z1, x2, y2, z2);
}


/**
 * Frees the memory for the line segment.
 */
LIBSBML_EXTERN
void
LineSegment_free (LineSegment_t *ls)
{
  delete ls;
}


/**
 * Initializes the start point with a copy of the given Point object.
 */
LIBSBML_EXTERN
void
LineSegment_setStart (LineSegment_t *ls, const Point_t *start)
{
  ls->setStart(start);
}


/**
 * Initializes the end point with a copy of the given Point object.
 */
LIBSBML_EXTERN
void
LineSegment_setEnd (LineSegment_t *ls, const Point_t *end)
{
  ls->setEnd(end);
}


/**
 * Returns the start point of the line.
 */ 
LIBSBML_EXTERN
Point_t *
LineSegment_getStart (LineSegment_t *ls)
{
  return ls->getStart();
}


/**
 * Returns the end point of the line.
 */ 
LIBSBML_EXTERN
Point_t *
LineSegment_getEnd (LineSegment_t *ls)
{
  return ls->getEnd();
}


/**
 * Does nothing since no defaults are defined for LineSegment.
 */ 
LIBSBML_EXTERN
void
LineSegment_initDefaults (LineSegment_t *ls)
{
  ls->initDefaults();
}

/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
LineSegment_t *
LineSegment_clone (const LineSegment_t *m)
{
  return static_cast<LineSegment*>( m->clone() );
}

/**
 * Returns non-zero if the id is set
 */
LIBSBML_EXTERN
int
LineSegment_isSetId (const LineSegment_t *ls)
{
  return static_cast <int> (ls->isSetId());
}

/**
 * Returns the id
 */
LIBSBML_EXTERN
const char *
LineSegment_getId (const LineSegment_t *ls)
{
  return ls->isSetId() ? ls->getId().c_str() : NULL;
}

/**
 * Sets the id
 */
LIBSBML_EXTERN
int
LineSegment_setId (LineSegment_t *ls, const char *sid)
{
  return (sid == NULL) ? ls->setId("") : ls->setId(sid);
}

/**
 * Unsets the id
 */
LIBSBML_EXTERN
void
LineSegment_unsetId (LineSegment_t *ls)
{
  ls->unsetId();
}

LIBSBML_CPP_NAMESPACE_END

