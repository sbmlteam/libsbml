/**
 * Filename    : CubicBezier.cpp
 * Description : SBML Layout CubicBezier source
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


#include "CubicBezier.h"
#include "LayoutUtilities.h"

#include <sbml/SBMLNamespaces.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * Creates a CubicBezier and returns the pointer.
 */
CubicBezier::CubicBezier() : LineSegment()
{
  this->mStartPoint.setElementName("start");
  this->mEndPoint.setElementName("end");
  this->mBasePoint1.setElementName("basePoint1");
  this->mBasePoint2.setElementName("basePoint2");
}

CubicBezier::CubicBezier (unsigned int level, unsigned int version):
   LineSegment (level, version)
{
  this->mStartPoint.setElementName("start");
  this->mEndPoint.setElementName("end");
  this->mBasePoint1.setElementName("basePoint1");
  this->mBasePoint2.setElementName("basePoint2");
}

                          
CubicBezier::CubicBezier (SBMLNamespaces *sbmlns) :
   LineSegment (sbmlns)
{
  this->mStartPoint.setElementName("start");
  this->mEndPoint.setElementName("end");
  this->mBasePoint1.setElementName("basePoint1");
  this->mBasePoint2.setElementName("basePoint2");
}
 
/**
 * Creates a CubicBezier with the given 2D coordinates and returns the
 * pointer.
 */
CubicBezier::CubicBezier (double x1, double y1, double x2, double y2)
  : LineSegment( x1, y1, 0.0, x2, y2, 0.0 )
{
  this->straighten();
  this->mBasePoint1.setElementName("basePoint1");
  this->mBasePoint2.setElementName("basePoint2");
}


/**
 * Creates a CubicBezier with the given 3D coordinates and returns the
 * pointer.
 */
CubicBezier::CubicBezier (double x1, double y1, double z1,
                          double x2, double y2, double z2)
  : LineSegment( x1, y1, z1, x2, y2, z2 )
{
  this->straighten();
  this->mBasePoint1.setElementName("basePoint1");
  this->mBasePoint2.setElementName("basePoint2");
}

/**
 * Copy constructor.
 */
CubicBezier::CubicBezier(const CubicBezier& orig):LineSegment()
{
//    this->mId=orig.mId;
    this->mStartPoint=orig.mStartPoint;
    this->mEndPoint=orig.mEndPoint;
    this->mBasePoint1=orig.mBasePoint1;
    this->mBasePoint2=orig.mBasePoint2;
    // attributes of SBase
//    this->mId=orig.mId;
//    this->mName=orig.mName;
    this->mMetaId=orig.mMetaId;
    if(orig.mNotes) this->mNotes=new XMLNode(*const_cast<CubicBezier&>(orig).getNotes());
    if(orig.mAnnotation) this->mAnnotation=new XMLNode(*const_cast<CubicBezier&>(orig).mAnnotation);
    this->mSBML=orig.mSBML;
    this->mSBOTerm=orig.mSBOTerm;
    this->mLine=orig.mLine;
    this->mColumn=orig.mColumn;

    if(orig.mCVTerms)
    {
      this->mCVTerms=new List();
      unsigned int i,iMax=orig.mCVTerms->getSize();
      for(i=0;i<iMax;++i)
      {
        this->mCVTerms->add(static_cast<CVTerm*>(orig.mCVTerms->get(i))->clone());
      }
    }
}


/**
 * Assignment operator.
 */
CubicBezier& CubicBezier::operator=(const CubicBezier& orig)
{
  if(&orig!=this)
  {
    this->mStartPoint=orig.mStartPoint;
    this->mEndPoint=orig.mEndPoint;
    this->mBasePoint1=orig.mBasePoint1;
    this->mBasePoint2=orig.mBasePoint2;
    this->mMetaId=orig.mMetaId;
    delete this->mNotes;
    this->mNotes=NULL;
    if(orig.mNotes) this->mNotes=new XMLNode(*const_cast<CubicBezier&>(orig).getNotes());
    delete this->mAnnotation;
    this->mAnnotation=NULL;
    if(orig.mAnnotation) this->mAnnotation=new XMLNode(*const_cast<CubicBezier&>(orig).mAnnotation);
    this->mSBML=orig.mSBML;
    this->mSBOTerm=orig.mSBOTerm;
    this->mLine=orig.mLine;
    this->mColumn=orig.mColumn;
    delete this->mCVTerms;
    this->mCVTerms=NULL;
    if(orig.mCVTerms)
    {
      this->mCVTerms=new List();
      unsigned int i,iMax=orig.mCVTerms->getSize();
      for(i=0;i<iMax;++i)
      {
        this->mCVTerms->add(static_cast<CVTerm*>(orig.mCVTerms->get(i))->clone());
      }
    }
  }
  
  return *this;
}



/**
 * Makes a line from a CubicBezier by setting both base points into the
 * middle between the start and the end point.
 */
void CubicBezier::straighten ()
{
  double x = (this->mEndPoint.getXOffset()+this->mStartPoint.getXOffset()) / 2.0;
  double y = (this->mEndPoint.getYOffset()+this->mStartPoint.getYOffset()) / 2.0;
  double z = (this->mEndPoint.getZOffset()+this->mStartPoint.getZOffset()) / 2.0;

  this->mBasePoint1.setOffsets(x, y, z);
  this->mBasePoint2.setOffsets(x, y, z);
}


/**
 * Creates a CubicBezier with the given points and returns the pointer.
 */
CubicBezier::CubicBezier (const Point* start, const Point* end)
  : LineSegment(start, end)
{
  this->straighten();
  this->mBasePoint1.setElementName("basePoint1");
  this->mBasePoint2.setElementName("basePoint2");
}


/**
 * Creates a CubicBezier with the given points and returns the pointer.
 */
CubicBezier::CubicBezier (const Point* start, const Point* base1,
                          const Point* base2, const Point* end)
  : LineSegment(start ,end )
{
    if(base1 && base2 && start && end)
    {
      this->mBasePoint1=*base1;
      this->mBasePoint1.setElementName("basePoint1");
      this->mBasePoint2=*base2;
      this->mBasePoint2.setElementName("basePoint2");
    }
    else
    {
        this->mStartPoint=Point();
        this->mEndPoint=Point();
    }
}

/**
 * Creates a new CubicBezier from the given XMLNode
 */
CubicBezier::CubicBezier(const XMLNode& node)
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
        else if(childName=="basePoint1")
        {
            this->mBasePoint1=Point(*child);
        }
        else if(childName=="basePoint2")
        {
            this->mBasePoint2=Point(*child);
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
CubicBezier::~CubicBezier ()
{
}


/**
 * Calls initDefaults from LineSegment.
 */ 
void
CubicBezier::initDefaults()
{
  LineSegment::initDefaults();
}


/**
 * Returns the first base point of the curve (the one closer to the
 * starting point).
 */ 
const Point*
CubicBezier::getBasePoint1() const
{
  return &this->mBasePoint1;
}


/**
 * Returns the first base point of the curve (the one closer to the
 * starting point).
 */ 
Point*
CubicBezier::getBasePoint1 ()
{
  return &this->mBasePoint1;
}


/**
 * Initializes first base point with a copy of the given point.
 */
void
CubicBezier::setBasePoint1 (const Point* p)
{
  if(p)
  {  
    this->mBasePoint1 = *p;
    this->mBasePoint1.setElementName("basePoint1");
  }
}


/**
 * Initializes first base point with the given ccordinates.
 */
void
CubicBezier::setBasePoint1 (double x, double y, double z)
{
  this->mBasePoint1.setOffsets(x, y ,z);
}


/**
 * Returns the second base point of the curve (the one closer to the
 * starting point).
 */ 
const Point*
CubicBezier::getBasePoint2 () const
{
  return &this->mBasePoint2;
}


/**
 * Returns the second base point of the curve (the one closer to the
 * starting point).
 */ 
Point*
CubicBezier::getBasePoint2 ()
{
  return &this->mBasePoint2;
}


/**
 * Initializes second base point with a copy of the given point.
 */
void CubicBezier::setBasePoint2 (const Point* p)
{
  if(p)
  {  
    this->mBasePoint2 = *p;
    this->mBasePoint2.setElementName("basePoint2");
  }
}


/**
 * Initializes second base point with the given ccordinates.
 */
void
CubicBezier::setBasePoint2 (double x, double y, double z)
{
  this->mBasePoint2.setOffsets(x, y, z);
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string& CubicBezier::getElementName () const 
{
  static const std::string name = "curveSegment";
  return name;
}

/**
 * @return a (deep) copy of this Model.
 */
SBase* 
CubicBezier::clone () const
{
    return new CubicBezier(*this);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
CubicBezier::createObject (XMLInputStream& stream)
{

  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "basePoint1")
  {
    object = &mBasePoint1;
  }
  else if(name == "basePoint2")
  {
    object = &mBasePoint2;
  }
  else
  {
      object = LineSegment::createObject(stream);
  }
 
  return object;
}

/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */

void CubicBezier::readAttributes (const XMLAttributes& attributes)
{
  LineSegment::readAttributes(attributes);
}

/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
CubicBezier::writeElements (XMLOutputStream& stream) const
{
  LineSegment::writeElements(stream);
  mBasePoint1.write(stream);
  mBasePoint2.write(stream);
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
void CubicBezier::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);
  stream.writeAttribute("xsi:type", "CubicBezier");
}

/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
CubicBezier::getTypeCode () const
{
  return SBML_LAYOUT_CUBICBEZIER;
}


/**
 * Accepts the given SBMLVisitor.

bool
CubicBezier::accept (SBMLVisitor& v) const
{
  bool result=v.visit(*this);
  this->mStartPoint.accept(v);
  this->mBasePoint1.accept(v);
  this->mBasePoint2.accept(v);
  this->mEndPoint.accept(v);
  v.leave(*this);
  return result;
}
*/

/**
 * Creates an XMLNode object from this.
 */
XMLNode CubicBezier::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("curveSegment", "", "");
  XMLAttributes att = XMLAttributes();
  // add the SBase Ids
  addSBaseAttributes(*this,att);
  att.add("type","CubicBezier","http://www.w3.org/2001/XMLSchema-instance","xsi");  XMLToken token = XMLToken(triple, att, xmlns); 
  XMLNode node(token);
  // add the notes and annotations
  if(this->mNotes) node.addChild(*this->mNotes);
  if(this->mAnnotation) node.addChild(*this->mAnnotation);
  // add start point
  node.addChild(this->mStartPoint.toXML("start"));
  // add end point
  node.addChild(this->mEndPoint.toXML("end"));
  // add start point
  node.addChild(this->mBasePoint1.toXML("basePoint1"));
  // add end point
  node.addChild(this->mBasePoint2.toXML("basePoint2"));
  return node;
}



/**
 * Creates a CubicBezier and returns the pointer.
 */
LIBSBML_EXTERN
CubicBezier_t *
CubicBezier_create (void)
{
  return new(std::nothrow) CubicBezier;
}

/**
 * Creates a new CubicBezier_t structure using the given SBML @p 
 * level and @p version values and a set of XMLNamespaces.
 *
 * @param level an unsigned int, the SBML Level to assign to this 
 * CubicBezier
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CubicBezier
 * 
 * @param xmlns XMLNamespaces, a pointer to an array of XMLNamespaces to
 * assign to this CubicBezier
 *
 * @return a pointer to the newly created CubicBezier_t structure.
 *
 * @note Once a CubicBezier has been added to an SBMLDocument, the @p 
 * level, @p version and @p xmlns namespaces for the document @em override 
 * those used to create the Reaction.  Despite this, the ability 
 * to supply the values at creation time is an important aid to creating 
 * valid SBML.  Knowledge of the intended SBML Level and Version 
 * determine whether it is valid to assign a particular value to an 
 * attribute, or whether it is valid to add an object to an existing 
 * SBMLDocument.
 */
LIBSBML_EXTERN
CubicBezier_t *
CubicBezier_createWithLevelVersionAndNamespaces (unsigned int level,
              unsigned int version)
{
  return new(std::nothrow)CubicBezier(level, version );
}


/**
 * Creates a CubicBezier with the given points and returns the pointer.
 */
LIBSBML_EXTERN
CubicBezier_t *
CubicBezier_createWithPoints (const Point_t *start, const Point_t *base1,
                              const Point_t *base2, const Point_t *end)
{
  return new(std::nothrow)CubicBezier(start , base1, base2 , end );
}


/**
 * Creates a CubicBezier with the given coordinates and returns the
 * pointer.
 */
LIBSBML_EXTERN
CubicBezier_t *
CubicBezier_createWithCoordinates (double x1, double y1, double z1,
                                   double x2, double y2, double z2,
                                   double x3, double y3, double z3,
                                   double x4, double y4, double z4)
{
  Point* p1=new Point(x1,y1,z1);  
  Point* p2=new Point(x2,y2,z2);  
  Point* p3=new Point(x3,y3,z3);  
  Point* p4=new  Point(x4,y4,z4);  
  CubicBezier* cb=new(std::nothrow)CubicBezier( p1,p2,p3,p4);
  delete p1;
  delete p2;
  delete p3;
  delete p4;
  return cb;
}


/**
 * Creates a CubicBezier object from a template.
 */
LIBSBML_EXTERN
CubicBezier_t *
CubicBezier_createFrom (const CubicBezier_t *temp)
{
  return new(std::nothrow) CubicBezier(temp ? *temp : CubicBezier());
}


/**
 * Frees the memory for the cubic bezier.
 */
LIBSBML_EXTERN
void
CubicBezier_free (CubicBezier_t *cb)
{
  delete cb;
}


/**
 * Initializes start point with a copy of the given point.
 */
LIBSBML_EXTERN
void
CubicBezier_setStart (CubicBezier_t *cb, const Point_t *start)
{
  LineSegment_setStart((LineSegment_t*)cb, start);
}


/**
 * Returns the starting point of the curve.
 */ 
LIBSBML_EXTERN
Point_t *
CubicBezier_getStart (CubicBezier_t *cb)
{
  return LineSegment_getStart(cb);
}


/**
 * Initializes end point with a copy of the given point.
 */
LIBSBML_EXTERN
void
CubicBezier_setEnd (CubicBezier_t *cb, const Point_t *end)
{
  LineSegment_setEnd((LineSegment_t*)cb, end);
}


/**
 * Returns the end point of the curve.
 */ 
LIBSBML_EXTERN
Point_t *
CubicBezier_getEnd (CubicBezier_t *cb)
{
  return LineSegment_getEnd(cb);
}


/**
 * Initializes the first base point with a copy of the given point.
 */
LIBSBML_EXTERN
void
CubicBezier_setBasePoint1 (CubicBezier_t *cb, const Point_t *point)
{
  cb->setBasePoint1(point);
}


/**
 * Returns the first base point of the curve (the one closer to the
 * starting point).
 */ 
LIBSBML_EXTERN
Point_t *
CubicBezier_getBasePoint1 (CubicBezier_t *cb)
{
  return cb->getBasePoint1();
}


/**
 * Initializes the second base point with a copy of the given point.
 */
LIBSBML_EXTERN
void
CubicBezier_setBasePoint2 (CubicBezier_t *cb, const Point_t *point)
{
  cb->setBasePoint2(point );
}


/**
 * Returns the second base point of the curve (the one closer to the
 * starting point).
 */ 
LIBSBML_EXTERN
Point_t *
CubicBezier_getBasePoint2 (CubicBezier_t *cb)
{
  return cb->getBasePoint2();
}


/**
 * Calls initDefaults from LineSegment.
 */ 
LIBSBML_EXTERN
void
CubicBezier_initDefaults (CubicBezier_t *cb)
{
  cb->initDefaults();
}

/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
CubicBezier_t *
CubicBezier_clone (const CubicBezier_t *m)
{
  return static_cast<CubicBezier*>( m->clone() );
}


/**
 * Returns non-zero if the id is set
 */
LIBSBML_EXTERN
int
CubicBezier_isSetId (const CubicBezier_t *cb)
{
  return static_cast <int> (cb->isSetId());
}

/**
 * Returns the id
 */
LIBSBML_EXTERN
const char *
CubicBezier_getId (const CubicBezier_t *cb)
{
  return cb->isSetId() ? cb->getId().c_str() : NULL;
}

/**
 * Sets the id
 */
LIBSBML_EXTERN
int
CubicBezier_setId (CubicBezier_t *cb, const char *sid)
{
  return (sid == NULL) ? cb->setId("") : cb->setId(sid);
}

/**
 * Unsets the id
 */
LIBSBML_EXTERN
void
CubicBezier_unsetId (CubicBezier_t *cb)
{
  cb->unsetId();
}

LIBSBML_CPP_NAMESPACE_END

