/**
 * Filename    : Curve.cpp
 * Description : SBML Layout Curve source
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

#include <assert.h>

#include "Curve.h"

#include "LineSegment.h"
#include "CubicBezier.h"
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
 * Creates a curve with an empty list of segments.
 */ 
Curve::Curve () : SBase ("", "", -1)
{
}

Curve::Curve (unsigned int level, unsigned int version):
   SBase (level , version)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}

                          
Curve::Curve (SBMLNamespaces *sbmlns) :
   SBase (sbmlns)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}
 

/**
 * Creates a new ReactionGlyph from the given XMLNode
 */
Curve::Curve(const XMLNode& node)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
    this->readAttributes(attributes);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="annotation")
        {
            this->mAnnotation=new XMLNode(*child);
        }
        else if(childName=="notes")
        {
            this->mNotes=new XMLNode(*child);
        }
        else if(childName=="listOfCurveSegments")
        {
            const XMLNode* innerChild;
            unsigned int i=0,iMax=child->getNumChildren();
            while(i<iMax)
            {
                innerChild=&child->getChild(i);
                const std::string innerChildName=innerChild->getName();
                if(innerChildName=="curveSegment")
                {
                    // get the type
                    const XMLAttributes& innerAttributes=innerChild->getAttributes();
                    int typeIndex=innerAttributes.getIndex("type");
                    if(typeIndex==-1 || innerAttributes.getURI(typeIndex)!="http://www.w3.org/2001/XMLSchema-instance")
                    {
                        // throw
                        ++i;
                        continue;
                    }
                    if(innerAttributes.getValue(typeIndex)=="LineSegment")
                    {
                      this->mCurveSegments.appendAndOwn(new LineSegment(*innerChild));
                    }
                    else if(innerAttributes.getValue(typeIndex)=="CubicBezier")
                    {
                      this->mCurveSegments.appendAndOwn(new CubicBezier(*innerChild));
                    }
                    else
                    {
                        // throw
                    }
                }
                else if(innerChildName=="annotation")
                {
                    this->mCurveSegments.setAnnotation(new XMLNode(*innerChild));
                }
                else if(innerChildName=="notes")
                {
                    this->mCurveSegments.setNotes(new XMLNode(*innerChild));
                }
                else
                {
                    // throw
                }
                ++i;
            }
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
Curve::~Curve ()
{
}


/**
 * Does nothing since no defaults are defined for Curve.
 */ 
void Curve::initDefaults ()
{
}


/**
 * Returns a reference to the ListOf object that holds all the curve
 * segments.
 */
const ListOfLineSegments*
Curve::getListOfCurveSegments () const
{
  return & this->mCurveSegments;
}


/**
 * Returns a reference to the ListOf object that holds all the curve
 * segments.
 */
ListOfLineSegments*
Curve::getListOfCurveSegments ()
{
  return &this->mCurveSegments;
}


/**
 * Returns a pointer to the curve segment with the given index.  If the
 * index is invalid, NULL is returned.
 */  
const LineSegment*
Curve::getCurveSegment (unsigned int index) const
{
  return dynamic_cast<const LineSegment*>( this->mCurveSegments.get(index) );
}


/**
 * Returns a pointer to the curve segment with the given index.  If the
 * index is invalid, NULL is returned.
 */  
LineSegment*
Curve::getCurveSegment (unsigned int index)
{
  return static_cast<LineSegment*>( this->mCurveSegments.get(index) );
}


/**
 * Adds a new CurveSegment to the end of the list.
 */ 
int
Curve::addCurveSegment (const LineSegment* segment)
{
  if (segment == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(segment->hasRequiredAttributes()) || !(segment->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != segment->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != segment->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else
  {
    /* if the ListOf is empty it doesnt know its parent */
    if (mCurveSegments.size() == 0)
    {
       mCurveSegments.setSBMLDocument(this->getSBMLDocument());
       mCurveSegments.setParentSBMLObject(this);
    }
    this->mCurveSegments.append(segment);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Returns the number of curve segments.
 */ 
unsigned int
Curve::getNumCurveSegments () const
{
  return this->mCurveSegments.size();
}


/**
 * Creates a new LineSegment and adds it to the end of the list.  A
 * reference to the new LineSegment object is returned.
 */
LineSegment*
Curve::createLineSegment ()
{
  LineSegment* ls = NULL;
  try
  {
    ls = new LineSegment(getSBMLNamespaces());
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
  }
  /* if the ListOf is empty it doesnt know its parent */
  if (mCurveSegments.size() == 0)
  {
    mCurveSegments.setSBMLDocument(this->getSBMLDocument());
    mCurveSegments.setParentSBMLObject(this);
  }

  if(ls != NULL)
  {
      this->mCurveSegments.appendAndOwn(ls);
  }
  return ls;
}


/**
 * Creates a new CubicBezier and adds it to the end of the list.  A
 * reference to the new CubicBezier object is returned.
 */
CubicBezier* Curve::createCubicBezier ()
{
  CubicBezier* cb = NULL;
  try
  {
    cb = new CubicBezier(getSBMLNamespaces());
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
  }
  /* if the ListOf is empty it doesnt know its parent */
  if (mCurveSegments.size() == 0)
  {
    mCurveSegments.setSBMLDocument(this->getSBMLDocument());
    mCurveSegments.setParentSBMLObject(this);
  }

  if(cb != NULL)
  {
      this->mCurveSegments.appendAndOwn(cb);
  }
  return cb;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string& Curve::getElementName () const 
{
  static const std::string name = "curve";
  return name;
}

/**
 * @return a (deep) copy of this Model.
 */
SBase* 
Curve::clone () const
{
    return new Curve(*this);
}


/**
 * Copy constructor.
 */
Curve::Curve(const Curve& source):SBase(source)
{
    // copy the line segments
    this->mCurveSegments=*source.getListOfCurveSegments();
}

/**
 * Assignment operator.
 */
Curve& Curve::operator=(const Curve& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    // copy the line segments
    this->mCurveSegments=*source.getListOfCurveSegments();
    
  }
  
  return *this;
}



/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
Curve::createObject (XMLInputStream& stream)
{

  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "listOfCurveSegments")
  {
    object = &mCurveSegments;
  }
 
  return object;
}

/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */

void Curve::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

}
/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Curve::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  mCurveSegments.write(stream);

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
void Curve::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode Curve::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("curve", "", "");
  XMLAttributes att = XMLAttributes();
  // add the SBase Ids
  addSBaseAttributes(*this,att);
  XMLToken token = XMLToken(triple, att, xmlns); 
  XMLNode node(token);
  // add the notes and annotations
  bool end=true;
  if(this->mNotes)
  {
      node.addChild(*this->mNotes);
      end=false;
  }
  if(this->mAnnotation)
  {
      node.addChild(*this->mAnnotation);
      end=false;
  }
  // add the list of line segments
  if(this->mCurveSegments.size()>0)
  {
      node.addChild(this->mCurveSegments.toXML());
      end=false;
  }
  if(end==true) node.setEnd();
  return node;
}


ListOfLineSegments& ListOfLineSegments::operator=(const ListOfLineSegments& source)
{
  if(&source!=this)
  {
    copySBaseAttributes(source,*this);
    this->mLine=source.getLine();
    this->mColumn=source.getColumn();
    //if(this->mNamespaces!=NULL)
    //{
    //    delete this->mNamespaces;
    //    this->mNamespaces=NULL;
    //}
    //if(source.getNamespaces()!=NULL)
    //{
    //  this->mNamespaces=new XMLNamespaces(*source.getNamespaces());
    //}
    // clear the old list
    unsigned int i=0,iMax=this->size();
    while(i<iMax)
    {
        LineSegment* ls=static_cast<LineSegment*>(this->remove(0));
        delete ls;
        ++i;
    }
    i=0;
    iMax=source.size();
    while(i<iMax)
    {
      this->append(source.get(i));
      ++i;
    }
  }
  
  return *this;
}

ListOfLineSegments::ListOfLineSegments(const ListOfLineSegments& source)
{
    ListOfLineSegments::operator=(source);
}


/**
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
SBase*
ListOfLineSegments::clone () const
{
  return new ListOfLineSegments(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfLineSegments::getItemTypeCode () const
{
  return SBML_LAYOUT_LINESEGMENT;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfLineSegments::getElementName () const
{
  static const std::string name = "listOfCurveSegments";
  return name;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfLineSegments::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "curveSegment")
  {
    std::string type = "LineSegment";
    stream.peek().getAttributes().readInto("xsi:type", type);

    if(type=="LineSegment")
    {
      object = new LineSegment();
    }
    else if(type=="CubicBezier")
    {
      object = new CubicBezier();
    }
  }
  
  if(object) mItems.push_back(object);

  return object;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode ListOfLineSegments::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("listOfCurveSegments", "http://projects.eml.org/bcb/sbml/level2", "");
  XMLAttributes att = XMLAttributes();
  XMLToken token = XMLToken(triple, att, xmlns); 
  XMLNode node(token);
  // add the notes and annotations
  bool end=true;
  if(this->mNotes)
  {
      node.addChild(*this->mNotes);
      end=false;
  }
  if(this->mAnnotation)
  {
      node.addChild(*this->mAnnotation);
      end=false;
  }
  unsigned int i,iMax=this->size();
  const LineSegment* object=NULL;
  for(i=0;i<iMax;++i)
  {
    object=dynamic_cast<const LineSegment*>(this->get(i));
    assert(object);
    node.addChild(object->toXML());
  }
  if(end==true && iMax==0)
  {
    node.setEnd();
  }
  return node;
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Curve::getTypeCode () const
{
  return SBML_LAYOUT_CURVE;
}


/**
 * Accepts the given SBMLVisitor.
 */
bool
Curve::accept (SBMLVisitor& v) const
{
    
  /*bool result=v.visit(*this);
  mCurveSegments.accept(v);
  v.leave(*this);*/
  return false;
}

/**
  * Sets the parent SBML object of this SBML object.
  *
  * @param sb the SBML object to use
  */
void 
Curve::setParentSBMLObject (SBase* sb)
{
  mParentSBMLObject = sb;
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Curve::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;

  mCurveSegments.setSBMLDocument(d);
}



/**
 * Calculates the bounding box for the curve.
 * Basepoints for cubic beziers are considered to belong inside the bounding
 * box.
 */
BoundingBox Curve::calculateBoundingBox() const
{
    double xMin=std::numeric_limits<double>::max();
    double yMin=xMin;
    double xMax=-xMin;
    double yMax=-xMin;
    double x,y;
    unsigned int i,iMax=this->getNumCurveSegments();
    const LineSegment* pLS=NULL;
    const CubicBezier* pCB=NULL;
    const Point* pP=NULL;
    for(i=0;i<iMax;++i)
    {
        pLS=this->getCurveSegment(i);
        pP=pLS->getStart();
        x=pP->x();
        y=pP->y();
        xMin=(xMin<x)?xMin:x;
        yMin=(yMin<y)?yMin:y;
        xMax=(xMax>x)?xMax:x;
        yMax=(yMax>y)?yMax:y;
        pP=pLS->getEnd();
        x=pP->x();
        y=pP->y();
        xMin=(xMin<x)?xMin:x;
        yMin=(yMin<y)?yMin:y;
        xMax=(xMax>x)?xMax:x;
        yMax=(yMax>y)?yMax:y;
        pCB=dynamic_cast<const CubicBezier*>(pLS);
        if(pCB)
        {
            pP=pCB->getBasePoint1();
            x=pP->x();
            y=pP->y();
            xMin=(xMin<x)?xMin:x;
            yMin=(yMin<y)?yMin:y;
            xMax=(xMax>x)?xMax:x;
            yMax=(yMax>y)?yMax:y;
            pP=pCB->getBasePoint2();
            x=pP->x();
            y=pP->y();
            xMin=(xMin<x)?xMin:x;
            yMin=(yMin<y)?yMin:y;
            xMax=(xMax>x)?xMax:x;
            yMax=(yMax>y)?yMax:y;
        }
    }
    return BoundingBox("bb",xMin,yMin,xMax-xMin,yMax-yMin);
}






/**
 * Creates a new curve and returns the pointer to it.
 */
LIBSBML_EXTERN
Curve_t *
Curve_create (void)
{
  return new(std::nothrow) Curve;
}

/** @cond doxygen-libsbml-internal */
/**
 * Creates a new Curve_t structure using the given SBML @p 
 * level and @p version values and a set of XMLNamespaces.
 *
 * @param level an unsigned int, the SBML Level to assign to this 
 * Curve
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Curve
 * 
 * @param xmlns XMLNamespaces, a pointer to an array of XMLNamespaces to
 * assign to this Curve
 *
 * @return a pointer to the newly created Curve_t structure.
 *
 * @note Once a Curve has been added to an SBMLDocument, the @p 
 * level, @p version and @p xmlns namespaces for the document @em override 
 * those used to create the Reaction.  Despite this, the ability 
 * to supply the values at creation time is an important aid to creating 
 * valid SBML.  Knowledge of the intended SBML Level and Version 
 * determine whether it is valid to assign a particular value to an 
 * attribute, or whether it is valid to add an object to an existing 
 * SBMLDocument.
 */
LIBSBML_EXTERN
Curve_t *
Curve_createWithLevelVersionAndNamespaces (unsigned int level,
              unsigned int version)
{
  return new(std::nothrow) Curve(level, version);
}
/** @endcond */


/**
 * Creates a new Curve object from a template.
 */
LIBSBML_EXTERN
Curve_t *
Curve_createFrom (const Curve_t *temp)
{
  return new(std::nothrow) Curve(temp ? *temp : Curve());
}


/**
 * Frees the memory taken by the Curve.
 */
LIBSBML_EXTERN
void
Curve_free (Curve_t *c)
{
  delete c;
}


/**
 * Adds a LineSegment.
 */
LIBSBML_EXTERN
void
Curve_addCurveSegment (Curve_t *c, LineSegment_t *ls)
{
  c->addCurveSegment(ls);
}


/**
 * Returns the number of line segments.
 */
LIBSBML_EXTERN
unsigned int
Curve_getNumCurveSegments (const Curve_t *c)
{
  return c->getNumCurveSegments();
}


/**
 * Returns the line segment with the given index.
 */
LIBSBML_EXTERN
LineSegment_t *
Curve_getCurveSegment (const Curve_t *c, unsigned int index)
{
  return const_cast<LineSegment*>(c->getCurveSegment(index));
}


/**
 * Returns the ListOf object that holds all the curve segments.
 */ 
LIBSBML_EXTERN
ListOf_t *
Curve_getListOfCurveSegments (Curve_t *c)
{
  return c->getListOfCurveSegments();
}


/**
 * Does nothing since no defaults are defined for Curve.
 */ 
LIBSBML_EXTERN
void
Curve_initDefaults (Curve_t *c)
{
  c->initDefaults();
}


/**
 * Creates a new LineSegment and adds it to the end of the list.  A pointer
 * to the new LineSegment object is returned.
 */
LIBSBML_EXTERN
LineSegment_t *
Curve_createLineSegment (Curve_t *c)
{
  return c->createLineSegment();
}


/**
 * Creates a new CubicBezier and adds it to the end of the list.  A pointer
 * to the new CubicBezier object is returned.
 */
LIBSBML_EXTERN
CubicBezier_t *
Curve_createCubicBezier (Curve_t *c)
{
  return c->createCubicBezier();
}

/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
Curve_t *
Curve_clone (const Curve_t *m)
{
  return static_cast<Curve*>( m->clone() );
}

LIBSBML_CPP_NAMESPACE_END

