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

#include <sstream>

#include "Dimensions.h"
#include "LayoutUtilities.h"
#include <sbml/xml/XMLErrorLog.h>
#include <sbml/SBMLErrorLog.h>
#include <sbml/SBMLNamespaces.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * Creates a new Dimensions object with all sizes set to 0.0.
 */ 
Dimensions::Dimensions (): SBase("", "", -1), mW(0.0), mH(0.0), mD(0.0)
{
}

                          
Dimensions::Dimensions (SBMLNamespaces *sbmlns) :
   SBase (sbmlns)
, mW(0.0), mH(0.0), mD(0.0)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}
 
/**
 * Creates a new Dimensions object with the given sizes.
 */ 
Dimensions::Dimensions (double width, double height, double depth) :
  SBase("", "", -1), mW(width), mH(height), mD(depth)
{
}

Dimensions::Dimensions(const Dimensions& orig):SBase()
{
    this->mH=orig.mH;
    this->mW=orig.mW;
    this->mD=orig.mD;
    // attributes of SBase
//    this->mId=orig.mId;
//    this->mName=orig.mName;
    this->mMetaId=orig.mMetaId;
    if(orig.mNotes) this->mNotes=new XMLNode(*const_cast<Dimensions&>(orig).getNotes());
    if(orig.mAnnotation) this->mAnnotation=new XMLNode(*const_cast<Dimensions&>(orig).mAnnotation);
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

Dimensions& Dimensions::operator=(const Dimensions& orig)
{
  if(&orig!=this)
  {
    this->mH=orig.mH;
    this->mW=orig.mW;
    this->mD=orig.mD;
    this->mMetaId=orig.mMetaId;
    delete this->mNotes;
    this->mNotes=NULL;
    if(orig.mNotes) this->mNotes=new XMLNode(*const_cast<Dimensions&>(orig).getNotes());
    delete this->mAnnotation;
    this->mAnnotation=NULL;
    if(orig.mAnnotation) this->mAnnotation=new XMLNode(*const_cast<Dimensions&>(orig).mAnnotation);
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
 * Creates a new Dimensions object from the given XMLNode
 */
Dimensions::Dimensions(const XMLNode& node)
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
        else
        {
            //throw;
        }
        ++n;
    }    
}

/**
 * Frees memory taken up by the Dimensions object.
 */ 
Dimensions::~Dimensions ()
{
}


/**
 * Returns the width.
 */
double
Dimensions::width() const
{
  return this->mW;
}


/**
 * Returns the height.
 */
double
Dimensions::height() const
{
  return this->mH;
}


/**
 * Returns the depth.
 */
double
Dimensions::depth () const
{
  return this->mD;
}


/**
 * Returns the width.
 */
double
Dimensions::getWidth() const
{
  return this->width();
}


/**
 * Returns the height.
 */
double
Dimensions::getHeight() const
{
  return this->height();
}


/**
 * Returns the depth.
 */
double
Dimensions::getDepth () const
{
  return this->depth();
}


/**
 * Sets the width to the given value.
 */ 
void
Dimensions::setWidth (double width)
{
  this->mW = width;
}


/**
 * Sets the height to the given value.
 */ 
void
Dimensions::setHeight (double height)
{
  this->mH = height;
}


/**
 * Sets the depth to the given value.
 */ 
void Dimensions::setDepth (double depth)
{
  this->mD = depth;
}


/**
 * Sets all sizes of the Dimensions object to the given values.
 */ 
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
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string& Dimensions::getElementName () const 
{
  static const std::string name = "dimensions";
  return name;
}

/**
 * @return a (deep) copy of this Model.
 */
SBase* 
Dimensions::clone () const
{
    return new Dimensions(*this);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
Dimensions::createObject (XMLInputStream& stream)
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

void Dimensions::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  attributes.readInto(std::string("width"), mW,dynamic_cast<XMLErrorLog*>(this->getErrorLog()),true);
  attributes.readInto(std::string("height"), mH,this->getErrorLog(),true);
  if(!attributes.readInto("depth", mD))
  {
      this->mD=0.0;
  }
}

/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Dimensions::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
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
void Dimensions::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);
  stream.writeAttribute("width", mW);
  stream.writeAttribute("height", mH);
  if(this->mD!=0.0)
  {
    stream.writeAttribute("depth", mD);
  }
}

/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Dimensions::getTypeCode () const
{
  return SBML_LAYOUT_DIMENSIONS;
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the SBML object's next
 * sibling object (if available).
 */
bool Dimensions::accept (SBMLVisitor& v) const
{
    //return v.visit(*this);
    return false;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode Dimensions::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("dimensions", "", "");
  XMLAttributes att = XMLAttributes();
  // add the SBase Ids
  addSBaseAttributes(*this,att);
  std::ostringstream os;
  os << this->mW;
  att.add("width",os.str());
  os.str("");
  os << this->mH;
  att.add("height",os.str());
  if(this->mD!=0.0)
  {
    os.str("");
    os << this->mD;
    att.add("depth",os.str());
  }
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
  if(end==true)
  {
    node.setEnd();
  }
  return node;
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


/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
Dimensions_t *
Dimensions_clone (const Dimensions_t *m)
{
  return static_cast<Dimensions*>( m->clone() );
}

LIBSBML_CPP_NAMESPACE_END

