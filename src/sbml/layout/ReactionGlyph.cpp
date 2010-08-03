/**
 * Filename    : ReactionGlyph.cpp
 * Description : SBML Layout ReactionGlyph source
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
#include <limits>
#include <algorithm>
#include <functional>

#include "ReactionGlyph.h"
#include "LayoutUtilities.h"


#include <sbml/SBMLNamespaces.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN


/**
 * Creates a new ReactionGlyph.  The list of species reference glyph is
 * empty and the id of the associated reaction is set to the empty string.
 */
ReactionGlyph::ReactionGlyph() :
    GraphicalObject()
{
}


/**
 * Creates a ReactionGlyph with the given id.
 */
ReactionGlyph::ReactionGlyph (const std::string& id):
    GraphicalObject( id          )
{
}

ReactionGlyph::ReactionGlyph (unsigned int level, unsigned int version):
   GraphicalObject (level, version)
{
}

                          
ReactionGlyph::ReactionGlyph (SBMLNamespaces *sbmlns) :
   GraphicalObject (sbmlns)
{
}
 

/**
 * Creates a ReactionGlyph with the given id and set the id of the
 * associated reaction to the second argument.
 */
ReactionGlyph::ReactionGlyph (const std::string& id,
                              const std::string& reactionId) : 
    GraphicalObject( id          )
  , mReaction       ( reactionId  )
{
}

/**
 * Creates a new ReactionGlyph from the given XMLNode
 */
ReactionGlyph::ReactionGlyph(const XMLNode& node)
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
            this->mAnnotation=new XMLNode(*child);
        }
        else if(childName=="notes")
        {
            this->mNotes=new XMLNode(*child);
        }
        else if(childName=="curve")
        {
            // since the copy constructor of ListOf does not make deep copies
            // of the objects, we have to add the individual curveSegments to the 
            // curve instead of just copying the whole curve.
            Curve* pTmpCurve=new Curve(*child);
            unsigned int i,iMax=pTmpCurve->getNumCurveSegments();
            for(i=0;i<iMax;++i)
            {
                this->mCurve.addCurveSegment(pTmpCurve->getCurveSegment(i));
            }
            // we also have to copy mAnnotations, mNotes, mCVTerms and mHistory
            if(pTmpCurve->isSetNotes()) this->mCurve.setNotes(new XMLNode(*pTmpCurve->getNotes()));
            if(pTmpCurve->isSetAnnotation()) this->mCurve.setAnnotation(new XMLNode(*pTmpCurve->getAnnotation()));
            if(pTmpCurve->getCVTerms()!=NULL)
            {
              iMax=pTmpCurve->getCVTerms()->getSize(); 
              for(i=0;i<iMax;++i)
              {
                this->mCurve.getCVTerms()->add(static_cast<CVTerm*>(pTmpCurve->getCVTerms()->get(i))->clone());
              }
            }
            delete pTmpCurve;
        }
        else if(childName=="listOfSpeciesReferenceGlyphs")
        {
            const XMLNode* innerChild;
            unsigned int i=0,iMax=child->getNumChildren();
            while(i<iMax)
            {
                innerChild=&child->getChild(i);
                const std::string innerChildName=innerChild->getName();
                if(innerChildName=="speciesReferenceGlyph")
                {
                    this->mSpeciesReferenceGlyphs.appendAndOwn(new SpeciesReferenceGlyph(*innerChild));
                }
                else if(innerChildName=="annotation")
                {
                    this->mSpeciesReferenceGlyphs.setAnnotation(new XMLNode(*innerChild));
                }
                else if(innerChildName=="notes")
                {
                    this->mSpeciesReferenceGlyphs.setNotes(new XMLNode(*innerChild));
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
 * Copy constructor.
 */
ReactionGlyph::ReactionGlyph(const ReactionGlyph& source):GraphicalObject(source)
{
    this->mReaction=source.getReactionId();
    this->mCurve=*source.getCurve();
    this->mSpeciesReferenceGlyphs=*source.getListOfSpeciesReferenceGlyphs();
}

/**
 * Assignment operator.
 */
ReactionGlyph& ReactionGlyph::operator=(const ReactionGlyph& source)
{
  if(&source!=this)
  {
    GraphicalObject::operator=(source);
    this->mReaction=source.getReactionId();
    this->mCurve=*source.getCurve();
    this->mSpeciesReferenceGlyphs=*source.getListOfSpeciesReferenceGlyphs();
  }
  
  return *this;
}



/**
 * Destructor.
 */ 
ReactionGlyph::~ReactionGlyph ()
{
} 


/**
 * Returns the id of the associated reaction.
 */  
const std::string&
ReactionGlyph::getReactionId () const
{
  return this->mReaction;
}


/**
 * Sets the id of the associated reaction.
 */ 
void
ReactionGlyph::setReactionId (const std::string& id)
{
  this->mReaction = id;
}


/**
 * Returns true if the id of the associated reaction is not the empty
 * string.
 */ 
bool
ReactionGlyph::isSetReactionId() const
{
  return ! this->mReaction.empty();
}


/**
 * Returns the ListOf object that hold the species reference glyphs.
 */  
const ListOfSpeciesReferenceGlyphs*
ReactionGlyph::getListOfSpeciesReferenceGlyphs () const
{
  return &this->mSpeciesReferenceGlyphs;
}


/**
 * Returns the ListOf object that hold the species reference glyphs.
 */  
ListOfSpeciesReferenceGlyphs*
ReactionGlyph::getListOfSpeciesReferenceGlyphs ()
{
  return &this->mSpeciesReferenceGlyphs;
}

/**
 * Returns the species reference glyph with the given index.  If the index
 * is invalid, NULL is returned.
 */ 
SpeciesReferenceGlyph*
ReactionGlyph::getSpeciesReferenceGlyph (unsigned int index) 
{
  return static_cast<SpeciesReferenceGlyph*>
  (
    this->mSpeciesReferenceGlyphs.get(index)
  );
}


/**
 * Returns the species reference glyph with the given index.  If the index
 * is invalid, NULL is returned.
 */ 
const SpeciesReferenceGlyph*
ReactionGlyph::getSpeciesReferenceGlyph (unsigned int index) const
{
  return static_cast<const SpeciesReferenceGlyph*>
  (
    this->mSpeciesReferenceGlyphs.get(index)
  );
}


/**
 * Adds a new species reference glyph to the list.
 */
int
ReactionGlyph::addSpeciesReferenceGlyph (const SpeciesReferenceGlyph* glyph)
{
  if (glyph == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(glyph->hasRequiredAttributes()) || !(glyph->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != glyph->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != glyph->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (glyph->isSetId() 
       && (getListOfSpeciesReferenceGlyphs()->get(glyph->getId())) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
      /* if the ListOf is empty it doesn't know its parent */
      if (mSpeciesReferenceGlyphs.size() == 0)
      {
          mSpeciesReferenceGlyphs.setSBMLDocument(this->getSBMLDocument());
          mSpeciesReferenceGlyphs.setParentSBMLObject(this);
      }
      this->mSpeciesReferenceGlyphs.append(glyph);

      return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Returns the number of species reference glyph objects.
 */ 
unsigned int
ReactionGlyph::getNumSpeciesReferenceGlyphs () const
{
  return this->mSpeciesReferenceGlyphs.size();
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
void ReactionGlyph::initDefaults ()
{
  GraphicalObject::initDefaults();
}


/**
 * Returns the curve object for the reaction glyph
 */ 
const Curve*
ReactionGlyph::getCurve () const
{
  return &this->mCurve;
}

/**
 * Returns the curve object for the reaction glyph
 */ 
Curve*
ReactionGlyph::getCurve () 
{
  return &this->mCurve;
}


/**
 * Sets the curve object for the reaction glyph.
 */ 
void ReactionGlyph::setCurve (const Curve* curve)
{
  if(!curve) return;
  this->mCurve = *curve;
}


/**
 * Returns true if the curve consists of one or more segments.
 */ 
bool ReactionGlyph::isSetCurve () const
{
  return this->mCurve.getNumCurveSegments() > 0;
}


/**
 * Creates a new SpeciesReferenceGlyph object, adds it to the end of the
 * list of species reference objects and returns a reference to the newly
 * created object.
 */
SpeciesReferenceGlyph*
ReactionGlyph::createSpeciesReferenceGlyph ()
{
  SpeciesReferenceGlyph* srg = NULL;

  try
  {
    srg = new SpeciesReferenceGlyph(getSBMLNamespaces());
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
  if (mSpeciesReferenceGlyphs.size() == 0)
  {
    mSpeciesReferenceGlyphs.setSBMLDocument(this->getSBMLDocument());
    mSpeciesReferenceGlyphs.setParentSBMLObject(this);
  }

  if(srg != NULL) 
  {
      this->mSpeciesReferenceGlyphs.appendAndOwn(srg);
  }
  return srg;
}


/**
 * Creates a new LineSegment object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
LineSegment*
ReactionGlyph::createLineSegment ()
{
  return this->mCurve.createLineSegment();
}

 
/**
 * Creates a new CubicBezier object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
CubicBezier*
ReactionGlyph::createCubicBezier ()
{
  return this->mCurve.createCubicBezier();
}

/**
 * Remove the species reference glyph with the given index.
 * A pointer to the object is returned. If no object has been removed, NULL
 * is returned.
 */
SpeciesReferenceGlyph*
ReactionGlyph::removeSpeciesReferenceGlyph(unsigned int index)
{
    SpeciesReferenceGlyph* srg=NULL;
    if(index < this->getNumSpeciesReferenceGlyphs())
    {
        srg=dynamic_cast<SpeciesReferenceGlyph*>(this->getListOfSpeciesReferenceGlyphs()->remove(index));
    }
    return srg;
}

/**
 * Remove the species reference glyph with the given id.
 * A pointer to the object is returned. If no object has been removed, NULL
 * is returned.
 */
SpeciesReferenceGlyph*
ReactionGlyph::removeSpeciesReferenceGlyph(const std::string& id)
{
    SpeciesReferenceGlyph* srg=NULL;
    unsigned int index=this->getIndexForSpeciesReferenceGlyph(id);
    if(index!=std::numeric_limits<unsigned int>::max())
    {
        srg=this->removeSpeciesReferenceGlyph(index);
    }
    return srg;
}

/**
 * Returns the index of the species reference glyph with the given id.
 * If the reaction glyph does not contain a species reference glyph with this
 * id, numreic_limits<int>::max() is returned.
 */
unsigned int
ReactionGlyph::getIndexForSpeciesReferenceGlyph(const std::string& id) const
{
    unsigned int i,iMax=this->getNumSpeciesReferenceGlyphs();
    unsigned int index=std::numeric_limits<unsigned int>::max();
    for(i=0;i<iMax;++i)
    {
        const SpeciesReferenceGlyph* srg=this->getSpeciesReferenceGlyph(i);
        if(srg->getId()==id)
        {
            index=i;
            break;
        }
    }
    return index;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string& ReactionGlyph::getElementName () const 
{
  static const std::string name = "reactionGlyph";
  return name;
}

/**
 * @return a (deep) copy of this Model.
 */
SBase* 
ReactionGlyph::clone () const
{
    return new ReactionGlyph(*this);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ReactionGlyph::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  
  SBase*        object = 0;

  if (name == "listOfSpeciesReferenceGlyphs")
  {
    object = &mSpeciesReferenceGlyphs;
  }
  else if(name=="curve")
  {
    object = &mCurve;
  }
  else
  {
    object=GraphicalObject::createObject(stream);
  }
  
  return object;
}

/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */

void ReactionGlyph::readAttributes (const XMLAttributes& attributes)
{
  GraphicalObject::readAttributes(attributes);

  attributes.readInto("reaction", mReaction);
}

/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
ReactionGlyph::writeElements (XMLOutputStream& stream) const
{
  if(this->isSetCurve())
  {
      SBase::writeElements(stream);
      mCurve.write(stream);
  }
  else
  {
    GraphicalObject::writeElements(stream);
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
void ReactionGlyph::writeAttributes (XMLOutputStream& stream) const
{
  GraphicalObject::writeAttributes(stream);
  if(this->isSetReactionId())
  {
    stream.writeAttribute("reaction", mReaction);
  }
}

/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
ReactionGlyph::getTypeCode () const
{
  return SBML_LAYOUT_REACTIONGLYPH;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode ReactionGlyph::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("reactionGlyph", "", "");
  XMLAttributes att = XMLAttributes();
  // add the SBase Ids
  addSBaseAttributes(*this,att);
  addGraphicalObjectAttributes(*this,att);
  if(this->isSetReactionId()) att.add("reaction",this->mReaction);
  XMLToken token = XMLToken(triple, att, xmlns); 
  XMLNode node(token);
  // add the notes and annotations
  if(this->mNotes) node.addChild(*this->mNotes);
  if(this->mAnnotation) node.addChild(*this->mAnnotation);
  if(this->mCurve.getNumCurveSegments()==0)
  {
    // write the bounding box
    node.addChild(this->mBoundingBox.toXML());
  }
  else
  {
    // add the curve
    node.addChild(this->mCurve.toXML());
  }
  // add the list of species reference glyphs
  if(this->mSpeciesReferenceGlyphs.size()>0)
  {
    node.addChild(this->mSpeciesReferenceGlyphs.toXML());
  }
  return node;
}



/**
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
SBase*
ListOfSpeciesReferenceGlyphs::clone () const
{
  return new ListOfSpeciesReferenceGlyphs(*this);
}

ListOfSpeciesReferenceGlyphs& ListOfSpeciesReferenceGlyphs::operator=(const ListOfSpeciesReferenceGlyphs& source)
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
        SBase* o=this->remove(0);
        delete o;
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

ListOfSpeciesReferenceGlyphs::ListOfSpeciesReferenceGlyphs(const ListOfSpeciesReferenceGlyphs& source)
{
    ListOfSpeciesReferenceGlyphs::operator=(source);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfSpeciesReferenceGlyphs::getItemTypeCode () const
{
  return SBML_LAYOUT_SPECIESREFERENCEGLYPH;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfSpeciesReferenceGlyphs::getElementName () const
{
  static const std::string name = "listOfSpeciesReferenceGlyphs";
  return name;
}

/* return nth item in list */
SpeciesReferenceGlyph *
ListOfSpeciesReferenceGlyphs::get(unsigned int n)
{
  return static_cast<SpeciesReferenceGlyph*>(ListOf::get(n));
}


/* return nth item in list */
const SpeciesReferenceGlyph *
ListOfSpeciesReferenceGlyphs::get(unsigned int n) const
{
  return static_cast<const SpeciesReferenceGlyph*>(ListOf::get(n));
}


/* return item by id */
SpeciesReferenceGlyph*
ListOfSpeciesReferenceGlyphs::get (const std::string& sid)
{
  return const_cast<SpeciesReferenceGlyph*>( 
    static_cast<const ListOfSpeciesReferenceGlyphs&>(*this).get(sid) );
}

/**
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
struct IdEqR_SpeciesReferenceGlyph : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEqR_SpeciesReferenceGlyph (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <SpeciesReferenceGlyph *> (sb)->getId() == id; }
};


/* return item by id */
const SpeciesReferenceGlyph*
ListOfSpeciesReferenceGlyphs::get (const std::string& sid) const
{
    std::vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqR_SpeciesReferenceGlyph(sid) );
  return (result == mItems.end()) ? 0 : static_cast <SpeciesReferenceGlyph*> (*result);
}



/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfSpeciesReferenceGlyphs::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "speciesReferenceGlyph")
  {
    object = new SpeciesReferenceGlyph();
    mItems.push_back(object);
  }

  return object;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode ListOfSpeciesReferenceGlyphs::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("listOfSpeciesReferenceGlyphs", "http://projects.eml.org/bcb/sbml/level2", "");
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
  const SpeciesReferenceGlyph* object=NULL;
  for(i=0;i<iMax;++i)
  {
    object=dynamic_cast<const SpeciesReferenceGlyph*>(this->get(i));
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
 * Accepts the given SBMLVisitor.
 
bool
ReactionGlyph::accept (SBMLVisitor& v) const
{
  bool result=v.visit(*this);
  if(this->mCurve.getNumCurveSegments()>0)
  {
    this->mCurve.accept(v);
  }
  else
  {
    this->mBoundingBox.accept(v);
  }
  this->mSpeciesReferenceGlyphs.accept(this);
  v.leave(*this);
  return result;
}
*/

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
ReactionGlyph::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;

  mSpeciesReferenceGlyphs.setSBMLDocument(d);
}










/**
 * Creates a new ReactionGlyph and returns the pointer to it.
 */
LIBSBML_EXTERN
ReactionGlyph_t *
ReactionGlyph_create (void)
{
  return new(std::nothrow) ReactionGlyph;
}

/** @cond doxygen-libsbml-internal */
/**
 * Creates a new ReactionGlyph_t structure using the given SBML @p 
 * level and @p version values and a set of XMLNamespaces.
 *
 * @param level an unsigned int, the SBML Level to assign to this 
 * ReactionGlyph
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * ReactionGlyph
 * 
 * @param xmlns XMLNamespaces, a pointer to an array of XMLNamespaces to
 * assign to this ReactionGlyph
 *
 * @return a pointer to the newly created ReactionGlyph_t structure.
 *
 * @note Once a ReactionGlyph has been added to an SBMLDocument, the @p 
 * level, @p version and @p xmlns namespaces for the document @em override 
 * those used to create the Reaction.  Despite this, the ability 
 * to supply the values at creation time is an important aid to creating 
 * valid SBML.  Knowledge of the intended SBML Level and Version 
 * determine whether it is valid to assign a particular value to an 
 * attribute, or whether it is valid to add an object to an existing 
 * SBMLDocument.
 */
LIBSBML_EXTERN
ReactionGlyph_t *
ReactionGlyph_createWithLevelVersionAndNamespaces (unsigned int level,
              unsigned int version)
{
  return new(std::nothrow) ReactionGlyph(level, version);
}
/** @endcond */

/**
 * Creates a new ReactionGlyph with the given id
 */
LIBSBML_EXTERN
ReactionGlyph_t *
ReactionGlyph_createWith (const char *sid)
{
  return new(std::nothrow) ReactionGlyph(sid ? sid : "", "");
}


/**
 * Creates a new ReactionGlyph referencing the give reaction.
 */
LIBSBML_EXTERN
ReactionGlyph_t *
ReactionGlyph_createWithReactionId (const char *id, const char *reactionId)
{
  return new(std::nothrow) ReactionGlyph(id ? id : "", reactionId ? reactionId : "");
}


/**
 * Creates a new ReactionGlyph object from a template.
 */
LIBSBML_EXTERN
ReactionGlyph_t *
ReactionGlyph_createFrom (const ReactionGlyph_t *temp)
{
  return new(std::nothrow) ReactionGlyph(*temp);
}


/**
 * Frees the memory taken up by the attributes.
 */
LIBSBML_EXTERN
void
ReactionGlyph_free (ReactionGlyph_t *rg)
{
  delete rg;
}


/**
 * Sets the reference reaction for the reaction glyph.
 */
LIBSBML_EXTERN
void
ReactionGlyph_setReactionId (ReactionGlyph_t *rg,const char *id)
{
    static_cast<ReactionGlyph*>(rg)->setReactionId( id ? id : "" );
}


/**
 * Gets the reference reactions id for the given reaction glyph.
 */
LIBSBML_EXTERN
const char *
ReactionGlyph_getReactionId (const ReactionGlyph_t *rg)
{
    return rg->isSetReactionId() ? rg->getReactionId().c_str() : NULL;
}


/**
 * Returns 0 if the reference reaction has not been set for this glyph and
 * 1 otherwise.
 */
LIBSBML_EXTERN
int
ReactionGlyph_isSetReactionId (const ReactionGlyph_t *rg)
{
  return static_cast<int>( rg->isSetReactionId() );
}


/**
 * Add a SpeciesReferenceGlyph object to the list of
 * SpeciesReferenceGlyphs.
 */
LIBSBML_EXTERN
void
ReactionGlyph_addSpeciesReferenceGlyph (ReactionGlyph_t         *rg,
                                        SpeciesReferenceGlyph_t *srg)
{
  rg->addSpeciesReferenceGlyph(srg);
}


/**
 * Returns the number of SpeciesReferenceGlyphs for the ReactionGlyph.
 */
LIBSBML_EXTERN
unsigned int
ReactionGlyph_getNumSpeciesReferenceGlyphs (const ReactionGlyph_t *rg)
{
  return rg->getNumSpeciesReferenceGlyphs();
}


/**
 * Returns the pointer to the SpeciesReferenceGlyphs for the given index.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t *
ReactionGlyph_getSpeciesReferenceGlyph (ReactionGlyph_t *rg,
                                        unsigned int           index)
{
  return rg->getSpeciesReferenceGlyph(index);
}


/**
 * Returns the list object that holds all species reference glyphs.
 */ 
LIBSBML_EXTERN
ListOf_t *
ReactionGlyph_getListOfSpeciesReferenceGlyphs (ReactionGlyph_t *rg)
{
  return rg->getListOfSpeciesReferenceGlyphs();
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
LIBSBML_EXTERN
void
ReactionGlyph_initDefaults (ReactionGlyph_t *rg)
{
  rg->initDefaults();
}


/**
 * Sets the curve for the reaction glyph.
 */
LIBSBML_EXTERN
void
ReactionGlyph_setCurve (ReactionGlyph_t *rg, Curve_t *c)
{
  rg->setCurve(c);
}


/**
 * Gets the Curve for the given reaction glyph.
 */
LIBSBML_EXTERN
Curve_t *
ReactionGlyph_getCurve (ReactionGlyph_t *rg)
{
  return rg->getCurve();
}


/**
 * Returns true if the Curve has one or more LineSegment.
 */
LIBSBML_EXTERN
int
ReactionGlyph_isSetCurve (ReactionGlyph_t *rg)
{
  return static_cast<int>( rg->isSetCurve() );
}


/**
 * Creates a new SpeciesReferenceGlyph_t object, adds it to the end of the
 * list of species reference objects and returns a pointer to the newly
 * created object.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t *
ReactionGlyph_createSpeciesReferenceGlyph (ReactionGlyph_t *rg)
{
  return rg->createSpeciesReferenceGlyph();
}


/**
 * Creates a new LineSegment object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
LIBSBML_EXTERN
LineSegment_t *
ReactionGlyph_createLineSegment (ReactionGlyph_t *rg)
{
  return rg->getCurve()->createLineSegment();
}


/**
 * Creates a new CubicBezier object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
LIBSBML_EXTERN
CubicBezier_t *
ReactionGlyph_createCubicBezier (ReactionGlyph_t *rg)
{
  return rg->getCurve()->createCubicBezier();
}


/**
 * Remove the species reference glyph with the given index.
 * A pointer to the object is returned. If no object has been removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t*
ReactionGlyph_removeSpeciesReferenceGlyph(ReactionGlyph_t* rg,unsigned int index)
{
    return rg->removeSpeciesReferenceGlyph(index);
}

/**
 * Remove the species reference glyph with the given id.
 * A pointer to the object is returned. If no object has been removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t*
ReactionGlyph_removeSpeciesReferenceGlyphWithId(ReactionGlyph_t* rg,const char* id)
{
    return rg->removeSpeciesReferenceGlyph(id);
}

/**
 * Returns the index of the species reference glyph with the given id.
 * If the reaction glyph does not contain a species reference glyph with this
 * id, UINT_MAX from limits.h is returned.
 */
LIBSBML_EXTERN
unsigned int
ReactionGlyph_getIndexForSpeciesReferenceGlyph(ReactionGlyph_t* rg,const char* id)
{
    return rg->getIndexForSpeciesReferenceGlyph(id);
}

/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
ReactionGlyph_t *
ReactionGlyph_clone (const ReactionGlyph_t *m)
{
  return static_cast<ReactionGlyph*>( m->clone() );
}


LIBSBML_CPP_NAMESPACE_END
