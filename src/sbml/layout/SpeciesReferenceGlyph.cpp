/**
 * Filename    : SpeciesReferenceGlyph.cpp
 * Description : SBML Layout SpeciesReferenceGlyph source
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


#include "SpeciesReferenceGlyph.h"
#include "LayoutUtilities.h"

#include <sbml/SBMLNamespaces.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string SpeciesReferenceGlyph::SPECIES_REFERENCE_ROLE_STRING[]={
    "undefined" 
   ,"substrate"
   ,"product"
   ,"sidesubstrate"
   ,"sideproduct"
   ,"modifier"
   ,"activator"
   ,"inhibitor"
   ,""
};



/**
 * Creates a new SpeciesReferenceGlyph.  The id if the associated species
 * reference and the id of the associated species glyph are set to the
 * empty string.  The role is set to SPECIES_ROLE_UNDEFINED.
 */
SpeciesReferenceGlyph::SpeciesReferenceGlyph () :
    GraphicalObject()
   ,mRole  ( SPECIES_ROLE_UNDEFINED )
  
{
}

SpeciesReferenceGlyph::SpeciesReferenceGlyph (unsigned int level, unsigned int version):
   GraphicalObject (level, version)
  ,mRole  ( SPECIES_ROLE_UNDEFINED )
{
}

                          
SpeciesReferenceGlyph::SpeciesReferenceGlyph (SBMLNamespaces *sbmlns) :
   GraphicalObject (sbmlns)
  ,mRole  ( SPECIES_ROLE_UNDEFINED )
{
}
 
/**
 * Creates a new SpeciesReferenceGlyph.  The id is given as the first
 * argument, the id of the associated species reference is given as the
 * second argument.  The third argument is the id of the associated species
 * glpyh and the fourth argument is the role.
 */ 
SpeciesReferenceGlyph::SpeciesReferenceGlyph
(
  const std::string& sid,
  const std::string& speciesGlyphId,
  const std::string& speciesReferenceId,
  SpeciesReferenceRole_t role
) :
    GraphicalObject ( sid                )
  , mSpeciesReference( speciesReferenceId )
  , mSpeciesGlyph    ( speciesGlyphId     )
  , mRole            ( role               )
{
}

/**
 * Creates a new SpeciesReferenceGlyph from the given XMLNode
 */
SpeciesReferenceGlyph::SpeciesReferenceGlyph(const XMLNode& node)
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
SpeciesReferenceGlyph::SpeciesReferenceGlyph(const SpeciesReferenceGlyph& source) :
    GraphicalObject(source)
{
    this->mSpeciesReference=source.getSpeciesReferenceId();
    this->mSpeciesGlyph=source.getSpeciesGlyphId();
    this->mRole=source.getRole();
    this->mCurve=*source.getCurve();
}

/**
 * Assignment operator.
 */
SpeciesReferenceGlyph& SpeciesReferenceGlyph::operator=(const SpeciesReferenceGlyph& source)
{
  if(&source!=this)
  {
    GraphicalObject::operator=(source);
    this->mSpeciesReference=source.getSpeciesReferenceId();
    this->mSpeciesGlyph=source.getSpeciesGlyphId();
    this->mRole=source.getRole();
    this->mCurve=*source.getCurve();
  }
  
  return *this;
}

/**
 * Destructor.
 */ 
SpeciesReferenceGlyph::~SpeciesReferenceGlyph ()
{
}


/**
 * Returns the id of the associated SpeciesGlyph.
 */ 
const std::string&
SpeciesReferenceGlyph::getSpeciesGlyphId () const
{
  return this->mSpeciesGlyph;
}


/**
 * Sets the id of the associated species glyph.
 */ 
void
SpeciesReferenceGlyph::setSpeciesGlyphId (const std::string& speciesGlyphId)
{
  this->mSpeciesGlyph = speciesGlyphId;
}


/**
 * Returns the id of the associated species reference.
 */ 
const std::string&
SpeciesReferenceGlyph::getSpeciesReferenceId () const
{
  return this->mSpeciesReference;
}


/**
 * Sets the id of the associated species reference.
 */ 
void
SpeciesReferenceGlyph::setSpeciesReferenceId (const std::string& id)
{
  this->mSpeciesReference=id;
}


/**
 * Returns the role.
 */ 
SpeciesReferenceRole_t
SpeciesReferenceGlyph::getRole() const
{
  return this->mRole;
}

/**
 * Returns a string representation for the role
 */
const std::string& SpeciesReferenceGlyph::getRoleString() const{
    return SpeciesReferenceGlyph::SPECIES_REFERENCE_ROLE_STRING[this->mRole];
}

/**
 * Sets the role based on a string.
 * The String can be one of
 * SUBSTRATE
 * PRODUCT
 * SIDESUBSTRATE
 * SIDEPRODUCT
 * MODIFIER
 * ACTIVATOR
 * INHIBITOR    
 */ 
void
SpeciesReferenceGlyph::setRole (const std::string& role)
{
       if ( role == "substrate"     ) this->mRole = SPECIES_ROLE_SUBSTRATE;
  else if ( role == "product"       ) this->mRole = SPECIES_ROLE_PRODUCT;
  else if ( role == "sidesubstrate" ) this->mRole = SPECIES_ROLE_SIDESUBSTRATE;
  else if ( role == "sideproduct"   ) this->mRole = SPECIES_ROLE_SIDEPRODUCT;
  else if ( role == "modifier"      ) this->mRole = SPECIES_ROLE_MODIFIER;
  else if ( role == "activator"     ) this->mRole = SPECIES_ROLE_ACTIVATOR;
  else if ( role == "inhibitor"     ) this->mRole = SPECIES_ROLE_INHIBITOR;
  else                                this->mRole = SPECIES_ROLE_UNDEFINED;
}


/**
 * Sets the role.
 */ 
void
SpeciesReferenceGlyph::setRole (SpeciesReferenceRole_t role)
{
  this->mRole=role;
}


/**
 * Returns the curve object for the species reference glyph
 */ 
Curve* SpeciesReferenceGlyph::getCurve() 
{
  return &this->mCurve;
}

/**
 * Returns the curve object for the species reference glyph
 */ 
const Curve* SpeciesReferenceGlyph::getCurve() const
{
  return &this->mCurve;
}


/**
 * Sets the curve object for the species reference glyph.
 */ 
void
SpeciesReferenceGlyph::setCurve (const Curve* curve)
{
  if(!curve) return;
  this->mCurve = *curve;
}


/**
 * Returns true if the curve consists of one or more segments.
 */ 
bool
SpeciesReferenceGlyph::isSetCurve () const
{
  return this->mCurve.getNumCurveSegments() > 0;
}


/**
 * Returns true if the id of the associated species glpyh is not the empty
 * string.
 */ 
bool
SpeciesReferenceGlyph::isSetSpeciesGlyphId () const
{
  return ! this->mSpeciesGlyph.empty();
}


/**
 * Returns true if the id of the associated species reference is not the
 * empty string.
 */ 
bool
SpeciesReferenceGlyph::isSetSpeciesReferenceId () const
{
  return ! this->mSpeciesReference.empty();
}


/**
 * Returns true of role is different from SPECIES_ROLE_UNDEFINED.
 */ 
bool SpeciesReferenceGlyph::isSetRole () const
{
  return ! (this->mRole == SPECIES_ROLE_UNDEFINED);
}


/**
 * Calls initDefaults on GraphicalObject and sets role to
 * SPECIES_ROLE_UNDEFINED.
 */ 
void
SpeciesReferenceGlyph::initDefaults ()
{
    GraphicalObject::initDefaults();
    this->mRole = SPECIES_ROLE_UNDEFINED;
}


/**
 * Creates a new LineSegment object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
LineSegment*
SpeciesReferenceGlyph::createLineSegment ()
{
  return this->mCurve.createLineSegment();
}


/**
 * Creates a new CubicBezier object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
CubicBezier*
SpeciesReferenceGlyph::createCubicBezier ()
{
  return this->mCurve.createCubicBezier();
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string& SpeciesReferenceGlyph::getElementName () const 
{
  static const std::string name = "speciesReferenceGlyph";
  return name;
}

/**
 * @return a (deep) copy of this Model.
 */
SBase* 
SpeciesReferenceGlyph::clone () const
{
    return new SpeciesReferenceGlyph(*this);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
SpeciesReferenceGlyph::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  
  SBase*        object = 0;

  if (name == "curve")
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

void SpeciesReferenceGlyph::readAttributes (const XMLAttributes& attributes)
{
  GraphicalObject::readAttributes(attributes);

  
  attributes.readInto("speciesReference", mSpeciesReference);
  attributes.readInto("speciesGlyph", mSpeciesGlyph);
  
  std::string role;
  if(attributes.readInto("role", role))
  {
    this->setRole(role);
  }
  else
  {
    this->setRole(SPECIES_ROLE_UNDEFINED);
  }
  
}

/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
SpeciesReferenceGlyph::writeElements (XMLOutputStream& stream) const
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
void SpeciesReferenceGlyph::writeAttributes (XMLOutputStream& stream) const
{
  GraphicalObject::writeAttributes(stream);
  if(this->isSetSpeciesReferenceId())
  {
    stream.writeAttribute("speciesReference", mSpeciesReference);
  }
  if(this->isSetSpeciesGlyphId())
  {
    stream.writeAttribute("speciesGlyph", mSpeciesGlyph);
  }
  if(this->isSetRole())
  {
    stream.writeAttribute("role", this->getRoleString().c_str() );
  }
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
SpeciesReferenceGlyph::getTypeCode () const
{
  return SBML_LAYOUT_SPECIESREFERENCEGLYPH;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode SpeciesReferenceGlyph::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("speciesReferenceGlyph", "", "");
  XMLAttributes att = XMLAttributes();
  // add the SBase Ids
  addSBaseAttributes(*this,att);
  addGraphicalObjectAttributes(*this,att);
  if(this->isSetSpeciesReferenceId()) att.add("speciesReference",this->mSpeciesReference);
  if(this->isSetSpeciesGlyphId()) att.add("speciesGlyph",this->mSpeciesGlyph);
  if(this->isSetRole()) att.add("role",this->getRoleString());
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
  return node;
}


/**
 * Accepts the given SBMLVisitor.

bool
SpeciesReferenceGlyph::accept (SBMLVisitor& v) const
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
  v.leave(*this);
  return result;
}
*/



/**
 * Creates a new SpeciesReferenceGlyph object and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t *
SpeciesReferenceGlyph_create(void)
{
  return new(std::nothrow) SpeciesReferenceGlyph;
}

/** @cond doxygen-libsbml-internal */
/**
 * Creates a new SpeciesReferenceGlyph_t structure using the given SBML @p 
 * level and @p version values and a set of XMLNamespaces.
 *
 * @param level an unsigned int, the SBML Level to assign to this 
 * SpeciesReferenceGlyph
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SpeciesReferenceGlyph
 * 
 * @param xmlns XMLNamespaces, a pointer to an array of XMLNamespaces to
 * assign to this SpeciesReferenceGlyph
 *
 * @return a pointer to the newly created SpeciesReferenceGlyph_t structure.
 *
 * @note Once a SpeciesReferenceGlyph has been added to an SBMLDocument, the @p 
 * level, @p version and @p xmlns namespaces for the document @em override 
 * those used to create the Reaction.  Despite this, the ability 
 * to supply the values at creation time is an important aid to creating 
 * valid SBML.  Knowledge of the intended SBML Level and Version 
 * determine whether it is valid to assign a particular value to an 
 * attribute, or whether it is valid to add an object to an existing 
 * SBMLDocument.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t *
SpeciesReferenceGlyph_createWithLevelVersionAndNamespaces (unsigned int level,
              unsigned int version)
{
  return new(std::nothrow) SpeciesReferenceGlyph(level, version);
}
/** @endcond */


/**
 * Creates a new SpeciesReferenceGlyph from a template.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t *
SpeciesReferenceGlyph_createFrom (const SpeciesReferenceGlyph_t *temp)
{
  return new(std::nothrow) SpeciesReferenceGlyph(*temp);
}


/**
 * Creates a new SpeciesReferenceGlyph object with the given id and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t *
SpeciesReferenceGlyph_createWith (const char *sid,
                                  const char *speciesReferenceId,
                                  const char *speciesGlyphId,
                                  SpeciesReferenceRole_t role)
{
  return new(std::nothrow)
    SpeciesReferenceGlyph(sid ? sid : "", speciesReferenceId ? speciesReferenceId : "", speciesGlyphId ? speciesGlyphId : "", role);
}


/**
 * Frees the memory for the SpeciesReferenceGlyph
 */
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_free(SpeciesReferenceGlyph_t *srg)
{
  delete srg;
}


/**
 * Sets the reference species for the species glyph.
 */
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_setSpeciesReferenceId (SpeciesReferenceGlyph_t *srg,
                                             const char *id)
{
    srg->setSpeciesReferenceId( id ? id : "" );
}


/**
 * Gets the reference species id for the given species glyph.
 */
LIBSBML_EXTERN
const char *
SpeciesReferenceGlyph_getSpeciesReferenceId (const SpeciesReferenceGlyph_t *srg)
{
    return srg->isSetSpeciesReferenceId() ? srg->getSpeciesReferenceId().c_str() : NULL;
}


/**
 * Returns 0 if the reference species reference has not been set for this
 * glyph and 1 otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReferenceGlyph_isSetSpeciesReferenceId
  (const SpeciesReferenceGlyph_t *srg)
{
    return (int)srg->isSetSpeciesReferenceId();
}


/**
 * Sets the species glyph reference for the species glyph.
 */
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_setSpeciesGlyphId (SpeciesReferenceGlyph_t *srg,
                                         const char *id)
{
    srg->setSpeciesGlyphId( id ? id : "" );
}


/**
 * Gets the reference speciess id for the given species glyph.
 */
LIBSBML_EXTERN
const char *
SpeciesReferenceGlyph_getSpeciesGlyphId (const SpeciesReferenceGlyph_t *srg)
{
    return srg->isSetSpeciesGlyphId() ? srg->getSpeciesGlyphId().c_str() : NULL;
}


/**
 * Returns 0 if  the reference species reference has not  been set for this
 * glyph and 1 otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReferenceGlyph_isSetSpeciesGlyphId (const SpeciesReferenceGlyph_t *srg)
{
  return static_cast<int>( srg->isSetSpeciesGlyphId() );
}


/**
 * Sets the curve for the species reference glyph.
 */
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_setCurve(SpeciesReferenceGlyph_t *srg, Curve_t *c)
{
  srg->setCurve(c);
}


/**
 * Gets the Curve for the given species reference glyph.
 */
LIBSBML_EXTERN
Curve_t *
SpeciesReferenceGlyph_getCurve (SpeciesReferenceGlyph_t *srg)
{
  return srg->getCurve();
}


/**
 * Returns true if the Curve has one or more LineSegment.
 */
LIBSBML_EXTERN
int
SpeciesReferenceGlyph_isSetCurve (SpeciesReferenceGlyph_t *srg)
{
  return static_cast<int>( srg->isSetCurve() );
}


/**
 * Sets the role of the species reference glyph based on the string.  The
 * string can be one of UNDEFINED, SUBSTRATE, PRODUCT, SIDESUBSTRATE,
 * SIDEPRODUCT, MODIFIER, INHIBITOR or ACTIVATOR.  If it is none of those,
 * the role is set to SPECIES_ROLE_UNDEFINED.
 */
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_setRole (SpeciesReferenceGlyph_t *srg,
                               const char *r)
{
  srg->setRole(r);
}


/**
 * Returns the role of the species reference.
 */ 
LIBSBML_EXTERN
SpeciesReferenceRole_t
SpeciesReferenceGlyph_getRole (const SpeciesReferenceGlyph_t *srg)
{
  return srg->getRole();
}

/**
 * Returns a string representation of the role of the species reference.
 */ 
LIBSBML_EXTERN
const char*
SpeciesReferenceGlyph_getRoleString(const SpeciesReferenceGlyph_t* srg){
    return srg->getRoleString().empty() ? NULL : srg->getRoleString().c_str();
}



/**
 * Returns true if the role is not SPECIES_ROLE_UNDEFINED.
 */ 
LIBSBML_EXTERN
int
SpeciesReferenceGlyph_isSetRole (const SpeciesReferenceGlyph_t *srg)
{
  return static_cast<int>( srg->isSetRole() );
}


/**
 * Calls initDefaults on GraphicalObject and sets role to
 * SPECIES_ROLE_UNDEFINED.
 */ 
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_initDefaults (SpeciesReferenceGlyph_t *srg)
{
  srg->initDefaults();
}


/**
 * Creates a new LineSegment object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
LIBSBML_EXTERN
LineSegment_t *
SpeciesReferenceGlyph_createLineSegment (SpeciesReferenceGlyph_t *srg)
{
  return srg->getCurve()->createLineSegment();
}  


/**
 * Creates a new CubicBezier object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
LIBSBML_EXTERN
CubicBezier_t *
SpeciesReferenceGlyph_createCubicBezier (SpeciesReferenceGlyph_t *srg)
{
  return srg->getCurve()->createCubicBezier();
}


/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t *
SpeciesReferenceGlyph_clone (const SpeciesReferenceGlyph_t *m)
{
  return static_cast<SpeciesReferenceGlyph*>( m->clone() );
}

/**
 * Returns non-zero if the id is set
 */
LIBSBML_EXTERN
int
SpeciesReferenceGlyph_isSetId (const SpeciesReferenceGlyph_t *srg)
{
  return static_cast <int> (srg->isSetId());
}

/**
 * Returns the id
 */
LIBSBML_EXTERN
const char *
SpeciesReferenceGlyph_getId (const SpeciesReferenceGlyph_t *srg)
{
  return srg->isSetId() ? srg->getId().c_str() : NULL;
}

/**
 * Sets the id
 */
LIBSBML_EXTERN
int
SpeciesReferenceGlyph_setId (SpeciesReferenceGlyph_t *srg, const char *sid)
{
  return (sid == NULL) ? srg->setId("") : srg->setId(sid);
}

/**
 * Unsets the id
 */
LIBSBML_EXTERN
void
SpeciesReferenceGlyph_unsetId (SpeciesReferenceGlyph_t *srg)
{
  srg->unsetId();
}

LIBSBML_CPP_NAMESPACE_END

