/**
 * Filename    : CompartmentGlyph.cpp
 * Description : SBML Layout CompartmentGlyph source
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


#include "CompartmentGlyph.h"
#include "LayoutUtilities.h"

#include <sbml/SBMLNamespaces.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * Default Constructor which creates a new CompartmentGlyph.  Id and
 * associated compartment id are unset.
 */
CompartmentGlyph::CompartmentGlyph () : GraphicalObject()
{
}


/**
 * Constructor which creates a new CompartmentGlyph with the given id.
 */
CompartmentGlyph::CompartmentGlyph (const std::string& id):
  GraphicalObject(id)
{
}

CompartmentGlyph::CompartmentGlyph (unsigned int level, unsigned int version):
   GraphicalObject (level, version)
{
}

                          
CompartmentGlyph::CompartmentGlyph (SBMLNamespaces *sbmlns) :
   GraphicalObject (sbmlns)
{
}
 
/**
 * Constructor which creates a new CompartmentGlyph.  Id and associated
 * compartment id are set to copies of the values given as arguments.
 */
CompartmentGlyph::CompartmentGlyph (const std::string& id,
                                    const std::string& compId) : 
  GraphicalObject(id), mCompartment(compId)
{
}

/**
 * Creates a new CompartmentGlyph from the given XMLNode
 */
CompartmentGlyph::CompartmentGlyph(const XMLNode& node)
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
CompartmentGlyph::CompartmentGlyph(const CompartmentGlyph& source):GraphicalObject(source)
{
    this->mCompartment=source.getCompartmentId();
}

/**
 * Assignment operator.
 */
CompartmentGlyph& CompartmentGlyph::operator=(const CompartmentGlyph& source)
{
  if(&source!=this)
  {
    GraphicalObject::operator=(source);
    this->mCompartment=source.getCompartmentId();    
  }
  
  return *this;
}

/**
 * Destructor.
 */        
CompartmentGlyph::~CompartmentGlyph ()
{
} 


/**
 * Returns the id of the associated compartment.
 */        
const std::string&
CompartmentGlyph::getCompartmentId () const
{
  return this->mCompartment;
}


/**
 * Sets the id of the associated compartment.
 */ 
void
CompartmentGlyph::setCompartmentId (const std::string& id)
{
  this->mCompartment = id;
}


/**
 * Returns true if the id of the associated compartment is not the empty
 * string.
 */  
bool 
CompartmentGlyph::isSetCompartmentId () const
{
  return ! this->mCompartment.empty();
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
void CompartmentGlyph::initDefaults ()
{
  GraphicalObject::initDefaults();
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
void CompartmentGlyph::writeElements (XMLOutputStream& stream) const
{
  GraphicalObject::writeElements(stream);
}

/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string& CompartmentGlyph::getElementName () const 
{
  static const std::string name = "compartmentGlyph";
  return name;
}

/**
 * @return a (deep) copy of this Model.
 */
SBase* 
CompartmentGlyph::clone () const
{
    return new CompartmentGlyph(*this);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
CompartmentGlyph::createObject (XMLInputStream& stream)
{
  //const std::string& name   = stream.peek().getName();
  SBase*        object = 0;

  object=GraphicalObject::createObject(stream);
  
  return object;
}

/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */

void CompartmentGlyph::readAttributes (const XMLAttributes& attributes)
{
  GraphicalObject::readAttributes(attributes);

  attributes.readInto("compartment", mCompartment);
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
void CompartmentGlyph::writeAttributes (XMLOutputStream& stream) const
{
  GraphicalObject::writeAttributes(stream);
  if(this->isSetCompartmentId())
  {
    stream.writeAttribute("compartment", mCompartment);
  }
}

/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
CompartmentGlyph::getTypeCode () const
{
  return SBML_LAYOUT_COMPARTMENTGLYPH;
}


/**
 * Creates an XMLNode object from this.
 */
XMLNode CompartmentGlyph::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("compartmentGlyph", "", "");
  XMLAttributes att = XMLAttributes();
  // add the SBase Ids
  addSBaseAttributes(*this,att);
  addGraphicalObjectAttributes(*this,att);
  if(this->isSetCompartmentId()) att.add("compartment",this->mCompartment);
  XMLToken token = XMLToken(triple, att, xmlns); 
  XMLNode node(token);
  // add the notes and annotations
  if(this->mNotes) node.addChild(*this->mNotes);
  if(this->mAnnotation) node.addChild(*this->mAnnotation);
  // write the bounding box
  node.addChild(this->mBoundingBox.toXML());
  return node;
}


/**
 * Creates a new CompartmentGlyph and returns the pointer to it.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_create(void)
{
  return new(std::nothrow) CompartmentGlyph;
}

/** @cond doxygen-libsbml-internal */
/**
 * Creates a new CompartmentGlyph_t structure using the given SBML @p 
 * level and @p version values and a set of XMLNamespaces.
 *
 * @param level an unsigned int, the SBML Level to assign to this 
 * Reaction
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Reaction
 * 
 * @param xmlns XMLNamespaces, a pointer to an array of XMLNamespaces to
 * assign to this Reaction
 *
 * @return a pointer to the newly created CompartmentGlyph_t structure.
 *
 * @note Once a BoundignBox has been added to an SBMLDocument, the @p 
 * level, @p version and @p xmlns namespaces for the document @em override 
 * those used to create the Reaction.  Despite this, the ability 
 * to supply the values at creation time is an important aid to creating 
 * valid SBML.  Knowledge of the intended SBML Level and Version 
 * determine whether it is valid to assign a particular value to an 
 * attribute, or whether it is valid to add an object to an existing 
 * SBMLDocument.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createWithLevelVersionAndNamespaces (unsigned int level,
              unsigned int version)
{
  return new(std::nothrow) CompartmentGlyph(level, version);
}
/** @endcond */


/**
 * Creates a new CompartmentGlyph from a template.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createFrom (const CompartmentGlyph_t *temp)
{
  return new(std::nothrow) CompartmentGlyph(*temp);
}


/**
 * Creates a new CompartmentGlyph with the given id
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createWith (const char *id)
{
  return new(std::nothrow) CompartmentGlyph(id ? id : "", "");
}


/**
 * Creates a new CompartmentGlyph with the given id
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createWithCompartmentId (const char *sid, const char *compId)
{
  return new(std::nothrow) CompartmentGlyph(sid ? sid : "", compId ? compId : "");
}


/**
 * Frees the memory taken by the given compartment glyph.
 */
LIBSBML_EXTERN
void
CompartmentGlyph_free (CompartmentGlyph_t *cg)
{
  delete cg;
}


/**
 * Sets the reference compartment for the compartment glyph.
 */
LIBSBML_EXTERN
void
CompartmentGlyph_setCompartmentId (CompartmentGlyph_t *cg, const char* id)
{
    static_cast<CompartmentGlyph*>(cg)->setCompartmentId( id ? id : "" );
}


/**
 * Gets the reference compartments id for the given compartment glyph.
 */
LIBSBML_EXTERN
const char *
CompartmentGlyph_getCompartmentId (const CompartmentGlyph_t *cg)
{
    return cg->isSetCompartmentId() ? cg->getCompartmentId().c_str() : NULL;
}


/**
 * Returns 0 if the reference compartment has not been set for this glyph
 * and 1 otherwise.
 */
LIBSBML_EXTERN
int
CompartmentGlyph_isSetCompartmentId (const CompartmentGlyph_t *cg)
{
  return static_cast<int>( cg->isSetCompartmentId() );
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
LIBSBML_EXTERN
void
CompartmentGlyph_initDefaults (CompartmentGlyph_t *cg)
{
  cg->initDefaults();
}

/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_clone (const CompartmentGlyph_t *m)
{
  return static_cast<CompartmentGlyph*>( m->clone() );
}

LIBSBML_CPP_NAMESPACE_END
