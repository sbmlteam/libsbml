/**
 * Filename    : SpeciesGlyph.cpp
 * Description : SBML Layout SpeciesGlyph source
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


#include "SpeciesGlyph.h"
#include "LayoutUtilities.h"
#include <sbml/SBMLNamespaces.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * Creates a new SpeciesGlyph with the id of the associated species set to
 * the empty string.
 */        
SpeciesGlyph::SpeciesGlyph () : GraphicalObject()
{
}

SpeciesGlyph::SpeciesGlyph (unsigned int level, unsigned int version):
   GraphicalObject (level, version)
{
}

                          
SpeciesGlyph::SpeciesGlyph (SBMLNamespaces *sbmlns) :
   GraphicalObject (sbmlns)
{
}
 

/**
 * Creates a new SpeciesGlyph with the given id.
 */ 
SpeciesGlyph::SpeciesGlyph (const std::string& sid):
    GraphicalObject( sid )
{
}

/**
 * Creates a new SpeciesGlyph with the given id and the id of the
 * associated species object set to the second argument.
 */ 
SpeciesGlyph::SpeciesGlyph (const std::string& sid,
                            const std::string& speciesId) : 
    GraphicalObject( sid )
  , mSpecies        ( speciesId )
{
}

/**
 * Creates a new SpeciesGlyph from the given XMLNode
 */
SpeciesGlyph::SpeciesGlyph(const XMLNode& node)
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
SpeciesGlyph::SpeciesGlyph(const SpeciesGlyph& source):GraphicalObject(source)
{
    this->mSpecies=source.getSpeciesId();
}

/**
 * Assignment operator.
 */
SpeciesGlyph& SpeciesGlyph::operator=(const SpeciesGlyph& source)
{
  if(&source!=this)
  {
    GraphicalObject::operator=(source);
    this->mSpecies=source.getSpeciesId();    
  }
  
  return *this;
}


/**
 * Destructor.
 */ 
SpeciesGlyph::~SpeciesGlyph ()
{
} 


/**
 * Returns the id of the associated species object.
 */ 
const std::string&
SpeciesGlyph::getSpeciesId () const
{
  return this->mSpecies;
}


/**
 * Sets the id of the associated species object.
 */ 
void
SpeciesGlyph::setSpeciesId (const std::string& id)
{
  this->mSpecies=id;
} 


/**
 * Returns true if the id of the associated species object is not the empty
 * string.
 */ 
bool
SpeciesGlyph::isSetSpeciesId () const
{
  return ! this->mSpecies.empty();
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
void SpeciesGlyph::initDefaults ()
{
  GraphicalObject::initDefaults();
}

/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string& SpeciesGlyph::getElementName () const 
{
  static const std::string name = "speciesGlyph";
  return name;
}

/**
 * @return a (deep) copy of this Model.
 */
SBase* 
SpeciesGlyph::clone () const
{
    return new SpeciesGlyph(*this);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
SpeciesGlyph::createObject (XMLInputStream& stream)
{
  SBase*        object = 0;

  object=GraphicalObject::createObject(stream);
  
  return object;
}

/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */

void SpeciesGlyph::readAttributes (const XMLAttributes& attributes)
{
  GraphicalObject::readAttributes(attributes);

  attributes.readInto("species", mSpecies);
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
void SpeciesGlyph::writeElements (XMLOutputStream& stream) const
{
  GraphicalObject::writeElements(stream);
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
void SpeciesGlyph::writeAttributes (XMLOutputStream& stream) const
{
  GraphicalObject::writeAttributes(stream);
  if(this->isSetSpeciesId())
  {
    stream.writeAttribute("species", mSpecies);
  }
}

/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
SpeciesGlyph::getTypeCode () const
{
  return SBML_LAYOUT_SPECIESGLYPH;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode SpeciesGlyph::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("speciesGlyph", "", "");
  XMLAttributes att = XMLAttributes();
  // add the SBase Ids
  addSBaseAttributes(*this,att);
  addGraphicalObjectAttributes(*this,att);
  if(this->isSetSpeciesId()) att.add("species",this->mSpecies);
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
 * Creates a new SpeciesGlyph and returns the pointer to it.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
SpeciesGlyph_create (void)
{
  return new(std::nothrow) SpeciesGlyph;
}

/** @cond doxygen-libsbml-internal */
/**
 * Creates a new SpeciesGlyph_t structure using the given SBML @p 
 * level and @p version values and a set of XMLNamespaces.
 *
 * @param level an unsigned int, the SBML Level to assign to this 
 * SpeciesGlyph
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SpeciesGlyph
 * 
 * @param xmlns XMLNamespaces, a pointer to an array of XMLNamespaces to
 * assign to this SpeciesGlyph
 *
 * @return a pointer to the newly created SpeciesGlyph_t structure.
 *
 * @note Once a SpeciesGlyph has been added to an SBMLDocument, the @p 
 * level, @p version and @p xmlns namespaces for the document @em override 
 * those used to create the Reaction.  Despite this, the ability 
 * to supply the values at creation time is an important aid to creating 
 * valid SBML.  Knowledge of the intended SBML Level and Version 
 * determine whether it is valid to assign a particular value to an 
 * attribute, or whether it is valid to add an object to an existing 
 * SBMLDocument.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
SpeciesGlyph_createWithLevelVersionAndNamespaces (unsigned int level,
              unsigned int version)
{
  return new(std::nothrow) SpeciesGlyph(level, version);
}
/** @endcond */


/**
 * Create a new SpeciesGlyph object from a template.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
SpeciesGlyph_createFrom (const SpeciesGlyph_t *temp)
{
  return new(std::nothrow) SpeciesGlyph(*temp);
}


/**
 * Creates a new SpeciesGlyph with the given id
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
SpeciesGlyph_createWith (const char *id)
{
  return new(std::nothrow) SpeciesGlyph(id ? id : "", "");
}


/**
 * Creates a new SpeciesGlyph referencing with the give species id.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
SpeciesGlyph_createWithSpeciesId (const char *sid, const char *speciesId)
{
  return new(std::nothrow) SpeciesGlyph(sid ? sid : "", speciesId ? speciesId : "");
}


/**
 * Frees the memory taken by the given compartment glyph.
 */
LIBSBML_EXTERN
void
SpeciesGlyph_free (SpeciesGlyph_t *sg)
{
  delete sg;
}


/**
 * Sets the associated species id. 
 */
LIBSBML_EXTERN
void
SpeciesGlyph_setSpeciesId (SpeciesGlyph_t *sg, const char *id)
{
    static_cast<SpeciesGlyph*>(sg)->setSpeciesId( id ? id : "" );
}


/**
 * Gets the the id of the associated species.
 */
LIBSBML_EXTERN
const char *
SpeciesGlyph_getSpeciesId (const SpeciesGlyph_t *sg)
{
    return sg->isSetSpeciesId() ? sg->getSpeciesId().c_str() : NULL ;
}



/**
 * Returns 0 if the  id of the associated species is the empty string.
 * otherwise.
 */
LIBSBML_EXTERN
int
SpeciesGlyph_isSetSpeciesId (const SpeciesGlyph_t *sg)
{
  return static_cast<int>( sg->isSetSpeciesId() );
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
LIBSBML_EXTERN
void
SpeciesGlyph_initDefaults (SpeciesGlyph_t *sg)
{
  sg->initDefaults();
}


/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
SpeciesGlyph_clone (const SpeciesGlyph_t *m)
{
  return static_cast<SpeciesGlyph*>( m->clone() );
}

LIBSBML_CPP_NAMESPACE_END

