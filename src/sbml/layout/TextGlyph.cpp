/**
 * Filename    : TextGlyph.cpp
 * Description : SBML Layout TextGlyph source
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


#include "TextGlyph.h"
#include "LayoutUtilities.h"
#include <sbml/SBMLNamespaces.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * Creates a new TextGlyph the ids of the associated GraphicalObject and
 * the originOfText are set to the empty string. The actual text is set to
 * the empty string as well.
 */  
TextGlyph::TextGlyph ()
{
}


/**
 * Creates a new TextGlpyh. The id is given as the first argument.
 */ 
TextGlyph::TextGlyph (const std::string& id):
  GraphicalObject(id)
{
}

/**
 * Creates a new TextGlpyh. The id is given as the first argument, the text
 * to be displayed as the second.  All other attirbutes are set to the
 * empty string.
 */ 
TextGlyph::TextGlyph (const std::string& id, const std::string& text):
  GraphicalObject(id), mText(text)
{
}

TextGlyph::TextGlyph (unsigned int level, unsigned int version):
   GraphicalObject (level, version)
{
}

                          
TextGlyph::TextGlyph (SBMLNamespaces *sbmlns) :
   GraphicalObject (sbmlns)
{
}
 
/**
 * Creates a new TextGlyph from the given XMLNode
 */
TextGlyph::TextGlyph(const XMLNode& node)
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
TextGlyph::TextGlyph(const TextGlyph& source):GraphicalObject(source)
{
    this->mText=source.getText();
    this->mOriginOfText=source.getOriginOfTextId();
    this->mGraphicalObject=source.getGraphicalObjectId();    
}

/**
 * Assignment operator.
 */
TextGlyph& TextGlyph::operator=(const TextGlyph& source)
{
  if(&source!=this)
  {
    GraphicalObject::operator=(source);
    this->mText=source.getText();
    this->mOriginOfText=source.getOriginOfTextId();
    this->mGraphicalObject=source.getGraphicalObjectId();    
  }
  
  return *this;
}

/**
 * Destructor.
 */ 
TextGlyph::~TextGlyph()
{
} 


/**
 * Returns the text to be displayed by the text glyph.
 */ 
const std::string&
TextGlyph::getText() const
{
  return this->mText;
}


/**
 * Sets the text to be displayed by the text glyph.
 */ 
void
TextGlyph::setText (const std::string& text)
{
  this->mText = text;
} 


/**
 * Returns the id of the associated graphical object.
 */ 
const std::string&
TextGlyph::getGraphicalObjectId () const
{
  return this->mGraphicalObject;
}


/**
 * Sets the id of the associated graphical object.
 */ 
void
TextGlyph::setGraphicalObjectId (const std::string& id)
{
  this->mGraphicalObject = id;
}


/**
 * Returns the id of the origin of text.
 */ 
const std::string&
TextGlyph::getOriginOfTextId () const
{
  return this->mOriginOfText;
}


/**
 * Sets the id of the origin of text.
 */ 
void
TextGlyph::setOriginOfTextId (const std::string& orig)
{
  this->mOriginOfText = orig;
}


/**
 * Returns true if the text is not the empty string.
 */ 
bool
TextGlyph::isSetText () const
{
  return ! this->mText.empty();
}


/**
 * Returns true if the id of the origin of text is not the empty string.
 */ 
bool
TextGlyph::isSetOriginOfTextId () const
{
  return ! this->mOriginOfText.empty();
}


/**
 * Returns true if the id of the associated graphical object is not the
 * empty string.
 */ 
bool
TextGlyph::isSetGraphicalObjectId () const
{
  return ! this->mGraphicalObject.empty();
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
void
TextGlyph::initDefaults()
{
  GraphicalObject::initDefaults();
}

/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string& TextGlyph::getElementName () const 
{
  static const std::string name = "textGlyph";
  return name;
}

/**
 * @return a (deep) copy of this Model.
 */
SBase* 
TextGlyph::clone () const
{
    return new TextGlyph(*this);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
TextGlyph::createObject (XMLInputStream& stream)
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

void TextGlyph::readAttributes (const XMLAttributes& attributes)
{
  GraphicalObject::readAttributes(attributes);

  attributes.readInto("text", mText);
  attributes.readInto("graphicalObject", mGraphicalObject);
  attributes.readInto("originOfText", mOriginOfText);
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
void TextGlyph::writeElements (XMLOutputStream& stream) const
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
void TextGlyph::writeAttributes (XMLOutputStream& stream) const
{
  GraphicalObject::writeAttributes(stream);
  if(this->isSetText())
  {
     stream.writeAttribute("text", mText);
  }
  else if(this->isSetOriginOfTextId())
  {
     stream.writeAttribute("originOfText", mOriginOfText);
  }
  if(this->isSetGraphicalObjectId())
  {
    stream.writeAttribute("graphicalObject", mGraphicalObject);
  }
}

/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
TextGlyph::getTypeCode () const
{
  return SBML_LAYOUT_TEXTGLYPH;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode TextGlyph::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("textGlyph", "", "");
  XMLAttributes att = XMLAttributes();
  // add the SBase Ids
  addSBaseAttributes(*this,att);
  addGraphicalObjectAttributes(*this,att);
  if(this->isSetText()) att.add("text",this->mText);
  if(this->isSetGraphicalObjectId()) att.add("graphicalObject",this->mGraphicalObject);
  if(this->isSetOriginOfTextId()) att.add("originOfText",this->mOriginOfText);
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
 * Creates a new TextGlyph and returns the pointer to it.
 */
LIBSBML_EXTERN
TextGlyph_t *
TextGlyph_create (void)
{
  return new(std::nothrow) TextGlyph;
}

/** @cond doxygen-libsbml-internal */
/**
 * Creates a new TextGlyph_t structure using the given SBML @p 
 * level and @p version values and a set of XMLNamespaces.
 *
 * @param level an unsigned int, the SBML Level to assign to this 
 * TextGlyph
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * TextGlyph
 * 
 * @param xmlns XMLNamespaces, a pointer to an array of XMLNamespaces to
 * assign to this TextGlyph
 *
 * @return a pointer to the newly created TextGlyph_t structure.
 *
 * @note Once a TextGlyph has been added to an SBMLDocument, the @p 
 * level, @p version and @p xmlns namespaces for the document @em override 
 * those used to create the Reaction.  Despite this, the ability 
 * to supply the values at creation time is an important aid to creating 
 * valid SBML.  Knowledge of the intended SBML Level and Version 
 * determine whether it is valid to assign a particular value to an 
 * attribute, or whether it is valid to add an object to an existing 
 * SBMLDocument.
 */
LIBSBML_EXTERN
TextGlyph_t *
TextGlyph_createWithLevelVersionAndNamespaces (unsigned int level,
              unsigned int version)
{
  return new(std::nothrow) TextGlyph(level, version);
}
/** @endcond */


/**
 * Creates a new TextGlyph from a template.
 */
LIBSBML_EXTERN
TextGlyph_t *
TextGlyph_createFrom (const TextGlyph_t *temp)
{
  return new(std::nothrow) TextGlyph(*temp);
}


/**
 * Creates a new TextGlyph with the given id
 */
LIBSBML_EXTERN
TextGlyph_t *
TextGlyph_createWith (const char *sid)
{
  return new(std::nothrow) TextGlyph(sid ? sid : "", "");
}


/**
 * Creates a new TextGlyph referencing the give text.
 */
LIBSBML_EXTERN
TextGlyph_t *
TextGlyph_createWithText (const char *id, const char *text)
{  
  return new(std::nothrow) TextGlyph(id ? id : "", text ? text : "");
}


/**
 * Frees the memory taken by the given text glyph.
 */
LIBSBML_EXTERN
void
TextGlyph_free (TextGlyph_t *tg)
{
  delete tg;
}


/**
 * Sets the text for the text glyph.
 */
LIBSBML_EXTERN
void
TextGlyph_setText (TextGlyph_t *tg, const char *text)
{
    tg->setText( text ? text : "" );
}


/**
 * Sets the id of the origin of the text for the text glyph.  This can be
 * the id of any valid sbml model object. The name of the object is then
 * taken as the text for the TextGlyph.
 */
LIBSBML_EXTERN
void
TextGlyph_setOriginOfTextId (TextGlyph_t *tg, const char *sid)
{
    tg->setOriginOfTextId( sid ? sid : "" );
}


/**
 * Sets the assoziated GraphicalObject id for the text glyph.  A TextGlyph
 * which is assoziated with a GraphicalObject can be considered as a label
 * to that object and they might for example be moved together in an
 * editor.
 */
LIBSBML_EXTERN
void
TextGlyph_setGraphicalObjectId (TextGlyph_t *tg, const char *sid)
{
    tg->setGraphicalObjectId( sid ? sid : "" );
}


/**
 * Returns the text associated with this text glyph.
 */
LIBSBML_EXTERN
const char *
TextGlyph_getText (const TextGlyph_t *tg)
{
    return tg->isSetText() ? tg->getText().c_str() : NULL;
}


/**
 * Returns the id of the origin of the text associated with this text
 * glyph.
 */
LIBSBML_EXTERN
const char *
TextGlyph_getGraphicalObjectId (const TextGlyph_t *tg)
{
    return tg->isSetGraphicalObjectId() ? tg->getGraphicalObjectId().c_str() : NULL;
}


/**
 * Returns the id of the graphical object associated with this text glyph.
 */
LIBSBML_EXTERN
const char *
TextGlyph_getOriginOfTextId (const TextGlyph_t *tg)
{
    return tg->isSetOriginOfTextId() ? tg->getOriginOfTextId().c_str() : NULL;
}


/**
 * Returns true is the text attribute is not the empty string.
 */
LIBSBML_EXTERN
int
TextGlyph_isSetText (const TextGlyph_t *tg)
{
  return static_cast<int>( tg->isSetText() );
}


/**
 * Returns true is the originOfText attribute is not the empty string.
 */
LIBSBML_EXTERN
int
TextGlyph_isSetOriginOfTextId (const TextGlyph_t *tg)
{
  return static_cast<int>( tg->isSetOriginOfTextId() );
}


/**
 * Returns true is the id of the associated graphical object is not the
 * empty string.
 */
LIBSBML_EXTERN
int
TextGlyph_isSetGraphicalObjectId (const TextGlyph_t *tg)
{
  return static_cast<int>( tg->isSetGraphicalObjectId() );
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
LIBSBML_EXTERN
void
TextGlyph_initDefaults (TextGlyph_t *tg)
{
  tg->initDefaults();
}

/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
TextGlyph_t *
TextGlyph_clone (const TextGlyph_t *m)
{
  return static_cast<TextGlyph*>( m->clone() );
}

LIBSBML_CPP_NAMESPACE_END

