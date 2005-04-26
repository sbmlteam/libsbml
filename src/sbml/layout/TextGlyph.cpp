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


#include "common/common.h"
#include "TextGlyph.h"


/**
 * Creates a new TextGlyph the ids of the associated GraphicalObject and
 * the originOfText are set to the empty string. The actual text is set to
 * the empty string as well.
 */  
LIBSBML_EXTERN
TextGlyph::TextGlyph ()
{
  init(SBML_LAYOUT_TEXTGLYPH);
}


/**
 * Creates a new TextGlpyh. The id is given as the first argument, the text
 * to be displayed as the second.  All other attirbutes are set to the
 * empty string.
 */ 
LIBSBML_EXTERN
TextGlyph::TextGlyph (const std::string& id, const std::string& text):
  GraphicalObject(id), text(text)
{
  init(SBML_LAYOUT_TEXTGLYPH);
}


/**
 * Destructor.
 */ 
LIBSBML_EXTERN
TextGlyph::~TextGlyph()
{
} 


/**
 * Returns the text to be displayed by the text glyph.
 */ 
LIBSBML_EXTERN
const std::string&
TextGlyph::getText() const
{
  return this->text;
}


/**
 * Sets the text to be displayed by the text glyph.
 */ 
LIBSBML_EXTERN
void
TextGlyph::setText (const std::string& text)
{
  this->text = text;
} 


/**
 * Returns the id of the associated graphical object.
 */ 
LIBSBML_EXTERN
const std::string&
TextGlyph::getGraphicalObjectId () const
{
  return this->graphicalObject;
}


/**
 * Sets the id of the associated graphical object.
 */ 
LIBSBML_EXTERN
void
TextGlyph::setGraphicalObjectId (const std::string& id)
{
  this->graphicalObject = id;
}


/**
 * Returns the id of the origin of text.
 */ 
LIBSBML_EXTERN
const std::string&
TextGlyph::getOriginOfTextId () const
{
  return this->originOfText;
}


/**
 * Sets the id of the origin of text.
 */ 
LIBSBML_EXTERN
void
TextGlyph::setOriginOfTextId (const std::string& orig)
{
  this->originOfText = orig;
}


/**
 * Returns true if the text is not the empty string.
 */ 
LIBSBML_EXTERN
bool
TextGlyph::isSetText () const
{
  return ! this->text.empty();
}


/**
 * Returns true if the id of the origin of text is not the empty string.
 */ 
LIBSBML_EXTERN
bool
TextGlyph::isSetOriginOfTextId () const
{
  return ! this->originOfText.empty();
}


/**
 * Returns true if the id of the associated graphical object is not the
 * empty string.
 */ 
LIBSBML_EXTERN
bool
TextGlyph::isSetGraphicalObjectId () const
{
  return ! this->graphicalObject.empty();
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
LIBSBML_EXTERN
void
TextGlyph::initDefaults()
{
  GraphicalObject::initDefaults();
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
