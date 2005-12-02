/**
 * Filename    : Layout.cpp
 * Description : SBML Layout Layout source
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
 * European Media Laboratory Research gGmbH have no obligations to provide
 * maintenance, support, updates, enhancements or modifications.  In no
 * event shall the European Media Laboratories Research gGmbH be liable to
 * any party for direct, indirect, special, incidental or consequential
 * damages, including lost profits, arising out of the use of this software
 * and its documentation, even if the European Media Laboratory Research
 * gGmbH have been advised of the possibility of such damage.  See the GNU
 * Lesser General Public License for more details.
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
#include "Layout.h"
#include <climits> 
#include <iostream>


/**
 * Creates a new Layout.
 */
LIBSBML_EXTERN
Layout::Layout () : SBase ()
{
  init(SBML_LAYOUT_LAYOUT);
}


/**
 * Creates a new Layout with the given id and dimensions.
 */
LIBSBML_EXTERN
Layout::Layout (const std::string& id, const Dimensions& dimensions) :
    SBase      ()
  , id         ( id )
  , dimensions ( dimensions )
{
  init(SBML_LAYOUT_LAYOUT);
}


/**
 * Destructor.
 */ 
LIBSBML_EXTERN
Layout::~Layout ()
{
}


/**
 * Does nothing since no defaults are defined for Layout.
 */ 
LIBSBML_EXTERN
void
Layout::initDefaults ()
{
}


/**
 * Returns the id.
 */ 
LIBSBML_EXTERN
const std::string&
Layout::getId () const
{
  return this->id;
}


/**
 * Sets the id to a copy of the given string.
 */ 
LIBSBML_EXTERN
void
Layout::setId (const std::string& id)
{
  this->id = id;
}


/**
 * Returns the dimensions of the layout.
 */ 
LIBSBML_EXTERN
const Dimensions&
Layout::getDimensions() const
{
  return this->dimensions;
}


/**
 * Returns the dimensions of the layout.
 */ 
LIBSBML_EXTERN
Dimensions&
Layout::getDimensions()
{
  return this->dimensions;
}


/**
 * Sets the dimensions of the layout.
 */ 
LIBSBML_EXTERN
void
Layout::setDimensions (const Dimensions& dimensions)
{
  this->dimensions = dimensions;
}


/**
 * Returns the ListOf object that holds all compartment glyphs.
 */ 
LIBSBML_EXTERN
const ListOf&
Layout::getListOfCompartmentGlyphs () const
{
  return this->compartmentGlyphs;
}


/**
 * Returns the ListOf object that holds all species glyphs.
 */ 
LIBSBML_EXTERN
const ListOf&
Layout::getListOfSpeciesGlyphs () const
{
  return this->speciesGlyphs;
}


/**
 * Returns the ListOf object that holds all reaction glyphs.
 */ 
LIBSBML_EXTERN
const ListOf&
Layout::getListOfReactionGlyphs () const
{
  return this->reactionGlyphs;
}


/**
 * Returns the ListOf object that holds all text glyphs.
 */ 
LIBSBML_EXTERN
const ListOf&
Layout::getListOfTextGlyphs () const
{
  return this->textGlyphs;
}


/**
 * Returns the ListOf object that holds all additonal graphical objects.
 */ 
LIBSBML_EXTERN
const ListOf&
Layout::getListOfAdditionalGraphicalObjects () const
{
  return this->additionalGraphicalObjects;
}


/**
 * Returns the ListOf object that holds all compartment glyphs.
 */ 
LIBSBML_EXTERN
ListOf&
Layout::getListOfCompartmentGlyphs ()
{
  return this->compartmentGlyphs;
}


/**
 * Returns the ListOf object that holds all species glyphs.
 */ 
LIBSBML_EXTERN
ListOf&
Layout::getListOfSpeciesGlyphs ()
{
  return this->speciesGlyphs;
}


/**
 * Returns the ListOf object that holds all reaction glyphs.
 */ 
LIBSBML_EXTERN
ListOf&
Layout::getListOfReactionGlyphs ()
{
  return this->reactionGlyphs;
}


/**
 * Returns the ListOf object that holds all text glyphs.
 */ 
LIBSBML_EXTERN
ListOf&
Layout::getListOfTextGlyphs ()
{
  return this->textGlyphs;
}


/**
 * Returns the ListOf object that holds all additional graphical objects.
 */ 
LIBSBML_EXTERN
ListOf&
Layout::getListOfAdditionalGraphicalObjects ()
{
  return this->additionalGraphicalObjects;
}


/**
 * Returns the compartment glyph with the given index.  If the index is
 * invalid, NULL is returned.
 */ 
LIBSBML_EXTERN
CompartmentGlyph*
Layout::getCompartmentGlyph (unsigned int index) const
{
  return static_cast<CompartmentGlyph*>( this->compartmentGlyphs.get(index) );
}


/**
 * Returns the species glyph with the given index.  If the index is
 * invalid, NULL is returned.
 */ 
LIBSBML_EXTERN
SpeciesGlyph*
Layout::getSpeciesGlyph (unsigned int index) const
{
  return static_cast<SpeciesGlyph*>( this->speciesGlyphs.get(index) );
}


/**
 * Returns the reaction glyph with the given index.  If the index is
 * invalid, NULL is returned.
 */ 
LIBSBML_EXTERN
ReactionGlyph*
Layout::getReactionGlyph (unsigned int index) const
{
  return static_cast<ReactionGlyph*>( this->reactionGlyphs.get(index) );
}


/**
 * Returns the text glyph with the given index.  If the index is invalid,
 * NULL is returned.
 */ 
LIBSBML_EXTERN
TextGlyph*
Layout::getTextGlyph (unsigned int index) const
{
  return static_cast<TextGlyph*>( this->textGlyphs.get(index) );
}


/**
 * Returns the additional graphical object with the given index.
 * If the index is invalid, NULL is returned.
 */ 
LIBSBML_EXTERN
GraphicalObject*
Layout::getAdditionalGraphicalObject (unsigned int index) const
{
  return static_cast<GraphicalObject*>
  ( 
    this->additionalGraphicalObjects.get(index)
  );
}


GraphicalObject*
Layout::getObjectWithId (const ListOf& list,const std::string& id) const
{
  GraphicalObject* object=NULL;
  unsigned int counter=0;
  while(counter < list.getNumItems()) {
    GraphicalObject* tmp=(GraphicalObject*)list.get(counter);
    if(tmp->getId()==id){
      object=tmp;
      break;
    }
    ++counter;
  }    
  return object;
}

LIBSBML_EXTERN
GraphicalObject*
Layout::removeObjectWithId (ListOf& list,const std::string& id)
{
  GraphicalObject* object=NULL;
  unsigned int counter=0;
  while(counter < list.getNumItems()) {
    GraphicalObject* tmp=(GraphicalObject*)list.get(counter);
    if(tmp->getId()==id){
      object=tmp;
      list.remove(counter);
      break;
    }
    ++counter;
  }    
  return object;
}

/**
 * Removes the compartment glyph with the given index from the layout.
 * A pointer to the compartment glyph that was removed is returned.
 * If no compartment glyph has been removed, NULL is returned.
 */
LIBSBML_EXTERN
CompartmentGlyph* Layout::removeCompartmentGlyph(unsigned int index)
{
    CompartmentGlyph* glyph=NULL;
    if(index < this->getNumCompartmentGlyphs())
    {
      glyph=dynamic_cast<CompartmentGlyph*>(this->getListOfCompartmentGlyphs().remove(index));
    }
    return glyph;
}

/**
 * Removes the species glyph with the given index from the layout.
 * A pointer to the species glyph that was removed is returned.
 * If no species glyph has been removed, NULL is returned.
 */
LIBSBML_EXTERN
SpeciesGlyph* Layout::removeSpeciesGlyph(unsigned int index)
{
    SpeciesGlyph* glyph=NULL;
    if(index < this->getNumSpeciesGlyphs())
    {
      glyph=dynamic_cast<SpeciesGlyph*>(this->getListOfSpeciesGlyphs().remove(index));
    }
    return glyph;
}

/**
 * Removes the reaction glyph with the given index from the layout.
 * A pointer to the reaction glyph that was removed is returned.
 * If no reaction glyph has been removed, NULL is returned.
 */
LIBSBML_EXTERN
ReactionGlyph* Layout::removeReactionGlyph(unsigned int index)
{
    ReactionGlyph* glyph=NULL;
    if(index < this->getNumReactionGlyphs())
    {
      glyph=dynamic_cast<ReactionGlyph*>(this->getListOfReactionGlyphs().remove(index));
    }
    return glyph;
}

/**
 * Removes the text glyph with the given index from the layout.
 * A pointer to the text glyph that was removed is returned.
 * If no text glyph has been removed, NULL is returned.
 */
LIBSBML_EXTERN
TextGlyph* Layout::removeTextGlyph(unsigned int index)
{
    TextGlyph* glyph=NULL;
    if(index < this->getNumTextGlyphs())
    {
      glyph=dynamic_cast<TextGlyph*>(this->getListOfTextGlyphs().remove(index));
    }
    return glyph;
}

/**
 * Removes the graphical object with the given index from the layout.
 * A pointer to the graphical object that was removed is returned.
 * If no graphical object has been removed, NULL is returned.
 */
LIBSBML_EXTERN
GraphicalObject* Layout::removeAdditionalGraphicalObject(unsigned int index)
{
    GraphicalObject* go=NULL;
    if(index < this->getNumAdditionalGraphicalObjects())
    {
      go=dynamic_cast<GraphicalObject*>(this->getListOfAdditionalGraphicalObjects().remove(index));
    }
    return go;
}

/**
 * Remove the compartment glyph with the given id.
 * A pointer to the removed compartment glyph is returned.
 * If no compartment glyph has been removed, NULL is returned.
 */
LIBSBML_EXTERN
CompartmentGlyph*
Layout::removeCompartmentGlyph(const std::string id)
{
    return dynamic_cast<CompartmentGlyph*>(this->removeObjectWithId(this->getListOfCompartmentGlyphs(),id));
}

/**
 * Remove the species glyph with the given id.
 * A pointer to the removed species glyph is returned.
 * If no species glyph has been removed, NULL is returned.
 */
LIBSBML_EXTERN
SpeciesGlyph*
Layout::removeSpeciesGlyph(const std::string id)
{
    return dynamic_cast<SpeciesGlyph*>(this->removeObjectWithId(this->getListOfSpeciesGlyphs(),id));
}

/**
 * Remove the species reference glyph with the given id.
 * A pointer to the removed species glyph is returned.
 * If no species glyph has been removed, NULL is returned.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph*
Layout::removeSpeciesReferenceGlyph(const std::string id)
{
    SpeciesReferenceGlyph *srg=NULL;
    unsigned int i,iMax=this->getNumReactionGlyphs();
    for(i=0;i<iMax;++i)
    {
        ReactionGlyph* rg=this->getReactionGlyph(i);
        unsigned int index=rg->getIndexForSpeciesReferenceGlyph(id);
        if(index!=std::numeric_limits<int>::max())
        {
            srg=rg->removeSpeciesReferenceGlyph(index);
            break;
        }
    }
    return srg;
}

/**
 * Remove the reaction glyph with the given id.
 * A pointer to the removed reaction glyph is returned.
 * If no reaction glyph has been removed, NULL is returned.
 */
LIBSBML_EXTERN
ReactionGlyph*
Layout::removeReactionGlyph(const std::string id)
{
    return dynamic_cast<ReactionGlyph*>(this->removeObjectWithId(this->getListOfReactionGlyphs(),id));
}

/**
 * Remove the text glyph with the given id.
 * A pointer to the removed text glyph is returned.
 * If no text glyph has been removed, NULL is returned.
 */
LIBSBML_EXTERN
TextGlyph*
Layout::removeTextGlyph(const std::string id)
{
    return dynamic_cast<TextGlyph*>(this->removeObjectWithId(this->getListOfTextGlyphs(),id));
}

/**
 * Remove the graphical object with the given id.
 * A pointer to the removed graphical object is returned.
 * If no graphical object has been removed, NULL is returned.
 */
LIBSBML_EXTERN
GraphicalObject*
Layout::removeAdditionalGraphicalObject(const std::string id)
{
    return this->removeObjectWithId(this->getListOfAdditionalGraphicalObjects(),id);
}

/**
 * Returns the compartment glyph that has the given id, or NULL if no
 * compartment glyph has the id.
 */
LIBSBML_EXTERN
CompartmentGlyph*
Layout::getCompartmentGlyph (const std::string& id) const
{
  return (CompartmentGlyph*) this->getObjectWithId(this->compartmentGlyphs, id);
}


/**
 * Returns the species glyph that has the given id, or NULL if no
 * compartment glyph has the id.
 */
LIBSBML_EXTERN
SpeciesGlyph*
Layout::getSpeciesGlyph (const std::string& id) const
{
  return (SpeciesGlyph*) this->getObjectWithId(this->speciesGlyphs, id);
}


/**
 * Returns the reaction glyph that has the given id, or NULL if no
 * compartment glyph has the id.
 */
LIBSBML_EXTERN
ReactionGlyph*
Layout::getReactionGlyph (const std::string& id) const
{
  return (ReactionGlyph*) this->getObjectWithId(this->reactionGlyphs, id);
}


/**
 * Returns the text glyph that has the given id, or NULL if no compartment
 * glyph has the id.
 */
LIBSBML_EXTERN
TextGlyph*
Layout::getTextGlyph (const std::string& id) const
{
  return (TextGlyph*) this->getObjectWithId(this->textGlyphs, id);
}


/**
 * Returns the additional graphicalo object that has the given id, or NULL
 * if no compartment glyph has the id.
 */
LIBSBML_EXTERN
GraphicalObject*
Layout::getAdditionalGraphicalObject (const std::string& id) const
{
  return this->getObjectWithId(this->additionalGraphicalObjects, id);
}


/**
 * Adds a new compartment glyph.
 */
LIBSBML_EXTERN
void
Layout::addCompartmentGlyph (CompartmentGlyph& glyph)
{
  this->compartmentGlyphs.append(&glyph);
}


/**
 * Adds a new species glyph.
 */
LIBSBML_EXTERN
void
Layout::addSpeciesGlyph (SpeciesGlyph& glyph)
{
  this->speciesGlyphs.append(&glyph);
}


/**
 * Adds a new reaction glyph.
 */
LIBSBML_EXTERN
void
Layout::addReactionGlyph (ReactionGlyph& glyph)
{
  this->reactionGlyphs.append(&glyph);
}


/**
 * Adds a new text glyph.
 */
LIBSBML_EXTERN
void
Layout::addTextGlyph (TextGlyph& glyph)
{
  this->textGlyphs.append(&glyph);
}


/**
 * Adds a new additional graphical object glyph.
 */
LIBSBML_EXTERN
void
Layout::addAdditionalGraphicalObject (GraphicalObject& glyph)
{
  this->additionalGraphicalObjects.append(&glyph);
}


/**
 * Returns the number of compartment glyphs for the layout.
 */
LIBSBML_EXTERN
unsigned int
Layout::getNumCompartmentGlyphs () const
{
  return this->compartmentGlyphs.getNumItems();
}


/**
 * Returns the number of species glyphs for the layout.
 */
LIBSBML_EXTERN
unsigned int
Layout::getNumSpeciesGlyphs () const
{
  return this->speciesGlyphs.getNumItems();
}


/**
 * Returns the number of reaction glyphs for the layout.
 */
LIBSBML_EXTERN
unsigned int
Layout::getNumReactionGlyphs () const
{
  return this->reactionGlyphs.getNumItems();
}


/**
 * Returns the number of text glyphs for the layout.
 */
LIBSBML_EXTERN
unsigned int
Layout::getNumTextGlyphs () const
{
  return this->textGlyphs.getNumItems();
}


/**
 * Returns the number of additional graphical objects for the layout.
 */
LIBSBML_EXTERN
unsigned int
Layout::getNumAdditionalGraphicalObjects () const
{
  return this->additionalGraphicalObjects.getNumItems();
}


/**
 * Returns true if the id is not the empty string.
 */ 
LIBSBML_EXTERN
bool
Layout::isSetId () const
{
  return ! this->id.empty();
}


/**
 * Creates a CompartmentGlyph object, adds it to the end of the compartment
 * glyph objects list and returns a reference to the newly created object.
 */
LIBSBML_EXTERN
CompartmentGlyph* 
Layout::createCompartmentGlyph ()
{
  CompartmentGlyph* p = new CompartmentGlyph();

  this->addCompartmentGlyph(*p);
  return p;
}


/**
 * Creates a SpeciesGlyph object, adds it to the end of the species glyph
 * objects list and returns a reference to the newly created object.
 */
LIBSBML_EXTERN
SpeciesGlyph* 
Layout::createSpeciesGlyph ()
{
  SpeciesGlyph* p = new SpeciesGlyph();

  this->addSpeciesGlyph(*p);
  return p;
}


/**
 * Creates a ReactionGlyph object, adds it to the end of the reaction glyph
 * objects list and returns a reference to the newly created object.
 */
LIBSBML_EXTERN
ReactionGlyph* 
Layout::createReactionGlyph ()
{
  ReactionGlyph* p = new ReactionGlyph();

  this->addReactionGlyph(*p);
  return p;
}


/**
 * Creates a TextGlyph object, adds it to the end of the text glyph objects
 * list and returns a reference to the newly created object.
 */
LIBSBML_EXTERN
TextGlyph* 
Layout::createTextGlyph ()
{
  TextGlyph* p = new TextGlyph();

  this->addTextGlyph(*p);
  return p;
}


/**
 * Creates a GraphicalObject object, adds it to the end of the additional
 * graphical objects list and returns a reference to the newly created
 * object.
 */
LIBSBML_EXTERN
GraphicalObject* 
Layout::createAdditionalGraphicalObject ()
{
  GraphicalObject* p = new GraphicalObject();

  this->addAdditionalGraphicalObject(*p);
  return p;
}


/**
 * Creates a new SpeciesReferenceGlyph for the last ReactionGlyph and adds
 * it to its list of SpeciesReferenceGlyph objects.  A pointer to the newly
 * created object is returned.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph* 
Layout::createSpeciesReferenceGlyph ()
{
  int size = this->reactionGlyphs.getNumItems();
  if (size == 0) return NULL;

  ReactionGlyph* r =(ReactionGlyph*) this->getReactionGlyph(size - 1);
  return &r->createSpeciesReferenceGlyph();
}


/**
 * Creates a new LineSegment for the Curve object of the last ReactionGlyph
 * or the last SpeciesReferenceGlyph in the last ReactionGlyph and adds it
 * to its list of SpeciesReferenceGlyph objects.  A pointer to the newly
 * created object is returned.
 */
LIBSBML_EXTERN
LineSegment* 
Layout::createLineSegment()
{
  int size = this->reactionGlyphs.getNumItems();
  if (size == 0) return NULL;

  LineSegment*   ls = NULL;
  ReactionGlyph* r  = (ReactionGlyph*) this->getReactionGlyph(size - 1);

  size = r->getListOfSpeciesReferenceGlyphs().getNumItems();
  if(size > 0)
  {
    SpeciesReferenceGlyph* srg = r->getSpeciesReferenceGlyph(size-1);
    ls = &srg->createLineSegment();
  }
  else
  {
    ls = &r->createLineSegment();
  }

  return ls;
}        


/**
 * Creates a new CubicBezier for the Curve object of the last ReactionGlyph
 * or the last SpeciesReferenceGlyph in the last ReactionGlyph and adds it
 * to its list of SpeciesReferenceGlyph objects.  A pointer to the newly
 * created object is returned.
 */
LIBSBML_EXTERN
CubicBezier* 
Layout::createCubicBezier ()
{
  int size = this->reactionGlyphs.getNumItems();
  if (size == 0) return NULL;

  CubicBezier*   cb = NULL;
  ReactionGlyph* r  = (ReactionGlyph*) this->getReactionGlyph(size - 1);

  size = r->getListOfSpeciesReferenceGlyphs().getNumItems();
  if(size > 0)
  {
    SpeciesReferenceGlyph* srg = r->getSpeciesReferenceGlyph(size-1);
    cb = &srg->createCubicBezier();
  }
  else
  {
    cb = &r->createCubicBezier();
  }

  return cb;
}    




/**
 * Creates a new Layout and returns a pointer to it.
 */
LIBSBML_EXTERN
Layout_t *
Layout_create (void)
{
  return new(std::nothrow) Layout;
}


/**
 * Creates a new Layout with the given id and returns a pointer to it.
 */
LIBSBML_EXTERN
Layout_t *
Layout_createWith (const char *sid)
{
  return new(std::nothrow) Layout(sid ? sid : "", Dimensions());
}


/**
 * Creates a Layout object from a template.
 */
LIBSBML_EXTERN
    Layout_t *
Layout_createFrom (const Layout_t *temp)
{
  return new(std::nothrow) Layout(*temp);
}


/**
 * Creates a new Layout with the given width, height and depth and returns
 * a pointer to it.  The depth value defaults to 0.0.
 */
LIBSBML_EXTERN
Layout_t *
Layout_createWithSize (const char *id,
                       double width, double height, double depth)
{
  return new (std::nothrow) Layout(id ? id : "", Dimensions(width, height, depth));
}


/**
 * Creates a new Layout with the given Dimensions and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
Layout_t *
Layout_createWithDimensions (const char *id, const Dimensions_t *dimensions)
{
  return new (std::nothrow) Layout(id ? id : "", *dimensions);
}


/** 
 * Frees the memory for the given layout.
 */
LIBSBML_EXTERN
void
Layout_free (Layout_t *l)
{
  delete l;
}


/**
 * Sets the id of the layout.
 */
LIBSBML_EXTERN
void
Layout_setId (Layout_t *l, const char *id)
{
    static_cast<Layout*>(l)->setId( id ? id : "" );
}


/**
 * Adds a new compartment glyph to the list of compartment glyphs.
 */
LIBSBML_EXTERN
void
Layout_addCompartmentGlyph (Layout_t *l, CompartmentGlyph_t *cg)
{
  l->addCompartmentGlyph(*cg);
}


/**
 * Adds a new species glyph to the list of species glyphs.
 */
LIBSBML_EXTERN
void
Layout_addSpeciesGlyph (Layout_t *l, SpeciesGlyph_t *sg)
{
  l->addSpeciesGlyph(*sg);
}


/**
 * Adds a new reaction glyph to the list of reaction glyphs.
 */
LIBSBML_EXTERN
void
Layout_addReactionGlyph (Layout_t *l, ReactionGlyph_t *rg)
{
  l->addReactionGlyph(*rg);
}


/**
 * Adds a new TextGlyph to the list of text glyphs.
 */
LIBSBML_EXTERN
void
Layout_addTextGlyph (Layout_t *l, TextGlyph_t *tg)
{
  l->addTextGlyph(*tg);
}


/**
 * Adds a new GraphicalObject to the list of additional graphical objects.
 */
LIBSBML_EXTERN
void
Layout_addAdditionalGraphicalObject (Layout_t *l, GraphicalObject_t *go)
{
  l->addAdditionalGraphicalObject(*go);
}


/**
 * Returns a pointer to the CompartmentGlyph with the given index.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
Layout_getCompartmentGlyph (const Layout_t *l, unsigned int index)
{
  return l->getCompartmentGlyph(index);
}


/**
 * Returns a pointer to the SpeciesGlyph with the given index.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
Layout_getSpeciesGlyph (const Layout_t *l, unsigned int index)
{
  return l->getSpeciesGlyph(index);
}


/**
 * Returns a pointer to the ReactionGlyph with the given index.
 */
LIBSBML_EXTERN
ReactionGlyph_t *
Layout_getReactionGlyph (const Layout_t *l, unsigned int index)
{
  return l->getReactionGlyph(index);
}


/**
 * Returns a pointer to the AdditionalGraphicalObject with the given index.
 */
LIBSBML_EXTERN
TextGlyph_t *
Layout_getTextGlyph (const Layout_t *l, unsigned int index)
{
  return l->getTextGlyph(index);
}



/**
 * Returns a pointer to the GraphicalObject with the given index.
 */
LIBSBML_EXTERN
GraphicalObject_t *
Layout_getAdditionalGraphicalObject (const Layout_t *l, unsigned int index)
{
  return l->getAdditionalGraphicalObject(index);
}


/**
 * Returns a pointer to the list of CompartmentGlyphs.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfCompartmentGlyphs (Layout_t *l)
{
  return & l->getListOfCompartmentGlyphs();
}


/**
 * Returns a pointer to the list of SpeciesGlyphs.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfSpeciesGlyphs (Layout_t *l)
{
  return & l->getListOfSpeciesGlyphs();
}


/**
 * Returns a pointer to the list of ReactionGlyphs.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfReactionGlyphs (Layout_t *l)
{
  return & l->getListOfReactionGlyphs();
}


/**
 * Returns a pointer to the list of TextGlyphs.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfTextGlyphs (Layout_t *l)
{
  return & l->getListOfTextGlyphs();
}


/**
 * Returns a pointer to the list of additional GraphicalObjects.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfAdditionalGraphicalObjects (Layout_t *l)
{
  return & l->getListOfAdditionalGraphicalObjects();
}


/**
 * Returns a the id of the layout.
 */
LIBSBML_EXTERN
const char *
Layout_getId (const Layout_t *l)
{
    return l->isSetId() ? l->getId().c_str() : NULL;
}


/**
 * Returns the number of CompartmentGlyphs.
 */
LIBSBML_EXTERN
unsigned int
Layout_getNumCompartmentGlyphs (const Layout_t *l)
{
  return l->getNumCompartmentGlyphs();
}


/**
 * Returns the number of SpeciesGlyphs.
 */
LIBSBML_EXTERN
unsigned int
Layout_getNumSpeciesGlyphs (const Layout_t *l)
{
  return l->getNumSpeciesGlyphs();
}


/**
 * Returns the number of ReactionGlyphs.
 */
LIBSBML_EXTERN
unsigned int
Layout_getNumReactionGlyphs (const Layout_t *l)
{
  return l->getNumReactionGlyphs();
}


/**
 * Returns the number of TextGlyphs.
 */
LIBSBML_EXTERN
unsigned int
Layout_getNumTextGlyphs (const Layout_t *l)
{
  return l->getNumTextGlyphs();
}


/**
 * Returns the number of additional GraphicalObject.
 */
LIBSBML_EXTERN
unsigned int
Layout_getNumAdditionalGraphicalObjects (const Layout_t *l)
{
  return l->getNumAdditionalGraphicalObjects();
}

/**
 * Returns 0 if the id has not been set, 1 otherwise
 */
LIBSBML_EXTERN
int
Layout_isSetId (const Layout_t *l)
{
  return static_cast<int>( l->isSetId() );
}


/**
 * Does nothing since no defaults are defined for Layout.
 */ 
LIBSBML_EXTERN
void
Layout_initDefaults (Layout_t *l)
{
  l->initDefaults();
}



/**
 * Creates a ComparmentGlyph_t object, adds it to the end of the
 * compartment glyphs objects list and returns a pointer to the newly
 * created object.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
Layout_createCompartmentGlyph (Layout_t *l)
{
  return l->createCompartmentGlyph();
}


/**
 * Creates a SpeciesGlyph object, adds it to the end of the species glyphs
 * objects list and returns a pointer to the newly created object.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
Layout_createSpeciesGlyph (Layout_t *l)
{
  return l->createSpeciesGlyph();
}


/**
 * Creates a ReactionGlyph_t object, adds it to the end of the reaction
 * glyphs objects list and returns a pointer to the newly created object.
 */
LIBSBML_EXTERN
ReactionGlyph_t *
Layout_createReactionGlyph (Layout_t *l)
{
  return l->createReactionGlyph();
}


/**
 * Creates a TextGlyph_t object, adds it to the end of the text glyphs
 * objects list and returns a pointer to the newly created object.
 */
LIBSBML_EXTERN
TextGlyph_t *
Layout_createTextGlyph (Layout_t *l)
{
  return l->createTextGlyph();
}


/**
 * Creates a GraphicalObject object, adds it to the end of the additional
 * graphical objects list and returns a pointer to the newly created
 * object.
 */
LIBSBML_EXTERN
GraphicalObject_t *
Layout_createAdditionalGraphicalObject (Layout_t *l)
{
  return l->createAdditionalGraphicalObject();
}

/**
 * Remove the compartment glyph with the given index.
 * A pointer to the removed object is returned. If no object was removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
CompartmentGlyph_t*
Layout_removeCompartmentGlyph(Layout_t* l, unsigned int index)
{
    return l->removeCompartmentGlyph(index);
}

/**
 * Remove the species glyph with the given index.
 * A pointer to the removed object is returned. If no object was removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
SpeciesGlyph_t*
Layout_removeSpeciesGlyph(Layout_t* l, unsigned int index)
{
    return l->removeSpeciesGlyph(index);
}

/**
 * Remove the reaction glyph with the given index.
 * A pointer to the removed object is returned. If no object was removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
ReactionGlyph_t*
Layout_removeReactionGlyph(Layout_t* l, unsigned int index)
{
    return l->removeReactionGlyph(index);
}

/**
 * Remove the text glyph with the given index.
 * A pointer to the removed object is returned. If no object was removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
TextGlyph_t*
Layout_removeTextGlyph(Layout_t* l, unsigned int index)
{
    return l->removeTextGlyph(index);
}

/**
 * Remove the graphical object with the given index.
 * A pointer to the removed object is returned. If no object was removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
GraphicalObject_t*
Layout_removeAdditionalGraphicalObject(Layout_t* l, unsigned int index)
{
    return l->removeAdditionalGraphicalObject(index);
}

/**
 * Remove the compartment glyph with the given id.
 * A pointer to the removed object is returned. If no object was removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
CompartmentGlyph_t*
Layout_removeCompartmentGlyphWithId(Layout_t* l, const char* id)
{
    return l->removeCompartmentGlyph(id);
}

/**
 * Remove the species glyph with the given id.
 * A pointer to the removed object is returned. If no object was removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
SpeciesGlyph_t*
Layout_removeSpeciesGlyphWithId(Layout_t* l, const char* id)
{
    return l->removeSpeciesGlyph(id);
}

/**
 * Remove the reaction glyph with the given id.
 * A pointer to the removed object is returned. If no object was removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
ReactionGlyph_t*
Layout_removeReactionGlyphWithId(Layout_t* l, const char* id)
{
    return l->removeReactionGlyph(id);
}

/**
 * Remove the text glyph with the given id.
 * A pointer to the removed object is returned. If no object was removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
TextGlyph_t*
Layout_removeTextGlyphWithId(Layout_t* l, const char* id)
{
    return l->removeTextGlyph(id);
}

/**
 * Remove the species reference glyph with the given id.
 * A pointer to the removed object is returned. If no object was removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph_t*
Layout_removeSpeciesReferenceGlyphWithId(Layout_t* l, const char* id)
{
    return l->removeSpeciesReferenceGlyph(id);
}

/**
 * Remove the graphical object with the given id.
 * A pointer to the removed object is returned. If no object was removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
GraphicalObject_t*
Layout_removeAdditionalGraphicalObjectWithId(Layout_t* l, const char* id)
{
    return l->removeAdditionalGraphicalObject(id);
}


