/**
 * Filename    : Layout.h
 * Description : SBML Layout Layout C++ Header
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


#ifndef Layout_H__
#define Layout_H__


#include "common/extern.h"


#ifdef __cplusplus


#include <string>

#include "sbml/SBase.h"
#include "sbml/ListOf.h"

#include "Dimensions.h"
#include "CompartmentGlyph.h"
#include "SpeciesGlyph.h"
#include "ReactionGlyph.h"
#include "TextGlyph.h"
#include "GraphicalObject.h"
#include "SpeciesReferenceGlyph.h"


class Layout : public SBase
{
protected:

  std::string id;
  Dimensions dimensions;
  ListOf compartmentGlyphs;
  ListOf speciesGlyphs;
  ListOf reactionGlyphs;
  ListOf textGlyphs;
  ListOf additionalGraphicalObjects;

  GraphicalObject*
  removeObjectWithId (ListOf& list, const std::string& id);

  GraphicalObject*
  getObjectWithId (const ListOf& list, const std::string& id) const;


public:

  /**
   * Creates a new Layout.
   */
  LIBSBML_EXTERN
  Layout ();

  /**
   * Creates a new Layout with the given id and dimensions.
   */
  LIBSBML_EXTERN
  Layout (const std::string& id, const Dimensions& dimensions);

  /**
   * Destructor.
   */ 
  LIBSBML_EXTERN
  virtual ~Layout ();


  /**
   * Does nothing since no defaults are defined for Layout.
   */ 
  LIBSBML_EXTERN
  void initDefaults ();    

        
  /**
   * Returns the id.
   */ 
  LIBSBML_EXTERN        
  const std::string& getId () const;
        
  /**
   * Sets the id to a copy of the given string.
   */ 
  LIBSBML_EXTERN  
  void setId (const std::string& id);

  /**
   * Returns the dimensions of the layout.
   */ 
  LIBSBML_EXTERN
  const Dimensions& getDimensions () const;

  /**
   * Returns the dimensions of the layout.
   */ 
  LIBSBML_EXTERN
  Dimensions& getDimensions ();

  /**
   * Sets the dimensions of the layout.
   */ 
  LIBSBML_EXTERN  
  void setDimensions (const Dimensions& dimensions);


  /**
   * Returns the ListOf object that holds all compartment glyphs.
   */ 
  LIBSBML_EXTERN
  const ListOf& getListOfCompartmentGlyphs () const;

  /**
   * Returns the ListOf object that holds all species glyphs.
   */ 
  LIBSBML_EXTERN 
  const ListOf& getListOfSpeciesGlyphs () const;

  /**
   * Returns the ListOf object that holds all reaction glyphs.
   */ 
  LIBSBML_EXTERN 
  const ListOf& getListOfReactionGlyphs () const;

  /**
   * Returns the ListOf object that holds all text glyphs.
   */ 
  LIBSBML_EXTERN 
  const ListOf& getListOfTextGlyphs () const;

  /**
   * Returns the ListOf object that holds all additonal graphical objects.
   */ 
  LIBSBML_EXTERN 
  const ListOf& getListOfAdditionalGraphicalObjects () const;
  
  /**
   * Returns the ListOf object that holds all compartment glyphs.
   */ 
  LIBSBML_EXTERN
  ListOf& getListOfCompartmentGlyphs ();

  /**
   * Returns the ListOf object that holds all species glyphs.
   */ 
  LIBSBML_EXTERN 
  ListOf& getListOfSpeciesGlyphs ();

  /**
   * Returns the ListOf object that holds all reaction glyphs.
   */ 
  LIBSBML_EXTERN 
  ListOf& getListOfReactionGlyphs ();

  /**
   * Returns the ListOf object that holds all text glyphs.
   */ 
  LIBSBML_EXTERN 
  ListOf& getListOfTextGlyphs ();

  /**
   * Returns the ListOf object that holds all additional graphical objects.
   */ 
  LIBSBML_EXTERN 
  ListOf& getListOfAdditionalGraphicalObjects ();


  /**
   * Returns the compartment glyph with the given index.
   * If the index is invalid, NULL is returned.
   */ 
  LIBSBML_EXTERN 
  CompartmentGlyph* getCompartmentGlyph (unsigned int index) const;

  /**
   * Returns the species glyph with the given index.
   * If the index is invalid, NULL is returned.
   */ 
  LIBSBML_EXTERN
  SpeciesGlyph* getSpeciesGlyph (unsigned int index) const;

  /**
   * Returns the reaction glyph with the given index.
   * If the index is invalid, NULL is returned.
   */ 
  LIBSBML_EXTERN
  ReactionGlyph* getReactionGlyph (unsigned int index) const;

  /**
   * Returns the text glyph with the given index.
   * If the index is invalid, NULL is returned.
   */ 
  LIBSBML_EXTERN
  TextGlyph* getTextGlyph (unsigned int index) const;

  /**
   * Returns the additional graphical object with the given index.
   * If the index is invalid, NULL is returned.
   */ 
  LIBSBML_EXTERN 
  GraphicalObject* getAdditionalGraphicalObject (unsigned int index) const;


  /**
   * Returns the compartment glyph that has the given id, or NULL if no
   * compartment glyph has the id.
   */
  LIBSBML_EXTERN
  CompartmentGlyph* getCompartmentGlyph (const std::string& id) const;

  /**
   * Returns the species glyph that has the given id, or NULL if no species
   * glyph has the id.
   */
  LIBSBML_EXTERN
  SpeciesGlyph* getSpeciesGlyph (const std::string& id) const;
        
  /**
   * Returns the reaction glyph that has the given id, or NULL if no
   * reaction glyph has the id.
   */
  LIBSBML_EXTERN
  ReactionGlyph* getReactionGlyph (const std::string& id) const;

  /**
   * Returns the text glyph that has the given id, or NULL if no text glyph
   * has the id.
   */
  LIBSBML_EXTERN
  TextGlyph* getTextGlyph (const std::string& id) const;

  /**
   * Returns the additional graphical object that has the given id, or NULL
   * if no graphical object has the id.
   */
  LIBSBML_EXTERN
  GraphicalObject* getAdditionalGraphicalObject (const std::string& id) const;


  /**
   * Adds a new compartment glyph.
   */
  LIBSBML_EXTERN
  void addCompartmentGlyph (CompartmentGlyph& glyph);

  /**
   * Adds a new species glyph.
   */
  LIBSBML_EXTERN
  void addSpeciesGlyph (SpeciesGlyph& glyph);

  /**
   * Adds a new reaction glyph.
   */
  LIBSBML_EXTERN
  void addReactionGlyph (ReactionGlyph& glyph);

  /**
   * Adds a new text glyph.
   */
  LIBSBML_EXTERN
  void addTextGlyph (TextGlyph& glyph);

  /**
   * Adds a new additional graphical object glyph.
   */
  LIBSBML_EXTERN
  void addAdditionalGraphicalObject (GraphicalObject& glyph);


  /**
   * Returns the number of compartment glyphs for the layout.
   */
  LIBSBML_EXTERN
  unsigned int getNumCompartmentGlyphs () const;

  /**
   * Returns the number of species glyphs for the layout.
   */
  LIBSBML_EXTERN 
  unsigned int getNumSpeciesGlyphs () const;

  /**
   * Returns the number of reaction glyphs for the layout.
   */
  LIBSBML_EXTERN
  unsigned int getNumReactionGlyphs () const;

  /**
   * Returns the number of text glyphs for the layout.
   */
  LIBSBML_EXTERN 
  unsigned int getNumTextGlyphs () const;

  /**
   * Returns the number of additional graphical objects for the layout.
   */
  LIBSBML_EXTERN
  unsigned int getNumAdditionalGraphicalObjects () const;


  /**
   * Returns true if the id is not the empty string.
   */ 
  LIBSBML_EXTERN
  bool isSetId () const;    


  /**
   * Creates a CompartmentGlyph object, adds it to the end of the
   * compartment glyph objects list and returns a pointer to the newly
   * created object.
   */
  LIBSBML_EXTERN
  CompartmentGlyph* createCompartmentGlyph ();

  /**
   * Creates a SpeciesGlyph object, adds it to the end of the species glyph
   * objects list and returns a pointer to the newly created object.
   */
  LIBSBML_EXTERN
  SpeciesGlyph* createSpeciesGlyph ();

  /**
   * Creates a ReactionGlyph object, adds it to the end of the reaction
   * glyph objects list and returns a pointer to the newly created
   * object.
   */
  LIBSBML_EXTERN
  ReactionGlyph* createReactionGlyph ();

  /**
   * Creates a TextGlyph object, adds it to the end of the text glyph
   * objects list and returns a pointer to the newly created object.
   */
  LIBSBML_EXTERN
  TextGlyph* createTextGlyph ();

  /**
   * Creates a GraphicalObject object, adds it to the end of the additional
   * graphical objects list and returns a pointer to the newly created
   * object.
   */
  LIBSBML_EXTERN
  GraphicalObject* createAdditionalGraphicalObject ();

  /**
   * Creates a new SpeciesReferenceGlyph for the last ReactionGlyph and
   * adds it to its list of SpeciesReferenceGlyph objects.  A pointer to
   * the newly created object is returned.
   */
  LIBSBML_EXTERN
  SpeciesReferenceGlyph* createSpeciesReferenceGlyph();

  /**
   * Creates a new LineSegment for the Curve object of the last
   * ReactionGlyph or the last SpeciesReferenceGlyph in the last
   * ReactionGlyph and adds it to its list of SpeciesReferenceGlyph
   * objects.  A pointer to the newly created object is returned.
   */
  LIBSBML_EXTERN
  LineSegment* createLineSegment ();

  /**
   * Creates a new CubicBezier for the Curve object of the last
   * ReactionGlyph or the last SpeciesReferenceGlyph in the last
   * ReactionGlyph and adds it to its list of SpeciesReferenceGlyph
   * objects.  A pointer to the newly created object is returned.
   */
  LIBSBML_EXTERN
  CubicBezier* createCubicBezier ();

  /**
   * Removes the compartment glyph with the given index from the layout.
   * A pointer to the compartment glyph that was removed is returned.
   * If no compartment glyph has been removed, NULL is returned.
   */
  LIBSBML_EXTERN
  CompartmentGlyph* removeCompartmentGlyph(unsigned int index);

  /**
   * Removes the species glyph with the given index from the layout.
   * A pointer to the species glyph that was removed is returned.
   * If no species glyph has been removed, NULL is returned.
   */
  LIBSBML_EXTERN
  SpeciesGlyph* removeSpeciesGlyph(unsigned int index);
  
  /**
   * Removes the reaction glyph with the given index from the layout.
   * A pointer to the reaction glyph that was removed is returned.
   * If no reaction glyph has been removed, NULL is returned.
   */
  LIBSBML_EXTERN
  ReactionGlyph* removeReactionGlyph(unsigned int index);
  
  /**
   * Removes the text glyph with the given index from the layout.
   * A pointer to the text glyph that was removed is returned.
   * If no text glyph has been removed, NULL is returned.
   */
  LIBSBML_EXTERN
  TextGlyph* removeTextGlyph(unsigned int index);
  
  /**
   * Removes the graphical object with the given index from the layout.
   * A pointer to the graphical object that was removed is returned.
   * If no graphical object has been removed, NULL is returned.
   */
  LIBSBML_EXTERN
  GraphicalObject* removeAdditionalGraphicalObject(unsigned int index);

  /**
   * Remove the compartment glyph with the given id.
   * A pointer to the removed compartment glyph is returned.
   * If no compartment glyph has been removed, NULL is returned.
   */
  LIBSBML_EXTERN
  CompartmentGlyph*
  removeCompartmentGlyph(const std::string id);


  /**
   * Remove the species glyph with the given id.
   * A pointer to the removed species glyph is returned.
   * If no species glyph has been removed, NULL is returned.
   */
  LIBSBML_EXTERN
  SpeciesGlyph*
  removeSpeciesGlyph(const std::string id);


  /**
   * Remove the reaction glyph with the given id.
   * A pointer to the removed reaction glyph is returned.
   * If no reaction glyph has been removed, NULL is returned.
   */
  LIBSBML_EXTERN
  ReactionGlyph*
  removeReactionGlyph(const std::string id);


  /**
   * Remove the species reference glyph with the given id.
   * A pointer to the removed species reference glyph is returned.
   * If no species reference glyph has been removed, NULL is returned.
   */
  LIBSBML_EXTERN
  SpeciesReferenceGlyph*
  removeSpeciesReferenceGlyph(const std::string id);


  /**
   * Remove the text glyph with the given id.
   * A pointer to the removed text glyph is returned.
   * If no text glyph has been removed, NULL is returned.
   */
  LIBSBML_EXTERN
  TextGlyph*
  removeTextGlyph(const std::string id);


  /**
   * Remove the graphical object with the given id.
   * A pointer to the removed graphical object is returned.
   * If no graphical object has been removed, NULL is returned.
   */
  LIBSBML_EXTERN
  GraphicalObject*
  removeAdditionalGraphicalObject(const std::string id);


};


#endif /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Creates a new Layout and returns a pointer to it.
 */
LIBSBML_EXTERN
Layout_t *
Layout_create (void);

/**
 * Creates a new Layout with the given id and returns a pointer to it.
 */
LIBSBML_EXTERN
Layout_t *
Layout_createWith (const char *sid);

/**
 * Creates a Layout object from a template.
 */
LIBSBML_EXTERN
Layout_t *
Layout_createFrom (const Layout_t *temp);

/**
 * Creates a new Layout with the given width, height and depth and returns
 * a pointer to it.  The depth value defaults to 0.0.
 */
LIBSBML_EXTERN
Layout_t *
Layout_createWithSize (const char *id,
                       double width, double height, double depth);

/**
 * Creates a new Layout with the given Dimensions and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
Layout_t *
Layout_createWithDimensions (const char *id, const Dimensions_t *dimensions);

/** 
 * Frees the memory for the given layout.
 */
LIBSBML_EXTERN
void 
Layout_free (Layout_t *l);


/**
 * Sets the id of the layout.
 */
LIBSBML_EXTERN
void
Layout_setId (Layout_t *l, const char *sid);


/**
 * Adds a new compartment glyph to the list of compartment glyphs.
 */
LIBSBML_EXTERN
void
Layout_addCompartmentGlyph (Layout_t *l, CompartmentGlyph_t *cg);

/**
 * Adds a new species glyph to the list of species glyphs.
 */
LIBSBML_EXTERN
void
Layout_addSpeciesGlyph (Layout_t *l, SpeciesGlyph_t *sg);

/**
 * Adds a new reaction glyph to the list of reaction glyphs.
 */
LIBSBML_EXTERN
void
Layout_addReactionGlyph (Layout_t *l, ReactionGlyph_t *rg);

/**
 * Adds a new GraphicalObject to the list of additional graphical objects.
 */
LIBSBML_EXTERN
void
Layout_addAdditionalGraphicalObject (Layout_t *l, GraphicalObject_t *go);

/**
 * Adds a new TextGlyph to the list of text glyphs.
 */
LIBSBML_EXTERN
void
Layout_addTextGlyph (Layout_t *l, TextGlyph_t *go);


/**
 * Returns a pointer to the CompartmentGlyph with the given index.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
Layout_getCompartmentGlyph (const Layout_t *l, unsigned int index);

/**
 * Returns a pointer to the SpeciesGlyph with the given index.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
Layout_getSpeciesGlyph (const Layout_t *l, unsigned int index);


/**
 * Returns a pointer to the ReactionGlyph with the given index.
 */
LIBSBML_EXTERN
ReactionGlyph_t *
Layout_getReactionGlyph (const Layout_t *l, unsigned int index);


/**
 * Returns a pointer to the AdditionalGraphicalObject with the given index.
 */
LIBSBML_EXTERN
GraphicalObject_t *
Layout_getAdditionalGraphicalObject (const Layout_t *l, unsigned int index);

/**
 * Returns a pointer to the GraphicalObject with the given index.
 */
LIBSBML_EXTERN
TextGlyph_t *
Layout_getTextGlyph (const Layout_t *l, unsigned int index);


/**
 * Returns a pointer to the list of CompartmentGlyphs.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfCompartmentGlyphs (Layout_t *l);

/**
 * Returns a pointer to the list of SpeciesGlyphs.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfSpeciesGlyphs (Layout_t *l);


/**
 * Returns a pointer to the list of ReactionGlyphs.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfReactionGlyphs (Layout_t *l);


/**
 * Returns a pointer to the list of additional GraphicalObjects.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfAdditionalGraphicalObjects (Layout_t *l);

/**
 * Returns a pointer to the list of TextGlyphs.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfTextGlyphs (Layout_t *l);



/**
 * Returns a the id of the layout.
 */
LIBSBML_EXTERN
const char *
Layout_getId (const Layout_t *l);

/**
 * Returns the number of CompartmentGlyphs.
 */
LIBSBML_EXTERN
unsigned int
Layout_getNumCompartmentGlyphs (const Layout_t *l);

/**
 * Returns the number of SpeciesGlyphs.
 */
LIBSBML_EXTERN
unsigned int
Layout_getNumSpeciesGlyphs (const Layout_t *l);


/**
 * Returns the number of ReactionGlyphs.
 */
LIBSBML_EXTERN
unsigned int
Layout_getNumReactionGlyphs (const Layout_t *l);


/**
 * Returns the number of additional GraphicalObjects.
 */
LIBSBML_EXTERN
unsigned int
Layout_getNumAdditionalGraphicalObjects (const Layout_t *l);

/**
 * Returns the number of TextGlyphs.
 */
LIBSBML_EXTERN
unsigned int
Layout_getNumTextGlyphs (const Layout_t *l);



/**
 * Returns 0 if the id has not been set, 1 otherwise
 */
LIBSBML_EXTERN
int
Layout_isSetId (const Layout_t *l);

/**
 * Removes the compartment glyph with the given index.  If the index is
 * invalid, nothing is deleted.
 */ 
LIBSBML_EXTERN
CompartmentGlyph_t *
Layout_removeCompartmentGlyph (Layout_t *l, unsigned int index);

/**
 * Removes the species glyph with the given index.  If the index is
 * invalid, nothing is deleted.
 */ 
LIBSBML_EXTERN
SpeciesGlyph_t *
Layout_removeSpeciesGlyph (Layout_t *l, unsigned int index);

/**
 * Removes the reaction glyph with the given index.  If the index is
 * invalid, nothing is deleted.
 */ 
LIBSBML_EXTERN
ReactionGlyph_t *
Layout_removeReactionGlyph (Layout_t *l, unsigned int index);
 
/**
 * Removes the text glyph with the given index.  If the index is invalid,
 * nothing is deleted.
 */ 
LIBSBML_EXTERN
TextGlyph_t *
Layout_removeTextGlyph (Layout_t *l, unsigned int index);
 
/**
 * Removes the graphical object with the given index.  If the index is
 * invalid, nothing is deleted.
 */ 
LIBSBML_EXTERN
GraphicalObject_t *
Layout_removeAdditionalGraphicalObject (Layout_t *l, unsigned int index);

/**
 * Removes the compartment glyph with the given id.  If the id is
 * not found, nothing is deleted.
 */ 
LIBSBML_EXTERN
CompartmentGlyph_t *
Layout_removeCompartmentGlyphWithId (Layout_t *l, const char* id);

/**
 * Removes the species glyph with the given id.  If the id is
 * not found, nothing is deleted.
 */ 
LIBSBML_EXTERN
SpeciesGlyph_t *
Layout_removeSpeciesGlyphWithId (Layout_t *l, const char* id);

/**
 * Removes the species reference glyph with the given id.  If the id is
 * not found, nothing is deleted.
 */ 
LIBSBML_EXTERN
SpeciesReferenceGlyph_t *
Layout_removeSpeciesReferenceGlyphWithId (Layout_t *l, const char* id);

/**
 * Removes the reaction glyph with the given id.  If the id is
 * not found, nothing is deleted.
 */ 
LIBSBML_EXTERN
ReactionGlyph_t *
Layout_removeReactionGlyphWithId (Layout_t *l, const char* id);
 
/**
 * Removes the text glyph with the given id.  If the id is not found,
 * nothing is deleted.
 */ 
LIBSBML_EXTERN
TextGlyph_t *
Layout_removeTextGlyphWithId (Layout_t *l, const char* id);
 
/**
 * Removes the graphical object with the given id.  If the id is
 * not found, nothing is deleted.
 */ 
LIBSBML_EXTERN
GraphicalObject_t *
Layout_removeAdditionalGraphicalObjectWithId (Layout_t *l, const char* );
    
/**
 * Does nothing since no defaults are defined for Layout.
 */ 
LIBSBML_EXTERN
void
Layout_initDefaults (Layout_t *l);


/**
 * Creates a ComparmentGlyph_t object, adds it to the end of the
 * compartment glyphs objects list and returns a pointer to the newly
 * created object.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
Layout_createCompartmentGlyph (Layout_t *);

/**
 * Creates a SpeciesGlyph object, adds it to the end of the species glyphs
 * objects list and returns a pointer to the newly created object.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
Layout_createSpeciesGlyph (Layout_t *);


/**
 * Creates a ReactionGlyph_t object, adds it to the end of the reaction
 * glyphs objects list and returns a pointer to the newly created object.
 */
LIBSBML_EXTERN
ReactionGlyph_t *
Layout_createReactionGlyph (Layout_t *);


/**
 * Creates a TextGlyph_t object, adds it to the end of the text glyphs
 * objects list and returns a pointer to the newly created object.
 */
LIBSBML_EXTERN
TextGlyph_t *
Layout_createTextGlyph (Layout_t *);


/**
 * Creates a GraphicalObject object, adds it to the end of the additional
 * graphical objects list and returns a pointer to the newly created
 * object.
 */
LIBSBML_EXTERN
GraphicalObject_t *
Layout_createAdditionalGraphicalObject (Layout_t *);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* Layout_H__ */
