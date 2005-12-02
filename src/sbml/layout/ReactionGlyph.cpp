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


#include "common/common.h"
#include "ReactionGlyph.h"


/**
 * Creates a new ReactionGlyph.  The list of species reference glyph is
 * empty and the id of the associated reaction is set to the empty string.
 */
LIBSBML_EXTERN
ReactionGlyph::ReactionGlyph() : curve( new Curve() )
{
  init(SBML_LAYOUT_REACTIONGLYPH);
}


/**
 * Creates a ReactionGlyph with the given id.
 */
LIBSBML_EXTERN
ReactionGlyph::ReactionGlyph (const std::string& id):
    GraphicalObject( id          )
  , curve          ( new Curve() )
{
  init(SBML_LAYOUT_REACTIONGLYPH);
}


/**
 * Creates a ReactionGlyph with the given id and set the id of the
 * associated reaction to the second argument.
 */
LIBSBML_EXTERN
ReactionGlyph::ReactionGlyph (const std::string& id,
                              const std::string& reactionId) : 
    GraphicalObject( id          )
  , reaction       ( reactionId  )
  , curve          ( new Curve() )
{
  init(SBML_LAYOUT_REACTIONGLYPH);
}


/**
 * Destructor.
 */ 
LIBSBML_EXTERN
ReactionGlyph::~ReactionGlyph ()
{
  delete this->curve;
} 


/**
 * Returns the id of the associated reaction.
 */  
LIBSBML_EXTERN
const std::string&
ReactionGlyph::getReactionId () const
{
  return this->reaction;
}


/**
 * Sets the id of the associated reaction.
 */ 
LIBSBML_EXTERN
void
ReactionGlyph::setReactionId (const std::string& id)
{
  this->reaction = id;
}


/**
 * Returns true if the id of the associated reaction is not the empty
 * string.
 */ 
LIBSBML_EXTERN
bool
ReactionGlyph::isSetReactionId() const
{
  return ! this->reaction.empty();
}


/**
 * Returns the ListOf object that hold the species reference glyphs.
 */  
LIBSBML_EXTERN
const ListOf&
ReactionGlyph::getListOfSpeciesReferenceGlyphs () const
{
  return this->speciesReferenceGlyphs;
}


/**
 * Returns the ListOf object that hold the species reference glyphs.
 */  
LIBSBML_EXTERN
ListOf&
ReactionGlyph::getListOfSpeciesReferenceGlyphs ()
{
  return this->speciesReferenceGlyphs;
}


/**
 * Returns the species reference glyph with the given index.  If the index
 * is invalid, NULL is returned.
 */ 
LIBSBML_EXTERN
SpeciesReferenceGlyph*
ReactionGlyph::getSpeciesReferenceGlyph (unsigned int index) const
{
  return static_cast<SpeciesReferenceGlyph*>
  (
    this->speciesReferenceGlyphs.get(index)
  );
}


/**
 * Adds a new species reference glyph to the list.
 */
LIBSBML_EXTERN
void
ReactionGlyph::addSpeciesReferenceGlyph (SpeciesReferenceGlyph& glyph)
{
  this->speciesReferenceGlyphs.append(&glyph);
}


/**
 * Returns the number of species reference glyph objects.
 */ 
LIBSBML_EXTERN
unsigned int
ReactionGlyph::getNumSpeciesReferenceGlyphs () const
{
  return this->speciesReferenceGlyphs.getNumItems();
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
LIBSBML_EXTERN
void ReactionGlyph::initDefaults ()
{
  GraphicalObject::initDefaults();
}


/**
 * Returns the curve object for the reaction glyph
 */ 
LIBSBML_EXTERN
Curve*
ReactionGlyph::getCurve () const
{
  return this->curve;
}


/**
 * Sets the curve object for the reaction glyph.
 */ 
LIBSBML_EXTERN
void ReactionGlyph::setCurve (Curve* curve)
{
  if(!curve) return;

  delete this->curve;
  this->curve = curve;
}


/**
 * Returns true if the curve consists of one or more segments.
 */ 
LIBSBML_EXTERN
bool ReactionGlyph::isSetCurve () const
{
  return this->curve->getNumCurveSegments() > 0;
}


/**
 * Creates a new SpeciesReferenceGlyph object, adds it to the end of the
 * list of species reference objects and returns a reference to the newly
 * created object.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph&
ReactionGlyph::createSpeciesReferenceGlyph ()
{
  SpeciesReferenceGlyph* srg = new SpeciesReferenceGlyph();

  this->addSpeciesReferenceGlyph(*srg);
  return *srg;
}


/**
 * Creates a new LineSegment object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
LIBSBML_EXTERN
LineSegment&
ReactionGlyph::createLineSegment ()
{
  return this->curve->createLineSegment();
}

 
/**
 * Creates a new CubicBezier object, adds it to the end of the list of
 * curve segment objects of the curve and returns a reference to the newly
 * created object.
 */
LIBSBML_EXTERN
CubicBezier&
ReactionGlyph::createCubicBezier ()
{
  return this->curve->createCubicBezier();
}

/**
 * Remove the species reference glyph with the given index.
 * A pointer to the object is returned. If no object has been removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
SpeciesReferenceGlyph*
ReactionGlyph::removeSpeciesReferenceGlyph(unsigned int index)
{
    SpeciesReferenceGlyph* srg=NULL;
    if(index < this->getNumSpeciesReferenceGlyphs())
    {
        srg=dynamic_cast<SpeciesReferenceGlyph*>(this->getListOfSpeciesReferenceGlyphs().remove(index));
    }
    return srg;
}

/**
 * Remove the species reference glyph with the given id.
 * A pointer to the object is returned. If no object has been removed, NULL
 * is returned.
 */
LIBSBML_EXTERN
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
LIBSBML_EXTERN
unsigned int
ReactionGlyph::getIndexForSpeciesReferenceGlyph(const std::string& id) const
{
    unsigned int i,iMax=this->getNumSpeciesReferenceGlyphs();
    unsigned int index=std::numeric_limits<unsigned int>::max();
    for(i=0;i<iMax;++i)
    {
        SpeciesReferenceGlyph* srg=this->getSpeciesReferenceGlyph(i);
        if(srg->getId()==id)
        {
            index=i;
            break;
        }
    }
    return index;
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
  rg->addSpeciesReferenceGlyph(*srg);
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
ReactionGlyph_getSpeciesReferenceGlyph (const ReactionGlyph_t *rg,
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
  return & rg->getListOfSpeciesReferenceGlyphs();
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
  return & rg->createSpeciesReferenceGlyph();
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
  return & rg->getCurve()->createLineSegment();
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
  return & rg->getCurve()->createCubicBezier();
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


