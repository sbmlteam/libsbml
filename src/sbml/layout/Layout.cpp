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


#include <climits> 
#include <iostream>
#include <limits>
#include <assert.h>
#include <algorithm>
#include <functional>

#include "Layout.h"
#include "LayoutUtilities.h"
#include <sbml/SBMLNamespaces.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * Creates a new Layout.
 */
Layout::Layout () : SBase ("","",-1)
{
}

Layout::Layout (unsigned int level, unsigned int version):
   SBase (level, version)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}

                          
Layout::Layout (SBMLNamespaces *sbmlns) :
   SBase (sbmlns)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}
 

/**
 * Creates a new Layout with the given id and dimensions.
 */
Layout::Layout (const std::string& id, const Dimensions* dimensions) :
    SBase      ()
  , mId (id)
{
    if(dimensions)
    {
        this->mDimensions=*dimensions;
    }
}


/**
 * Creates a new Layout from the given XMLNode
 */
Layout::Layout(const XMLNode& node)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
    this->readAttributes(attributes);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="dimensions")
        {
            this->mDimensions=Dimensions(*child);
        }
        else if(childName=="annotation")
        {
            this->setAnnotation(child);
        }
        else if(childName=="notes")
        {
            this->mNotes=new XMLNode(*child);
        }
        else if(childName=="listOfCompartmentGlyphs")
        {
            const XMLNode* innerChild;
            unsigned int i=0,iMax=child->getNumChildren();
            while(i<iMax)
            {
                innerChild=&child->getChild(i);
                const std::string innerChildName=innerChild->getName();
                ListOf& list=this->mCompartmentGlyphs;
                if(innerChildName=="compartmentGlyph")
                {
                    list.appendAndOwn(new CompartmentGlyph(*innerChild));
                }
                else if(innerChildName=="annotation")
                {
                    list.setAnnotation(new XMLNode(*innerChild));
                }
                else if(innerChildName=="notes")
                {
                    list.setNotes(new XMLNode(*innerChild));
                }
                else
                {
                    // throw
                }
                ++i;
            }
        }
        else if(childName=="listOfSpeciesGlyphs")
        {
            const XMLNode* innerChild;
            unsigned int i=0,iMax=child->getNumChildren();
            while(i<iMax)
            {
                innerChild=&child->getChild(i);
                const std::string innerChildName=innerChild->getName();
                ListOf& list=this->mSpeciesGlyphs;
                if(innerChildName=="speciesGlyph")
                {
                    list.appendAndOwn(new SpeciesGlyph(*innerChild));
                }
                else if(innerChildName=="annotation")
                {
                    list.setAnnotation(new XMLNode(*innerChild));
                }
                else if(innerChildName=="notes")
                {
                    list.setNotes(new XMLNode(*innerChild));
                }
                else
                {
                    // throw
                }
                ++i;
            }
        }
        else if(childName=="listOfReactionGlyphs")
        {
            const XMLNode* innerChild;
            unsigned int i=0,iMax=child->getNumChildren();
            while(i<iMax)
            {
                innerChild=&child->getChild(i);
                const std::string innerChildName=innerChild->getName();
                ListOf& list=this->mReactionGlyphs;
                if(innerChildName=="reactionGlyph")
                {
                    list.appendAndOwn(new ReactionGlyph(*innerChild));
                }
                else if(innerChildName=="annotation")
                {
                    list.setAnnotation(new XMLNode(*innerChild));
                }
                else if(innerChildName=="notes")
                {
                    list.setNotes(new XMLNode(*innerChild));
                }
                else
                {
                    // throw
                }
                ++i;
            }
        }
        else if(childName=="listOfTextGlyphs")
        {
            const XMLNode* innerChild;
            unsigned int i=0,iMax=child->getNumChildren();
            while(i<iMax)
            {
                innerChild=&child->getChild(i);
                const std::string innerChildName=innerChild->getName();
                ListOf& list=this->mTextGlyphs;
                if(innerChildName=="textGlyph")
                {
                    list.appendAndOwn(new TextGlyph(*innerChild));
                }
                else if(innerChildName=="annotation")
                {
                    list.setAnnotation(new XMLNode(*innerChild));
                }
                else if(innerChildName=="notes")
                {
                    list.setNotes(new XMLNode(*innerChild));
                }
                else
                {
                    // throw
                }
                ++i;
            }
        }
        else if(childName=="listOfAdditionalGraphicalObjects")
        {
            const XMLNode* innerChild;
            unsigned int i=0,iMax=child->getNumChildren();
            while(i<iMax)
            {
                innerChild=&child->getChild(i);
                const std::string innerChildName=innerChild->getName();
                ListOf& list=this->mAdditionalGraphicalObjects;
                if(innerChildName=="graphicalObject")
                {
                    list.appendAndOwn(new GraphicalObject(*innerChild));
                }
                else if(innerChildName=="annotation")
                {
                    list.setAnnotation(new XMLNode(*innerChild));
                }
                else if(innerChildName=="notes")
                {
                    list.setNotes(new XMLNode(*innerChild));
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
Layout::Layout(const Layout& source):SBase(source)
{
    this->mId=source.getId();
    this->mDimensions=*source.getDimensions();
    this->mCompartmentGlyphs=*source.getListOfCompartmentGlyphs();
    this->mSpeciesGlyphs=*source.getListOfSpeciesGlyphs();
    this->mReactionGlyphs=*source.getListOfReactionGlyphs();
    this->mTextGlyphs=*source.getListOfTextGlyphs();
    this->mAdditionalGraphicalObjects=*source.getListOfAdditionalGraphicalObjects();
}

/**
 * Assignment operator.
 */
Layout& Layout::operator=(const Layout& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mId = source.mId;
    this->mDimensions=*source.getDimensions();
    this->mCompartmentGlyphs=*source.getListOfCompartmentGlyphs();
    this->mSpeciesGlyphs=*source.getListOfSpeciesGlyphs();
    this->mReactionGlyphs=*source.getListOfReactionGlyphs();
    this->mTextGlyphs=*source.getListOfTextGlyphs();
    this->mAdditionalGraphicalObjects=*source.getListOfAdditionalGraphicalObjects();
  }
  
  return *this;
}


/**
 * Destructor.
 */ 
Layout::~Layout ()
{
}


/**
 * Does nothing since no defaults are defined for Layout.
 */ 
void
Layout::initDefaults ()
{
}


/**
  * Returns the value of the "id" attribute of this Layout.
  */
const std::string& Layout::getId () const
{
  return mId;
}


/**
  * Predicate returning @c true or @c false depending on whether this
  * Layout's "id" attribute has been set.
  */
bool Layout::isSetId () const
{
  return (mId.empty() == false);
}

/**
  * Sets the value of the "id" attribute of this Layout.
  */
int Layout::setId (const std::string& id)
{
  if (!(SyntaxChecker::isValidSBMLSId(id)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mId = id;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
  * Unsets the value of the "id" attribute of this Layout.
  */
void Layout::unsetId ()
{
  mId.erase();
}

/**
 * Returns true if all required attributes are set
 * on the layout.
 * Currently the only required attribute is the id.
 */
bool Layout::hasRequiredAttributes() const
{
    return this->isSetId();
}

/**
 * Returns true if all required elements are set
 * on the layout.
 * Currently the only required element are are the dimensions.
 * The dimensions are actually always set. Maybe we should check
 * if the values make sense.
 */
bool Layout::hasRequiredElements() const
{
    return true;
}


/**
 * Returns the dimensions of the layout.
 */ 
const Dimensions*
Layout::getDimensions() const
{
  return &this->mDimensions;
}


/**
 * Returns the dimensions of the layout.
 */ 
Dimensions*
Layout::getDimensions()
{
  return &this->mDimensions;
}


/**
 * Sets the dimensions of the layout.
 */ 
void
Layout::setDimensions (const Dimensions* dimensions)
{
  if(dimensions==NULL) return;  
  this->mDimensions = *dimensions;
}


/**
 * Returns the ListOf object that holds all compartment glyphs.
 */ 
const ListOfCompartmentGlyphs*
Layout::getListOfCompartmentGlyphs () const
{
  return &this->mCompartmentGlyphs;
}


/**
 * Returns the ListOf object that holds all species glyphs.
 */ 
const ListOfSpeciesGlyphs*
Layout::getListOfSpeciesGlyphs () const
{
  return &this->mSpeciesGlyphs;
}


/**
 * Returns the ListOf object that holds all reaction glyphs.
 */ 
const ListOfReactionGlyphs*
Layout::getListOfReactionGlyphs () const
{
  return &this->mReactionGlyphs;
}


/**
 * Returns the ListOf object that holds all text glyphs.
 */ 
const ListOfTextGlyphs*
Layout::getListOfTextGlyphs () const
{
  return &this->mTextGlyphs;
}


/**
 * Returns the ListOf object that holds all additonal graphical objects.
 */ 
const ListOfGraphicalObjects*
Layout::getListOfAdditionalGraphicalObjects () const
{
  return &this->mAdditionalGraphicalObjects;
}


/**
 * Returns the ListOf object that holds all compartment glyphs.
 */ 
ListOfCompartmentGlyphs*
Layout::getListOfCompartmentGlyphs ()
{
  return &this->mCompartmentGlyphs;
}


/**
 * Returns the ListOf object that holds all species glyphs.
 */ 
ListOfSpeciesGlyphs*
Layout::getListOfSpeciesGlyphs ()
{
  return &this->mSpeciesGlyphs;
}


/**
 * Returns the ListOf object that holds all reaction glyphs.
 */ 
ListOfReactionGlyphs*
Layout::getListOfReactionGlyphs ()
{
  return &this->mReactionGlyphs;
}


/**
 * Returns the ListOf object that holds all text glyphs.
 */ 
ListOfTextGlyphs*
Layout::getListOfTextGlyphs ()
{
  return &this->mTextGlyphs;
}


/**
 * Returns the ListOf object that holds all additional graphical objects.
 */ 
ListOfGraphicalObjects*
Layout::getListOfAdditionalGraphicalObjects ()
{
  return &this->mAdditionalGraphicalObjects;
}


/**
 * Returns the compartment glyph with the given index.  If the index is
 * invalid, NULL is returned.
 */ 
CompartmentGlyph*
Layout::getCompartmentGlyph (unsigned int index) 
{
  return static_cast<CompartmentGlyph*>( this->mCompartmentGlyphs.get(index) );
}

/**
 * Returns the compartment glyph with the given index.  If the index is
 * invalid, NULL is returned.
 */ 
const CompartmentGlyph*
Layout::getCompartmentGlyph (unsigned int index) const
{
  return static_cast<const CompartmentGlyph*>( this->mCompartmentGlyphs.get(index) );
}


/**
 * Returns the species glyph with the given index.  If the index is
 * invalid, NULL is returned.
 */ 
SpeciesGlyph*
Layout::getSpeciesGlyph (unsigned int index) 
{
  return static_cast<SpeciesGlyph*>( this->mSpeciesGlyphs.get(index) );
}

/**
 * Returns the species glyph with the given index.  If the index is
 * invalid, NULL is returned.
 */ 
const SpeciesGlyph*
Layout::getSpeciesGlyph (unsigned int index) const
{
  return static_cast<const SpeciesGlyph*>( this->mSpeciesGlyphs.get(index) );
}


/**
 * Returns the reaction glyph with the given index.  If the index is
 * invalid, NULL is returned.
 */ 
ReactionGlyph*
Layout::getReactionGlyph (unsigned int index) 
{
  return static_cast<ReactionGlyph*>( this->mReactionGlyphs.get(index) );
}

/**
 * Returns the reaction glyph with the given index.  If the index is
 * invalid, NULL is returned.
 */ 
const ReactionGlyph*
Layout::getReactionGlyph (unsigned int index) const
{
  return static_cast<const ReactionGlyph*>( this->mReactionGlyphs.get(index) );
}


/**
 * Returns the text glyph with the given index.  If the index is invalid,
 * NULL is returned.
 */ 
TextGlyph*
Layout::getTextGlyph (unsigned int index) 
{
  return static_cast<TextGlyph*>( this->mTextGlyphs.get(index) );
}

/**
 * Returns the text glyph with the given index.  If the index is invalid,
 * NULL is returned.
 */ 
const TextGlyph*
Layout::getTextGlyph (unsigned int index) const
{
  return static_cast<const TextGlyph*>( this->mTextGlyphs.get(index) );
}


/**
 * Returns the additional graphical object with the given index.
 * If the index is invalid, NULL is returned.
 */ 
GraphicalObject*
Layout::getAdditionalGraphicalObject (unsigned int index) 
{
  return static_cast<GraphicalObject*>
  ( 
    this->mAdditionalGraphicalObjects.get(index)
  );
}

/**
 * Returns the additional graphical object with the given index.
 * If the index is invalid, NULL is returned.
 */ 
const GraphicalObject*
Layout::getAdditionalGraphicalObject (unsigned int index) const
{
  return static_cast<const GraphicalObject*>
  ( 
    this->mAdditionalGraphicalObjects.get(index)
  );
}


const GraphicalObject*
Layout::getObjectWithId (const ListOf* list,const std::string& id) const
{
  const GraphicalObject* object=NULL;
  unsigned int counter=0;
  while(counter < list->size()) {
    const GraphicalObject* tmp=dynamic_cast<const GraphicalObject*>(list->get(counter));
    if(tmp->getId()==id){
      object=tmp;
      break;
    }
    ++counter;
  }    
  return object;
}

GraphicalObject*
Layout::getObjectWithId (ListOf* list,const std::string& id) 
{
  GraphicalObject* object=NULL;
  unsigned int counter=0;
  while(counter < list->size()) {
    GraphicalObject* tmp=dynamic_cast<GraphicalObject*>(list->get(counter));
    if(tmp->getId()==id){
      object=tmp;
      break;
    }
    ++counter;
  }    
  return object;
}

GraphicalObject*
Layout::removeObjectWithId (ListOf* list,const std::string& id)
{
  GraphicalObject* object=NULL;
  unsigned int counter=0;
  while(counter < list->size()) {
    GraphicalObject* tmp=dynamic_cast<GraphicalObject*>(list->get(counter));
    if(tmp->getId()==id){
      object=tmp;
      list->remove(counter);
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
CompartmentGlyph* Layout::removeCompartmentGlyph(unsigned int index)
{
    CompartmentGlyph* glyph=NULL;
    if(index < this->getNumCompartmentGlyphs())
    {
      glyph=dynamic_cast<CompartmentGlyph*>(this->getListOfCompartmentGlyphs()->remove(index));
    }
    return glyph;
}

/**
 * Removes the species glyph with the given index from the layout.
 * A pointer to the species glyph that was removed is returned.
 * If no species glyph has been removed, NULL is returned.
 */
SpeciesGlyph* Layout::removeSpeciesGlyph(unsigned int index)
{
    SpeciesGlyph* glyph=NULL;
    if(index < this->getNumSpeciesGlyphs())
    {
      glyph=dynamic_cast<SpeciesGlyph*>(this->getListOfSpeciesGlyphs()->remove(index));
    }
    return glyph;
}

/**
 * Removes the reaction glyph with the given index from the layout.
 * A pointer to the reaction glyph that was removed is returned.
 * If no reaction glyph has been removed, NULL is returned.
 */
ReactionGlyph* Layout::removeReactionGlyph(unsigned int index)
{
    ReactionGlyph* glyph=NULL;
    if(index < this->getNumReactionGlyphs())
    {
      glyph=dynamic_cast<ReactionGlyph*>(this->getListOfReactionGlyphs()->remove(index));
    }
    return glyph;
}

/**
 * Removes the text glyph with the given index from the layout.
 * A pointer to the text glyph that was removed is returned.
 * If no text glyph has been removed, NULL is returned.
 */
TextGlyph* Layout::removeTextGlyph(unsigned int index)
{
    TextGlyph* glyph=NULL;
    if(index < this->getNumTextGlyphs())
    {
      glyph=dynamic_cast<TextGlyph*>(this->getListOfTextGlyphs()->remove(index));
    }
    return glyph;
}

/**
 * Removes the graphical object with the given index from the layout.
 * A pointer to the graphical object that was removed is returned.
 * If no graphical object has been removed, NULL is returned.
 */
GraphicalObject* Layout::removeAdditionalGraphicalObject(unsigned int index)
{
    GraphicalObject* go=NULL;
    if(index < this->getNumAdditionalGraphicalObjects())
    {
      go=dynamic_cast<GraphicalObject*>(this->getListOfAdditionalGraphicalObjects()->remove(index));
    }
    return go;
}

/**
 * Remove the compartment glyph with the given id.
 * A pointer to the removed compartment glyph is returned.
 * If no compartment glyph has been removed, NULL is returned.
 */
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
SpeciesReferenceGlyph*
Layout::removeSpeciesReferenceGlyph(const std::string id)
{
    SpeciesReferenceGlyph *srg=NULL;
    unsigned int i,iMax=this->getNumReactionGlyphs();
    for(i=0;i<iMax;++i)
    {
        ReactionGlyph* rg=this->getReactionGlyph(i);
        unsigned int index=rg->getIndexForSpeciesReferenceGlyph(id);
        if(index!=std::numeric_limits<unsigned int>::max())
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
GraphicalObject*
Layout::removeAdditionalGraphicalObject(const std::string id)
{
    return this->removeObjectWithId(this->getListOfAdditionalGraphicalObjects(),id);
}

/**
 * Returns the compartment glyph that has the given id, or NULL if no
 * compartment glyph has the id.
 */
CompartmentGlyph*
Layout::getCompartmentGlyph (const std::string& id) 
{
  return (CompartmentGlyph*) this->getObjectWithId(&this->mCompartmentGlyphs, id);
}

/**
 * Returns the compartment glyph that has the given id, or NULL if no
 * compartment glyph has the id.
 */
const CompartmentGlyph*
Layout::getCompartmentGlyph (const std::string& id) const
{
  return (const CompartmentGlyph*) this->getObjectWithId(&this->mCompartmentGlyphs, id);
}


/**
 * Returns the species glyph that has the given id, or NULL if no
 * compartment glyph has the id.
 */
const SpeciesGlyph*
Layout::getSpeciesGlyph (const std::string& id) const
{
  return (const SpeciesGlyph*) this->getObjectWithId(&this->mSpeciesGlyphs, id);
}

/**
 * Returns the species glyph that has the given id, or NULL if no
 * compartment glyph has the id.
 */
SpeciesGlyph*
Layout::getSpeciesGlyph (const std::string& id) 
{
  return (SpeciesGlyph*) this->getObjectWithId(&this->mSpeciesGlyphs, id);
}


/**
 * Returns the reaction glyph that has the given id, or NULL if no
 * compartment glyph has the id.
 */
const ReactionGlyph*
Layout::getReactionGlyph (const std::string& id) const
{
  return (const ReactionGlyph*) this->getObjectWithId(&this->mReactionGlyphs, id);
}

/**
 * Returns the reaction glyph that has the given id, or NULL if no
 * compartment glyph has the id.
 */
ReactionGlyph*
Layout::getReactionGlyph (const std::string& id) 
{
  return (ReactionGlyph*) this->getObjectWithId(&this->mReactionGlyphs, id);
}


/**
 * Returns the text glyph that has the given id, or NULL if no compartment
 * glyph has the id.
 */
const TextGlyph*
Layout::getTextGlyph (const std::string& id) const
{
  return (const TextGlyph*) this->getObjectWithId(&this->mTextGlyphs, id);
}


/**
 * Returns the text glyph that has the given id, or NULL if no compartment
 * glyph has the id.
 */
TextGlyph*
Layout::getTextGlyph (const std::string& id) 
{
  return (TextGlyph*) this->getObjectWithId(&this->mTextGlyphs, id);
}


/**
 * Returns the additional graphicalo object that has the given id, or NULL
 * if no compartment glyph has the id.
 */
const GraphicalObject*
Layout::getAdditionalGraphicalObject (const std::string& id) const
{
  return this->getObjectWithId(&this->mAdditionalGraphicalObjects, id);
}

/**
 * Returns the additional graphicalo object that has the given id, or NULL
 * if no compartment glyph has the id.
 */
GraphicalObject*
Layout::getAdditionalGraphicalObject (const std::string& id) 
{
  return this->getObjectWithId(&this->mAdditionalGraphicalObjects, id);
}


/**
 * Adds a new compartment glyph.
 */
int
Layout::addCompartmentGlyph (const CompartmentGlyph* glyph)
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
       && (getListOfCompartmentGlyphs()->get(glyph->getId())) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
      /* if the ListOf is empty it doesnt know its parent */
      if (mCompartmentGlyphs.size() == 0)
      {
          mCompartmentGlyphs.setSBMLDocument(this->getSBMLDocument());
          mCompartmentGlyphs.setParentSBMLObject(this);
      }
      this->mCompartmentGlyphs.append(glyph);

      return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Adds a new species glyph.
 */
int
Layout::addSpeciesGlyph (const SpeciesGlyph* glyph)
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
       && (getListOfSpeciesGlyphs()->get(glyph->getId())) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
      /* if the ListOf is empty it doesnt know its parent */
      if (mSpeciesGlyphs.size() == 0)
      {
          mSpeciesGlyphs.setSBMLDocument(this->getSBMLDocument());
          mSpeciesGlyphs.setParentSBMLObject(this);
      }
      this->mSpeciesGlyphs.append(glyph);

      return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Adds a new reaction glyph.
 */
int
Layout::addReactionGlyph (const ReactionGlyph* glyph)
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
       && (getListOfReactionGlyphs()->get(glyph->getId())) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
      /* if the ListOf is empty it doesnt know its parent */
      if (mReactionGlyphs.size() == 0)
      {
          mReactionGlyphs.setSBMLDocument(this->getSBMLDocument());
          mReactionGlyphs.setParentSBMLObject(this);
      }
      this->mReactionGlyphs.append(glyph);

      return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Adds a new text glyph.
 */
int
Layout::addTextGlyph (const TextGlyph* glyph)
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
       && (getListOfTextGlyphs()->get(glyph->getId())) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
      /* if the ListOf is empty it doesnt know its parent */
      if (mTextGlyphs.size() == 0)
      {
          mTextGlyphs.setSBMLDocument(this->getSBMLDocument());
          mTextGlyphs.setParentSBMLObject(this);
      }
      this->mTextGlyphs.append(glyph);

      return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Adds a new additional graphical object glyph.
 */
int
Layout::addAdditionalGraphicalObject (const GraphicalObject* glyph)
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
       && (getListOfAdditionalGraphicalObjects()->get(glyph->getId())) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
      /* if the ListOf is empty it doesnt know its parent */
      if (mAdditionalGraphicalObjects.size() == 0)
      {
          mAdditionalGraphicalObjects.setSBMLDocument(this->getSBMLDocument());
          mAdditionalGraphicalObjects.setParentSBMLObject(this);
      }
      this->mAdditionalGraphicalObjects.append(glyph);

      return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Returns the number of compartment glyphs for the layout.
 */
unsigned int
Layout::getNumCompartmentGlyphs () const
{
  return this->mCompartmentGlyphs.size();
}


/**
 * Returns the number of species glyphs for the layout.
 */
unsigned int
Layout::getNumSpeciesGlyphs () const
{
  return this->mSpeciesGlyphs.size();
}


/**
 * Returns the number of reaction glyphs for the layout.
 */
unsigned int
Layout::getNumReactionGlyphs () const
{
  return this->mReactionGlyphs.size();
}


/**
 * Returns the number of text glyphs for the layout.
 */
unsigned int
Layout::getNumTextGlyphs () const
{
  return this->mTextGlyphs.size();
}


/**
 * Returns the number of additional graphical objects for the layout.
 */
unsigned int
Layout::getNumAdditionalGraphicalObjects () const
{
  return this->mAdditionalGraphicalObjects.size();
}


/**
 * Creates a CompartmentGlyph object, adds it to the end of the compartment
 * glyph objects list and returns a reference to the newly created object.
 */
CompartmentGlyph* 
Layout::createCompartmentGlyph ()
{
  CompartmentGlyph* p = NULL;
  try
  {
    p = new CompartmentGlyph(getSBMLNamespaces());
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
  if (mCompartmentGlyphs.size() == 0)
  {
    mCompartmentGlyphs.setSBMLDocument(this->getSBMLDocument());
    mCompartmentGlyphs.setParentSBMLObject(this);
  }

  if(p != NULL)
  {
      this->mCompartmentGlyphs.appendAndOwn(p);
  }
  return p;
}


/**
 * Creates a SpeciesGlyph object, adds it to the end of the species glyph
 * objects list and returns a reference to the newly created object.
 */
SpeciesGlyph* 
Layout::createSpeciesGlyph ()
{
  SpeciesGlyph* p = NULL;
  try
  {
    p = new SpeciesGlyph(getSBMLNamespaces());
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
  if (mSpeciesGlyphs.size() == 0)
  {
    mSpeciesGlyphs.setSBMLDocument(this->getSBMLDocument());
    mSpeciesGlyphs.setParentSBMLObject(this);
  }

  if(p != NULL)
  {
      this->mSpeciesGlyphs.appendAndOwn(p);
  }
  return p;
}


/**
 * Creates a ReactionGlyph object, adds it to the end of the reaction glyph
 * objects list and returns a reference to the newly created object.
 */
ReactionGlyph* 
Layout::createReactionGlyph ()
{
  ReactionGlyph* p = NULL;
  try
  {
    p = new ReactionGlyph(getSBMLNamespaces());
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
  if (mReactionGlyphs.size() == 0)
  {
    mReactionGlyphs.setSBMLDocument(this->getSBMLDocument());
    mReactionGlyphs.setParentSBMLObject(this);
  }

  if(p != NULL)
  {
      this->mReactionGlyphs.appendAndOwn(p);
  }
  return p;
}


/**
 * Creates a TextGlyph object, adds it to the end of the text glyph objects
 * list and returns a reference to the newly created object.
 */
TextGlyph* 
Layout::createTextGlyph ()
{
  TextGlyph* p = NULL;
  try
  {
    p = new TextGlyph(getSBMLNamespaces());
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
  if (mTextGlyphs.size() == 0)
  {
    mTextGlyphs.setSBMLDocument(this->getSBMLDocument());
    mTextGlyphs.setParentSBMLObject(this);
  }

  if(p != NULL)
  {
      this->mTextGlyphs.appendAndOwn(p);
  }
  return p;
}


/**
 * Creates a GraphicalObject object, adds it to the end of the additional
 * graphical objects list and returns a reference to the newly created
 * object.
 */
GraphicalObject* 
Layout::createAdditionalGraphicalObject ()
{
  GraphicalObject* p = NULL;
  try
  {
    p = new GraphicalObject(getSBMLNamespaces());
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
  if (mAdditionalGraphicalObjects.size() == 0)
  {
    mAdditionalGraphicalObjects.setSBMLDocument(this->getSBMLDocument());
    mAdditionalGraphicalObjects.setParentSBMLObject(this);
  }

  if(p != NULL)
  {
      this->mAdditionalGraphicalObjects.appendAndOwn(p);
  }
  return p;
}


/**
 * Creates a new SpeciesReferenceGlyph for the last ReactionGlyph and adds
 * it to its list of SpeciesReferenceGlyph objects.  A pointer to the newly
 * created object is returned.
 */
SpeciesReferenceGlyph* 
Layout::createSpeciesReferenceGlyph ()
{
  int size = this->mReactionGlyphs.size();
  if (size == 0) return NULL;

  ReactionGlyph* r =dynamic_cast<ReactionGlyph*> (this->getReactionGlyph(size - 1));
  return r->createSpeciesReferenceGlyph();
}


/**
 * Creates a new LineSegment for the Curve object of the last ReactionGlyph
 * or the last SpeciesReferenceGlyph in the last ReactionGlyph and adds it
 * to its list of SpeciesReferenceGlyph objects.  A pointer to the newly
 * created object is returned.
 */
LineSegment* 
Layout::createLineSegment()
{
  int size = this->mReactionGlyphs.size();
  if (size == 0) return NULL;

  LineSegment*   ls = NULL;
  ReactionGlyph* r  = dynamic_cast<ReactionGlyph*> (this->getReactionGlyph(size - 1));

  size = r->getListOfSpeciesReferenceGlyphs()->size();
  if(size > 0)
  {
    SpeciesReferenceGlyph* srg = r->getSpeciesReferenceGlyph(size-1);
    ls = srg->createLineSegment();
  }
  else
  {
    ls = r->createLineSegment();
  }

  return ls;
}        


/**
 * Creates a new CubicBezier for the Curve object of the last ReactionGlyph
 * or the last SpeciesReferenceGlyph in the last ReactionGlyph and adds it
 * to its list of SpeciesReferenceGlyph objects.  A pointer to the newly
 * created object is returned.
 */
CubicBezier* 
Layout::createCubicBezier ()
{
  int size = this->mReactionGlyphs.size();
  if (size == 0) return NULL;

  CubicBezier*   cb = NULL;
  ReactionGlyph* r  = dynamic_cast<ReactionGlyph*>( this->getReactionGlyph(size - 1));

  size = r->getListOfSpeciesReferenceGlyphs()->size();
  if(size > 0)
  {
    SpeciesReferenceGlyph* srg = r->getSpeciesReferenceGlyph(size-1);
    cb = srg->createCubicBezier();
  }
  else
  {
    cb = r->createCubicBezier();
  }

  return cb;
}    

/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
Layout::getElementName () const
{
  static const std::string name = "layout";
  return name;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
Layout::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "listOfCompartmentGlyphs")
  {
    object = &mCompartmentGlyphs;
  }

  else if ( name == "listOfSpeciesGlyphs"      ) object = &mSpeciesGlyphs;
  else if ( name == "listOfReactionGlyphs"       ) object = &mReactionGlyphs;
  else if ( name == "listOfTextGlyphs"            ) object = &mTextGlyphs;
  else if ( name == "listOfAdditionalGraphicalObjects") object = &mAdditionalGraphicalObjects;
  else if ( name == "dimensions"               ) object = &mDimensions;

  return object;
}


/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Layout::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  attributes.readInto("id", mId);

}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Layout::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("id", mId);
}


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Layout::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  mDimensions.write(stream);
  if (getNumCompartmentGlyphs() > 0)
  {
    mCompartmentGlyphs.write(stream);
  }

  if ( getNumSpeciesGlyphs() > 0 ) mSpeciesGlyphs.write(stream);

  if ( getNumReactionGlyphs() > 0 ) mReactionGlyphs.write(stream);
  if ( getNumTextGlyphs    () > 0 ) mTextGlyphs.write(stream);

  if ( getNumAdditionalGraphicalObjects() > 0 )
  {
      mAdditionalGraphicalObjects.write(stream);
  }
}


/**
 * Creates an XMLNode object from this.
 */
XMLNode Layout::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple layout_triple = XMLTriple("layout", "", "");
  XMLAttributes id_att = XMLAttributes();
  id_att.add("id", this->mId);
  // add the SBase Ids
  addSBaseAttributes(*this,id_att);
  XMLToken layout_token = XMLToken(layout_triple, id_att, xmlns); 
  XMLNode layout_node(layout_token);
  // add the notes and annotations
  if(this->mNotes) layout_node.addChild(*this->mNotes);
  if(this->mAnnotation) layout_node.addChild(*this->mAnnotation);
  // add the dimensions
  layout_node.addChild(this->mDimensions.toXML());
  // add the list of compartment glyphs
  if(this->mCompartmentGlyphs.size()>0)
  {
    layout_node.addChild(this->mCompartmentGlyphs.toXML());
  }
  // add the list of species glyphs
  if(this->mSpeciesGlyphs.size()>0)
  {
    layout_node.addChild(this->mSpeciesGlyphs.toXML());
  }
  // add the list of reaction glyphs
  if(this->mReactionGlyphs.size()>0)
  {
    layout_node.addChild(this->mReactionGlyphs.toXML());
  }
  // add the list of text glyphs
  if(this->mTextGlyphs.size()>0)
  {
    layout_node.addChild(this->mTextGlyphs.toXML());
  }
  // add the list of additional graphical objects
  if(this->mAdditionalGraphicalObjects.size()>0)
  {
    layout_node.addChild(this->mAdditionalGraphicalObjects.toXML());
  }
  return layout_node;
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Layout::getTypeCode () const
{
  return SBML_LAYOUT_LAYOUT;
}

SBase*
Layout::clone() const
{
    return new Layout(*this);
}


/**
 * Accepts the given SBMLVisitor.
 */
bool
Layout::accept (SBMLVisitor& v) const
{
  /*  
  bool result=v.visit(*this);
  this->mDimensions.accept(v);
  this->mCompartmentGlyphs.accept(v);
  this->mSpeciesGlyphs.accept(v);
  this->mReactionGlyphs.accept(v);
  this->mTextGlyphs.accept(v);
  this->mAdditionalGraphicalObjects.accept(v);
  v.leave(*this);*/
  return false;
}





/**
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
SBase*
ListOfLayouts::clone () const
{
  return new ListOfLayouts(*this);
}

ListOfLayouts& ListOfLayouts::operator=(const ListOfLayouts& source)
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
        SBase* o=static_cast<SBase*>(this->remove(0));
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

ListOfLayouts::ListOfLayouts(const ListOfLayouts& source)
{
    ListOfLayouts::operator=(source);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfLayouts::getItemTypeCode () const
{
  return SBML_LAYOUT_LAYOUT;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfLayouts::getElementName () const
{
  static const std::string name = "listOfLayouts";
  return name;
}

/* return nth item in list */
Layout *
ListOfLayouts::get(unsigned int n)
{
  return static_cast<Layout*>(ListOf::get(n));
}


/* return nth item in list */
const Layout *
ListOfLayouts::get(unsigned int n) const
{
  return static_cast<const Layout*>(ListOf::get(n));
}


/**
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
struct IdEqR_Layout : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEqR_Layout (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <Layout *> (sb)->getId() == id; }
};


/* return item by id */
Layout*
ListOfLayouts::get (const std::string& sid)
{
  return const_cast<Layout*>( 
    static_cast<const ListOfLayouts&>(*this).get(sid) );
}


/* return item by id */
const Layout*
ListOfLayouts::get (const std::string& sid) const
{
    std::vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqR_Layout(sid) );
  return (result == mItems.end()) ? 0 : static_cast <Layout*> (*result);
}



/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfLayouts::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "layout")
  {
    object = new Layout();
    mItems.push_back(object);
  }

  return object;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode ListOfLayouts::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  xmlns.add("http://projects.eml.org/bcb/sbml/level2", "");
  xmlns.add("http://www.w3.org/2001/XMLSchema-instance", "xsi");
  XMLTriple triple = XMLTriple("listOfLayouts", "http://projects.eml.org/bcb/sbml/level2", "");
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
  const Layout* layout=NULL;
  for(i=0;i<iMax;++i)
  {
    layout=dynamic_cast<const Layout*>(this->get(i));
    assert(layout);
    node.addChild(layout->toXML());
  }  
  if(end==true && iMax==0) node.setEnd();
  return node;
}

/**
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
SBase*
ListOfCompartmentGlyphs::clone () const
{
  return new ListOfCompartmentGlyphs(*this);
}

ListOfCompartmentGlyphs& ListOfCompartmentGlyphs::operator=(const ListOfCompartmentGlyphs& source)
{
  if(&source!=this)
  {
    copySBaseAttributes(source,*this);
    // clear the old list
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
    unsigned int i=0,iMax=this->size();
    while(i<iMax)
    {
        SBase* o=static_cast<SBase*>(this->remove(0));
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

ListOfCompartmentGlyphs::ListOfCompartmentGlyphs(const ListOfCompartmentGlyphs& source)
{
    ListOfCompartmentGlyphs::operator=(source);
}



/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfCompartmentGlyphs::getItemTypeCode () const
{
  return SBML_LAYOUT_COMPARTMENTGLYPH;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfCompartmentGlyphs::getElementName () const
{
  static const std::string name = "listOfCompartmentGlyphs";
  return name;
}

/* return nth item in list */
CompartmentGlyph *
ListOfCompartmentGlyphs::get(unsigned int n)
{
  return static_cast<CompartmentGlyph*>(ListOf::get(n));
}


/* return nth item in list */
const CompartmentGlyph *
ListOfCompartmentGlyphs::get(unsigned int n) const
{
  return static_cast<const CompartmentGlyph*>(ListOf::get(n));
}


/* return item by id */
CompartmentGlyph*
ListOfCompartmentGlyphs::get (const std::string& sid)
{
  return const_cast<CompartmentGlyph*>( 
    static_cast<const ListOfCompartmentGlyphs&>(*this).get(sid) );
}

/**
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
struct IdEqR_GraphicalObject : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEqR_GraphicalObject (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <GraphicalObject *> (sb)->getId() == id; }
};


/* return item by id */
const CompartmentGlyph*
ListOfCompartmentGlyphs::get (const std::string& sid) const
{
    std::vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqR_GraphicalObject(sid) );
  return (result == mItems.end()) ? 0 : static_cast <CompartmentGlyph*> (*result);
}



/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfCompartmentGlyphs::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "compartmentGlyph")
  {
    object = new CompartmentGlyph();
    mItems.push_back(object);
  }

  return object;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode ListOfCompartmentGlyphs::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("listOfCompartmentGlyphs", "http://projects.eml.org/bcb/sbml/level2", "");
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
  const CompartmentGlyph* object=NULL;
  for(i=0;i<iMax;++i)
  {
    object=dynamic_cast<const CompartmentGlyph*>(this->get(i));
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
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
SBase*
ListOfSpeciesGlyphs::clone () const
{
  return new ListOfSpeciesGlyphs(*this);
}

ListOfSpeciesGlyphs& ListOfSpeciesGlyphs::operator=(const ListOfSpeciesGlyphs& source)
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
        SBase* o=static_cast<SBase*>(this->remove(0));
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


ListOfSpeciesGlyphs::ListOfSpeciesGlyphs(const ListOfSpeciesGlyphs& source)
{
    ListOfSpeciesGlyphs::operator=(source);
}

/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfSpeciesGlyphs::getItemTypeCode () const
{
  return SBML_LAYOUT_SPECIESGLYPH;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfSpeciesGlyphs::getElementName () const
{
  static const std::string name = "listOfSpeciesGlyphs";
  return name;
}

/* return nth item in list */
SpeciesGlyph *
ListOfSpeciesGlyphs::get(unsigned int n)
{
  return static_cast<SpeciesGlyph*>(ListOf::get(n));
}


/* return nth item in list */
const SpeciesGlyph *
ListOfSpeciesGlyphs::get(unsigned int n) const
{
  return static_cast<const SpeciesGlyph*>(ListOf::get(n));
}



/* return item by id */
SpeciesGlyph*
ListOfSpeciesGlyphs::get (const std::string& sid)
{
  return const_cast<SpeciesGlyph*>( 
    static_cast<const ListOfSpeciesGlyphs&>(*this).get(sid) );
}

/* return item by id */
const SpeciesGlyph*
ListOfSpeciesGlyphs::get (const std::string& sid) const
{
    std::vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqR_GraphicalObject(sid) );
  return (result == mItems.end()) ? 0 : static_cast <SpeciesGlyph*> (*result);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfSpeciesGlyphs::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "speciesGlyph")
  {
    object = new SpeciesGlyph();
    mItems.push_back(object);
  }

  return object;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode ListOfSpeciesGlyphs::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("listOfSpeciesGlyphs", "http://projects.eml.org/bcb/sbml/level2", "");
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
  const SpeciesGlyph* object=NULL;
  for(i=0;i<iMax;++i)
  {
    object=dynamic_cast<const SpeciesGlyph*>(this->get(i));
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
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
SBase*
ListOfReactionGlyphs::clone () const
{
  return new ListOfReactionGlyphs(*this);
}

ListOfReactionGlyphs& ListOfReactionGlyphs::operator=(const ListOfReactionGlyphs& source)
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
        SBase* o=static_cast<SBase*>(this->remove(0));
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

ListOfReactionGlyphs::ListOfReactionGlyphs(const ListOfReactionGlyphs& source)
{
    ListOfReactionGlyphs::operator=(source);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfReactionGlyphs::getItemTypeCode () const
{
  return SBML_LAYOUT_REACTIONGLYPH;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfReactionGlyphs::getElementName () const
{
  static const std::string name = "listOfReactionGlyphs";
  return name;
}

/* return nth item in list */
ReactionGlyph *
ListOfReactionGlyphs::get(unsigned int n)
{
  return static_cast<ReactionGlyph*>(ListOf::get(n));
}


/* return nth item in list */
const ReactionGlyph *
ListOfReactionGlyphs::get(unsigned int n) const
{
  return static_cast<const ReactionGlyph*>(ListOf::get(n));
}


/* return item by id */
ReactionGlyph*
ListOfReactionGlyphs::get (const std::string& sid)
{
  return const_cast<ReactionGlyph*>( 
    static_cast<const ListOfReactionGlyphs&>(*this).get(sid) );
}

/* return item by id */
const ReactionGlyph*
ListOfReactionGlyphs::get (const std::string& sid) const
{
    std::vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqR_GraphicalObject(sid) );
  return (result == mItems.end()) ? 0 : static_cast <ReactionGlyph*> (*result);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfReactionGlyphs::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "reactionGlyph")
  {
    object = new ReactionGlyph();
    mItems.push_back(object);
  }

  return object;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode ListOfReactionGlyphs::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("listOfReactionGlyphs", "http://projects.eml.org/bcb/sbml/level2", "");
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
  const ReactionGlyph* object=NULL;
  for(i=0;i<iMax;++i)
  {
    object=dynamic_cast<const ReactionGlyph*>(this->get(i));
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
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
SBase*
ListOfTextGlyphs::clone () const
{
  return new ListOfTextGlyphs(*this);
}

ListOfTextGlyphs& ListOfTextGlyphs::operator=(const ListOfTextGlyphs& source)
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
        SBase* o=static_cast<SBase*>(this->remove(0));
        delete o;
        ++i;
    }
    i=0;iMax=source.size();
    while(i<iMax)
    {
      this->append(source.get(i));
      ++i;
    }
  }
  
  return *this;
}

ListOfTextGlyphs::ListOfTextGlyphs(const ListOfTextGlyphs& source)
{
    ListOfTextGlyphs::operator=(source);
}

/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfTextGlyphs::getItemTypeCode () const
{
  return SBML_LAYOUT_TEXTGLYPH;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfTextGlyphs::getElementName () const
{
  static const std::string name = "listOfTextGlyphs";
  return name;
}

/* return nth item in list */
TextGlyph *
ListOfTextGlyphs::get(unsigned int n)
{
  return static_cast<TextGlyph*>(ListOf::get(n));
}


/* return nth item in list */
const TextGlyph *
ListOfTextGlyphs::get(unsigned int n) const
{
  return static_cast<const TextGlyph*>(ListOf::get(n));
}


/* return item by id */
TextGlyph*
ListOfTextGlyphs::get (const std::string& sid)
{
  return const_cast<TextGlyph*>( 
    static_cast<const ListOfTextGlyphs&>(*this).get(sid) );
}


/* return item by id */
const TextGlyph*
ListOfTextGlyphs::get (const std::string& sid) const
{
    std::vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqR_GraphicalObject(sid) );
  return (result == mItems.end()) ? 0 : static_cast <TextGlyph*> (*result);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfTextGlyphs::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "textGlyph")
  {
    object = new TextGlyph();
    mItems.push_back(object);
  }

  return object;
}

/**
 * Creates an XMLNode object from this.
 */
XMLNode ListOfTextGlyphs::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("listOfTextGlyphs", "http://projects.eml.org/bcb/sbml/level2", "");
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
  const TextGlyph* object=NULL;
  for(i=0;i<iMax;++i)
  {
    object=dynamic_cast<const TextGlyph*>(this->get(i));
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
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
SBase*
ListOfGraphicalObjects::clone () const
{
  return new ListOfGraphicalObjects(*this);
}

ListOfGraphicalObjects& ListOfGraphicalObjects::operator=(const ListOfGraphicalObjects& source)
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
        SBase* o=static_cast<SBase*>(this->remove(0));
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

ListOfGraphicalObjects::ListOfGraphicalObjects(const ListOfGraphicalObjects& source)
{
    ListOfGraphicalObjects::operator=(source);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfGraphicalObjects::getItemTypeCode () const
{
  return SBML_LAYOUT_GRAPHICALOBJECT;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfGraphicalObjects::getElementName () const
{
  static const std::string name = "listOfAdditionalGraphicalObjects";
  return name;
}

/* return nth item in list */
GraphicalObject *
ListOfGraphicalObjects::get(unsigned int n)
{
  return static_cast<GraphicalObject*>(ListOf::get(n));
}


/* return nth item in list */
const GraphicalObject *
ListOfGraphicalObjects::get(unsigned int n) const
{
  return static_cast<const GraphicalObject*>(ListOf::get(n));
}


/* return item by id */
GraphicalObject*
ListOfGraphicalObjects::get (const std::string& sid)
{
  return const_cast<GraphicalObject*>( 
    static_cast<const ListOfGraphicalObjects&>(*this).get(sid) );
}

/* return item by id */
const GraphicalObject*
ListOfGraphicalObjects::get (const std::string& sid) const
{
    std::vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqR_GraphicalObject(sid) );
  return (result == mItems.end()) ? 0 : static_cast <GraphicalObject*> (*result);
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfGraphicalObjects::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "graphicalObject")
  {
    object = new GraphicalObject();
    mItems.push_back(object);
  }

  return object;
}


/**
 * Creates an XMLNode object from this.
 */
XMLNode ListOfGraphicalObjects::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple("listOfAdditionalGraphicalObjects", "http://projects.eml.org/bcb/sbml/level2", "");
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
  const GraphicalObject* object=NULL;
  for(i=0;i<iMax;++i)
  {
    object=dynamic_cast<const GraphicalObject*>(this->get(i));
    assert(object);
    node.addChild(object->toXML());
  }
  if(end==true && iMax==0)
  {
    node.setEnd();
  }
  return node;
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Layout::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
  mDimensions.setSBMLDocument(d);
  mCompartmentGlyphs.setSBMLDocument(d);
  mSpeciesGlyphs .setSBMLDocument(d);
  mReactionGlyphs.setSBMLDocument(d);
  mTextGlyphs.setSBMLDocument(d);
  mAdditionalGraphicalObjects.setSBMLDocument(d);
}

/**
  * Sets the parent SBML object of this SBML object.
  *
  * @param sb the SBML object to use
  */
void 
Layout::setParentSBMLObject (SBase* sb)
{
  mParentSBMLObject = sb;
}



/**
 * This methods calculates the bounding box of the layout.
 * It traverses all layout objects and looks for the minimal and maximal x
 * and y values that occur in the layout.
 * These values are returned in the form of a bounding box where the minimal
 * values are stored in the position and the maxima are given as the minimal
 * values plus the corresponding dimension.
 */
BoundingBox Layout::calculateBoundingBox() const
{
    double minX=std::numeric_limits<double>::max();
    double minY=minX;
    double maxX=-minX;
    double maxY=-minX;

    const GraphicalObject* pObject=NULL;
    const Curve* pCurve=NULL;
    const BoundingBox* pBB;
    const Point* pP=NULL;
    const Dimensions* pDim;
    unsigned int i,iMax=this->getNumCompartmentGlyphs();
    double x,y,x2,y2;
    for(i=0;i<iMax;++i)
    {
        pObject=this->getCompartmentGlyph(i);
        pBB=pObject->getBoundingBox();
        pP=pBB->getPosition();
        x=pP->x();
        y=pP->y();
        pDim=pBB->getDimensions();
        x2=x+pDim->getWidth();
        y2=y+pDim->getHeight();
        minX=(minX<x)?minX:x;
        minY=(minY<y)?minY:y;
        maxX=(maxX>x2)?maxX:x2;
        maxY=(maxY>y2)?maxY:y2;
    }
    iMax=this->getNumSpeciesGlyphs();
    for(i=0;i<iMax;++i)
    {
        pObject=this->getSpeciesGlyph(i);
        pBB=pObject->getBoundingBox();
        pP=pBB->getPosition();
        x=pP->x();
        y=pP->y();
        pDim=pBB->getDimensions();
        x2=x+pDim->getWidth();
        y2=y+pDim->getHeight();
        minX=(minX<x)?minX:x;
        minY=(minY<y)?minY:y;
        maxX=(maxX>x2)?maxX:x2;
        maxY=(maxY>y2)?maxY:y2;
    }
    iMax=this->getNumTextGlyphs();
    for(i=0;i<iMax;++i)
    {
        pObject=this->getTextGlyph(i);
        pBB=pObject->getBoundingBox();
        pP=pBB->getPosition();
        x=pP->x();
        y=pP->y();
        pDim=pBB->getDimensions();
        x2=x+pDim->getWidth();
        y2=y+pDim->getHeight();
        minX=(minX<x)?minX:x;
        minY=(minY<y)?minY:y;
        maxX=(maxX>x2)?maxX:x2;
        maxY=(maxY>y2)?maxY:y2;
    }
    iMax=this->getNumAdditionalGraphicalObjects();
    for(i=0;i<iMax;++i)
    {
        pObject=this->getAdditionalGraphicalObject(i);
        pBB=pObject->getBoundingBox();
        pP=pBB->getPosition();
        x=pP->x();
        y=pP->y();
        pDim=pBB->getDimensions();
        x2=x+pDim->getWidth();
        y2=y+pDim->getHeight();
        minX=(minX<x)?minX:x;
        minY=(minY<y)?minY:y;
        maxX=(maxX>x2)?maxX:x2;
        maxY=(maxY>y2)?maxY:y2;
    }
    const ReactionGlyph* pRG=NULL;
    const SpeciesReferenceGlyph* pSRG=NULL;
    unsigned int j,jMax;
    iMax=this->getNumReactionGlyphs();
    for(i=0;i<iMax;++i)
    {
        pRG=this->getReactionGlyph(i);
        if(pRG->isSetCurve())
        {
            pCurve=pRG->getCurve();
            BoundingBox bb=pCurve->calculateBoundingBox();
            pP=bb.getPosition();
            x=pP->x();
            y=pP->y();
            pDim=bb.getDimensions();
            x2=x+pDim->getWidth();
            y2=y+pDim->getHeight();
            minX=(minX<x)?minX:x;
            minY=(minY<y)?minY:y;
            maxX=(maxX>x2)?maxX:x2;
            maxY=(maxY>y2)?maxY:y2;
        }
        else
        {
          pBB=pRG->getBoundingBox();
          pP=pBB->getPosition();
          x=pP->x();
          y=pP->y();
          pDim=pBB->getDimensions();
          x2=x+pDim->getWidth();
          y2=y+pDim->getHeight();
          minX=(minX<x)?minX:x;
          minY=(minY<y)?minY:y;
          maxX=(maxX>x2)?maxX:x2;
          maxY=(maxY>y2)?maxY:y2;
        }
        jMax=pRG->getNumSpeciesReferenceGlyphs();
        for(j=0;j<jMax;++j)
        {
            pSRG=pRG->getSpeciesReferenceGlyph(j);
            if(pSRG->isSetCurve())
            {
                pCurve=pSRG->getCurve();
                BoundingBox bb=pCurve->calculateBoundingBox();
                pP=bb.getPosition();
                x=pP->x();
                y=pP->y();
                pDim=bb.getDimensions();
                x2=x+pDim->getWidth();
                y2=y+pDim->getHeight();
                minX=(minX<x)?minX:x;
                minY=(minY<y)?minY:y;
                maxX=(maxX>x2)?maxX:x2;
                maxY=(maxY>y2)?maxY:y2;
            }
            else
            {
                pBB=pSRG->getBoundingBox();
                pP=pBB->getPosition();
                x=pP->x();
                y=pP->y();
                pDim=pBB->getDimensions();
                x2=x+pDim->getWidth();
                y2=y+pDim->getHeight();
                minX=(minX<x)?minX:x;
                minY=(minY<y)?minY:y;
                maxX=(maxX>x2)?maxX:x2;
                maxY=(maxY>y2)?maxY:y2;
            }
        }
    }
    return BoundingBox("bb",minX,minY,maxX-minX,maxY-minY);
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

/** @cond doxygen-libsbml-internal */
/**
 * Creates a new Layout_t structure using the given SBML @p 
 * level and @p version values and a set of XMLNamespaces.
 *
 * @param level an unsigned int, the SBML Level to assign to this 
 * Layout
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Layout
 * 
 * @param xmlns XMLNamespaces, a pointer to an array of XMLNamespaces to
 * assign to this Layout
 *
 * @return a pointer to the newly created Layout_t structure.
 *
 * @note Once a Layout has been added to an SBMLDocument, the @p 
 * level, @p version and @p xmlns namespaces for the document @em override 
 * those used to create the Reaction.  Despite this, the ability 
 * to supply the values at creation time is an important aid to creating 
 * valid SBML.  Knowledge of the intended SBML Level and Version 
 * determine whether it is valid to assign a particular value to an 
 * attribute, or whether it is valid to add an object to an existing 
 * SBMLDocument.
 */
LIBSBML_EXTERN
Layout_t *
Layout_createWithLevelVersionAndNamespaces (unsigned int level,
              unsigned int version)
{
  return new(std::nothrow) Layout(level, version);
}
/** @endcond */


/**
 * Creates a new Layout with the given id and returns a pointer to it.
 */
LIBSBML_EXTERN
Layout_t *
Layout_createWith (const char *sid)
{
  Dimensions* d=new Dimensions();
  Layout_t* l=new(std::nothrow) Layout(sid ? sid : "", d);
  delete d;
  return l;
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
    Dimensions* d=new Dimensions(width,height,depth);
    Layout_t* l=new (std::nothrow) Layout(id ? id : "", d);
    delete d;
    return l;
}


/**
 * Creates a new Layout with the given Dimensions and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
Layout_t *
Layout_createWithDimensions (const char *id, const Dimensions_t *dimensions)
{
  return new (std::nothrow) Layout(id ? id : "", dimensions);
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
 * Sets the dimensions of the layout.
 */
LIBSBML_EXTERN
void
Layout_setDimensions (Layout_t *l, const Dimensions_t *dimensions)
{
    static_cast<Layout*>(l)->setDimensions( dimensions );
}


/**
 * Adds a new compartment glyph to the list of compartment glyphs.
 */
LIBSBML_EXTERN
void
Layout_addCompartmentGlyph (Layout_t *l, CompartmentGlyph_t *cg)
{
  l->addCompartmentGlyph(cg);
}


/**
 * Adds a new species glyph to the list of species glyphs.
 */
LIBSBML_EXTERN
void
Layout_addSpeciesGlyph (Layout_t *l, SpeciesGlyph_t *sg)
{
  l->addSpeciesGlyph(sg);
}


/**
 * Adds a new reaction glyph to the list of reaction glyphs.
 */
LIBSBML_EXTERN
void
Layout_addReactionGlyph (Layout_t *l, ReactionGlyph_t *rg)
{
  l->addReactionGlyph(rg);
}


/**
 * Adds a new TextGlyph to the list of text glyphs.
 */
LIBSBML_EXTERN
void
Layout_addTextGlyph (Layout_t *l, TextGlyph_t *tg)
{
  l->addTextGlyph(tg);
}


/**
 * Adds a new GraphicalObject to the list of additional graphical objects.
 */
LIBSBML_EXTERN
void
Layout_addAdditionalGraphicalObject (Layout_t *l, GraphicalObject_t *go)
{
  l->addAdditionalGraphicalObject(go);
}


/**
 * Returns a pointer to the CompartmentGlyph with the given index.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
Layout_getCompartmentGlyph (Layout_t *l, unsigned int index)
{
  return l->getCompartmentGlyph(index);
}


/**
 * Returns a pointer to the SpeciesGlyph with the given index.
 */
LIBSBML_EXTERN
SpeciesGlyph_t *
Layout_getSpeciesGlyph (Layout_t *l, unsigned int index)
{
  return l->getSpeciesGlyph(index);
}


/**
 * Returns a pointer to the ReactionGlyph with the given index.
 */
LIBSBML_EXTERN
ReactionGlyph_t *
Layout_getReactionGlyph (Layout_t *l, unsigned int index)
{
  return l->getReactionGlyph(index);
}


/**
 * Returns a pointer to the AdditionalGraphicalObject with the given index.
 */
LIBSBML_EXTERN
TextGlyph_t *
Layout_getTextGlyph (Layout_t *l, unsigned int index)
{
  return l->getTextGlyph(index);
}



/**
 * Returns a pointer to the GraphicalObject with the given index.
 */
LIBSBML_EXTERN
GraphicalObject_t *
Layout_getAdditionalGraphicalObject (Layout_t *l, unsigned int index)
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
  return l->getListOfCompartmentGlyphs();
}


/**
 * Returns a pointer to the list of SpeciesGlyphs.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfSpeciesGlyphs (Layout_t *l)
{
  return l->getListOfSpeciesGlyphs();
}


/**
 * Returns a pointer to the list of ReactionGlyphs.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfReactionGlyphs (Layout_t *l)
{
  return l->getListOfReactionGlyphs();
}


/**
 * Returns a pointer to the list of TextGlyphs.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfTextGlyphs (Layout_t *l)
{
  return l->getListOfTextGlyphs();
}


/**
 * Returns a pointer to the list of additional GraphicalObjects.
 */
LIBSBML_EXTERN
ListOf_t *
Layout_getListOfAdditionalGraphicalObjects (Layout_t *l)
{
  return l->getListOfAdditionalGraphicalObjects();
}



/**
 * @return the dimensions of the layout
 */
LIBSBML_EXTERN
Dimensions_t*
Layout_getDimensions(Layout_t *l)
{
  return l->getDimensions();
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

/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
Layout_t *
Layout_clone (const Layout_t *m)
{
  return static_cast<Layout*>( m->clone() );
}


/**
 * Returns non-zero if the id is set
 */
LIBSBML_EXTERN
int
Layout_isSetId (const Layout_t *l)
{
  return static_cast <int> (l->isSetId());
}

/**
 * Returns the id
 */
LIBSBML_EXTERN
const char *
Layout_getId (const Layout_t *l)
{
  return l->isSetId() ? l->getId().c_str() : NULL;
}

/**
 * Sets the id
 */
LIBSBML_EXTERN
int
Layout_setId (Layout_t *l, const char *sid)
{
  return (sid == NULL) ? l->setId("") : l->setId(sid);
}

/**
 * Unsets the id
 */
LIBSBML_EXTERN
void
Layout_unsetId (Layout_t *l)
{
  l->unsetId();
}

LIBSBML_CPP_NAMESPACE_END

