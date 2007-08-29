/**
 * @file    SBase.cpp
 * @brief   Implementation of SBase, the base object of all SBML objects.
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sstream>

#include <sbml/xml/XMLError.h>
#include <sbml/xml/XMLErrorLog.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>

#include <sbml/util/util.h>

#include <sbml/annotation/RDFAnnotation.h>

#include <sbml/KineticLaw.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLErrorLog.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/ListOf.h>
#include <sbml/SBase.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */

/**
 * elements permitted on the body element of xhtml
 */

static const char * XHTML_ELEMENTS[] =
{
      "a"
    , "abbr"
    , "acronym"
    , "address"
    , "applet"
    , "b"
    , "basefont"
    , "bdo"
    , "big"
    , "blockquote"
    , "br"
    , "button"
    , "center"
    , "cite"
    , "code"
    , "del"
    , "dfn"
    , "dir"
    , "div"
    , "dl"
    , "em"
    , "fieldset"
    , "font"
    , "form"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "h5"
    , "h6"
    , "hr"
    , "i"
    , "iframe"
    , "img"
    , "input"
    , "ins"
    , "isindex"
    , "kbd"
    , "label"
    , "map"
    , "menu"
    , "noframes"
    , "noscript"
    , "object"
    , "ol"
    , "p"
    , "pre"
    , "q"
    , "s"
    , "samp"
    , "script"
    , "select"
    , "small"
    , "span"
    , "strike"
    , "strong"
    , "sub"
    , "sup"
    , "table"
    , "textarea"
    , "tt"
    , "u"
    , "ul"
    , "var"
};


/** @cond doxygen-libsbml-internal */
/**
 * Only subclasses may create SBase objects.
 */
SBase::SBase (const std::string& id, const std::string& name, int sbo) :
   mId        ( id   )
 , mName      ( name )
 , mNotes     ( 0 )
 , mAnnotation( 0 )
 , mNamespaces( 0 )
 , mSBML      ( 0 )
 , mSBOTerm   ( sbo )
 , mLine      ( 0 )
 , mColumn    ( 0 )
 , mCVTerms   ( 0 )
{
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Creates a new SBase object with the given sboTerm.
 * Only subclasses may create SBase objects.
 */
SBase::SBase (int sbo) :
   mId        ( ""   )
 , mName      ( "" )
 , mNotes     ( 0 )
 , mAnnotation( 0 )
 , mNamespaces( 0 )
 , mSBML      ( 0 )
 , mSBOTerm   ( sbo )
 , mLine      ( 0 )
 , mColumn    ( 0 )
 , mCVTerms   ( 0 )
{
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Copy constructor. Creates a copy of this SBase object.
 */
SBase::SBase(const SBase& orig)
{
    this->mId     = orig.mId;
    this->mName   = orig.mName;
    this->mMetaId = orig.mMetaId;

    if(orig.mNotes) 
      this->mNotes = new XMLNode(*const_cast<SBase&>(orig).getNotes());
    else
      this->mNotes = 0;
    
    if(orig.mAnnotation) 
      this->mAnnotation = new XMLNode(*const_cast<SBase&>(orig).mAnnotation);
    else
      this->mAnnotation = 0;
    
    this->mSBML       = orig.mSBML;
    this->mSBOTerm    = orig.mSBOTerm;
    this->mLine       = orig.mLine;
    this->mColumn     = orig.mColumn;

    if(orig.mNamespaces)
      this->mNamespaces = new XMLNamespaces(*const_cast<SBase&>(orig).mNamespaces);
    else
      this->mNamespaces = 0;

    if(orig.mCVTerms)
    {
      this->mCVTerms  = new List();
      unsigned int i,iMax = orig.mCVTerms->getSize();
      for(i = 0; i < iMax; ++i)
      {
        this->mCVTerms
          ->add(static_cast<CVTerm*>(orig.mCVTerms->get(i))->clone());
      }
    }
    else
    {
      this->mCVTerms = 0;
    }
}
/** @endcond doxygen-libsbml-internal */


/**
 * Destroy this SBase object.
 */
SBase::~SBase ()
{
  if (mNotes)       delete mNotes;
  if (mAnnotation)  delete mAnnotation;
  if (mNamespaces)  delete mNamespaces;
  if (mCVTerms)
  {  
    unsigned int size = mCVTerms->getSize();
    while (size--) delete static_cast<CVTerm*>( mCVTerms->remove(0) );
    delete mCVTerms;
  }
}

/**
 * Assignment operator
 */
SBase& SBase::operator=(const SBase& orig)
{
    this->mId     = orig.mId;
    this->mName   = orig.mName;
    this->mMetaId = orig.mMetaId;

    if(orig.mNotes) 
      this->mNotes = new XMLNode(*const_cast<SBase&>(orig).getNotes());
    else
      this->mNotes = 0;
    
    if(orig.mAnnotation) 
      this->mAnnotation = new XMLNode(*const_cast<SBase&>(orig).mAnnotation);
    else
      this->mAnnotation = 0;
    
    this->mSBML       = orig.mSBML;
    this->mSBOTerm    = orig.mSBOTerm;
    this->mLine       = orig.mLine;
    this->mColumn     = orig.mColumn;

    if(orig.mNamespaces)
      this->mNamespaces = new XMLNamespaces(*const_cast<SBase&>(orig).mNamespaces);
    else
      this->mNamespaces = 0;

    if(orig.mCVTerms)
    {
      this->mCVTerms  = new List();
      unsigned int i,iMax = orig.mCVTerms->getSize();
      for(i = 0; i < iMax; ++i)
      {
        this->mCVTerms
          ->add(static_cast<CVTerm*>(orig.mCVTerms->get(i))->clone());
      }
    }
    else
    {
      this->mCVTerms = 0;
    }

    return *this;

}


/**
 * @return the metaid of this SBML object.
 */
const string&
SBase::getMetaId () const
{
  return mMetaId;
}


/**
 * @return the metaid of this SBML object.
 */
string&
SBase::getMetaId ()
{
  return mMetaId;
}


/**
 * @return the id of this SBML object.
 */
const string&
SBase::getId () const
{
  return mId;
}


/**
 * @return the name of this SBML object.
 */
const string&
SBase::getName () const
{
  return (getLevel() == 1) ? mId : mName;
}


/**
 * @return the notes of this SBML object.
 */
XMLNode*
SBase::getNotes()
{
  return mNotes;
}


/**
 * @return the notes of this SBML object by string.
 */
std::string
SBase::getNotesString() 
{
  return XMLNode::convertXMLNodeToString(mNotes);
}


/**
 * @return the annotation of this SBML object.
 */
XMLNode* 
SBase::getAnnotation ()
{
  syncAnnotation();

  return mAnnotation;
}


/**
 * @return the annotation of this SBML object by string.
 */
std::string
SBase::getAnnotationString ()
{
  return XMLNode::convertXMLNodeToString(getAnnotation());
}


/**
  * @return the Namespaces associated with this SBML object
  */
XMLNamespaces*
SBase::getNamespaces() const
{
  return mNamespaces;
}


/**
 * @return the parent SBMLDocument of this SBML object.
 */
const SBMLDocument*
SBase::getSBMLDocument () const
{
  return mSBML;
}


/**
 * @return the sboTerm as an integer.  If not set,
 * sboTerm will be -1. 
 */
int
SBase::getSBOTerm () const
{
  return mSBOTerm;
}


/**
 * @return the line number of this SBML object.
 */
unsigned int
SBase::getLine () const
{
  return mLine;
}


/**
 * @return the column number of this SBML object.
 */
unsigned int
SBase::getColumn () const
{
  return mColumn;
}


/**
 * @return true if the metaid of this SBML object has been set, false
 * otherwise.
 */
bool
SBase::isSetMetaId () const
{
  return (mMetaId.empty() == false);
}


/**
 * @return true if the id of this SBML object has been set, false
 * otherwise.
 */
bool
SBase::isSetId () const
{
  return (mId.empty() == false);
}


/**
 * @return true if the name of this SBML object has been set, false
 * otherwise.
 */
bool
SBase::isSetName () const
{
  return (getLevel() == 1) ? (mId.empty() == false) : 
                            (mName.empty() == false);
}


/**
 * @return true if the notes of this SBML object has been set, false
 * otherwise.
 */
bool
SBase::isSetNotes () const
{
  return (mNotes != 0);
}


/**
 * @return true if the annotation of this SBML object has been set,
 * false otherwise.
 */
bool
SBase::isSetAnnotation () const
{
  const_cast <SBase *> (this)->syncAnnotation();
  return (mAnnotation != 0);
}


/**
 * @return true if the sboTerm has been set, false
 * otherwise.
 */
bool
SBase::isSetSBOTerm () const
{
  return (mSBOTerm != -1);
}


/**
 * Sets the metaid field of the given SBML object to a copy of metaid.
 */
void
SBase::setMetaId (const std::string& metaid)
{
  mMetaId = metaid;
}


/**
 * Sets the id of this SBML object to a copy of sid.
 */
void
SBase::setId (const std::string& sid)
{
  mId = sid;
}


/**
 * Sets the name of this SBML object to a copy of name.
 */
void
SBase::setName (const std::string& name)
{
  if (getLevel() == 1) mId = name;
  else mName = name;
}

/**
 * Sets the annotation of this SBML object to a copy of annotation.
 */
void 
SBase::setAnnotation (const XMLNode* annotation)
{
  if ( (mAnnotation != annotation) || !annotation)
  { 
    delete mAnnotation;
    if (annotation)
    {
      // check for annotation tags and add if necessary
      const string&  name = annotation->getName();
      if (name != "annotation")
      {
        XMLToken ann_t = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());
        mAnnotation = new XMLNode(ann_t);
        mAnnotation->addChild(*annotation);
      }
      else
      {
        mAnnotation = annotation->clone();
      }
    }
    else
    {
      // unset annotation if annotation is NULL, 
      mAnnotation = 0;
    }
  }

  if (mCVTerms)
  {
    // delete existing mCVTerms (if any)
    unsigned int size = mCVTerms->getSize();
    while (size--) delete static_cast<CVTerm*>( mCVTerms->remove(0) );
    delete mCVTerms;
    mCVTerms = NULL;
  }

  if(mAnnotation)
  {
    // parse mAnnotation (if any) and set mCVTerms 
    mCVTerms = new List();
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
  }
}

/**
 * Sets the annotation (by string) of this SBML object to a copy of annotation.
 */
void
SBase::setAnnotation (const std::string& annotation)
{
  if(annotation.empty()) 
  {
    unsetAnnotation();
    return;
  }

  XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
  XMLNode* annt_xmln = XMLNode::convertStringToXMLNode(annotation,xmlns); 
  if(annt_xmln)
  {
    setAnnotation(annt_xmln);
    delete annt_xmln;
  }
}


/**
 * Appends annotation to the existing annotations.
 * This allows other annotations to be preserved whilst
 * adding additional information.
 */
void 
SBase::appendAnnotation (const XMLNode* annotation)
{
  if(!annotation) return;

  XMLNode* new_annotation = NULL;
  const string&  name = annotation->getName();

  // check for annotation tags and add if necessary 
  if (name != "annotation")
  {
    XMLToken ann_t = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());
    new_annotation = new XMLNode(ann_t);
    new_annotation->addChild(*annotation);
  }
  else
  {
    new_annotation = annotation->clone();
  }

  // parse new_annotation and add mCVTerms (if any) 
  RDFAnnotationParser::parseRDFAnnotation(new_annotation,mCVTerms);

  // delete RDFAnnotation (CVTerm and ModelHistory) from new_annotation 
//  XMLNode* tmp_annotation = RDFAnnotationParser::deleteRDFAnnotation(new_annotation);
//  delete new_annotation;
//  new_annotation = tmp_annotation;

  if (mAnnotation != 0)
  {
    // if mAnnotation is just <annotation/> need to tell
    // it to no longer be an end
    if (mAnnotation->isEnd())
    {
      mAnnotation->unsetEnd();
    }

    for(unsigned int i=0; i < new_annotation->getNumChildren(); i++)
    {
      mAnnotation->addChild(new_annotation->getChild(i));
    }
  }
  else
  {
    setAnnotation(new_annotation);
  }

  delete new_annotation;
}

/**
 * Appends annotation (by string) to the existing annotations.
 * This allows other annotations to be preserved whilst
 * adding additional information.
 */
void
SBase::appendAnnotation (const std::string& annotation)
{
  XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
  XMLNode* annt_xmln = XMLNode::convertStringToXMLNode(annotation,xmlns);
  if(annt_xmln)
  {
    appendAnnotation(annt_xmln);
    delete annt_xmln;
  }
}



/**
 * Sets the notes of this SBML object to a copy of notes.
 */
void 
SBase::setNotes(const XMLNode* notes)
{
  if (mNotes == notes) return;
   
  delete mNotes;
  const string&  name = notes->getName();

  if (notes != 0)
  {
    /* check for notes tags and add if necessary */

    if (name == "notes")
    {
      mNotes = static_cast<XMLNode*>( notes->clone() );
    }
    else
    {
      XMLToken notes_t = XMLToken(XMLTriple("notes", "", ""), XMLAttributes());
      mNotes = new XMLNode(notes_t);
      mNotes->addChild(*notes);
    }
  }
  else
  {
    mNotes = 0;
  }
}

/**
 * Sets the notes (by std::string) of this SBML object to a copy of notes.
 */
void
SBase::setNotes(const std::string& notes)
{
  if (notes.empty())
  {
    unsetNotes();
    return;
  }

  XMLNode* notes_xmln = XMLNode::convertStringToXMLNode(notes);
  if(notes_xmln)
  {
    setNotes(notes_xmln);
    delete notes_xmln;
  }
}


/**
 * Appends notes to the existing notes.
 * This allows other notes to be preserved whilst
 * adding additional information.
 */
void 
SBase::appendNotes(const XMLNode* notes)
{
  if(!notes) return;

  const string&  name = notes->getName();

  if (mNotes != 0)
  {
    XMLNode *newNotes = new XMLNode();
    /* check for notes tags on the added notes and strip if present*/
    if (name == "notes")
    {
      int num_children = notes->getNumChildren();
      for(int i=0; i < num_children; i++)
      {
        newNotes->addChild(notes->getChild(i));
      }
    }
    else
    {
      newNotes= notes->clone();
    }


    /*
     * BUT we also have the issue of the rules relating to notes
     * contents and where to add them ie we cannot add a second body element
     * etc...
     */
    XMLToken notesToken = XMLToken(XMLTriple("notes", "", ""), XMLAttributes());

    XMLNode* newHTMLTag=NULL;
    XMLNode* newHeadTag=NULL;
    XMLNode* newBodyTag=NULL;
    unsigned int i;

    if (mNotes->getNumChildren() == 1 
      && mNotes->getChild(0).getName() == "html")
    {
      XMLNode* NotesTag = new XMLNode(notesToken);
      XMLNode* HTMLTag = new XMLNode(mNotes->getChild(0));
      HTMLTag->removeChildren();
      XMLNode* BodyTag = new XMLNode(mNotes->getChild(0).getChild(1));
      XMLNode* HeadTag = new XMLNode(mNotes->getChild(0).getChild(0));

      if (newNotes->getNumChildren() == 2 
        && newNotes->getName() == "html")
      {
        // add head and body from new notes to existing head and body
        newHeadTag = new XMLNode(newNotes->getChild(0));
        newBodyTag = new XMLNode(newNotes->getChild(1));

        for(i = 0; i < newHeadTag->getNumChildren(); i++)
        {
          HeadTag->addChild(*newHeadTag->getChild(i).clone());
        }
        for(i = 0; i < newBodyTag->getNumChildren(); i++)
        {
          BodyTag->addChild(*newBodyTag->getChild(i).clone());
        }
        
        HTMLTag->addChild(*HeadTag);
        HTMLTag->addChild(*BodyTag);
        NotesTag->addChild(*HTMLTag);
        delete newHeadTag;
        delete newBodyTag;
        setNotes(NotesTag);
      }
      else if (newNotes->getName() == "body")
      {
        // add contents of new body to existing body
        newBodyTag = new XMLNode(*newNotes);
        for(i = 0; i < newBodyTag->getNumChildren(); i++)
        {
          BodyTag->addChild(*newBodyTag->getChild(i).clone());
        }
 
        HTMLTag->addChild(*HeadTag);
        HTMLTag->addChild(*BodyTag);
        NotesTag->addChild(*HTMLTag);
        delete newBodyTag;
        setNotes(NotesTag);
      }
      else
      {
        // add new notes to body of existing

        for(i = 0; i < newNotes->getNumChildren(); i++)
        {
          BodyTag->addChild(*newNotes->getChild(i).clone());
        }

        HTMLTag->addChild(*HeadTag);
        HTMLTag->addChild(*BodyTag);
        NotesTag->addChild(*HTMLTag);
        setNotes(NotesTag);
      }

      delete HTMLTag;
      delete BodyTag;
      delete HeadTag;

    }

    else if (mNotes->getNumChildren() == 1 
      && mNotes->getChild(0).getName() == "body")
    {
      XMLNode* NotesTag = new XMLNode(notesToken);

      if (newNotes->getNumChildren() == 2 
        && newNotes->getName() == "html")
      {
        /* in this case the original doesnt have a html tag
         * so we need to add one
         */
        XMLNode* HTMLTag = new XMLNode(*newNotes);
        HTMLTag->removeChildren();
        XMLNode* BodyTag = new XMLNode(newNotes->getChild(1));
        XMLNode* HeadTag = new XMLNode(newNotes->getChild(0));
        
        // add body from existing notes to newly created body
        newBodyTag = new XMLNode(mNotes->getChild(0));

        for(i = 0; i < newBodyTag->getNumChildren(); i++)
        {
          BodyTag->addChild(*newBodyTag->getChild(i).clone());
        }
        
        HTMLTag->addChild(*HeadTag);
        HTMLTag->addChild(*BodyTag);
        NotesTag->addChild(*HTMLTag);
        delete HTMLTag;
        delete BodyTag;
        delete HeadTag;
        delete newBodyTag;
        setNotes(NotesTag);
      }
      else if (newNotes->getName() == "body")
      {
        XMLNode* BodyTag = new XMLNode(mNotes->getChild(0));
        // add contents of new body to existing body
        newBodyTag = new XMLNode(*newNotes);
        for(i = 0; i < newBodyTag->getNumChildren(); i++)
        {
          BodyTag->addChild(*newBodyTag->getChild(i).clone());
        }
        
        NotesTag->addChild(*BodyTag);
        delete BodyTag;
        delete newBodyTag;
        setNotes(NotesTag);
      }
      else
      {
        // add new notes to body of existing
        XMLNode* BodyTag = new XMLNode(mNotes->getChild(0));
        
        /* TO DO: first "child" is not returned properly from 
         * convertStringToXMLNode
         */
        for(i = 0; i < newNotes->getNumChildren(); i++)
        {
          BodyTag->addChild(*newNotes->getChild(i).clone());
        }
        NotesTag->addChild(*BodyTag);
        delete BodyTag;
        setNotes(NotesTag);
      }
    }

    else
    {

      if (newNotes->getNumChildren() == 2 
        && newNotes->getName() == "html")
      {
        /* in this case the original doesnt have a html tag
         * so we need to add one
         */
        XMLNode* NotesTag = new XMLNode(notesToken);
        XMLNode* HTMLTag = new XMLNode(*newNotes);
        HTMLTag->removeChildren();
        XMLNode* BodyTag = new XMLNode(newNotes->getChild(1));
        XMLNode* HeadTag = new XMLNode(newNotes->getChild(0));
        
        for(i = 0; i < mNotes->getNumChildren(); i++)
        {
          BodyTag->addChild(*mNotes->getChild(i).clone());
        }
        
        HTMLTag->addChild(*HeadTag);
        HTMLTag->addChild(*BodyTag);
        NotesTag->addChild(*HTMLTag);
        delete HTMLTag;
        delete BodyTag;
        delete HeadTag;
        setNotes(NotesTag);
      }
      else if (newNotes->getName() == "body")
      {
        /* in this case the original doesnt have a body tag
         * so we need to add one
         */
        XMLNode* NotesTag = new XMLNode(notesToken);
        XMLNode* BodyTag = new XMLNode(*newNotes);
        for(i = 0; i < mNotes->getNumChildren(); i++)
        {
          BodyTag->addChild(*mNotes->getChild(i).clone());
        }
        
        NotesTag->addChild(*BodyTag);
        delete BodyTag;
        setNotes(NotesTag);
      }
      else
      {
        /* TO DO: first "child" is not returned properly from 
         * convertStringToXMLNode
         */
        XMLNode* NotesTag = new XMLNode(*mNotes);

        for(i = 0; i < newNotes->getNumChildren(); i++)
        {
          NotesTag->addChild(*newNotes->getChild(i).clone());
        }
        setNotes(NotesTag);
      }
    }

    delete newNotes;
  }
  else
  {
    /* check for notes tags and add if necessary */

    if (name == "notes")
    {
      setNotes(notes);
    }
    else
    {
      XMLToken ann_t = XMLToken(XMLTriple("notes", "", ""), XMLAttributes());
      XMLNode * ann = new XMLNode(ann_t);
      ann->addChild(*notes);
      setNotes(ann);
    }
  }

}

/**
 * Appends notes (by string) to the existing notes.
 * This allows other notes to be preserved whilst
 * adding additional information.
 */
void
SBase::appendNotes(const std::string& notes)
{
  XMLNode* notes_xmln = XMLNode::convertStringToXMLNode(notes);
  if(notes_xmln)
  {
    appendNotes(notes_xmln);
    delete notes_xmln;
  }
}


/**
 * Sets the parent SBMLDocument of this SBML object.
 */
void
SBase::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
}


/**
 * Sets the sboTerm field to value.
 */
void
SBase::setSBOTerm (int value)
{
  mSBOTerm = value;
}


/**
 * Unsets the metaid of this SBML object.
 */
void
SBase::unsetMetaId ()
{
  mMetaId.erase();
}


/**
 * Unsets the id of this SBML object.
 */
void
SBase::unsetId ()
{
  mId.erase();
}


/**
 * Unsets the name of this SBML object.
 */
void
SBase::unsetName ()
{
  if (getLevel() == 1) mId.erase();
  else mName.erase();
}


/**
 * Unsets the notes of this SBML object.
 */
void
SBase::unsetNotes ()
{
  delete mNotes;
  mNotes = 0;
}

/**
 * Unsets the annotation of this SBML object.
 */
void
SBase::unsetAnnotation ()
{
  XMLNode* empty = NULL;
  setAnnotation(empty);
}


/**
 * Unsets the sboTerm of this SBML object.
 */
void
SBase::unsetSBOTerm ()
{
  mSBOTerm = -1;
}


/**
 * Adds a copy of the given CVTerm to this SBML object.
 */
void
SBase::addCVTerm(CVTerm * term)
{
  unsigned int added = 0;
  if (mCVTerms == NULL)
  {
    mCVTerms = new List();
    mCVTerms->add((void *) term->clone());
  }
  else
  {
    /* check whether there are any other qualifiers of the same sort already in the list */
    QualifierType_t type = term->getQualifierType();
    if (type == BIOLOGICAL_QUALIFIER)
    {
      BiolQualifierType_t biol = term->getBiologicalQualifierType();
      
      for (unsigned int n = 0; n < mCVTerms->getSize() && added == 0; n++)
      {
        if (biol == static_cast <CVTerm *>(mCVTerms->get(n))->getBiologicalQualifierType())
        {
          for (int r = 0; r < term->getResources()->getLength(); r++)
          {
            static_cast <CVTerm *>(mCVTerms->get(n))->addResource(
              term->getResources()->getValue(r));
          }
          added = 1;
        }
      }
    }
    else if (type == MODEL_QUALIFIER)
    {
      ModelQualifierType_t model = term->getModelQualifierType();
      
      for (unsigned int n = 0; n < mCVTerms->getSize() && added == 0; n++)
      {
        if (model == static_cast <CVTerm *>(mCVTerms->get(n))->getModelQualifierType())
        {
          for (int r = 0; r < term->getResources()->getLength(); r++)
          {
            static_cast <CVTerm *>(mCVTerms->get(n))->addResource(
              term->getResources()->getValue(r));
          }
          added = 1;
        }
      }
    }
    if (added == 0)
    {
      /* no matching terms already in list */
      mCVTerms->add((void *) term->clone());
    }

  }
}


/**
 * @return the list of CVTerms for this SBML object.
 */
List*
SBase::getCVTerms()
{
  return mCVTerms;
}


/**
 * @return the list of CVTerms for this SBML object.
 */
List*
SBase::getCVTerms() const
{
  return mCVTerms;
}

/**
 * Returns the number of CVTerm objects in the annotations of this SBML
 * object.
 * 
 * @return the number of CVTerms for this SBML object.
 */
unsigned int 
SBase::getNumCVTerms()
{
  if (mCVTerms)
  {
    return mCVTerms->getSize();
  }
  else
  {
    return 0;
  }
}


/**
 * Returns the nth CVTerm in the list of CVTerms of this SBML
 * object.
 * 
 * @param n unsigned int the index of the CVTerm to retrieve
 *
 * @return the nth CVTerm in the list of CVTerms for this SBML object.
 */
CVTerm* 
SBase::getCVTerm(unsigned int n)
{
  return static_cast <CVTerm*> (mCVTerms->get(n));
}


/**
 * @return the parent Model of this SBML object.
 */
const Model*
SBase::getModel () const
{
  return (mSBML != 0) ? mSBML->getModel() : 0;
}


/**
 * @return the SBML level of this SBML object.
 */
unsigned int
SBase::getLevel () const
{
  return (mSBML) ? mSBML->mLevel : SBMLDocument::getDefaultLevel();
}


/**
 * @return the SBML version of this SBML object.
 */
unsigned int
SBase::getVersion () const
{
  return (mSBML) ? mSBML->mVersion : SBMLDocument::getDefaultVersion();
}


/**
  * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
  * (default).
  *
  * This method MAY return the typecode of this SBML object or it MAY
  * return SBML_UNKNOWN.  That is, subclasses of SBase are not required to
  * implement this method to return a typecode.  This method is meant
  * primarily for the LibSBML C interface where class and subclass
  * information is not readily available.
  *
  * @see getElementName()
  */
SBMLTypeCode_t
SBase::getTypeCode () const
{
  return SBML_UNKNOWN;
}


/**
 * @return the partial SBML that describes this SBML object.
 */
char*
SBase::toSBML ()
{
  ostringstream    os;
  XMLOutputStream  stream(os, "UTF-8", false);

  write(stream);

  return safe_strdup( os.str().c_str() );
}


/** @cond doxygen-libsbml-internal */
/**
 * Reads (initializes) this SBML object by reading from XMLInputStream.
 */
void
SBase::read (XMLInputStream& stream)
{
  if ( !stream.peek().isStart() ) return;

  const XMLToken  element  = stream.next();
  int             position =  0;

  setSBaseFields( element );
  readAttributes( element.getAttributes() );

  if ( element.isEnd() ) return;

  while ( stream.isGood() )
  {
    stream.skipText();
    const XMLToken& next = stream.peek();

    // Re-check stream.isGood() because stream.peek() could hit something.
    if ( !stream.isGood() ) break;

    if ( next.isEndFor(element) )
    {
      stream.next();
      break;
    }
    else if ( next.isStart() )
    {
      SBase * object = createObject(stream);

      if (object)
      {
        checkOrderAndLogError(object, position);
        position = object->getElementPosition();

        object->mSBML = mSBML;
        object->read(stream);

        if ( !stream.isGood() ) break;

        if (object->getTypeCode() == SBML_SPECIES_REFERENCE 
            && object->getLevel() == 2)
        {
          static_cast <SpeciesReference *> (object)->sortMath();
        }
        checkListOfPopulated(object);
      }
      else if ( !( readOtherXML(stream)
                   || readAnnotation(stream)
                   || readNotes(stream) ))
      {
        logError( SBMLError::UnrecognizedElement, getLevel(), getVersion(),
                  "Unrecognized element '" + next.getName() + "'");
        stream.skipPastEnd( stream.next() );
      }
    }
    else
    {
      stream.skipPastEnd( stream.next() );
    }
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Writes (serializes) this SBML object by writing it to XMLOutputStream.
 */
void
SBase::write (XMLOutputStream& stream) const
{
  stream.startElement( getElementName() );

  writeAttributes( stream );
  writeElements  ( stream );

  stream.endElement( getElementName() );
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
SBase::writeElements (XMLOutputStream& stream) const
{
  if ( mNotes      ) stream << *mNotes;

  /*
   * NOTE: CVTerms on a model have already been dealt with
   */

  const_cast <SBase *> (this)->syncAnnotation();
  if (mAnnotation) stream << *mAnnotation;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to create, store, and then
 * return an SBML object corresponding to the next XMLToken in the
 * XMLInputStream.
 *
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
SBase::createObject (XMLInputStream&)
{
  return 0;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
SBase::readOtherXML (XMLInputStream&)
{
  return false;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * @return true if read an <annotation> element from the stream
 */
bool
SBase::readAnnotation (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();

  if (name == "annotation")
  {
//    XMLNode* new_annotation = NULL;
    // If this is a level 1 document then annotations are not allowed on
    // the sbml container
    if (getLevel() == 1 && getTypeCode() == SBML_DOCUMENT)
    {
      logError(SBMLError::AnnotationNotesNotAllowedLevel1);
    }


    // If an annotation already exists, log it as an error and replace
    // the content of the existing annotation with the new one.

    if (mAnnotation)
    {
      logError(SBMLError::NotSchemaConformant, getLevel(), getVersion(),
             "Multiple annotation elements not permitted on the same element");
    }

    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    if(mCVTerms)
    {
      unsigned int size = mCVTerms->getSize();
      while (size--) delete static_cast<CVTerm*>( mCVTerms->remove(0) );
      delete mCVTerms; 
    }
    mCVTerms = new List();
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
//    new_annotation = RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
//    delete mAnnotation;
//    mAnnotation = new_annotation;
    return true;
  }

  return false;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * @return true if read a <notes> element from the stream
 */
bool
SBase::readNotes (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();

  if (name == "notes")
  {
    // If this is a level 1 document then notes are not allowed on
    // the sbml container
    if (getLevel() == 1 && getTypeCode() == SBML_DOCUMENT)
    {
      logError(SBMLError::AnnotationNotesNotAllowedLevel1);
    }

    // If a notes element already exists, then it is an error.
    // If an annotation element already exists, then the ordering is wrong.
    // In either case, replace existing content with the new notes read.

    if (mNotes)
    {
      logError(SBMLError::NotSchemaConformant, getLevel(), getVersion(),
               "Multiple notes elements not permitted on the same element");
    }
    else if (mAnnotation)
    {
      logError(SBMLError::NotSchemaConformant, getLevel(), getVersion(),
               "Incorrect ordering of annotation and notes elements");
    }

    delete mNotes;
    mNotes = new XMLNode(stream);
    checkXHTML(mNotes);
    return true;
  }

  return false;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
SBase::getElementPosition () const
{
  return -1;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * @return the SBMLErrorLog used to log errors during while reading and
 * validating SBML.
 */
SBMLErrorLog*
SBase::getErrorLog ()
{
  return (mSBML != 0) ? mSBML->getErrorLog() : 0;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Convenience method for easily logging problems from within method
 * implementations.
 *
 * This is essentially a short form of getErrorLog()->logError(...)
 */
void
SBase::logError (  unsigned int       id
                 , const unsigned int level
                 , const unsigned int version
                 , const std::string& details )
{
  if ( SBase::getErrorLog() ) 
    getErrorLog()->logError(id, getLevel(), getVersion(), details);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SBase::readAttributes (const XMLAttributes& attributes)
{
  const_cast<XMLAttributes&>(attributes).setErrorLog(getErrorLog());

  attributes.readInto("metaid", mMetaId);
  /*
   * at present Xerces on Windows does not correctly read multibyte characters
   * so we exclude this check
   */
  if (isSetMetaId())
    checkMetaIdSyntax();

}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SBase::writeAttributes (XMLOutputStream& stream) const
{
  if (mNamespaces) stream << *mNamespaces;

  if ( getLevel() == 2 && !mMetaId.empty() )
  {
    stream.writeAttribute("metaid", mMetaId);
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Synchronizes the annotation of this SBML object. 
 */
void
SBase::syncAnnotation ()
{
  if (this->getTypeCode() != SBML_MODEL)
  {
    if(mAnnotation)
    {
      XMLNode* new_annotation = RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
      if(!new_annotation)
      {
         XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());
         new_annotation = new XMLNode(ann_token);
         new_annotation->addChild(*mAnnotation);
      }
      delete mAnnotation;
      mAnnotation = new_annotation;
    }
  }

  XMLNode * cvTerms = RDFAnnotationParser::parseCVTerms(this);

  if (cvTerms)
  {
    if (!mAnnotation)
    {
      mAnnotation = cvTerms;
    }
    else
    {
      if (mAnnotation->isEnd())
      {
        mAnnotation->unsetEnd();
      }
      mAnnotation->addChild(cvTerms->getChild(0));
    }
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Checks that SBML element has been read in the proper order.  If object
 * is not in the expected position, an error is logged.
 */
void
SBase::checkOrderAndLogError (SBase* object, int expected)
{
  int actual = object->getElementPosition();

  if (actual != -1 && actual < expected)
  {
    SBMLError::SBMLCode error = SBMLError::IncorrectOrderInModel;

    if (object->getTypeCode() == SBML_LIST_OF)
    {
      SBMLTypeCode_t tc = static_cast<ListOf*>(object)->getItemTypeCode();

      if (tc == SBML_SPECIES_REFERENCE || tc == SBML_MODIFIER_SPECIES_REFERENCE)
      {
        error = SBMLError::IncorrectOrderInReaction;
      }
    }
    else if (object->getTypeCode() == SBML_TRIGGER)
    {
      error = SBMLError::IncorrectOrderInEvent;
    }

    logError(error, getLevel(), getVersion());
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
  * Checks that an SBML ListOf element has been populated.  
  * If a listOf element has been declared with no elements, 
  * an error is logged.
  */
void 
SBase::checkListOfPopulated(SBase* object)
{
  if (object->getTypeCode() == SBML_LIST_OF)
  {
    // Check that the list has at least one element.
    if (static_cast <ListOf*> (object)->size() == 0)
    {
      SBMLTypeCode_t tc = static_cast<ListOf*>(object)->getItemTypeCode();
      SBMLError::SBMLCode error = SBMLError::EmptyListElement;

      // By default, the error will be the EmptyListElement error, unless
      // we have a special case for which SBML has a separate error code.
      switch (tc)
      {
      case SBML_UNIT:
        error = SBMLError::EmptyListOfUnits;
        break;

      case SBML_SPECIES_REFERENCE:
      case SBML_MODIFIER_SPECIES_REFERENCE:
        error = SBMLError::EmptyListInReaction;
        break;

      case SBML_PARAMETER:
        // If listOfParameters is inside a KineticLaw, we have a separate code.
        if (this->getTypeCode() == SBML_KINETIC_LAW)
        {
          error = SBMLError::EmptyListInKineticLaw;
        }
        break;

      default:;
      }

      logError(error, getLevel(), getVersion());
    }
  }
  else if (object->getTypeCode() == SBML_KINETIC_LAW)
  {
    /* 
     * if nothing has been set in the kineticLaw we assume its is empty
     */
    if (static_cast <KineticLaw *> (object)->isSetMath()           == 0  &&
        static_cast <KineticLaw *> (object)->isSetFormula()        == 0  &&
        static_cast <KineticLaw *> (object)->isSetTimeUnits()      == 0  &&
        static_cast <KineticLaw *> (object)->isSetSubstanceUnits() == 0  &&
        static_cast <KineticLaw *> (object)->isSetSBOTerm()        == 0  &&
        static_cast <KineticLaw *> (object)->getNumParameters()    == 0)
    {
      logError(SBMLError::EmptyListInReaction, getLevel(), getVersion());
    }
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
  * Checks the syntax of a metaid attribute.
  * The syntax of a metaid is XML 1.0 type ID. The literal representation of 
  * this type consists of strings of characters restricted to:
  *
  *  - NCNameChar ::= letter | digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
  *  - ID ::= ( letter | '_' | ':' ) NCNameChar*
  *
  * If the syntax of the metaid attribute of this object is incorrect, 
  * an error is logged
  */
void 
SBase::checkMetaIdSyntax()
{
  string& metaid = getMetaId();
  string::iterator it = metaid.begin();
 
  // first character must be a letter or '_' or ':'
  unsigned char c = *it;
  bool okay;

  if (c < 0x80)
  {
    okay = (isUnicodeLetter(it, 1) || c == '_' || c == ':');
    it++;
  }
  else if (c >> 5 == 0x6)
  {
    okay = (isUnicodeLetter(it, 2));
    it++;
    it++;
  }
  else if (c >> 4 == 0xe)
  {
    okay = (isUnicodeLetter(it, 3));
    it++;
    it++;
    it++;
  }
  else
  {
    okay = false;
    it++;
  }


  // remaining chars must be 
  // letter | digit | '.' | '-' | ' ' | ':' | CombiningChar | Extender
  while (okay && it < metaid.end())
  {
    c = *it;
    
    // need to find multibyte sequences
    if (c < 0x80)
    {
      okay = (
          isUnicodeLetter(it, 1)  ||
          isUnicodeDigit(it, 1)   ||
            c == '.'              ||
            c == '-'              ||
            c == '_'              ||
            c == ':'              );
    }
    else if (c >> 5 == 0x6)
    {
      okay = (
          isUnicodeLetter(it, 2)  ||
          isUnicodeDigit(it, 2)   ||
          isCombiningChar(it, 2)  ||
          isExtender(it, 2)       );
      it++;
    }
    else if (c >> 4 == 0xe)
    {
      okay = (
          isUnicodeLetter(it, 3)  ||
          isUnicodeDigit(it, 3)   ||
          isCombiningChar(it, 3)  ||
          isExtender(it, 3)       );
      it++;
      it++;
    }
    else if (c >> 3 == 0x1e)
    {
      okay = (
          isUnicodeLetter(it, 4)  ||
          isUnicodeDigit(it, 4)   ||
          isCombiningChar(it, 4)  ||
          isExtender(it, 4)       );
      it += 3;
    }

    it++;

  }

  if (!okay) logError(SBMLError::InvalidMetaidSyntax, getLevel(), getVersion());
}
/** @endcond doxygen-libsbml-internal */

  
/** @cond doxygen-libsbml-internal */
/**
  * Checks the syntax of the id attribute.
  * The syntax of an id is of type SId which is defined as:
  *
  *  - letter ::= 'a'..'z','A'..'Z'
  *  - digit  ::= '0'..'9'
  *  - idChar ::= letter | digit | '_'
  *  - SId    ::= ( letter | '_' ) idChar*
  *
  * If the syntax of the id attribute of this object is incorrect, 
  * an error is logged
  */
void 
SBase::checkIdSyntax()
{
  string& id        = const_cast<string &> (getId());
  unsigned int size = id.size();

  if (size == 0)
  {
    // Identifiers are not required on the following objects, so it's ok
 //   // if they're zero-length.

 //   if (getTypeCode() == SBML_MODEL
        //|| getTypeCode() == SBML_ALGEBRAIC_RULE
        //|| getTypeCode() == SBML_EVENT
        //|| getTypeCode() == SBML_MODIFIER_SPECIES_REFERENCE
        //|| getTypeCode() == SBML_SPECIES_REFERENCE)
 //   {
      return;
 //   }
 //   else
 //   {
 //     // This is a schema validation error: no id on an object that needs it.
 //     logError(SBMLError::NotSchemaConformant, getLevel(), getVersion(),
        //       "Missing 'id' on an element that requires an identifier");
 //     return;
 //   }
  }

  unsigned int n = 0;

  char c = id[n];
  bool okay = (isalpha(c) || (c == '_'));
  n++;

  while (okay && n < size)
  {
    c = id[n];
    okay = (isalnum(c) || c == '_');
    n++;
  }

  if (!okay) logError(SBMLError::InvalidIdSyntax);
}


/**
  * Checks the syntax of the unit attribute.
  * The syntax of an unit is of type UnitSId which is defined as:
  *
  *  - letter ::= 'a'..'z','A'..'Z'
  *  - digit  ::= '0'..'9'
  *  - idChar ::= letter | digit | '_'
  *  - UnitSId    ::= ( letter | '_' ) idChar*
  *
  * If the syntax of the unit attribute of this object is incorrect, 
  * an error is logged
  */
void 
SBase::checkUnitSyntax(unsigned int flag)
{
  std::string units = "";
  if (getTypeCode() == SBML_SPECIES)
  {
    if (flag != 0)
      units = static_cast <Species*> (this)->getSpatialSizeUnits();
    else
      units = static_cast <Species*> (this)->getUnits();
  }

  else if (getTypeCode() == SBML_EVENT)
  {
    units = static_cast <Event*> (this)->getTimeUnits();
  }
  else if (getTypeCode() == SBML_COMPARTMENT)
  {
    units = static_cast <Compartment*> (this)->getUnits();
  }
  else if (getTypeCode() == SBML_PARAMETER)
  {
    units = static_cast <Parameter*> (this)->getUnits();
  }
  else
  {
    units  = "";
  }

  unsigned int size = units.size();

  if (size == 0)
  {
      return;
  }

  unsigned int n = 0;

  char c = units[n];
  bool okay = (isalpha(c) || (c == '_'));
  n++;

  while (okay && n < size)
  {
    units = units[n];
    okay = (isalnum(c) || c == '_');
    n++;
  }

  if (!okay) logError(SBMLError::InvalidUnitIdSyntax);

}


/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
  * Checks the annotation does not declare an sbml namespace.
  * If the annotation declares an sbml namespace an error is logged.
  */
void
SBase::checkAnnotation()
{
  unsigned int nNodes = 0;
  unsigned int match = 0;
  int n = 0;
  while (nNodes < mAnnotation->getNumChildren())
  {
    XMLNode topLevel = mAnnotation->getChild(nNodes);

    match = 0;
    n = 0;
    while(!match && n < topLevel.getNamespaces().getLength())
    {
      match += !strcmp(topLevel.getNamespaces().getURI(n).c_str(), 
                                          "http://www.sbml.org/sbml/level1");
      match += !strcmp(topLevel.getNamespaces().getURI(n).c_str(), 
                                          "http://www.sbml.org/sbml/level2");
      match += !strcmp(topLevel.getNamespaces().getURI(n).c_str(), 
                                "http://www.sbml.org/sbml/level2/version2");
      match += !strcmp(topLevel.getNamespaces().getURI(n).c_str(), 
                                "http://www.sbml.org/sbml/level2/version3");
      n++;
    }
    if (match > 0)
    {
      logError(SBMLError::SBMLNamespaceInAnnotation);
      break;
    }
    nNodes++;
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Checks that the XHTML is valid.
 * If the xhtml does not conform to the specification of valid xhtml within
 * an sbml document, an error is logged.
 */
void
SBase::checkXHTML(const XMLNode * xhtml)
{
  const string&  name = xhtml->getName();
  unsigned int i, errorNS, errorXML, errorDOC, errorELEM;
  int n;

  if (name == "notes")
  {
    errorNS   = SBMLError::NotesNotInXHTMLNamespace;
    errorXML  = SBMLError::NotesContainsXMLDecl;
    errorDOC  = SBMLError::NotesContainsDOCTYPE;
    errorELEM = SBMLError::InvalidNotesContent;
  }
  else if (name == "message")
  {
    errorNS   = SBMLError::ConstraintNotInXHTMLNamespace;
    errorXML  = SBMLError::ConstraintContainsXMLDecl;
    errorDOC  = SBMLError::ConstraintContainsDOCTYPE;
    errorELEM = SBMLError::InvalidConstraintContent;
  }
  else                                  // We shouldn't ever get to this point.
  {
    logError(SBMLError::UnknownError);
    return;
  }

  /*
   * errors relating to a misplaced XML or DOCTYPE declaration 
   * will also cause a parser error.
   * since parsing will terminate at this error, then if it has occurred
   * it will be in the XML currently being checked and so a more
   * informative message can be added
   */
  for (i = 0; i < getErrorLog()->getNumErrors(); i++)
  {
    if (getErrorLog()->getError(i)->getErrorId() == XMLError::BadXMLDeclLocation)
    {
      logError(errorXML);
    }
    if (getErrorLog()->getError(i)->getErrorId() == XMLError::NotWellFormed)
    {
      logError(errorDOC);
    }
  }

  /*
   * namespace declaration is variable
   * if a whole html tag has been used
   * or a whole body tag then namespace can be implicitly declared
   *
   * HOWEVER if more than one permitted elements have been used 
   * each MUST explicitly declare the namespace
   */
  bool implicitNSdecl = false;
  if( mSBML->getNamespaces() != NULL)
  /* check for implicit declaration */
  {
    for (n = 0; n < mSBML->getNamespaces()->getLength(); n++)
    {
      if (!strcmp(mSBML->getNamespaces()->getURI(n).c_str(), 
                                         "http://www.w3.org/1999/xhtml"))
      {
        implicitNSdecl = true;
        break;
      }
    }
  }

  unsigned int children = xhtml->getNumChildren();
  static const int size = sizeof(XHTML_ELEMENTS) / sizeof(XHTML_ELEMENTS[0]);

  int index;
  bool found;
  bool match;
  if (children > 1)
  {
    /* each element must declare namespace */
    for (i=0; i < children; i++)
    {
      const char * top = xhtml->getChild(i).getName().c_str();

      index = util_bsearchStringsI(XHTML_ELEMENTS, top, 0, size - 1);
      found = (index < size);

      if (!found)
      {
        logError(errorELEM);
      }
      else
      {
        const XMLToken elem = xhtml->getChild(i);
        match = false;
        for (n = 0; n < elem.getNamespaces().getLength(); n++)
        {
          if (!strcmp(elem.getNamespaces().getURI(n).c_str(), 
                                               "http://www.w3.org/1999/xhtml"))
          {
            match = true;
            break;
          }
        }
        if (!match)
        {
          logError(errorELEM);
        }
      }
    }

  }
  else
  {
    /* only one element which can be html or body with either implicit/explicit
     * namespace declaration
     * OR could be one of the listed elements.
     */

    const XMLToken top_elem = xhtml->getChild(0);
    const string& top_name = top_elem.getName();
    const char* top_name_c = top_name.c_str();
    index = util_bsearchStringsI(XHTML_ELEMENTS, top_name_c, 0, size - 1);
    found = (index < size);
  

    if (top_name != "html" && top_name != "body" && !found)
    {
      logError(errorELEM);
    }
    else
    {
      match = false;
      for (n = 0; n < top_elem.getNamespaces().getLength(); n++)
      {
        if (!strcmp(top_elem.getNamespaces().getURI(n).c_str(), 
                                              "http://www.w3.org/1999/xhtml"))
        {
          match = true;
          break;
        }
      }
      if (!implicitNSdecl && !match)
      {
        logError(errorNS);
      }
    }
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
  * Checks if a character is part of the Unicode Letter set.
  * @return true if the character is a part of the set, false otherwise.
  */
bool 
SBase::isUnicodeLetter(std::string::iterator it, unsigned int numBytes)
{
  /*
  * Letter ::=  BaseChar | Ideographic 
  * BaseChar ::=  [#x0041-#x005A] | [#x0061-#x007A] | [#x00C0-#x00D6] | 
  * [#x00D8-#x00F6] | [#x00F8-#x00FF] | [#x0100-#x0131] | [#x0134-#x013E] | 
  * [#x0141-#x0148] | [#x014A-#x017E] | [#x0180-#x01C3] | [#x01CD-#x01F0] | 
  * [#x01F4-#x01F5] | [#x01FA-#x0217] | [#x0250-#x02A8] | [#x02BB-#x02C1] | 
  * #x0386 | [#x0388-#x038A] | #x038C | [#x038E-#x03A1] | [#x03A3-#x03CE] | 
  * [#x03D0-#x03D6] | #x03DA | #x03DC | #x03DE | #x03E0 | [#x03E2-#x03F3] | 
  * [#x0401-#x040C] | [#x040E-#x044F] | [#x0451-#x045C] | [#x045E-#x0481] | 
  * [#x0490-#x04C4] | [#x04C7-#x04C8] | [#x04CB-#x04CC] | [#x04D0-#x04EB] | 
  * [#x04EE-#x04F5] | [#x04F8-#x04F9] | [#x0531-#x0556] | #x0559 | 
  * [#x0561-#x0586] | [#x05D0-#x05EA] | [#x05F0-#x05F2] | [#x0621-#x063A] | 
  * [#x0641-#x064A] | [#x0671-#x06B7] | [#x06BA-#x06BE] | [#x06C0-#x06CE] | 
  * [#x06D0-#x06D3] | #x06D5 | [#x06E5-#x06E6] | [#x0905-#x0939] | #x093D | 
  * [#x0958-#x0961] | [#x0985-#x098C] | [#x098F-#x0990] | [#x0993-#x09A8] | 
  * [#x09AA-#x09B0] | #x09B2 | [#x09B6-#x09B9] | [#x09DC-#x09DD] | 
  * [#x09DF-#x09E1] | [#x09F0-#x09F1] | [#x0A05-#x0A0A] | [#x0A0F-#x0A10] | 
  * [#x0A13-#x0A28] | [#x0A2A-#x0A30] | [#x0A32-#x0A33] | [#x0A35-#x0A36] | 
  * [#x0A38-#x0A39] | [#x0A59-#x0A5C] | #x0A5E | [#x0A72-#x0A74] | 
  * [#x0A85-#x0A8B] | #x0A8D | [#x0A8F-#x0A91] | [#x0A93-#x0AA8] | 
  * [#x0AAA-#x0AB0] | [#x0AB2-#x0AB3] | [#x0AB5-#x0AB9] | #x0ABD | #x0AE0 | 
  * [#x0B05-#x0B0C] | [#x0B0F-#x0B10] | [#x0B13-#x0B28] | [#x0B2A-#x0B30] | 
  * [#x0B32-#x0B33] | [#x0B36-#x0B39] | #x0B3D | [#x0B5C-#x0B5D] | 
  * [#x0B5F-#x0B61] | [#x0B85-#x0B8A] | [#x0B8E-#x0B90] | [#x0B92-#x0B95] | 
  * [#x0B99-#x0B9A] | #x0B9C | [#x0B9E-#x0B9F] | [#x0BA3-#x0BA4] | 
  * [#x0BA8-#x0BAA] | [#x0BAE-#x0BB5] | [#x0BB7-#x0BB9] | [#x0C05-#x0C0C] | 
  * [#x0C0E-#x0C10] | [#x0C12-#x0C28] | [#x0C2A-#x0C33] | [#x0C35-#x0C39] | 
  * [#x0C60-#x0C61] | [#x0C85-#x0C8C] | [#x0C8E-#x0C90] | [#x0C92-#x0CA8] | 
  * [#x0CAA-#x0CB3] | [#x0CB5-#x0CB9] | #x0CDE | [#x0CE0-#x0CE1] | 
  * [#x0D05-#x0D0C] | [#x0D0E-#x0D10] | [#x0D12-#x0D28] | [#x0D2A-#x0D39] | 
  * [#x0D60-#x0D61] | [#x0E01-#x0E2E] | #x0E30 | [#x0E32-#x0E33] | 
  * [#x0E40-#x0E45] | [#x0E81-#x0E82] | #x0E84 | [#x0E87-#x0E88] | #x0E8A | 
  * #x0E8D | [#x0E94-#x0E97] | [#x0E99-#x0E9F] | [#x0EA1-#x0EA3] | #x0EA5 | 
  * #x0EA7 | [#x0EAA-#x0EAB] | [#x0EAD-#x0EAE] | #x0EB0 | [#x0EB2-#x0EB3] | 
  * #x0EBD | [#x0EC0-#x0EC4] | [#x0F40-#x0F47] | [#x0F49-#x0F69] | 
  * [#x10A0-#x10C5] | [#x10D0-#x10F6] | #x1100 | [#x1102-#x1103] | 
  * [#x1105-#x1107] | #x1109 | [#x110B-#x110C] | [#x110E-#x1112] | #x113C | 
  * #x113E | #x1140 | #x114C | #x114E | #x1150 | [#x1154-#x1155] | #x1159 | 
  * [#x115F-#x1161] | #x1163 | #x1165 | #x1167 | #x1169 | [#x116D-#x116E] | 
  * [#x1172-#x1173] | #x1175 | #x119E | #x11A8 | #x11AB | [#x11AE-#x11AF] | 
  * [#x11B7-#x11B8] | #x11BA | [#x11BC-#x11C2] | #x11EB | #x11F0 | #x11F9 | 
  * [#x1E00-#x1E9B] | [#x1EA0-#x1EF9] | [#x1F00-#x1F15] | [#x1F18-#x1F1D] | 
  * [#x1F20-#x1F45] | [#x1F48-#x1F4D] | [#x1F50-#x1F57] | #x1F59 | #x1F5B | 
  * #x1F5D | [#x1F5F-#x1F7D] | [#x1F80-#x1FB4] | [#x1FB6-#x1FBC] | #x1FBE | 
  * [#x1FC2-#x1FC4] | [#x1FC6-#x1FCC] | [#x1FD0-#x1FD3] | [#x1FD6-#x1FDB] | 
  * [#x1FE0-#x1FEC] | [#x1FF2-#x1FF4] | [#x1FF6-#x1FFC] | #x2126 | 
  * [#x212A-#x212B] | #x212E | [#x2180-#x2182] | [#x3041-#x3094] | 
  * [#x30A1-#x30FA] | [#x3105-#x312C] | [#xAC00-#xD7A3]  
  * Ideographic ::=  [#x4E00-#x9FA5] | #x3007 | [#x3021-#x3029]
  */
  bool letter = false;


  unsigned char c1 = *it;
  unsigned char c2 ;/* = *(it+1); */
  unsigned char c3 ;/* = *(it+2); */
  
  switch (numBytes)
  {
  case 1:
    if (c1 >= 65 && c1 <= 90)
    {
      letter = true;
    }
    else if (c1 >= 97 && c1 <= 122)
    {
      letter = true;
    }
  break;
  case 2:
    c2 = *(it+1);
    switch (c1)
    {
      case 224:
        if ((128 <= c2 && 150 >= c2)
        ||  (152 <= c2 && 182 >= c2)
        ||  (184 <= c2 && 191 >= c2))
        {
          letter = true;
        }
      break;
      case 196:
        if ((128 <= c2 && 177 >= c2)
        ||  (180 <= c2 && 190 >= c2))
        {
          letter = true;
        }
      break;
      case 197:
        if ((129 <= c2 && 136 >= c2)
        ||  (138 <= c2 && 190 >= c2))
        {
          letter = true;
        }
      break;
      case 198:
        if ((128 <= c2 && 191 >= c2))
        {
          letter = true;
        }
      break;
      case 199:
        if ((128 <= c2 && 131 >= c2)
        ||  (141 <= c2 && 176 >= c2)
        ||  (180 <= c2 && 181 >= c2)
        ||  (186 <= c2 && 191 >= c2))
        {
          letter = true;
        }
      break;
      case 200:
        if ((128 <= c2 && 151 >= c2))
        {
          letter = true;
        }
      break;
      case 201:
        if ((144 <= c2 && 191 >= c2))
        {
          letter = true;
        }
      break;
      case 202:
        if ((128 <= c2 && 168 >= c2)
        ||  (187 <= c2 && 191 >= c2))
        {
          letter = true;
        }
      break;
      case 203:
        if ((128 <= c2 && 129 >= c2))
        {
          letter = true;
        }
      break;
      case 206:
        if ((c2 == 134)
        ||  (136 <= c2 && 138 >= c2)
        ||  (c2 == 140)
        ||  (142 <= c2 && 161 >= c2)
        ||  (163 <= c2 && 191 >= c2))
        {
          letter = true;
        }
      break;
      case 207:
        if ((128 <= c2 && 142 >= c2)
        ||  (144 <= c2 && 150 >= c2)
        ||  (c2 == 154)
        ||  (c2 == 158)
        ||  (c2 == 160)
        ||  (162 <= c2 && 179 >= c2))
        {
          letter = true;
        }
      break;
      case 208:
        if ((129 <= c2 && 140 >= c2)
        ||  (142 <= c2 && 191 >= c2))
        {
          letter = true;
        }
      break;
      case 209:
        if ((128 <= c2 && 143 >= c2)
        ||  (145 <= c2 && 156 >= c2)
        ||  (158 <= c2 && 191 >= c2))
        {
          letter = true;
        }
      break;
      case 210:
        if ((128 <= c2 && 129 >= c2)
        ||  (144 <= c2 && 191 >= c2))
        {
          letter = true;
        }
      break;
      case 211:
        if ((128 <= c2 && 132 >= c2)
        ||  (135 <= c2 && 136 >= c2)
        ||  (139 <= c2 && 140 >= c2)
        ||  (144 <= c2 && 171 >= c2)
        ||  (174 <= c2 && 181 >= c2)
        ||  (184 <= c2 && 185 >= c2))
        {
          letter = true;
        }
      break;
      case 212:
        if ((177 <= c2 && 191 >= c2))
        {
          letter = true;
        }
      break;
      case 213:
        if ((128 <= c2 && 150 >= c2)
        ||  (c2 == 153)
        ||  (161 <= c2 && 191 >= c2))
        {
          letter = true;
        }
      break;
      case 214:
        if ((128 <= c2 && 134 >= c2))
        {
          letter = true;
        }
      break;
      case 215:
        if ((144 <= c2 && 170 >= c2)
        ||  (176 <= c2 && 178 >= c2))
        {
          letter = true;
        }
      break;
      case 216:
        if ((161 <= c2 && 186 >= c2))
        {
          letter = true;
        }
      break;
      case 217:
        if ((129 <= c2 && 138 >= c2)
        ||  (177 <= c2 && 191 >= c2))
        {
          letter = true;
        }
      break;
      case 218:
        if ((128 <= c2 && 183 >= c2)
        ||  (186 <= c2 && 190 >= c2))
        {
          letter = true;
        }
      break;
      case 219:
        if ((128 <= c2 && 142 >= c2)
        ||  (144 <= c2 && 147 >= c2)
        ||  (c2 == 149)
        ||  (165 <= c2 && 166 >= c2))
        {
          letter = true;
        }
        break;
    }
    break;
case 3:
  c2 = *(it+1);
  c3 = *(it+2);
  switch (c1)
  {
    case 224:
      switch (c2)
      {
        case 164:
          if ((133 <= c3 && 185 >= c3)
          ||  (c3 == 189))
          {
            letter = true;
          }
          break;
        case 165:
          if ((152 <= c3 && 161 >= c3))
          {
            letter = true;
          }
          break;
        case 166:
          if ((133 <= c3 && 140 >= c3)
          ||  (143 <= c3 && 144 >= c3)
          ||  (147 <= c3 && 168 >= c3)
          ||  (170 <= c3 && 176 >= c3)
          ||  (c3 == 178)
          ||  (182 <= c3 && 185 >= c3))
          {
            letter = true;
          }
          break;
        case 167:
          if ((156 <= c3 && 157 >= c3)
          ||  (159 <= c3 && 161 >= c3)
          ||  (176 <= c3 && 177 >= c3))
          {
            letter = true;
          }
          break;
        case 168:
          if ((133 <= c3 && 138 >= c3)
          ||  (143 <= c3 && 144 >= c3)
          ||  (147 <= c3 && 168 >= c3)
          ||  (170 <= c3 && 176 >= c3)
          ||  (178 <= c3 && 179 >= c3)
          ||  (181 <= c3 && 182 >= c3)
          ||  (184 <= c3 && 185 >= c3))
          {
            letter = true;
          }
          break;
        case 169:
          if ((153 <= c3 && 156 >= c3)
          ||  (c3 == 158)
          ||  (178 <= c3 && 180 >= c3))
          {
            letter = true;
          }
          break;
        case 170:
          if ((133 <= c3 && 139 >= c3)
          ||  (c3 == 141)
          ||  (143 <= c3 && 145 >= c3)
          ||  (147 <= c3 && 168 >= c3)
          ||  (170 <= c3 && 176 >= c3)
          ||  (178 <= c3 && 179 >= c3)
          ||  (181 <= c3 && 185 >= c3)
          ||  (c3 == 189))
          {
            letter = true;
          }
          break;
        case 171:
          if ((c3 == 160))
          {
            letter = true;
          }
          break;
        case 172:
          if ((133 <= c3 && 140 >= c3)
          ||  (143 <= c3 && 144 >= c3)
          ||  (147 <= c3 && 168 >= c3)
          ||  (170 <= c3 && 176 >= c3)
          ||  (178 <= c3 && 179 >= c3)
          ||  (182 <= c3 && 185 >= c3)
          ||  (c3 == 189))
          {
            letter = true;
          }
          break;
        case 173:
          if ((156 <= c3 && 157 >= c3)
          ||  (159 <= c3 && 161 >= c3))
          {
            letter = true;
          }
          break;
        case 174:
          if ((133 <= c3 && 138 >= c3)
          ||  (142 <= c3 && 144 >= c3)
          ||  (146 <= c3 && 149 >= c3)
          ||  (153 <= c3 && 154 >= c3)
          ||  (c3 == 156)
          ||  (158 <= c3 && 159 >= c3)
          ||  (163 <= c3 && 164 >= c3)
          ||  (168 <= c3 && 170 >= c3)
          ||  (174 <= c3 && 181 >= c3)
          ||  (183 <= c3 && 185 >= c3))
          {
            letter = true;
          }
          break;
        case 176:
          if ((133 <= c3 && 140 >= c3)
          ||  (142 <= c3 && 144 >= c3)
          ||  (146 <= c3 && 168 >= c3)
          ||  (170 <= c3 && 179 >= c3)
          ||  (181 <= c3 && 185 >= c3))
          {
            letter = true;
          }
          break;
        case 177:
          if ((160 <= c3 && 161 >= c3))
          {
            letter = true;
          }
          break;
        case 178:
          if ((133 <= c3 && 140 >= c3)
          ||  (142 <= c3 && 144 >= c3)
          ||  (146 <= c3 && 168 >= c3)
          ||  (170 <= c3 && 179 >= c3)
          ||  (181 <= c3 && 185 >= c3))
          {
            letter = true;
          }
          break;
        case 179:
          if ((c3 == 158)
          ||  (160 <= c3 && 161 >= c3))
          {
            letter = true;
          }
          break;
        case 180:
          if ((133 <= c3 && 140 >= c3)
          ||  (142 <= c3 && 144 >= c3)
          ||  (146 <= c3 && 168 >= c3)
          ||  (170 <= c3 && 185 >= c3))
          {
            letter = true;
          }
          break;
        case 181:
          if ((160 <= c3 && 161 >= c3))
          {
            letter = true;
          }
          break;
        case 184:
          if ((129 <= c3 && 174 >= c3)
          ||  (c3 == 176)
          ||  (178 <= c3 && 179 >= c3))
          {
            letter = true;
          }
          break;
        case 185:
          if ((128 <= c3 && 133 >= c3))
          {
            letter = true;
          }
          break;
        case 186:
          if ((129 <= c3 && 130 >= c3)
          ||  (c3 == 132)
          ||  (135 <= c3 && 136 >= c3)
          ||  (c3 == 138)
          ||  (c3 == 141)
          ||  (148 <= c3 && 151 >= c3)
          ||  (153 <= c3 && 159 >= c3)
          ||  (161 <= c3 && 163 >= c3)
          ||  (c3 == 165)
          ||  (c3 == 167)
          ||  (170 <= c3 && 171 >= c3)
          ||  (173 <= c3 && 174 >= c3)
          ||  (c3 == 176)
          ||  (178 <= c3 && 179 >= c3)
          ||  (c3 == 189))
          {
            letter = true;
          }
          break;
        case 187:
          if ((128 <= c3 && 132 >= c3))
          {
            letter = true;
          }
          break;
        case 189:
          if ((128 <= c3 && 135 >= c3)
          ||  (137 <= c3 && 169 >= c3))
          {
            letter = true;
          }
          break;
        default:
          break;
        }
    break;
    case 225:
      switch (c2)
      {
        case 130:
          if ((160 <= c3 && 191 >= c3))
          {
            letter = true;
          }
          break;
        case 131:
          if ((128 <= c3 && 133 >= c3)
          ||  (144 <= c3 && 182 >= c3))
          {
            letter = true;
          }
          break;
        case 132:
          if ((c3 == 128)
          ||  (130 <= c3 && 131 >= c3)
          ||  (133 <= c3 && 135 >= c3)
          ||  (c3 == 137)
          ||  (139 <= c3 && 140 >= c3)
          ||  (142 <= c3 && 146 >= c3)
          ||  (c3 == 188)
          ||  (c3 == 190))
          {
            letter = true;
          }
          break;
        case 133:
          if ((c3 == 128)
          ||  (c3 == 140)
          ||  (c3 == 142)
          ||  (c3 == 144)
          ||  (148 <= c3 && 149 >= c3)
          ||  (c3 == 153)
          ||  (159 <= c3 && 161 >= c3)
          ||  (c3 == 163)
          ||  (c3 == 165)
          ||  (c3 == 167)
          ||  (c3 == 169)
          ||  (173 <= c3 && 174 >= c3)
          ||  (178 <= c3 && 179 >= c3)
          ||  (c3 == 181))
          {
            letter = true;
          }
          break;
        case 134:
          if ((c3 == 158)
          ||  (c3 == 168)
          ||  (c3 == 171)
          ||  (174 <= c3 && 175 >= c3)
          ||  (183 <= c3 && 184 >= c3)
          ||  (c3 == 186)
          ||  (188 <= c3 && 191 >= c3))
          {
            letter = true;
          }
          break;
        case 135:
          if ((128 <= c3 && 130 >= c3)
          ||  (c3 == 171)
          ||  (c3 == 176)
          ||  (c3 == 185))
          {
            letter = true;
          }
          break;
        case 184:
          if ((128 <= c3 && 191 >= c3))
          {
            letter = true;
          }
          break;
        case 185:
          if ((128 <= c3 && 191 >= c3))
          {
            letter = true;
          }
          break;
        case 186:
          if ((128 <= c3 && 155 >= c3)
          ||  (160 <= c3 && 191 >= c3))
          {
            letter = true;
          }
          break;
        case 187:
          if ((128 <= c3 && 185 >= c3))
          {
            letter = true;
          }
          break;
        case 188:
          if ((128 <= c3 && 149 >= c3)
          ||  (152 <= c3 && 157 >= c3)
          ||  (160 <= c3 && 191 >= c3))
          {
            letter = true;
          }
          break;
        case 189:
          if ((128 <= c3 && 133 >= c3)
          ||  (136 <= c3 && 141 >= c3)
          ||  (144 <= c3 && 151 >= c3)
          ||  (c3 == 153)
          ||  (c3 == 155)
          ||  (c3 == 157)
          ||  (159 <= c3 && 189 >= c3))
          {
            letter = true;
          }
          break;
        case 190:
          if ((128 <= c3 && 180 >= c3)
          ||  (182 <= c3 && 188 >= c3)
          ||  (c3 == 190))
          {
            letter = true;
          }
          break;
        case 191:
          if ((134 <= c3 && 140 >= c3)
          ||  (144 <= c3 && 147 >= c3)
          ||  (150 <= c3 && 155 >= c3)
          ||  (160 <= c3 && 172 >= c3)
          ||  (178 <= c3 && 180 >= c3)
          ||  (182 <= c3 && 188 >= c3))
          {
            letter = true;
          }
          break;
        default:
          break;
        }
    break;
    case 212:
      switch (c2)
      {
        case 191:
          if ((130 <= c3 && 132 >= c3))
          {
            letter = true;
          }
          break;
        default:
          break;
        }
    break;
    case 226:
      switch (c2)
      {
        case 132:
          if ((c3 == 166)
          ||  (170 <= c3 && 171 >= c3)
          ||  (c3 == 174))
          {
            letter = true;
          }
          break;
        case 134:
          if ((128 <= c3 && 130 >= c3))
          {
            letter = true;
          }
          break;
        default:
          break;
        }
    break;
    case 227:
      switch (c2)
      {
        case 128:
          if ((c3 == 135)
          ||  (161 <= c3 && 169 >= c3))
          {
            letter = true;
          }
          break;
        case 129:
          if ((129 <= c3 && 191 >= c3))
          {
            letter = true;
          }
          break;
        case 130:
          if ((128 <= c3 && 148 >= c3)
          ||  (161 <= c3 && 191 >= c3))
          {
            letter = true;
          }
          break;
        case 131:
          if ((128 <= c3 && 186 >= c3))
          {
            letter = true;
          }
          break;
        case 132:
          if ((133 <= c3 && 172 >= c3))
          {
            letter = true;
          }
          break;
       default:
          break;
        }
    break;
    case 228:
      if (c2 >= 184)
      {
        letter = true;
      }
    break;
    case 233:
      if (128 <= c2 && 189 >= c2)
      {
        letter = true;
      }
      else if (c2 == 190)
      {
        if (128 <= c3 && 165 >= c3)
        {
          letter = true;
        }
      }
    break;
    case 234:
      if (c2 >= 176)
      {
        letter = true;
      }
    break;
    case 229:
    case 230:
    case 231:
    case 232:
    case 235:
    case 236:
      {
        letter = true;
      }
    break;
    case 237:
      if (128 <= c2 && 157 >= c2)
      {
        letter = true;
      }
      else if (c2 == 158)
      {
        if (128 <= c3 && 163 >= c3)
        {
          letter = true;
        }
      }
    break;

    }
    break;
  default:
    break;
  }
      
  return letter; 
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
  * Checks if a character is part of the Unicode Digit set.
  * @return true if the character is a part of the set, false otherwise.
  */
bool 
SBase::isUnicodeDigit(std::string::iterator it, unsigned int numBytes)
{
  /*
  * Digit ::=  [#x0030-#x0039] | [#x0660-#x0669] | [#x06F0-#x06F9] | 
  * [#x0966-#x096F] | [#x09E6-#x09EF] | [#x0A66-#x0A6F] | [#x0AE6-#x0AEF] | 
  * [#x0B66-#x0B6F] | [#x0BE7-#x0BEF] | [#x0C66-#x0C6F] | [#x0CE6-#x0CEF] | 
  * [#x0D66-#x0D6F] | [#x0E50-#x0E59] | [#x0ED0-#x0ED9] | [#x0F20-#x0F29]    
  */
  bool digit = false;


  unsigned char c1 = *it;
  unsigned char c2 ;/* = *(it+1); */
  unsigned char c3 ;/* = *(it+2); */
  
  switch (numBytes)
  {
  case 1:
    if (48 <= c1 && 57 >= c1)
    {
      digit = true;
    }
    break;
  case 2:
    c2 = *(it+1);
    switch (c1)
    {
      case 217:
        if ((160 <= c2 && 169 >= c2))
        {
          digit = true;
        }
      break;
      case 219:
        if ((176 <= c2 && 185 >= c2))
        {
          digit = true;
        }
      break;
    }
    break;
  case 3:
  c2 = *(it+1);
  c3 = *(it+2);
  switch (c1)
  {
    case 224:
      switch (c2)
      {
        case 165:
          if ((166 <= c3 && 175 >= c3))
          {
            digit = true;
          }
          break;
        case 167:
          if ((166 <= c3 && 175 >= c3))
          {
            digit = true;
          }
          break;
        case 169:
          if ((166 <= c3 && 175 >= c3))
          {
            digit = true;
          }
          break;
        case 171:
          if ((166 <= c3 && 175 >= c3))
          {
            digit = true;
          }
          break;
        case 173:
          if ((166 <= c3 && 175 >= c3))
          {
            digit = true;
          }
          break;
        case 175:
          if ((167 <= c3 && 175 >= c3))
          {
            digit = true;
          }
          break;
        case 177:
          if ((166 <= c3 && 175 >= c3))
          {
            digit = true;
          }
          break;
        case 179:
          if ((166 <= c3 && 175 >= c3))
          {
            digit = true;
          }
          break;
        case 181:
          if ((166 <= c3 && 175 >= c3))
          {
            digit = true;
          }
          break;
        case 185:
          if ((144 <= c3 && 153 >= c3))
          {
            digit = true;
          }
          break;
        case 187:
          if ((144 <= c3 && 153 >= c3))
          {
            digit = true;
          }
          break;
        case 188:
          if ((160 <= c3 && 169 >= c3))
          {
            digit = true;
          }
          break;
      }

      break;
    default:
      break;
  }

  break;
  }
      
  return digit; 
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
  * Checks if a character is part of the Unicode CombiningChar set.
  * @return true if the character is a part of the set, false otherwise.
  */
bool 
SBase::isCombiningChar(std::string::iterator it, unsigned int numBytes)
{
  bool combiningChar = false;

  /* combiningChar unicodes in UTF-8 decimal form

  UNICODE    UTF-8(1)  UTF-8(2)   UTF-8(3)
  #x0300 -    204      128 - 191
    #x0345    205      128 - 133
  #x0360 - 1  205      160 - 161
  #x0483 - 6  210      131 - 134
  #x0591 - A1 214      145 - 161
  #x05A3 - B9 214      163 - 185
  #x05BB - D  214      187 - 189
  #x05BF      214      191
  #x05C1 - 2  215      129 - 130
  #x05C4      215      132
  #x064B - 52 217      139 - 146
  #x0670      217      176
  #x06D6 - F  219      150 - 159
  #x06E0 - 4  219      160 - 164
  #x06E7 - 8  219      167 - 168
  #x06EA - D  219      170 - 173
  #x0901 - 3  224      164        129 - 131
  #x093C      224      164        188
  #x093E      224      164        190 - 191
        - 4C  224      165        128 - 140
  #x094D      224      165        141
  #x0951 - 4  224      165        145 - 148
  #x0962 - 3  224      165        162 - 163
  #x0981 - 3  224      166        129 - 131
  #x09BC      224      166        188
  #x09BE - F  224      166        190 - 191
  #x09C0 - 4  224      167        128 - 132
  #x09C7 - 8  224      167        135 - 136
  #x09CB - D  224      167        139 - 141
  #x09D7      224      167        151
  #x09E2 - 3  224      167        162 - 163
  #x0A02      224      168        130
  #x0A3C      224      168        188
  #x0A3E - F  224      168        190 - 191
  #x0A40 - 2  224      169        128 - 130
  #x0A47 - 8  224      169        135 - 136
  #x0A4B - D  224      169        139 - 141
  #x0A70 - 1  224      169        176 - 177
  #x0A81 - 3  224      170        129 - 131
  #x0ABC      224      170        188
  #x0ABE      224      170        190 - 191
     -    C5  224      171        128 - 133
  #x0AC7 - 9  224      171        135 - 137
  #x0ACB - D  224      171        139 - 141
  #x0B01 - 3  224      172        129 - 131
  #x0B3C      224      172        188
  #x0B3E      224      172        190 - 191
     -    43  224      173        128 - 131
  #x0B47 - 8  224      173        135 - 136
  #x0B4B - D  224      173        139 - 141
  #x0B56 - 7  224      173        150 - 151
  #x0B82 - 3  224      174        130 - 131
  #x0BBE      224      174        190 - 191
     -    C2  224      175        128 - 130
  #x0BC6 - 8  224      175        134 - 136
  #x0BCA - D  224      175        138 - 141
  #x0BD7      224      175        151
  #x0C01 - 3  224      176        129 - 131
  #x0C3E      224      176        190 - 191
     -    44  224      177        128 - 132
  #x0C46 - 8  224      177        134 - 136
  #x0C4A - D  224      177        138 - 141
  #x0C55 - 6  224      177        149 - 150
  #x0C82 - 3  224      178        130 - 131
  #x0CBE      224      178        190 - 191
     -    C4  224      179        128 - 132
  #x0CC6 - 8  224      179        134 - 136
  #x0CCA - D  224      179        138 - 141
  #x0CD5 - 6  224      179        149 - 150 
  #x0D02 - 3  224      180        130 - 131
  #x0D3E      224      180        190 - 191
     -    43  224      181        128 - 131
  #x0D46 - 8  224      181        134 - 136
  #x0D4A - D  224      181        138 - 141
  #x0D57      224      181        151
  #x0E31      224      184        177
  #x0E34 - A  224      184        180 - 186
  #x0E47 - E  224      185        135 - 142
  #x0EB1      224      186        177
  #x0EB4 - 9  224      186        180 - 185
  #x0EBB - C  224      186        187 - 188
  #x0EC8 - D  224      187        136 - 141
  #x0F18 - 9  224      188        152 - 153
  #x0F35      224      188        181
  #x0F37      224      188        183
  #x0F39      224      188        185
  #x0F3E - F  224      188        190 - 191
  #x0F71      224      189        177 - 191
     -    84  224      190        128 - 132
  #x0F86 - B  224      190        134 - 139
  #x0F90 - 5  224      190        144 - 149
  #x0F97      224      190        151
  #x0F99      224      190        153 - 
     -    AD  224      190              173
  #x0FB1 - 7  224      190        177 - 183
  #x0FB9      224      190        185
  #x20D0 - C  226      131        144 - 156
  #x20E1      226      131        161
  #x302A - F  227      128        170 - 175
  #x3099 - A  227      130        153 - 154

  */

  unsigned char c1 = *it;
  unsigned char c2 ;/* = *(it+1); */
  unsigned char c3 ;/* = *(it+2); */
  
  switch (numBytes)
  {
  case 2:
   c2 = *(it+1);
   if (c1 == 204)
    {
      if (c2 >= 128 && c2 <= 191)
      {
        combiningChar = true;
      }
    }
    else if (c1 == 205)
    {
      if (c2 >= 128 && c2 <= 133)
      {
        combiningChar = true;
      }
      else if ( c2 == 160 || c2 == 161)
      {
        combiningChar = true;
      }
    }
    else if (c1 == 210)
    {
      if (c2 >= 131 && c2 <= 134)
      {
        combiningChar = true;
      }
    }
    else if (c1 == 214)
    {
      if (c2 >= 145 && c2 <= 161)
      {
        combiningChar = true;
      }
      else if (c2 >= 163 && c2 <= 185)
      {
        combiningChar = true;
      }
      else if (c2 >= 187 && c2 <= 189)
      {
        combiningChar = true;
      }
      else if (c2 == 191)
      {
        combiningChar = true;
      }
    }
    else if (c1 == 215)
    {
      if (c2 >= 129 && c2 <= 130)
      {
        combiningChar = true;
      }
      else if (c2 == 132)
      {
        combiningChar = true;
      }
    }
    else if (c1 == 217)
    {
      if (c2 >= 139 && c2 <= 146)
      {
        combiningChar = true;
      }
      else if (c2 == 176)
      {
        combiningChar = true;
      }
    }
    else if (c1 == 219)
    {
      if (c2 >= 150 && c2 <= 159)
      {
        combiningChar = true;
      }
      else if (c2 >= 160 && c2 <= 164)
      {
        combiningChar = true;
      }
      else if (c2 >= 167 && c2 <= 168)
      {
        combiningChar = true;
      }
      else if (c2 >= 170 && c2 <= 173)
      {
        combiningChar = true;
      }
    }
    break;
  case 3:
    c2 = *(it+1);
    c3 = *(it+2);
    if (c1 == 226)
    { 
      if (c2 == 131)
      {
        if (c3 == 161
          || (144 <= c3 && 156 >= c3))
        {
          combiningChar = true;
        }
      }
    }
    else if (c1 == 227)
    {
      if (c2 == 128)
      {
        if (170 <= c3 && 175 >= c3)
        {
          combiningChar = true;
        }
      }
      else if (c2 == 130)
      {
        if (153 <= c3 && 154 >= c3)
        {
          combiningChar = true;
        }
      }
    }
    else if (c1 == 224)
    {
      switch (c2)
      {
      case 164:
        if (  (129 <= c3 && 131 >= c3)  ||
              (c3 == 188)               ||
              (190 <= c3 && 191 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 165:
        if (  (128 <= c3 && 140 >= c3)  ||
              (c3 == 141)               ||
              (145 <= c3 && 148 >= c3)  ||
              (162 <= c3 && 163 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 166:
        if (  (129 <= c3 && 131 >= c3)  ||
              (c3 == 188)               ||
              (190 <= c3 && 191 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 167:
        if (  (128 <= c3 && 132 >= c3)  ||
              (135 <= c3 && 136 >= c3)  ||
              (139 <= c3 && 141 >= c3)  ||
              (c3 == 151)               ||
              (162 <= c3 && 163 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 168:
        if (  (c3 == 130)               ||
              (c3 == 188)               ||
              (190 <= c3 && 191 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 169:
        if (  (128 <= c3 && 130 >= c3)  ||
              (135 <= c3 && 136 >= c3)  ||
              (139 <= c3 && 141 >= c3)  ||
              (176 <= c3 && 177 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 170:
        if (  (129 <= c3 && 131 >= c3)  ||
              (c3 == 188)               ||
              (190 <= c3 && 191 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 171:
        if (  (128 <= c3 && 130 >= c3)  ||
              (135 <= c3 && 137 >= c3)  ||
              (139 <= c3 && 141 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 172:
        if (  (129 <= c3 && 131 >= c3)  ||
              (c3 == 188)               ||
              (190 <= c3 && 191 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 173:
        if (  (128 <= c3 && 131 >= c3)  ||
              (135 <= c3 && 136 >= c3)  ||
              (139 <= c3 && 141 >= c3)  ||
              (150 <= c3 && 151 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 174:
        if (  (130 <= c3 && 131 >= c3)  ||
              (190 <= c3 && 191 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 175:
        if (  (128 <= c3 && 130 >= c3)  ||
              (134 <= c3 && 136 >= c3)  ||
              (138 <= c3 && 141 >= c3)  ||
              (c3 == 151)               )
        {
          combiningChar = true;
        }

        break;
      case 176:
        if (  (129 <= c3 && 131 >= c3)  ||
              (190 <= c3 && 191 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 177:
        if (  (128 <= c3 && 132 >= c3)  ||
              (134 <= c3 && 136 >= c3)  ||
              (138 <= c3 && 141 >= c3)  ||
              (149 <= c3 && 150 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 178:
        if (  (130 <= c3 && 131 >= c3)  ||
              (190 <= c3 && 191 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 179:
        if (  (128 <= c3 && 132 >= c3)  ||
              (134 <= c3 && 136 >= c3)  ||
              (138 <= c3 && 141 >= c3)  ||
              (149 <= c3 && 150 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 180:
        if (  (130 <= c3 && 131 >= c3)  ||
              (190 <= c3 && 191 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 181:
        if (  (128 <= c3 && 131 >= c3)  ||
              (134 <= c3 && 136 >= c3)  ||
              (138 <= c3 && 141 >= c3)  ||
              (c3 == 151)               )
        {
          combiningChar = true;
        }

        break;
      case 184:
        if (  (c3 == 170)               ||
              (180 <= c3 && 186 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 185:
        if (  (135 <= c3 && 142 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 186:
        if (  (c3 == 177)               ||
              (180 <= c3 && 185 >= c3)  ||
              (187 <= c3 && 188 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 187:
        if (  (136 <= c3 && 141 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 188:
        if (  (152 <= c3 && 153 >= c3)  ||
              (c3 == 181)               ||
              (c3 == 183)               ||
              (c3 == 185)               ||
              (190 <= c3 && 191 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 189:
        if (  (177 <= c3 && 191 >= c3)  )
        {
          combiningChar = true;
        }

        break;
      case 190:
        if (  (128 <= c3 && 132 >= c3)  ||
              (134 <= c3 && 139 >= c3)  ||
              (144 <= c3 && 149 >= c3)  ||
              (c3 == 151)               ||
              (153 <= c3 && 173 >= c3)  ||
              (177 <= c3 && 183 >= c3)  ||
              (c3 == 185)               )
        {
          combiningChar = true;
        }

        break;
      default:
        break;
      }

    }
  default:
    break;
  }
      
  return combiningChar; 
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
  * Checks if a character is part of the Unicode Extender set.
  * @return true if the character is a part of the set, false otherwise.
  */
bool 
SBase::isExtender(std::string::iterator it, unsigned int numBytes)
{
  bool extender = false;

  /* extender unicodes in UTF-8 decimal form

  UNICODE UTF-8(1)  UTF-8(2)  UTF-8(3)
  #x00B7  194       183
  #x02D0  203       144
  #x02D1  203       145
  #x0387  206       135
  #x0640  217       128
  #x0E46  224       185       134
  #x0EC6  224       187       134
  #x3005  227       128       133
  #x3031- 227       128       177-
  #x3035                      181
  #x309D  227       130       157
  #x309E  227       130       158
  #x30FC- 227       131       188-
  #x30FE                      190

  */

  unsigned char c1 = *it;
  unsigned char c2 ;/* = *(it+1); */
  unsigned char c3 ;/* = *(it+2); */
  
  switch (numBytes)
  {
  case 2:
    c2 = *(it+1);
    if (c1 == 194 && c2 == 183)
    {
      extender = true;
    }
    else if (c1 == 203)
    {
      if (c2 == 144 || c2 == 145)
      {
        extender = true;
      }
    }
    else if (c1 == 206 && c2 == 135)
    {
      extender = true;
    }
    else if (c1 == 217 && c2 == 128)
    {
      extender = true;
    }
    break;
  case 3:
    c2 = *(it+1);
    c3 = *(it+2);
    if (c1 == 224)
    {
      if (c2 == 185 || c2 == 187)
      {
        if (c3 == 134)
        {
          extender = true;
        }
      }
    }
    else if (c1 == 227)
    {
      if (c2 == 128)
      {
        if (c3 == 133 || (c3 >= 177 && c3 <= 181))
        {
          extender = true;
        }
      }
      else if (c2 == 130)
      {
        if (c3 == 157 || c3 == 158)
        {
          extender = true;
        }
      }
      else if (c2 == 131)
      {
         if (c3 >= 188 && c3 <= 190)
        {
          extender = true;
        }
     }
    }
  default:
    break;
  }
      
  return extender; 
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Stores the location (line and column) and any XML namespaces (for
 * roundtripping) declared on this SBML (XML) element.
 */
void
SBase::setSBaseFields (const XMLToken& element)
{
  mLine   = element.getLine  ();
  mColumn = element.getColumn();

  if (element.getNamespaces().getLength() > 0)
  {
    mNamespaces = new XMLNamespaces( element.getNamespaces() );
  }
}
/** @endcond doxygen-libsbml-internal */



/** @cond doxygen-c-only */

/**
 * Adds a copy of the given CVTerm to this SBML object.
 *
 * @param sb the object to add the CVTerm to
 * @param term the CVTerm_t to assign
 */
LIBSBML_EXTERN
void 
SBase_addCVTerm(SBase_t *sb, CVTerm_t *term)
{
  sb->addCVTerm(term);
}


/**
 * Returns a list of CVTerm objects in the annotations of this SBML
 * object.
 *
 * @param sb the object to getCVTerms from
 * 
 * @return the list of CVTerms for this SBML object.
 */
LIBSBML_EXTERN
List_t* 
SBase_getCVTerms(SBase_t *sb)
{
  return sb->getCVTerms();
}


/**
 * Returns the number of CVTerm objects in the annotations of this SBML
 * object.
 *
 * @param sb the object to getCVTerms from
 * 
 * @return the number of CVTerms for this SBML object.
 */
LIBSBML_EXTERN
unsigned int 
SBase_getNumCVTerms(SBase_t *sb)
{
  return sb->getNumCVTerms();
}

/**
 * Returns the nth CVTerm in the list of CVTerms of this SBML
 * object.
 *
 * @param sb the object to getCVTerms from
 * @param n unsigned int the index of the CVTerm to retrieve
 *
 * @return the nth CVTerm in the list of CVTerms for this SBML object.
 */
LIBSBML_EXTERN
CVTerm_t* 
SBase_getCVTerm(SBase_t *sb, unsigned int n)
{
  return static_cast <CVTerm_t *> (sb->getCVTerm(n));
}




/**
 * Returns the value of the "metaid" attribute of the given SBase_t
 * structure.
 *
 * @param sb the SBase_t structure
 * 
 * @return the value of the "metaid" attribute of @p sb
 */
LIBSBML_EXTERN
const char *
SBase_getMetaId (const SBase_t *sb)
{
  return sb->isSetMetaId() ? sb->getMetaId().c_str() : NULL;
}


/**
 * Returns the value of the "id" attribute of the given SBase_t
 * structure.
 *
 * @param sb the SBase_t structure
 * 
 * @return the value of the "id" attribute of @p sb
 */
LIBSBML_EXTERN
const char *
SBase_getId (const SBase_t *sb)
{
  return sb->isSetId() ? sb->getId().c_str() : NULL;
}


/**
 * Returns the value of the "name" attribute of the given SBase_t
 * structure.
 *
 * @param sb the SBase_t structure
 * 
 * @return the value of the "name" attribute of @p sb
 */
LIBSBML_EXTERN
const char *
SBase_getName (const SBase_t *sb)
{
  return sb->isSetName() ? sb->getName().c_str() : NULL;
}


/**
 * Returns the parent SBMLDocument_t structure of the given SBase_t
 * structure.
 *
 * @param sb the SBase_t structure
 * 
 * @return the parent SBMLDocument of this SBML object.
 */
LIBSBML_EXTERN
const SBMLDocument_t *
SBase_getSBMLDocument (const SBase_t *sb)
{
  return sb->getSBMLDocument();
}


/**
 * Returns the integer portion of the value of the "sboTerm" attribute of
 * the given SBase_t structure.
 *
 * In SBML Level 2 Versions 2 and 3, the data type of the attribute is a
 * string of the form SBO:NNNNNNN, where NNNNNNN is a seven digit integer
 * number; libSBML simplifies the representation by only storing the
 * NNNNNNN integer portion.  Thus, in libSBML, the "sboTerm" attribute on
 * SBase_t has data type @c int, and SBO identifiers are stored simply as
 * integers.  SBO terms are a type of optional annotation, and each
 * different class of SBML object derived from SBase_t imposes its own
 * requirements about the values permitted for "sboTerm".  Please consult
 * the SBML Level 2 Version 3 specification for more information about
 * the use of SBO and the "sboTerm" attribute.
 *
 * @param sb the SBase_t structure
 * 
 * @return the value of the "sboTerm" attribute as an integer, or @c -1
 * if the value is not set.
 */
LIBSBML_EXTERN
int
SBase_getSBOTerm (const SBase_t *sb)
{
  return sb->getSBOTerm();
}


/**
 * Returns the SBML Level of the overall SBML document.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return the SBML level of the given object.
 * 
 * @see getVersion()
 */
LIBSBML_EXTERN
unsigned int
SBase_getLevel (const SBase_t *sb)
{
  return sb->getLevel();
}


/**
 * Returns the Version within the SBML Level of the overall SBML document.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return the SBML version of the given object.
 *
 * @see getLevel()
 */
LIBSBML_EXTERN
unsigned int
SBase_getVersion (const SBase_t *sb)
{
  return sb->getVersion();
}


/**
 * Returns the notes from given SBML object.
 *
 * @param sb the given SBML object.
 *
 * @return the XMLNode_t structure representing the notes from this object.
 */
LIBSBML_EXTERN
XMLNode_t *
SBase_getNotes (SBase_t *sb)
{
  return sb->getNotes();
}


/**
 * Returns the notes string from given SBML object.
 * The string is owned by the caller and should be freed
 * (with free()) when no longer needed.  
 *
 * @param sb the given SBML object.
 *
 * @return the string (char*) representing the notes from this object.
 */
LIBSBML_EXTERN
char*
SBase_getNotesString (SBase_t *sb)
{
  return sb->isSetNotes() ? safe_strdup(sb->getNotesString().c_str()) : NULL;
}


/**
 * Returns the annotation from given SBML object.
 *
 * @param sb the given SBML object.
 *
 * @return the XMLNode_t structure representing the annotation from this object.
 */
LIBSBML_EXTERN
XMLNode_t *
SBase_getAnnotation (SBase_t *sb)
{
  return sb->getAnnotation();
}


/**
 * Returns the annotation string from given SBML object.
 * The string is owned by the caller and should be freed
 * (with free()) when no longer needed.
 *
 * @param sb the given SBML object.
 *
 * @return the string (char*) representing the annotation from this object.
 */
LIBSBML_EXTERN
char*
SBase_getAnnotationString (SBase_t *sb)
{
  return sb->isSetAnnotation() ? safe_strdup(sb->getAnnotationString().c_str()) : NULL;
}


/**
 * Predicate returning nonzero true or false depending on whether the given
 * structure's "metaid" attribute has been set.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return nonzero (for true) if the "metaid" attribute of this SBML object
 * has been set, zero (for false) otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetMetaId (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetMetaId() );
}


/**
 * Predicate returning nonzero true or false depending on whether the given
 * structure's "id" attribute has been set.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return nonzero (for true) if the "id" attribute of this SBML object
 * has been set, zero (for false) otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetId (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetId() );
}


/**
 * Predicate returning nonzero true or false depending on whether the given
 * structure's "name" attribute has been set.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return nonzero (for true) if the "name" attribute of this SBML object
 * has been set, zero (for false) otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetName (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetName() );
}


/**
 * Predicate returning nonzero true or false depending on whether the given
 * structure's "notes" subelement has been set.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return nonzero (for true) if the "notes" subelement of this SBML object
 * has been set, zero (for false) otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetNotes (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetNotes() );
}


/**
 * Predicate returning nonzero true or false depending on whether the given
 * structure's "annotation" subelement has been set.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return nonzero (for true) if the "annotation" subelement of this SBML object
 * has been set, zero (for false) otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetAnnotation (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetAnnotation() );
}


/**
 * Predicate returning nonzero true or false depending on whether the given
 * structure's "sboTerm" attribute has been set.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return nonzero (for true) if the "sboTerm" attribute of this SBML object
 * has been set, zero (for false) otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetSBOTerm (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetSBOTerm() );
}


/**
 * Sets the value of the "metaid" attribute of the given object.
 *
 * The string @p metaid is copied.  The value of @p metaid must be an
 * identifier conforming to the syntax defined by the XML 1.0 data type
 * ID.  Among other things, this type requires that a value is unique
 * among all the values of type XML ID in an SBMLDocument.  Although SBML
 * only uses XML ID for the "metaid" attribute, callers should be careful
 * if they use XML ID's in XML portions of a model that are not defined
 * by SBML, such as in the application-specific content of the
 * "annotation" subelement.
 *
 * @param sb the SBase_t structure
 *
 * @param metaid the identifier string to use as the value of the
 * "metaid" attribute
 */
LIBSBML_EXTERN
void
SBase_setMetaId (SBase_t *sb, const char *metaid)
{
  (metaid == NULL) ? sb->unsetMetaId() : sb->setMetaId(metaid);
}


/**
 * Sets the value of the "id" attribute of this SBML object.
 *
 * The string @p sid is copied.  Note that SBML has strict requirements
 * for the syntax of identifiers.  The following is summary of the
 * definition of the SBML identifier type @c SId (here expressed in an
 * extended form of BNF notation):
 * @code
 *   letter ::= 'a'..'z','A'..'Z'
 *   digit  ::= '0'..'9'
 *   idChar ::= letter | digit | '_'
 *   SId    ::= ( letter | '_' ) idChar*
 * @endcode
 * The equality of SBML identifiers is determined by an exact character
 * sequence match; i.e., comparisons must be performed in a
 * case-sensitive manner.  In addition, there are a few conditions for
 * the uniqueness of identifiers in an SBML model.  Please consult the
 * SBML specifications for the exact formulations.
 *
 * @param sb the SBase_t structure
 *
 * @param sid the string to use as the identifier of this object
 */
LIBSBML_EXTERN
void
SBase_setId (SBase_t *sb, const char *sid)
{
  (sid == NULL) ? sb->unsetId() : sb->setId(sid);
}


/**
 * Sets the value of the "name" attribute of this SBML object.
 *
 * The string in @p name is copied.
 *
 * @param sb the SBase_t structure
 *
 * @param name the new name for the object
 */
LIBSBML_EXTERN
void
SBase_setName (SBase_t *sb, const char *name)
{
  (name == NULL) ? sb->unsetName() : sb->setName(name);
}


/**
 * Sets the value of the "sboTerm" attribute.
 *
 * In SBML Level 2 Versions 2 and 3, the data type of the SBML "sboTerm"
 * attribute is a string of the form SBO:NNNNNNN, where NNNNNNN is a seven
 * digit integer number; libSBML simplifies the representation by only
 * storing the NNNNNNN integer portion.  Thus, in libSBML, the "sboTerm"
 * attribute on SBase_t has data type @c int, and SBO identifiers are
 * stored simply as integers.  SBO terms are a type of optional annotation,
 * and each different class of SBML object derived from SBase_t imposes its
 * own requirements about the values permitted for "sboTerm".  Please
 * consult the SBML Level 2 Version 3 specification for more information
 * about the use of SBO and the "sboTerm" attribute.
 *
 * @param sb the SBase_t structure
 *
 * @param value the NNNNNNN integer portion of the SBO identifier
 */
LIBSBML_EXTERN
void
SBase_setSBOTerm (SBase_t *sb, int value)
{
  sb->setSBOTerm(value);
}

/**
 * Sets the notes for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param notes the XMLNode_t structure respresenting the notes.
 */
LIBSBML_EXTERN
void
SBase_setNotes (SBase_t *sb, XMLNode_t *notes)
{
  sb->setNotes(notes);
}


/**
 * Sets the notes for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param notes the string (const char*) respresenting the notes.
 */
LIBSBML_EXTERN
void
SBase_setNotesString (SBase_t *sb, char *notes)
{
  if(notes == NULL)
  {
    sb->unsetNotes();
  }
  else
  {
    sb->setNotes(notes);
  }
}


/**
 * Appends the notes for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param notes the XMLNode_t structure respresenting the notes.
 */
LIBSBML_EXTERN
void
SBase_appendNotes (SBase_t *sb, XMLNode_t *notes)
{
  sb->appendNotes(notes);
}


/**
 * Appends the notes for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param notes the string (const char*) respresenting the notes.
 */
LIBSBML_EXTERN
void
SBase_appendNotesString (SBase_t *sb, char *notes)
{
  if(notes != NULL)
  {
    sb->appendNotes(notes);
  }
}


/**
 * Sets the annotation for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param annotation the XMLNode_t structure respresenting the annotation.
 */
LIBSBML_EXTERN
void
SBase_setAnnotation (SBase_t *sb, XMLNode_t *annotation)
{
  sb->setAnnotation(annotation);
}


/**
 * Sets the annotation for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param annotation the string (const char*) respresenting the annotation.
 */
LIBSBML_EXTERN
void
SBase_setAnnotationString (SBase_t *sb, char *annotation)
{
  if(annotation == NULL)
  {
    sb->unsetAnnotation();
  }
  else
  {
    sb->setAnnotation(annotation);
  }
}


/**
 * Appends the annotation for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param annotation the XMLNode_t structure respresenting the annotation.
 */
LIBSBML_EXTERN
void
SBase_appendAnnotation (SBase_t *sb, XMLNode_t *annotation)
{
  sb->appendAnnotation(annotation);
}


/**
 * Appends the annotation for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param annotation the string (const char*) respresenting the annotation.
 */
LIBSBML_EXTERN
void
SBase_appendAnnotationString (SBase_t *sb, char *annotation)
{
  if(annotation != NULL)
  {
    sb->appendAnnotation(annotation);
  }
}


/**
 * Unsets the "metaid" attribute of the given object.
 *
 * @param sb the SBase_t structure
 */
LIBSBML_EXTERN
void
SBase_unsetMetaId (SBase_t *sb)
{
  sb->unsetMetaId();
}


/**
 * Unsets the "id" attribute of the given object.
 *
 * @param sb the SBase_t structure
 */
LIBSBML_EXTERN
void
SBase_unsetId (SBase_t *sb)
{
  sb->unsetId();
}


/**
 * Unsets the "name" attribute of the given object.
 *
 * @param sb the SBase_t structure
 */
LIBSBML_EXTERN
void
SBase_unsetName (SBase_t *sb)
{
  sb->unsetName();
}


/**
 * Unsets the "notes" subelement of the given object.
 *
 * @param sb the SBase_t structure
 */
LIBSBML_EXTERN
void
SBase_unsetNotes (SBase_t *sb)
{
  sb->unsetNotes();
}


/**
 * Unsets the "annotation" subelement of the given object.
 *
 * @param sb the SBase_t structure
 */
LIBSBML_EXTERN
void
SBase_unsetAnnotation (SBase_t *sb)
{
  sb->unsetAnnotation();
}


/**
 * Unsets the "sboTerm" attribute of the given object.
 *
 * @param sb the SBase_t structure
 */
LIBSBML_EXTERN
void
SBase_unsetSBOTerm (SBase_t *sb)
{
  sb->unsetSBOTerm();
}


/**
 * Returns the Model_t structure in which the given instance is located.
 *
 * @param sb the SBase_t structure
 * 
 * @return the parent Model_t strucdture of the given object.
 */
LIBSBML_EXTERN
const Model_t *
SBase_getModel (const SBase_t *sb)
{
  return sb->getModel();
}


/**
 * Returns the libSBML type code for the given structure.
 *
 * This method MAY return the typecode of this SBML object or it MAY
 * return SBML_UNKNOWN.  That is, subclasses of SBase are not required to
 * implement this method to return a typecode.  This method is meant
 * primarily for the LibSBML C interface where class and subclass
 * information is not readily available.
 *
 * @param sb the SBase_t structure
 *
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
LIBSBML_EXTERN
SBMLTypeCode_t
SBase_getTypeCode (const SBase_t *sb)
{
  return sb->getTypeCode();
}


/**
 * Returns the XML element name of the given structure.
 *
 * This is overridden by subclasses to return a string appropriate to the
 * SBML component.  For example, Model defines it as returning "model",
 * CompartmentType defines it as returning "compartmentType", etc.
 *
 * @param sb the SBase_t structure
 */
LIBSBML_EXTERN
const char *
SBase_getElementName (const SBase_t *sb)
{
  return sb->getElementName().empty() ? NULL : sb->getElementName().c_str();
}


/**
 * Returns the line number on which the given object first appears in the
 * XML representation of the SBML document.
 *
 * @param sb the SBase_t structure
 * 
 * @return the line number of the given structure
 *
 * @see getColumn().
 */
LIBSBML_EXTERN
unsigned int
SBase_getLine (const SBase_t *sb)
{
  return sb->getLine();
}


/**
 * Returns the column number on which the given object first appears in the
 * XML representation of the SBML document.
 *
 * @param sb the SBase_t structure
 * 
 * @return the column number of this SBML object.
 * 
 * @see getLine().
 */
LIBSBML_EXTERN
unsigned int
SBase_getColumn (const SBase_t *sb)
{
  return sb->getColumn();
}



/** @endcond doxygen-c-only */
