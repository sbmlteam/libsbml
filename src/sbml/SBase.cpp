/**
 * \file    SBase.cpp
 * \brief   Base object of all SBML objects
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <sstream>

#include <sbml/xml/XMLError.h>
#include <sbml/xml/XMLErrorLog.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>

#include <sbml/util/util.h>

#include "SBMLDocument.h"
#include "SBase.h"


using namespace std;


/**
 * Only subclasses may create SBase objects.
 */
SBase::SBase (const string& id, const string& name) :
   mId        ( id   )
 , mName      ( name )
 , mNotes     ( 0 )
 , mAnnotation( 0 )
 , mNamespaces( 0 )
 , mSBML      ( 0 )
 , mLine      ( 0 )
 , mColumn    ( 0 )
{
}


/**
 * Destroy this SBase object.
 */
LIBSBML_EXTERN
SBase::~SBase ()
{
  delete mNotes;
  delete mAnnotation;
  delete mNamespaces;
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
  return (mName.empty() == false);
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
  return (mAnnotation != 0);
}


/**
 * Sets the metaid field of the given SBML object to a copy of metaid.
 */
void
SBase::setMetaId (const string& id)
{
  mMetaId = id;
}


/**
 * Sets the id of this SBML object to a copy of sid.
 */
void
SBase::setId (const string& sid)
{
  mId = sid;
}


/**
 * Sets the name of this SBML object to a copy of name.
 */
void
SBase::setName (const string& name)
{
  if (getLevel() == 1) mId = name;
  else mName = name;
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
LIBSBML_EXTERN
void
SBase::unsetNotes ()
{
  delete mNotes;
}

/**
 * Unsets the annotation of this SBML object.
 */
LIBSBML_EXTERN
void
SBase::unsetAnnotation ()
{
  delete mAnnotation;
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
 */
SBMLTypeCode_t
SBase::getTypeCode () const
{
  return SBML_UNKNOWN;
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


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
SBase::getElementPosition () const
{
  return -1;
}


/**
 * @return the SBMLErrorLog used to log errors during while reading and
 * validating SBML.
 */
SBMLErrorLog*
SBase::getErrorLog ()
{
  return (mSBML != 0) ? mSBML->getErrorLog() : 0;
}


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


/**
 * Sets the parent SBMLDocument of this SBML object.
 */
void
SBase::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
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


/**
 * Reads (initializes) this SBML object by reading from XMLInputStream.
 */
void
SBase::read (XMLInputStream& stream)
{
  if ( !stream.peek().isStart() ) return;

  const XMLToken  element  = stream.next();
  int             position = 0;

  setSBaseFields( element );
  readAttributes( element.getAttributes() );

  if ( element.isEnd() ) return;

  while ( stream.isGood() )
  {
    stream.skipText();
    const XMLToken& next = stream.peek();

    if ( next.isEndFor(element) )
    {
      stream.next();
      break;
    }
    else if ( next.isStart() )
    {
      SBase* object = createObject(stream);

      if (object)
      {
        // checkOrder( position, object->getElementPosition(),  log);
        // position = object->getElementPosition();

        object->mSBML = mSBML;
        object->read(stream);
      }
      else if ( !readOtherXML(stream) )
      {
        // logUnrecognized(next, log);
        stream.skipPastEnd( stream.next() );
      }
    }
  }
}


/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SBase::readAttributes (const XMLAttributes& attributes)
{
  attributes.readInto("metaid", mMetaId);
}


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


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
SBase::writeElements (XMLOutputStream& stream) const
{
  if ( mNotes      ) stream << *mNotes;
  if ( mAnnotation ) stream << *mAnnotation;
}




/**
 * @return the metaid of this SBML object.
 */
LIBSBML_EXTERN
const char *
SBase_getMetaId (const SBase_t *sb)
{
  return sb->isSetMetaId() ? sb->getMetaId().c_str() : NULL;
}


/**
 * @return the id of this SBML object.
 */
LIBSBML_EXTERN
const char *
SBase_getId (const SBase_t *sb)
{
  return sb->isSetId() ? sb->getId().c_str() : NULL;
}


/**
 * @return the name of this SBML object.
 */
LIBSBML_EXTERN
const char *
SBase_getName (const SBase_t *sb)
{
  return sb->isSetName() ? sb->getName().c_str() : NULL;
}


/**
 * @return 1 if the metaid of this SBML object has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetMetaId (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetMetaId() );
}


/**
 * @return 1 if the id of this SBML object has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetId (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetId() );
}


/**
 * @return 1 if the name of this SBML object has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetName (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetName() );
}


/**
 * @return 1 if the notes of this SBML object has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetNotes (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetNotes() );
}


/**
 * @return 1 if the annotation of this SBML object has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetAnnotation (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetAnnotation() );
}


/**
 * Sets the metaid field of the given SBML object to a copy of metaid.
 */
LIBSBML_EXTERN
void
SBase_setMetaId (SBase_t *sb, const char *metaid)
{
  (metaid == NULL) ? sb->unsetMetaId() : sb->setMetaId(metaid);
}


/**
 * Sets the id field of the given SBML object to a copy of sid.
 */
LIBSBML_EXTERN
void
SBase_setId (SBase_t *sb, const char *sid)
{
  (sid == NULL) ? sb->unsetId() : sb->setId(sid);
}


/**
 * Sets the name field of the given SBML object to a copy of name.
 */
LIBSBML_EXTERN
void
SBase_setName (SBase_t *sb, const char *name)
{
  (name == NULL) ? sb->unsetName() : sb->setName(name);
}


/**
 * Unsets the metaid of this SBML object.
 */
LIBSBML_EXTERN
void
SBase_unsetMetaId (SBase_t *sb)
{
  sb->unsetMetaId();
}


/**
 * Unsets the id of this SBML object.
 */
LIBSBML_EXTERN
void
SBase_unsetId (SBase_t *sb)
{
  sb->unsetId();
}


/**
 * Unsets the name of this SBML object.
 */
LIBSBML_EXTERN
void
SBase_unsetName (SBase_t *sb)
{
  sb->unsetName();
}


/**
 * Unsets the notes of this SBML object.
 */
LIBSBML_EXTERN
void
SBase_unsetNotes (SBase_t *sb)
{
  sb->unsetNotes();
}


/**
 * Unsets the annotation of this SBML object.
 */
LIBSBML_EXTERN
void
SBase_unsetAnnotation (SBase_t *sb)
{
  sb->unsetAnnotation();
}


/**
 * @return the parent SBMLDocument of this SBML object.
 */
LIBSBML_EXTERN
const SBMLDocument_t *
SBase_getSBMLDocument (const SBase_t *sb)
{
  return sb->getSBMLDocument();
}


/**
 * @return the parent Model of this SBML object.
 */
LIBSBML_EXTERN
const Model_t *
SBase_getModel (const SBase_t *sb)
{
  return sb->getModel();
}


/**
 * @return the SBML level of this SBML object.
 */
LIBSBML_EXTERN
unsigned int
SBase_getLevel (const SBase_t *sb)
{
  return sb->getLevel();
}


/**
 * @return the SBML version of this SBML object.
 */
LIBSBML_EXTERN
unsigned int
SBase_getVersion (const SBase_t *sb)
{
  return sb->getVersion();
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 */
LIBSBML_EXTERN
SBMLTypeCode_t
SBase_getTypeCode (const SBase_t *sb)
{
  return sb->getTypeCode();
}


/**
 * @return the XML element name of this SBML object.
 */
LIBSBML_EXTERN
const char *
SBase_getElementName (const SBase_t *sb)
{
  return sb->getElementName().empty() ? NULL : sb->getElementName().c_str();
}


/**
 * @return the line number of this SBML object.
 */
LIBSBML_EXTERN
unsigned int
SBase_getLine (const SBase_t *sb)
{
  return sb->getLine();
}


/**
 * @return the column number of this SBML object.
 */
LIBSBML_EXTERN
unsigned int
SBase_getColumn (const SBase_t *sb)
{
  return sb->getColumn();
}
