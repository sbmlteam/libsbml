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

#include "KineticLaw.h"
#include "SBMLDocument.h"
#include "ListOf.h"
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
  * @return the Namespaces associated with this SBML object
  */
XMLNamespaces*
SBase::getNamespaces() const
{
  return mNamespaces;
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
  int             position =  0;

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
      SBase * object = createObject(stream);

      if (object)
      {
        checkOrderAndLogError(object, position);
        position = object->getElementPosition();

        object->mSBML = mSBML;
        object->read(stream);
        checkListOfPopulated(object);

      }
      else if ( !readOtherXML(stream) )
      {
        mSBML->getErrorLog()->unrecognizedElement(next);
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
  if (isSetMetaId())
    checkMetaIdSyntax();
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
 * Checks that SBML element has was read in the proper order.  If object
 * is not in the expected position, an error is logged.
 */
void
SBase::checkOrderAndLogError (SBase* object, int expected)
{
  int actual = object->getElementPosition();

  if (actual != -1 && actual < expected)
  {
    unsigned int error = 20202;

    if (object->getTypeCode() == SBML_LIST_OF)
    {
      SBMLTypeCode_t tc = static_cast<ListOf*>(object)->getItemTypeCode();

      if (tc == SBML_SPECIES_REFERENCE || tc == SBML_MODIFIER_SPECIES_REFERENCE)
      {
        error = 21102;
      }
    }

    mSBML->getErrorLog()->logError(error);
  }
}


/**
  * Checks that an SBML ListOf element has been populated.  
  * If not, an error is logged.
  */
void 
SBase::checkListOfPopulated(SBase* object)
{
  unsigned int error = 20203;

  if (object->getTypeCode() == SBML_LIST_OF)
  {
    SBMLTypeCode_t tc = static_cast<ListOf*>(object)->getItemTypeCode();
    
    /* check that list has at least one element */
    if (static_cast <ListOf*> (object)->size() == 0)
    {
      if (tc == SBML_UNIT)
      {
        error = 20409;
      }
      else if (tc == SBML_SPECIES_REFERENCE || tc == SBML_MODIFIER_SPECIES_REFERENCE)
      {
        error = 21103;
      }
      else if (tc == SBML_PARAMETER)
      {
        /* if the listOfParameters is inside a KineticLaw
        */
        if (this->getTypeCode() == SBML_KINETIC_LAW)
        {
          error = 21123;
        }
      }
      mSBML->getErrorLog()->logError(error);
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
        error = 21103;
        mSBML->getErrorLog()->logError(error);
    }
  }
}


/**
  * Checks the syntax of a "metaid"
  * if incorrect, an error is logged
  */
void 
SBase::checkMetaIdSyntax()
{
  string& metaid = getMetaId();
  string::iterator it = metaid.begin();
 
  // first character must be a letter or '_' or ':'
  unsigned char c = *it;
  bool okay = (isalpha(c) || c == '_' || c == ':');
  it++;

  // remaining chars must be 
  // letter | digit | ’.’ | ’-’ | ’ ’ | ':' | CombiningChar | Extender
  while (okay && it < metaid.end())
  {
    c = *it;
    
    // need to find multibyte sequences
    if (c < 0x80)
    {
      okay = (
            isalnum(c) || 
            c == '.'   ||
            c == '-'   ||
            c == '_'   ||
            c == ':'   );
    }
    else if (c >> 5 == 0x6)
    {
      okay = (
          isCombiningChar(it, 2)  ||
          isExtender(it, 2)       );
      it++;
    }
    else if (c >> 4 == 0xe)
    {
      okay = (
          isCombiningChar(it, 3)  ||
          isExtender(it, 3)       );
      it++;
      it++;
    }
    else if (c >> 3 == 0x1e)
    {
      okay = (
          isCombiningChar(it, 4)  ||
          isExtender(it, 4)       );
      it += 3;
    }

    it++;

  }


  if (!okay)   
    mSBML->getErrorLog()->logError(10309);
}

  
/**
  * checks if a character is part of the CombiningCharacter set
  * CombiningChar ::=  [#x0300-#x0345] | [#x0360-#x0361] | [#x0483-#x0486] | 
  * [#x0591-#x05A1] | [#x05A3-#x05B9] | [#x05BB-#x05BD] | #x05BF | [#x05C1-#x05C2] | 
  * #x05C4 | [#x064B-#x0652] | #x0670 | [#x06D6-#x06DC] | [#x06DD-#x06DF] | 
  * [#x06E0-#x06E4] | [#x06E7-#x06E8] | [#x06EA-#x06ED] | [#x0901-#x0903] | #x093C | 
  * [#x093E-#x094C] | #x094D | [#x0951-#x0954] | [#x0962-#x0963] | [#x0981-#x0983] | 
  * #x09BC | #x09BE | #x09BF | [#x09C0-#x09C4] | [#x09C7-#x09C8] | [#x09CB-#x09CD] | 
  * #x09D7 | [#x09E2-#x09E3] | #x0A02 | #x0A3C | #x0A3E | #x0A3F | [#x0A40-#x0A42] | 
  * [#x0A47-#x0A48] | [#x0A4B-#x0A4D] | [#x0A70-#x0A71] | [#x0A81-#x0A83] | #x0ABC | 
  * [#x0ABE-#x0AC5] | [#x0AC7-#x0AC9] | [#x0ACB-#x0ACD] | [#x0B01-#x0B03] | #x0B3C | 
  * [#x0B3E-#x0B43] | [#x0B47-#x0B48] | [#x0B4B-#x0B4D] | [#x0B56-#x0B57] | [#x0B82-#x0B83] | 
  * [#x0BBE-#x0BC2] | [#x0BC6-#x0BC8] | [#x0BCA-#x0BCD] | #x0BD7 | [#x0C01-#x0C03] | 
  * [#x0C3E-#x0C44] | [#x0C46-#x0C48] | [#x0C4A-#x0C4D] | [#x0C55-#x0C56] | [#x0C82-#x0C83] | 
  * [#x0CBE-#x0CC4] | [#x0CC6-#x0CC8] | [#x0CCA-#x0CCD] | [#x0CD5-#x0CD6] | [#x0D02-#x0D03] | 
  * [#x0D3E-#x0D43] | [#x0D46-#x0D48] | [#x0D4A-#x0D4D] | #x0D57 | #x0E31 | [#x0E34-#x0E3A] |
  * [#x0E47-#x0E4E] | #x0EB1 | [#x0EB4-#x0EB9] | [#x0EBB-#x0EBC] | [#x0EC8-#x0ECD] | 
  * [#x0F18-#x0F19] | #x0F35 | #x0F37 | #x0F39 | #x0F3E | #x0F3F | [#x0F71-#x0F84] | 
  * [#x0F86-#x0F8B] | [#x0F90-#x0F95] | #x0F97 | [#x0F99-#x0FAD] | [#x0FB1-#x0FB7] | #x0FB9 | 
  * [#x20D0-#x20DC] | #x20E1 | [#x302A-#x302F] | #x3099 | #x309A  
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
  unsigned char c2 = *(++it);
  unsigned char c3 = *(it+2);
  
  switch (numBytes)
  {
  case 2:
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

/**
  * checks if a character is part of the Extender set
  * Extender ::=  #x00B7 | #x02D0 | #x02D1 | #x0387 | #x0640 | 
  * #x0E46 | #x0EC6 | #x3005 | [#x3031-#x3035] | [#x309D-#x309E] | [#x30FC-#x30FE] 
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
  unsigned char c2 = *(++it);
  unsigned char c3 = *(it+2);
  
  switch (numBytes)
  {
  case 2:
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

  
/**
  * Checks the syntax of a "id"
  * if incorrect, an error is logged
  */
void 
SBase::checkIdSyntax()
{
  const string& id = getId();
  unsigned int size = id.size();

  if (size == 0)
    return;

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

  if (!okay)   
    mSBML->getErrorLog()->logError(10310);
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
