/**
 * \file    SpeciesType.cpp
 * \brief   SBML SpeciesType
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2006 California Institute of Technology and Japan Science and
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


#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include "SBML.h"
#include "SBMLVisitor.h"
#include "SBMLDocument.h"
#include "Model.h"
#include "SpeciesType.h"


using namespace std;


/**
 * Creates a new SpeciesType, optionally with its id and name
 * attributes set.
 */
SpeciesType::SpeciesType (const string& id, const string& name) :
  SBase(id, name)
{
}


/**
 * Destroys this SpeciesType.
 */
SpeciesType::~SpeciesType ()
{
}


/**
 * Copy constructor.
 */
SpeciesType::SpeciesType(const SpeciesType& orig) :
      SBase(orig)
{
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next
 * SpeciesType (if available).
 */
bool
SpeciesType::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this SpeciesType.
 */
SBase*
SpeciesType::clone () const
{
  return new SpeciesType(*this);
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
SpeciesType::getTypeCode () const
{
  return SBML_SPECIES_TYPE;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
SpeciesType::getElementName () const
{
  static const string name = "speciesType";
  return name;
}

/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
SpeciesType::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();


  if (name == "annotation")
  {
    /* if annotation already exists then it is an error 
     */
    if (mAnnotation)
    {
      mSBML->getErrorLog()->logError(10103);
    }
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    mCVTerms = new List();
    parseRDFAnnotation(mAnnotation, mCVTerms);
    mAnnotation = deleteRDFAnnotation(mAnnotation);
    read = true;
  }
  else if (name == "notes")
  {
    /* if notes already exists then it is an error 
     * if annotation already exists then ordering is wrong
     */
    if (mNotes || mAnnotation)
    {
      mSBML->getErrorLog()->logError(10103);
    }

    delete mNotes;
    mNotes = new XMLNode(stream);
    checkNotes();
    read = true;
  }

  return read;
}

/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SpeciesType::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // id: SId  { use="required" }  (L2v2)
  //
  attributes.readInto("id", mId);
  SBase::checkIdSyntax();

  //
  // name: string  { use="optional" }  (L2v2)
  //
  attributes.readInto("name", mName);
  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 3) 
    mSBOTerm = SBML::readSBOTerm(attributes, this->getErrorLog());
}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SpeciesType::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // id: SId  { use="required" }  (L2v2)
  //
  stream.writeAttribute("id", mId);

  //
  // name: string  { use="optional" }  (L2v2)
  //
  stream.writeAttribute("name", mName);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3)
  //
  if (level == 2 && version == 3) 
    SBML::writeSBOTerm(stream, mSBOTerm);
}




/**
 * @return a (deep) copy of this ListOfSpeciesTypes.
 */
SBase*
ListOfSpeciesTypes::clone () const
{
  return new ListOfSpeciesTypes(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfSpeciesTypes::getItemTypeCode () const
{
  return SBML_SPECIES_TYPE;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
ListOfSpeciesTypes::getElementName () const
{
  static const string name = "listOfSpeciesTypes";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfSpeciesTypes::getElementPosition () const
{
  return 4;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfSpeciesTypes::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "speciesType")
  {
    object = new SpeciesType();
    mItems.push_back(object);
  }

  return object;
}




/**
 * Creates a new SpeciesType and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_create ()
{
  return new(nothrow) SpeciesType;
}


/**
 * Creates a new SpeciesType with the given id and name and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_createWith (const char *sid, const char *name)
{
  return new(nothrow) SpeciesType(sid ? sid : "", name ? name : "");
}


/**
 * Frees the given SpeciesType.
 */
LIBSBML_EXTERN
void
SpeciesType_free (SpeciesType_t *st)
{
  delete st;
}


/**
 * @return a (deep) copy of the given SpeciesType.
 */
LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_clone (const SpeciesType_t *st)
{
  return static_cast<SpeciesType*>( st->clone() );
}


/**
 * @return the id of this SpeciesType
 */
LIBSBML_EXTERN
const char *
SpeciesType_getId (const SpeciesType_t *st)
{
  return st->isSetId() ? st->getId().c_str() : NULL;
}


/**
 * @return the name of this SpeciesType.
 */
LIBSBML_EXTERN
const char *
SpeciesType_getName (const SpeciesType_t *st)
{
  return st->isSetName() ? st->getName().c_str() : NULL;
}


/**
 * @return true (non-zero) if the id of this SpeciesType has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesType_isSetId (const SpeciesType_t *st)
{
  return static_cast<int>( st->isSetId() );
}


/**
 * @return true (non-zero) if the name of this SpeciesType has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesType_isSetName (const SpeciesType_t *st)
{
  return static_cast<int>( st->isSetName() );
}


/**
 * Sets the id of this SpeciesType to a copy of sid.
 */
LIBSBML_EXTERN
void
SpeciesType_setId (SpeciesType_t *st, const char *sid)
{
  (sid == NULL) ? st->unsetId() : st->setId(sid);
}


/**
 * Sets the name of this SpeciesType to a copy of name.
 */
LIBSBML_EXTERN
void
SpeciesType_setName (SpeciesType_t *st, const char *name)
{
  (name == NULL) ? st->unsetName() : st->setName(name);
}


/**
 * Unsets the name of this SpeciesType.
 */
LIBSBML_EXTERN
void
SpeciesType_unsetName (SpeciesType_t *st)
{
  st->unsetName();
}
