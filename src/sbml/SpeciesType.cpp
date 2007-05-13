/**
 * @file    SpeciesType.cpp
 * @brief   Implementation of SpeciesType and ListOfSpeciesTypes.
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

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/SpeciesType.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


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
 * Copy constructor. Creates a copy of this SpeciesType.
 */
SpeciesType::SpeciesType(const SpeciesType& orig) :
      SBase(orig)
{
}


/**
 * Assignment operator
 */
SpeciesType& SpeciesType::operator=(const SpeciesType& rhs)
{
  this->SBase::operator =(rhs);
  return *this;
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
 * @return the name of this element ie "speciesType".
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
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
    mAnnotation = RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
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
    checkXHTML(mNotes);
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
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());
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
    SBO::writeTerm(stream, mSBOTerm);
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
 * @return the name of this element ie "listOfSpeciesTypes".
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



/** @cond doxygen-c-only */


/**
 * Creates a new, empty SpeciesType and returns a pointer to it.
 *
 * It is worth emphasizing that the structure returned by this constructor
 * is empty and that there are no default values assigned to such things as
 * identifiers and names.  Note that in SBML Level 2 and beyond, the
 * "id" (identifier) attribute of a SpeciesType is required to have a
 * value.  Thus, callers are cautioned to assign a value after calling this
 * constructor, for example using SpeciesType_setName().
 *
 * @return a pointer to the newly created SpeciesType structure.
 */
LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_create ()
{
  return new(nothrow) SpeciesType;
}


/**
 * Creates a new SpeciesType with the given @p id and @p name attribute
 * values.
 *
 * In SBML Level 2 and beyond, the identifier attribute of a
 * SpeciesType is required to have a value, but the name is optional.
 * Programs calling this function can legitimately use an empty string for
 * the @p name argument.
 *
 * @param sid the value to assign as the identifier of this SpeciesType
 * @param name the value to assign as the name of this SpeciesType
 *
 * @return a pointer to the newly created SpeciesType_t structure.
 */
LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_createWith (const char *sid, const char *name)
{
  return new(nothrow) SpeciesType(sid ? sid : "", name ? name : "");
}


/**
 * Frees the given SpeciesType_t structure.
 *
 * @param ct the SpeciesType_t structure to be freed.
 */
LIBSBML_EXTERN
void
SpeciesType_free (SpeciesType_t *st)
{
  delete st;
}


/**
 * Creates a deep copy of the given SpeciesType_t structure
 * 
 * @param ct the SpeciesType_t structure to be copied
 * 
 * @return a (deep) copy of this SpeciesType_t structure.
 */
LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_clone (const SpeciesType_t *st)
{
  return static_cast<SpeciesType*>( st->clone() );
}


/**
 * Takes a SpeciesType_t structure and returns its identifier.
 *
 * @param ct the SpeciesType_t structure whose identifier is sought
 * 
 * @return the identifier of this SpeciesType_t, as a pointer to a string.
 */
LIBSBML_EXTERN
const char *
SpeciesType_getId (const SpeciesType_t *st)
{
  return st->isSetId() ? st->getId().c_str() : NULL;
}


/**
 * Takes a SpeciesType_t structure and returns its name.
 *
 * @param ct the SpeciesType_t whose name is sought.
 *
 * @return the name of this SpeciesType_t, as a pointer to a string.
 */
LIBSBML_EXTERN
const char *
SpeciesType_getName (const SpeciesType_t *st)
{
  return st->isSetName() ? st->getName().c_str() : NULL;
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * SpeciesType_t structure's identifier has been set.
 *
 * @param ct the SpeciesType_t structure to query
 * 
 * @return @c non-zero (true) if the "id" field of the given
 * SpeciesType has been set, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesType_isSetId (const SpeciesType_t *st)
{
  return static_cast<int>( st->isSetId() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * SpeciesType_t structure's name has been set.
 *
 * @param ct the SpeciesType_t structure to query
 * 
 * @return @c non-zero (true) if the "name" field of the given
 * SpeciesType has been set, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesType_isSetName (const SpeciesType_t *st)
{
  return static_cast<int>( st->isSetName() );
}


/**
 * Assigns the identifier of a SpeciesType_t structure.
 *
 * This makes a copy of the string passed as the argument @p sid.
 *
 * @param ct the SpeciesType_t structure to set.
 * @param sid the string to use as the identifier.
 */
LIBSBML_EXTERN
void
SpeciesType_setId (SpeciesType_t *st, const char *sid)
{
  (sid == NULL) ? st->unsetId() : st->setId(sid);
}


/**
 * Assign the name of a SpeciesType_t structure.
 *
 * This makes a copy of the string passed as the argument @p name.
 *
 * @param ct the SpeciesType_t structure to set.
 * @param name the string to use as the name.
 */
LIBSBML_EXTERN
void
SpeciesType_setName (SpeciesType_t *st, const char *name)
{
  (name == NULL) ? st->unsetName() : st->setName(name);
}


/**
 * Unsets the name of a SpeciesType.
 * 
 * @param ct the SpeciesType_t structure whose name is to be unset.
 */
LIBSBML_EXTERN
void
SpeciesType_unsetName (SpeciesType_t *st)
{
  st->unsetName();
}



/** @endcond doxygen-c-only */
