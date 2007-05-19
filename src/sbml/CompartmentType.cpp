/**
 * @file    CompartmentType.cpp
 * @brief   Implementation of CompartmentType and ListOfCompartmentTypes.
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
#include <sbml/CompartmentType.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new CompartmentType, optionally with its id and name
 * attributes set.
 */
CompartmentType::CompartmentType (const std::string& id, const std::string& name) :
  SBase(id, name)
{
}


/**
 * Destroys this CompartmentType.
 */
CompartmentType::~CompartmentType ()
{
}


/**
 * Copy constructor. Creates a copy of this CompartmentType.
 */
CompartmentType::CompartmentType(const CompartmentType& orig) :
      SBase(orig)
{
}


/**
 * Assignment operator
 */
CompartmentType& CompartmentType::operator=(const CompartmentType& rhs)
{
  this->SBase::operator =(rhs);
  return *this;
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next
 * CompartmentType (if available).
 */
bool
CompartmentType::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this CompartmentType.
 */
SBase*
CompartmentType::clone () const
{
  return new CompartmentType(*this);
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
CompartmentType::getTypeCode () const
{
  return SBML_COMPARTMENT_TYPE;
}


/**
 * @return the name of this element ie "compartmentType".
 */
const string&
CompartmentType::getElementName () const
{
  static const string name = "compartmentType";
  return name;
}

/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
CompartmentType::readOtherXML (XMLInputStream& stream)
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
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CompartmentType::readAttributes (const XMLAttributes& attributes)
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
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CompartmentType::writeAttributes (XMLOutputStream& stream) const
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
/** @endcond doxygen-libsbml-internal */



/**
 * @return a (deep) copy of this ListOfCompartmentTypes.
 */
SBase*
ListOfCompartmentTypes::clone () const
{
  return new ListOfCompartmentTypes(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfCompartmentTypes::getItemTypeCode () const
{
  return SBML_COMPARTMENT_TYPE;
}


/**
 * @return the name of this element ie "listOfCompartmentTypes".
 */
const string&
ListOfCompartmentTypes::getElementName () const
{
  static const string name = "listOfCompartmentTypes";
  return name;
}


/** @cond doxygen-libsbml-internal */
/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfCompartmentTypes::getElementPosition () const
{
  return 3;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfCompartmentTypes::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "compartmentType")
  {
    object = new CompartmentType();
    mItems.push_back(object);
  }

  return object;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


/**
 * Creates a new, empty CompartmentType and returns a pointer to it.
 *
 * It is worth emphasizing that the structure returned by this constructor
 * is empty and that there are no default values assigned to such things as
 * identifiers and names.  Note that in SBML Level 2 and beyond, the
 * "id" (identifier) attribute of a CompartmentType is required to have a
 * value.  Thus, callers are cautioned to assign a value after calling this
 * constructor, for example using CompartmentType_setName().
 *
 * @return a pointer to the newly created CompartmentType structure.
 */
LIBSBML_EXTERN
CompartmentType_t *
CompartmentType_create ()
{
  return new(nothrow) CompartmentType;
}


/**
 * Creates a new CompartmentType with the given @p id and @p name attribute
 * values.
 *
 * In SBML Level 2 and beyond, the identifier attribute of a
 * CompartmentType is required to have a value, but the name is optional.
 * Programs calling this function can legitimately use an empty string for
 * the @p name argument.
 *
 * @param sid the value to assign as the identifier of this CompartmentType
 * @param name the value to assign as the name of this CompartmentType
 *
 * @return a pointer to the newly created CompartmentType_t structure.
 */
LIBSBML_EXTERN
CompartmentType_t *
CompartmentType_createWith (const char *sid, const char *name)
{
  return new(nothrow) CompartmentType(sid ? sid : "", name ? name : "");
}


/**
 * Frees the given CompartmentType_t structure.
 *
 * @param ct the CompartmentType_t structure to be freed.
 */
LIBSBML_EXTERN
void
CompartmentType_free (CompartmentType_t *ct)
{
  delete ct;
}


/**
 * Creates a deep copy of the given CompartmentType_t structure
 * 
 * @param ct the CompartmentType_t structure to be copied
 * 
 * @return a (deep) copy of this CompartmentType_t structure.
 */
LIBSBML_EXTERN
CompartmentType_t *
CompartmentType_clone (const CompartmentType_t *ct)
{
  return static_cast<CompartmentType*>( ct->clone() );
}


/**
 * Takes a CompartmentType_t structure and returns its identifier.
 *
 * @param ct the CompartmentType_t structure whose identifier is sought
 * 
 * @return the identifier of this CompartmentType_t, as a pointer to a string.
 */
LIBSBML_EXTERN
const char *
CompartmentType_getId (const CompartmentType_t *ct)
{
  return ct->isSetId() ? ct->getId().c_str() : NULL;
}


/**
 * Takes a CompartmentType_t structure and returns its name.
 *
 * @param ct the CompartmentType_t whose name is sought.
 *
 * @return the name of this CompartmentType_t, as a pointer to a string.
 */
LIBSBML_EXTERN
const char *
CompartmentType_getName (const CompartmentType_t *ct)
{
  return ct->isSetName() ? ct->getName().c_str() : NULL;
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * CompartmentType_t structure's identifier has been set.
 *
 * @param ct the CompartmentType_t structure to query
 * 
 * @return @c non-zero (true) if the "id" field of the given
 * CompartmentType has been set, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
CompartmentType_isSetId (const CompartmentType_t *ct)
{
  return static_cast<int>( ct->isSetId() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * CompartmentType_t structure's name has been set.
 *
 * @param ct the CompartmentType_t structure to query
 * 
 * @return @c non-zero (true) if the "name" field of the given
 * CompartmentType has been set, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
CompartmentType_isSetName (const CompartmentType_t *ct)
{
  return static_cast<int>( ct->isSetName() );
}


/**
 * Assigns the identifier of a CompartmentType_t structure.
 *
 * This makes a copy of the string passed as the argument @p sid.
 *
 * @param ct the CompartmentType_t structure to set.
 * @param sid the string to use as the identifier.
 */
LIBSBML_EXTERN
void
CompartmentType_setId (CompartmentType_t *ct, const char *sid)
{
  (sid == NULL) ? ct->unsetId() : ct->setId(sid);
}


/**
 * Assign the name of a CompartmentType_t structure.
 *
 * This makes a copy of the string passed as the argument @p name.
 *
 * @param ct the CompartmentType_t structure to set.
 * @param name the string to use as the name.
 */
LIBSBML_EXTERN
void
CompartmentType_setName (CompartmentType_t *ct, const char *name)
{
  (name == NULL) ? ct->unsetName() : ct->setName(name);
}


/**
 * Unsets the name of a CompartmentType.
 * 
 * @param ct the CompartmentType_t structure whose name is to be unset.
 */
LIBSBML_EXTERN
void
CompartmentType_unsetName (CompartmentType_t *ct)
{
  ct->unsetName();
}


/** @endcond doxygen-c-only */
