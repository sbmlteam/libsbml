/**
 * \file    CompartmentType.cpp
 * \brief   SBML CompartmentType
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

#include "SBMLVisitor.h"
#include "SBMLDocument.h"
#include "Model.h"
#include "CompartmentType.h"


using namespace std;


/**
 * Creates a new CompartmentType, optionally with its id and name
 * attributes set.
 */
CompartmentType::CompartmentType (const string& id, const string& name) :
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
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
CompartmentType::getElementName () const
{
  static const string name = "compartmentType";
  return name;
}

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
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    mCVTerms = new List();
    parseRDFAnnotation(mAnnotation, mCVTerms);
    checkAnnotation();
    read = true;
  }
  else if (name == "notes")
  {
    delete mNotes;
    mNotes = new XMLNode(stream);
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
CompartmentType::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  //
  // id: SId  { use="required" }  (L2v2)
  //
  attributes.readInto("id", mId);
  SBase::checkIdSyntax();

  //
  // name: string  { use="optional" }  (L2v2)
  //
  attributes.readInto("name", mName);
}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CompartmentType::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  //
  // id: SId  { use="required" }  (L2v2)
  //
  stream.writeAttribute("id", mId);

  //
  // name: string  { use="optional" }  (L2v2)
  //
  stream.writeAttribute("name", mName);
}




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
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
ListOfCompartmentTypes::getElementName () const
{
  static const string name = "listOfCompartmentTypes";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfCompartmentTypes::getElementPosition () const
{
  return 3;
}


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




/**
 * Creates a new CompartmentType and returns a pointer to it.
 */
LIBSBML_EXTERN
CompartmentType_t *
CompartmentType_create ()
{
  return new(nothrow) CompartmentType;
}


/**
 * Creates a new CompartmentType with the given id and name and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
CompartmentType_t *
CompartmentType_createWith (const char *sid, const char *name)
{
  return new(nothrow) CompartmentType(sid ? sid : "", name ? name : "");
}


/**
 * Frees the given CompartmentType.
 */
LIBSBML_EXTERN
void
CompartmentType_free (CompartmentType_t *ct)
{
  delete ct;
}


/**
 * @return a (deep) copy of the given CompartmentType.
 */
LIBSBML_EXTERN
CompartmentType_t *
CompartmentType_clone (const CompartmentType_t *ct)
{
  return static_cast<CompartmentType*>( ct->clone() );
}


/**
 * @return the id of this CompartmentType
 */
LIBSBML_EXTERN
const char *
CompartmentType_getId (const CompartmentType_t *ct)
{
  return ct->isSetId() ? ct->getId().c_str() : NULL;
}


/**
 * @return the name of this CompartmentType.
 */
LIBSBML_EXTERN
const char *
CompartmentType_getName (const CompartmentType_t *ct)
{
  return ct->isSetName() ? ct->getName().c_str() : NULL;
}


/**
 * @return true (non-zero) if the id of this CompartmentType has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
CompartmentType_isSetId (const CompartmentType_t *ct)
{
  return static_cast<int>( ct->isSetId() );
}


/**
 * @return true (non-zero) if the name of this CompartmentType has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
CompartmentType_isSetName (const CompartmentType_t *ct)
{
  return static_cast<int>( ct->isSetName() );
}


/**
 * Sets the id of this CompartmentType to a copy of sid.
 */
LIBSBML_EXTERN
void
CompartmentType_setId (CompartmentType_t *ct, const char *sid)
{
  (sid == NULL) ? ct->unsetId() : ct->setId(sid);
}


/**
 * Sets the name of this CompartmentType to a copy of name.
 */
LIBSBML_EXTERN
void
CompartmentType_setName (CompartmentType_t *ct, const char *name)
{
  (name == NULL) ? ct->unsetName() : ct->setName(name);
}


/**
 * Unsets the name of this CompartmentType.
 */
LIBSBML_EXTERN
void
CompartmentType_unsetName (CompartmentType_t *ct)
{
  ct->unsetName();
}
