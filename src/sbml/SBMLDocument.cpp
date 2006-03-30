/**
 * \file    SBMLDocument.cpp
 * \brief   Top-level container for all things SBML
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


#include <iostream>

#include "xml/XMLAttributes.h"
#include "xml/XMLInputStream.h"
#include "xml/XMLOutputStream.h"

// #include "validator/ConsistencyValidator.h"      FIXME
// #include "validator/L1CompatibilityValidator.h"  FIXME

#include "Model.h"
#include "SBMLVisitor.h"
#include "SBMLDocument.h"


using namespace std;


/**
 * @return the most recent SBML specification level (at the time this
 * libSBML was released).
 */
unsigned int
SBMLDocument::getDefaultLevel ()
{
  return 2;
}


/**
 * @return the most recent SBML specification version (at the time this
 * libSBML was released).
 */
unsigned int
SBMLDocument::getDefaultVersion ()
{
  return 2;
}


/**
 * Creates a new SBMLDocument.  If not specified, the SBML level and
 * version attributes default to the most recent SBML specification (at the
 * time this libSBML was released).
 */
SBMLDocument::SBMLDocument (unsigned int level, unsigned int version) :
   mLevel  ( level   )
 , mVersion( version )
 , mModel  ( 0       )
{
  if (mLevel   == 0)  mLevel   = getDefaultLevel  ();
  if (mVersion == 0)  mVersion = getDefaultVersion();
}


/**
 * Copies this SBMLDocument.
 */
SBMLDocument::SBMLDocument (const SBMLDocument& rhs) :
   mLevel   ( rhs.mLevel   )
 , mVersion ( rhs.mVersion )
 , mModel   ( 0            )
{
  if (rhs.mModel) mModel = static_cast<Model*>( rhs.mModel->clone() );
}


/**
 * Destroys this SBMLDocument.
 */
SBMLDocument::~SBMLDocument ()
{
  delete mModel;
}


/**
 * Accepts the given SBMLVisitor.
 */
bool
SBMLDocument::accept (SBMLVisitor& v) const
{
  v.visit(*this);
  if (mModel) mModel->accept(v);
  v.leave(*this);

  return true;
}


/**
 * @return a (deep) copy of this SBMLDocument.
 */
SBase*
SBMLDocument::clone () const
{
  return new SBMLDocument(*this);
}


/**
 * @return the Model contained in this SBMLDocument.
 */
const Model*
SBMLDocument::getModel () const
{
  return mModel;
}


/**
 * @return the Model contained in this SBMLDocument.
 */
Model*
SBMLDocument::getModel ()
{
  return mModel;
}


/**
 * Sets the level and version of this SBMLDocument.  Valid
 * combinations are currently:
 *
 *   - Level 1 Version 1
 *   - Level 1 Version 2
 *   - Level 2 Version 1
 *   - Level 2 Version 2
 */
void
SBMLDocument::setLevelAndVersion (unsigned int level, unsigned int version)
{
  mLevel   = level;
  mVersion = version;
}

/**
 * Sets the Model for this SBMLDocument to a copy of the given Model.
 */
void
SBMLDocument::setModel (const Model* m)
{
  if (mModel == m) return;


  delete mModel;
  mModel = (m != 0) ? new Model(*m) : 0;
}


/**
 * Creates a new Model (optionally with its id attribute set) inside this
 * SBMLDocument and returns it.
 */
Model*
SBMLDocument::createModel (const string& sid)
{
  if (mModel) delete mModel;
  mModel = new Model(sid);

  return mModel;
}


/**
 * Performs a set of semantic consistency checks on the document.  Query
 * the results by calling getWarning(), getNumError(),and getNumFatal().
 *
 * @return the number of failed checks (errors) encountered.
 */
unsigned int
SBMLDocument::checkConsistency ()  // FIXME
{
  return 0;
  /*
  unsigned int nerrors = 0;

  ConsistencyValidator validator;
  validator.init();


  if (getModel() == NULL)
  {
    error.add( new ParseMessage(1000, "No model present.", 0, 0) );
    nerrors = 1;
  }
  else
  {
    // HACK: Currently, the Validator uses 'id' instead of 'name',
    // irrespective of Level.
    if (level == 1) getModel()->moveAllNamesToIds();

    nerrors = validator.validate(*this);

    //
    // Add the Validator messages to the SBMLDocument's list of error
    // messages.
    //
    // FIXME: When the custom List class is replaced with an STL List, the
    // code below can be replaced with:
    //
    //   copy(messages.begin(), messages.end(), back_inserter(error));
    //
    const list<ParseMessage>& messages = validator.getMessages();

    list<ParseMessage>::const_iterator end = messages.end();
    list<ParseMessage>::const_iterator iter;

    for (iter = messages.begin(); iter != end; iter++)
    {
      error.add( new ParseMessage(*iter) );
    }

    // HACK: Change back...
    if (level == 1) getModel()->moveAllIdsToNames();
  }

  return nerrors;
  */
}


/**
 * Performs a set of semantic consistency checks on the document to establish
 * whether it is compatible with L1 and can be converted.  Query
 * the results by calling getWarning(), getNumError(),and getNumFatal().
 *
 * @return the number of failed checks (errors) encountered.
 */
unsigned int
SBMLDocument::checkL1Compatibility ()  // FIXME
{
  return 0;
  /*
  unsigned int nerrors = 0;

  L1CompatibilityValidator validator;
  validator.init();


  if (getModel() == NULL)
  {
    error.add( new ParseMessage(1000, "No model present.", 0, 0) );
    nerrors = 1;
  }
  else
  {

    nerrors = validator.validate(*this);

    //
    // Add the Validator messages to the SBMLDocument's list of error
    // messages.
    //
    // FIXME: When the custom List class is replaced with an STL List, the
    // code below can be replaced with:
    //
    //   copy(messages.begin(), messages.end(), back_inserter(error));
    //
    const list<ParseMessage>& messages = validator.getMessages();

    list<ParseMessage>::const_iterator end = messages.end();
    list<ParseMessage>::const_iterator iter;

    for (iter = messages.begin(); iter != end; iter++)
    {
      error.add( new ParseMessage(*iter) );
    }

  }

  return nerrors;
  */
}


/**
 * @return the nth error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumErrors() - 1.
 */
XMLError*
SBMLDocument::getError (unsigned int n)  // FIXME
{
  return 0;
}


/**
 * @return the number of errors encountered during the parse of this
 * SBMLDocument.
 */
unsigned int
SBMLDocument::getNumErrors () const  // FIXME
{
  return 0;
}


/**
 * Prints all errors encountered during the parse of this SBMLDocument to
 * the given stream.  If no errors have occurred, i.e.  getNumErrors() ==
 * 0, no output will be sent to stream. The format of the output is:
 *
 *   N Error(s):
 *     line: (id) message
 */
void
SBMLDocument::printErrors (ostream& stream)  // FIXME
{
  /*
  unsigned int n, size;

  if ((size = getNumErrors()) > 0)
	{
      stream << size << " Error(s):" << endl;
      for (n = 0; n < size; n++) stream << "  " << *(getError(n));
	}
  */
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
SBMLDocument::getTypeCode () const
{
  return SBML_DOCUMENT;
}


/**
 * Subclasses should override this method to return XML element name for
 * this SBML object.
 */
const string&
SBMLDocument::getElementName () const
{
  static const string name = "sbml";
  return name;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
SBMLDocument::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "model")
  {
    delete mModel;

    mModel = new Model();
    object = mModel;
  }

  return object;
}


/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SBMLDocument::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  //
  // level: positiveInteger  { use="required" fixed="1" }  (L1v1)
  // level: positiveInteger  { use="required" fixed="2" }  (L2v1)
  //
  attributes.readInto("level", mLevel);

  //
  // version: positiveInteger  { use="required" fixed="1" }  (L1v1, L2v1)
  // version: positiveInteger  { use="required" fixed="2" }  (L1v2, L2v2)
  //
  attributes.readInto("version", mVersion);
}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SBMLDocument::writeAttributes (XMLOutputStream& stream)
{
  SBase::writeAttributes(stream);

  //
  // level: positiveInteger  { use="required" fixed="1" }  (L1v1)
  // level: positiveInteger  { use="required" fixed="2" }  (L2v1)
  //
  stream.writeAttribute("level", mLevel);

  //
  // version: positiveInteger  { use="required" fixed="1" }  (L1v1, L2v1)
  // version: positiveInteger  { use="required" fixed="2" }  (L1v2, L2v2)
  //
  stream.writeAttribute("version", mVersion);
}


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
SBMLDocument::writeElements (XMLOutputStream& stream)
{
  SBase::writeElements(stream);
  if (mModel) mModel->write(stream);
}


/**
 * Sets the level of this SBMLDocument to the given level number.  Valid
 * levels are currently 1 and 2.
 *
void
SBMLDocument::setLevel (unsigned int level)  // FIXME
{
  mLevel = level;

  unsigned int curLevel = this->level;
  unsigned int newLevel = level;


  if (curLevel == 1 && newLevel == 2)
  {
    this->level = 2;

    if (model != NULL)
    {
      SBML_convertToL2((Model_t *) model, (SBase_t *) this);
    }
  }
  else if (curLevel == 2 && newLevel == 1)
  {
    int nerrors = this->checkL1Compatibility();

	  if (nerrors == 0)
	  {
		  this->level = 1;

		  if (model != NULL)
      {
        SBML_convertModelToL1((Model_t *) model, (SBase_t *) this);
      }
	  }
  }
  else
  {
    this->level = newLevel;
  }

  if (this->level == 2)
  {
    version = 1;
  }
  else if (this->level == 1)
  {
    version = 2;
  }

  if (model != NULL)
  {
    model->mLevel   = this->level;
    model->mVersion = this->version;
  }
}*/


/**
 * Sets the version of this SBMLDocument to the given version number.
 * Valid versions are currently 1 and 2 for SBML L1 and 1 for SBML L2.
 *
void
SBMLDocument::setVersion (unsigned int version)  // FIXME
{
  mVersion = version;

  this->version = version;

  if (model != 0)
  {
    model->mVersion = version;
  }
}*/




/**
 * Creates a new SBMLDocument and returns a pointer to it.
 *
 * The SBML level defaults to 2 and version defaults to 1.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_create ()
{
  return new(nothrow) SBMLDocument;
}


/**
 * Creates a new SBMLDocument with the given level and version.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_createWith (unsigned int level, unsigned int version)
{
  return new(nothrow) SBMLDocument(level, version);
}


/**
 * Frees the given SBMLDocument.
 */
LIBSBML_EXTERN
void
SBMLDocument_free (SBMLDocument_t *d)
{
  delete d;
}


/**
 * @return a (deep) copy of this SBMLDocument.
 */
SBMLDocument_t *
SBMLDocument_clone (const SBMLDocument_t *d)
{
  return static_cast<SBMLDocument_t*>( d->clone() );
}


/**
 * @return the level of this SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getLevel (const SBMLDocument_t *d)
{
  return d->getLevel();
}


/**
 * @return the version of this SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getVersion (const SBMLDocument_t *d)
{
  return d->getVersion();
}


/**
 * @return the Model associated with this SBMLDocument.
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_getModel (SBMLDocument_t *d)
{
  return d->getModel();
}


/**
 * Sets the level and version of this SBMLDocument.  Valid
 * combinations are currently:
 *
 *   - Level 1 Version 1
 *   - Level 1 Version 2
 *   - Level 2 Version 1
 *   - Level 2 Version 2
 */
void
SBMLDocument_setLevelAndVersion (  SBMLDocument_t *d
                                 , unsigned int    level
                                 , unsigned int    version )
{
  d->setLevelAndVersion(level, version);
}


/**
 * Sets the Model for this SBMLDocument to a copy of the given Model.
 */
LIBSBML_EXTERN
void
SBMLDocument_setModel (SBMLDocument_t *d, const Model_t *m)
{
  d->setModel(m);
}


/**
 * Creates a new Model inside this SBMLDocument and returns it.
 */
Model*
SBMLDocument_createModel (SBMLDocument_t *d)
{
  return d->createModel();
}


/**
 * Performs a set of semantic consistency checks on the document.  Query
 * the results by calling getWarning(), getNumError(),and getNumFatal().
 *
 * @return the number of failed checks (errors) encountered.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_checkConsistency (SBMLDocument_t *d)
{
  return d->checkConsistency();
}


/**
 * @return the nth error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumErrors() - 1.
 *
LIBSBML_EXTERN
XMLError_t *
SBMLDocument_getError (SBMLDocument_t *d, unsigned int n)  // FIXME
{
  return 0;
}
*/

/**
 * @return the number of errors encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getNumErrors (const SBMLDocument_t *d)
{
  return d->getNumErrors();
}


/**
 * Prints all errors encountered during the parse of this SBMLDocument to
 * the given stream.  If no errors have occurred, i.e.
 * SBMLDocument_getNumErrors(d) == 0, no output will be sent to stream. The
 * format of the output is:
 *
 *   N Error(s):
 *     line: (id) message
 */
LIBSBML_EXTERN
void
SBMLDocument_printErrors (SBMLDocument_t *d, FILE *stream)
{
  /*
  unsigned int n, size;


  if ((size = SBMLDocument_getNumErrors(d)) > 0)
  {
    printf("%d Error(s):\n", size);
  
    for (n = 0; n < size; n++)
    {
      fprintf(stream, "  ");
      ParseMessage_print(SBMLDocument_getError(d, n), stream);
    }
  }
  */
}
