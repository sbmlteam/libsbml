/**
 * @file    SBMLDocument.cpp
 * @brief   Top-level container for all things SBML
 * @author  Ben Bornstein
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

#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLError.h>

#include <sbml/validator/ConsistencyValidator.h>
#include <sbml/validator/L1CompatibilityValidator.h>
#include <sbml/validator/L2v1CompatibilityValidator.h>
#include <sbml/validator/L2v2CompatibilityValidator.h>

#include <sbml/Model.h>
#include <sbml/SBMLErrorLog.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


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
  return 3;
}


/**
 * Creates a new SBMLDocument.  If not specified, the SBML level and
 * version attributes default to the most recent SBML specification (at the
 * time this libSBML was released).
 */
SBMLDocument::SBMLDocument (unsigned int level, unsigned int version) :
   mLevel   ( level   )
 , mVersion ( version )
 , mModel   ( 0       )
{
  mSBML = this;

  if (mLevel   == 0)  mLevel   = getDefaultLevel  ();
  if (mVersion == 0)  mVersion = getDefaultVersion();
}


/**
 * Destroys this SBMLDocument.
 */
SBMLDocument::~SBMLDocument ()
{
  delete mModel;
}


/**
 * Creates a copy of this SBMLDocument.
 */
SBMLDocument::SBMLDocument (const SBMLDocument& rhs) :
   SBase    ( rhs          )
 , mLevel   ( rhs.mLevel   )
 , mVersion ( rhs.mVersion )
 , mModel   ( 0            )
{
  mSBML = this;

  if (rhs.mModel) mModel = static_cast<Model*>( rhs.mModel->clone() );
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
 *   - Level 2 Version 3
 *
 * @note Some models cannot be converted from their existing
 * level and version to other particular combinations.
 * This function checks whether the required conversion 
 * is possible.
 */
void
SBMLDocument::setLevelAndVersion (unsigned int level, unsigned int version)
{
  if (mModel != 0)
  {
    if (mLevel == 1 && level == 2)
    {
      mModel->convertToL2();
    }
    else if (mLevel == 2)
    {
      if (level == 1 && checkL1Compatibility() == 0)
      {
        mModel->convertToL1();
      }
      /* check for conversion between L2 versions */
      else if (mVersion > 1 && version == 1)
      {
        checkL2v1Compatibility();
      }
      else if (mVersion == 3 && version == 2)
      {
        checkL2v2Compatibility();
      }
    }
  }

  mLevel   = level;
  mVersion = version;

  if (mNamespaces == 0) mNamespaces = new XMLNamespaces;



  if (mLevel == 1)
  {
    mNamespaces->add("http://www.sbml.org/sbml/level1");
  }
  else if (mLevel == 2 && mVersion == 1)
  {
    mNamespaces->add("http://www.sbml.org/sbml/level2");
  }
  else if (mLevel == 2 && mVersion == 2)
  {
    mNamespaces->add("http://www.sbml.org/sbml/level2/version2");
  }
  else if (mLevel == 2 && mVersion == 3)
  {
    mNamespaces->add("http://www.sbml.org/sbml/level2/version3");
  }
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

  if (mModel) mModel->setSBMLDocument(this);
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

  mModel->setSBMLDocument(this);

  return mModel;
}


/**
 * Performs a set of semantic consistency checks on the document.  Query
 * the results by calling getNumErrors() and getError().
 *
 * @return the number of failed checks (errors) encountered.
 */
unsigned int
SBMLDocument::checkConsistency ()
{
  unsigned int nerrors = 0;

  ConsistencyValidator validator;
  validator.init();


  /* this check has become part of readSBML */
  //if (getModel() == 0)
  //{
  //  getErrorLog()->logError(20201, 0);
  //  nerrors = 1;
  //}
  //else
  {
    nerrors += validator.validate(*this);
    if (nerrors) mErrorLog.add( validator.getMessages() );
  }

  return nerrors;
}


/**
 * Performs a set of semantic consistency checks on the document to establish
 * whether it is compatible with L1 and can be converted.  Query
 * the results by calling getNumErrors() and getError().
 *
 * @return the number of failed checks (errors) encountered.
 */
unsigned int
SBMLDocument::checkL1Compatibility ()
{
  if (mModel == 0) return 0;

  L1CompatibilityValidator validator;
  validator.init();

  unsigned int nerrors = validator.validate(*this);
  if (nerrors) mErrorLog.add( validator.getMessages() );

  return nerrors;
}


/**
 * Performs a set of semantic consistency checks on the document to establish
 * whether it is compatible with L2v1 and can be converted.  Query
 * the results by calling getNumErrors() and getError().
 *
 * @return the number of failed checks (errors) encountered.
 */
unsigned int
SBMLDocument::checkL2v1Compatibility ()
{
  if (mModel == 0) return 0;

  L2v1CompatibilityValidator validator;
  validator.init();

  unsigned int nerrors = validator.validate(*this);
  if (nerrors) mErrorLog.add( validator.getMessages() );

  return nerrors;
}


/**
 * Performs a set of semantic consistency checks on the document to establish
 * whether it is compatible with L2v2 and can be converted.  Query
 * the results by calling getNumErrors() and getError().
 *
 * @return the number of failed checks (errors) encountered.
 */
unsigned int
SBMLDocument::checkL2v2Compatibility ()
{
  if (mModel == 0) return 0;

  L2v2CompatibilityValidator validator;
  validator.init();

  unsigned int nerrors = validator.validate(*this);
  if (nerrors) mErrorLog.add( validator.getMessages() );

  return nerrors;
}


/**
 * @return the nth error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumErrors() - 1.
 */
const XMLError*
SBMLDocument::getError (unsigned int n) const
{
  return mErrorLog.getError(n);
}


/**
 * @return the number of errors encountered during the parse of this
 * SBMLDocument.
 */
unsigned int
SBMLDocument::getNumErrors () const
{
  return mErrorLog.getNumErrors();
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
SBMLDocument::printErrors (ostream& stream) const
{
  unsigned int n, size;

  if ((size = getNumErrors()) > 0)
	{
    stream << size << " Error(s):" << endl;
    for (n = 0; n < size; n++) stream << "  " << *(getError(n));
	}
}


/**
 * Sets the parent SBMLDocument of this SBML object.
 */
void
SBMLDocument::setSBMLDocument (SBMLDocument* d)
{
  // No-op
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
 * @return the name of this element ie "sbml".
 */
const string&
SBMLDocument::getElementName () const
{
  static const string name = "sbml";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
SBMLDocument::getElementPosition () const
{
  return 1;
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
  * @return the Namespaces associated with this SBML object
  */
XMLNamespaces* 
SBMLDocument::getNamespaces() const
{
  return SBase::mNamespaces;
}


/**
 * @return the SBMLErrorLog used to log errors during while reading and
 * validating SBML.
 */
SBMLErrorLog*
SBMLDocument::getErrorLog ()
{
  return &mErrorLog;
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
  // version: positiveInteger  { use="required" fixed="3" }  (L2v3)
  //
  attributes.readInto("version", mVersion);

  /* check that sbml namespace has been set */
  unsigned int match = 0;
  if (mNamespaces == NULL)
  {
    getErrorLog()->logError(20101);
  }
  else 
  {
    for (int n = 0; n < mNamespaces->getLength(); n++)
    {
      if (!strcmp(mNamespaces->getURI(n).c_str(), "http://www.sbml.org/sbml/level1"))
      {
        match = 1;
        if (mLevel != 1)
        {
          getErrorLog()->logError(20102);
        }
        if (mVersion != 2)
        {
          getErrorLog()->logError(20103);
        }
        break;
      }
      else if (!strcmp(mNamespaces->getURI(n).c_str(), "http://www.sbml.org/sbml/level2"))
      {
        match = 1;
        if (mLevel != 2)
        {
          getErrorLog()->logError(20102);
        }
        if (mVersion != 1)
        {
          getErrorLog()->logError(20103);
        }
        break;
      }
      else if (!strcmp(mNamespaces->getURI(n).c_str(), "http://www.sbml.org/sbml/level2/version2"))
      {
        match = 1;
        if (mLevel != 2)
        {
          getErrorLog()->logError(20102);
        }
        if (mVersion != 2)
        {
          getErrorLog()->logError(20103);
        }
        break;
      }
      else if (!strcmp(mNamespaces->getURI(n).c_str(), "http://www.sbml.org/sbml/level2/version3"))
      {
        match = 1;
        if (mLevel != 2)
        {
          getErrorLog()->logError(20102);
        }
        if (mVersion != 3)
        {
          getErrorLog()->logError(20103);
        }
        break;
      }
    }
    if (match == 0)
    {
      getErrorLog()->logError(20101);
    }

  }
}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SBMLDocument::writeAttributes (XMLOutputStream& stream) const
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
  // version: positiveInteger  { use="required" fixed="3" }  (L2v3)
  //
  stream.writeAttribute("version", mVersion);
}


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
SBMLDocument::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  if (mModel) mModel->write(stream);
}




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
LIBSBML_EXTERN
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
 *
 * @note Some models cannot be converted from their existing
 * level and version to other particular combinations.
 * This function checks whether the required conversion 
 * is possible.
 */
LIBSBML_EXTERN
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
LIBSBML_EXTERN
Model_t *
SBMLDocument_createModel (SBMLDocument_t *d)
{
  return d->createModel();
}


/**
 * Performs a set of semantic consistency checks on the document.  Query
 * the results by calling getNumErrors() and getError().
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
 * Performs a set of semantic consistency checks on the document to
 * establish whether it is compatible with L1 and can be converted.
 * Query the results by calling getNumErrors() and getError().
 *
 * @return the number of failed checks (errors) encountered.
 */
LIBSBML_EXTERN
unsigned int 
SBMLDocument_checkL1Compatibility (SBMLDocument_t *d)
{
  return d->checkL1Compatibility();
}


/**
 * Performs a set of semantic consistency checks on the document to
 * establish whether it is compatible with L2v1 and can be converted.
 * Query the results by calling getNumErrors() and getError().
 *
 * @return the number of failed checks (errors) encountered.
 */
LIBSBML_EXTERN
unsigned int 
SBMLDocument_checkL2v1Compatibility (SBMLDocument_t *d)
{
  return d->checkL2v1Compatibility();
}



/**
 * Performs a set of semantic consistency checks on the document to
 * establish whether it is compatible with L2v2 and can be converted.
 * Query the results by calling getNumErrors() and getError().
 *
 * @return the number of failed checks (errors) encountered.
 */
LIBSBML_EXTERN
unsigned int 
SBMLDocument_checkL2v2Compatibility (SBMLDocument_t *d)
{
  return d->checkL2v2Compatibility();
}



/**
 * @return the nth error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumErrors() - 1.
 */
LIBSBML_EXTERN
const XMLError_t *
SBMLDocument_getError (SBMLDocument_t *d, unsigned int n)
{
  return d->getError(n);
}


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
  unsigned int size = d->getNumErrors();


  if (size > 0)
  {
    printf("%d Error(s):\n", size);
  
    for (unsigned int n = 0; n < size; n++)
    {
      fprintf(stream, "  ");
      XMLError_print(d->getError(n), stream);
    }
  }
}
