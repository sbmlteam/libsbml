/**
 * @file    SBMLDocument.cpp
 * @brief   Implementation of the top-level container for an SBML Model and
 *          associated data. 
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

#include <iostream>

#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLError.h>

#include <sbml/validator/ConsistencyValidator.h>
#include <sbml/validator/IdentifierConsistencyValidator.h>
#include <sbml/validator/MathMLConsistencyValidator.h>
#include <sbml/validator/SBOConsistencyValidator.h>
#include <sbml/validator/UnitConsistencyValidator.h>
#include <sbml/validator/OverdeterminedValidator.h>
#include <sbml/validator/L1CompatibilityValidator.h>
#include <sbml/validator/L2v1CompatibilityValidator.h>
#include <sbml/validator/L2v2CompatibilityValidator.h>
#include <sbml/validator/L2v3CompatibilityValidator.h>

#include <sbml/Model.h>
#include <sbml/SBMLErrorLog.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */

/*
 * Function to check whether an error reported by a compatability validation
 * prior to conversion between levels/versions can be ignored.
 * Some conversions will lose information but the model will still be valid
 * when converted.
 */
static unsigned int ignorable[] = {
  92001,
  92003,
  92004,
  92005,
  92006,
  93001,
  91003,
  91005,
  91006,
  91013
};


/** @cond doxygen-libsbml-internal */
/*
 * Predicate returning true if the errors encountered are not ignorable.
 */
bool
SBMLDocument::conversion_errors(unsigned int errors)
{
  for (unsigned int i = 0; i < errors; i++)
  {
    bool failure = true;

    for (unsigned int n = 0; n < sizeof(ignorable)/sizeof(ignorable[0]); n++)
    {
      if (getError(i)->getId() == ignorable[n])
      {
	      failure = false;
	       break;
      }
    }

    if (failure) return failure;
  }

  return false;
}
/** @endcond doxygen-libsbml-internal */


/**
 * Get the most recent Level of SBML supported by this release of
 * libSBML.
 *
 * This is the "default" level in the sense that libSBML will create
 * models of this SBML Level unless told otherwise.
 * 
 * @return the number representing the most recent SBML specification level
 * (at the time this libSBML was released).
 */
unsigned int
SBMLDocument::getDefaultLevel ()
{
  return 2;
}


/**
 * Get the most recent Version with the most recent Level of SBML supported
 * by this release of libSBML.
 *
 * This is the "default" version in the sense that libSBML will create
 * models of this SBML Level and Version unless told otherwise.
 * 
 * @return the number representing the most recent SBML specification
 * version (at the time this libSBML was released).
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

  // This needs to be fixed some other way.
  //  setLevelAndVersion(mLevel,mVersion);
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
SBMLDocument::SBMLDocument (const SBMLDocument& orig) :
   SBase    ( orig          )
 , mLevel   ( orig.mLevel   )
 , mVersion ( orig.mVersion )
 , mModel   ( 0             )
{
  mSBML = this;

  if (orig.mModel) mModel = static_cast<Model*>( orig.mModel->clone() );
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
 * @li Level 1 Version 1
 * @li Level 1 Version 2
 * @li Level 2 Version 1
 * @li Level 2 Version 2
 * @li Level 2 Version 3
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
      if (level == 1)
      {
        if (!conversion_errors(checkL1Compatibility()))
        {
          mModel->convertToL1();
        }
      }
      /* check for conversion between L2 versions */
      else if (version == 1)
      {
        checkL2v1Compatibility();
      }
      else if (version == 2)
      {
        checkL2v2Compatibility();
      }
      else if (version == 3)
      {
        checkL2v3Compatibility();
      }
    }
  }

  std::ostringstream conversion_note;
  conversion_note << "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n";
  conversion_note << "<p> This sbml model has been converted from SBML Level ";
  conversion_note << mLevel << " Version " << mVersion << ".</p>\n";
  if (getNumErrors() > 0)
  {
    conversion_note << "<p> The following should be noted: </p>\n";
    for (unsigned int n = 0; n < getNumErrors(); n++)
    {
      conversion_note << "<p> (" << getError(n)->getId() << ") " 
        << getError(n)->getMessage() << " </p>\n";
    }
    if (!conversion_errors(getNumErrors()))
    {
      conversion_note << "<p> Conversion successful. </p>\n";
    }
    else
    {
      conversion_note << "<p> Conversion NOT successful. </p>\n";
    }
      
  }
  conversion_note << "</body>\n";

  if (mModel) mModel->appendNotes(conversion_note.str());

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
SBMLDocument::createModel (const std::string& sid)
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
SBMLDocument::checkConsistency (unsigned char checks)
{
  unsigned int nerrors = 0;

  /* determine which validators to run */
  bool id    = ((checks & 0x01) == 0x01);
  bool sbml  = ((checks & 0x02) == 0x02);
  bool sbo   = ((checks & 0x04) == 0x04);
  bool math  = ((checks & 0x08) == 0x08);
  bool units = ((checks & 0x10) == 0x10);
  bool over  = ((checks & 0x20) == 0x20);

  IdentifierConsistencyValidator id_validator;
  ConsistencyValidator validator;
  SBOConsistencyValidator sbo_validator;
  MathMLConsistencyValidator math_validator;
  UnitConsistencyValidator unit_validator;
  OverdeterminedValidator over_validator;

  /* calls each specified validator in turn 
   * - stopping when errors are encountered */

  if (id)
  {
    id_validator.init();
    nerrors += id_validator.validate(*this);
    if (nerrors) 
    {
      mErrorLog.add( id_validator.getMessages() );
      return nerrors;
    }
  }

  if (sbml)
  {
    validator.init();
    nerrors += validator.validate(*this);
    if (nerrors) 
    {
      mErrorLog.add( validator.getMessages() );
      /* only want to bail if errors not warnings */
      if (mErrorLog.getNumSeverityErrors() > 0)
        return nerrors;
    }
  }

  if (sbo)
  {
    sbo_validator.init();
    nerrors += sbo_validator.validate(*this);
    if (nerrors) 
    {
      mErrorLog.add( sbo_validator.getMessages() );
      /* only want to bail if errors not warnings */
      if (mErrorLog.getNumSeverityErrors() > 0)
        return nerrors;
    }
  }

  if (math)
  {
    math_validator.init();
    nerrors += math_validator.validate(*this);
    if (nerrors) 
    {
      mErrorLog.add( math_validator.getMessages() );
      /* only want to bail if errors not warnings */
      if (mErrorLog.getNumSeverityErrors() > 0)
        return nerrors;
    }
  }

  if (units)
  {
    unit_validator.init();
    nerrors += unit_validator.validate(*this);
    if (nerrors) 
    {
      mErrorLog.add( unit_validator.getMessages() );
      /* only want to bail if errors not warnings */
      if (mErrorLog.getNumSeverityErrors() > 0)
        return nerrors;
    }
  }

  if (over)
  {
    over_validator.init();
    nerrors += over_validator.validate(*this);
    if (nerrors) 
    {
      mErrorLog.add( over_validator.getMessages() );
    }
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
 * Performs a set of semantic consistency checks on the document to establish
 * whether it is compatible with L2v3 and can be converted.  Query
 * the results by calling getNumErrors() and getError().
 *
 * @return the number of failed checks (errors) encountered.
 */
unsigned int
SBMLDocument::checkL2v3Compatibility ()
{
  if (mModel == 0) return 0;

  L2v3CompatibilityValidator validator;
  validator.init();

  unsigned int nerrors = validator.validate(*this);
  if (nerrors) mErrorLog.add( validator.getMessages() );

  return nerrors;
}


/**
 * @return the nth error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumErrors() - 1.
 */
const SBMLError*
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
 *   N error(s):
 *     line N: (id) message
 */
void
SBMLDocument::printErrors (std::ostream& stream) const
{
  unsigned int numErrors = getNumErrors();

  if (numErrors > 0)
  {
    stream << numErrors << " error" << ((numErrors > 1) ? "s:" : ":") << endl;
    for (unsigned int n = 0; n < numErrors; n++)
    {
      stream << "  " << *(getError(n));
    }
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


/** @cond doxygen-libsbml-internal */
/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
SBMLDocument::getElementPosition () const
{
  return 1;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
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
/** @endcond doxygen-libsbml-internal */


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


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SBMLDocument::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);
  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("level");
  expectedAttributes.push_back("version");
  expectedAttributes.push_back("metaid");

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    if (std::find(begin, end, name) == end)
    {
      getErrorLog()->logError(SBMLError::NotSchemaConformant, getDefaultLevel(),
        getDefaultVersion(), "Attribute " + name + " is not part of SBMLDocument");
    }
  }


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
    logError(SBMLError::InvalidNamespaceOnSBML);
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
          logError(SBMLError::MissingOrInconsistentLevel);
        }
        if (mVersion != 1 && mVersion != 2)
        {
          logError(SBMLError::MissingOrInconsistentVersion);
        }
       break;
      }
      else if (!strcmp(mNamespaces->getURI(n).c_str(), "http://www.sbml.org/sbml/level2"))
      {
        match = 1;
        if (mLevel != 2)
        {
          logError(SBMLError::MissingOrInconsistentLevel);
        }
        if (mVersion != 1)
        {
          logError(SBMLError::MissingOrInconsistentVersion);
        }
        break;
      }
      else if (!strcmp(mNamespaces->getURI(n).c_str(), "http://www.sbml.org/sbml/level2/version2"))
      {
        match = 1;
        if (mLevel != 2)
        {
          logError(SBMLError::MissingOrInconsistentLevel);
        }
        if (mVersion != 2)
        {
          logError(SBMLError::MissingOrInconsistentVersion);
        }
        break;
      }
      else if (!strcmp(mNamespaces->getURI(n).c_str(), "http://www.sbml.org/sbml/level2/version3"))
      {
        match = 1;
        if (mLevel != 2)
        {
          logError(SBMLError::MissingOrInconsistentLevel);
        }
        if (mVersion != 3)
        {
          logError(SBMLError::MissingOrInconsistentVersion);
        }
        break;
      }
    }
    if (match == 0)
    {
      logError(SBMLError::InvalidNamespaceOnSBML);
    }

  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SBMLDocument::writeAttributes (XMLOutputStream& stream) const
{
  if (mNamespaces == 0)
  {
     XMLNamespaces xmlns;

     if (mLevel == 1)
     {
        xmlns.add("http://www.sbml.org/sbml/level1");
     }
     else if (mLevel == 2 && mVersion == 1)
     {
       xmlns.add("http://www.sbml.org/sbml/level2");
     }
     else if (mLevel == 2 && mVersion == 2)
     {
       xmlns.add("http://www.sbml.org/sbml/level2/version2");
     }
     else if (mLevel == 2 && mVersion == 3)
     {
       xmlns.add("http://www.sbml.org/sbml/level2/version3");
     }
     stream << xmlns;
  }  

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
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
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
/** @endcond doxygen-libsbml-internal */



/** @cond doxygen-c-only */



/**
 * Creates a new, empty SBMLDocument_t structure.
 *
 * The SBML Level and Version attributes default to the most recent SBML
 * specification (at the time this libSBML was released).
 *
 * @return the SBMLDocument_t structure created
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_create ()
{
  return new(nothrow) SBMLDocument;
}


/**
 * Creates a new, empty SBMLDocument_t structure with given values for the
 * SBML Level and Version.
 *
 * If not specified, the SBML Level and Version attributes default to the
 * most recent SBML specification (at the time this libSBML was
 * released).
 *
 * @param level an integer for the SBML Level
 * @param version an integer for the Version within the SBML Level
 *
 * @return the SBMLDocument_t structure created
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_createWithLevelAndVersion (unsigned int level, unsigned int version)
{
  return new(nothrow) SBMLDocument(level, version);
}


/**
 * Frees the given SBMLDocument_t structure.
 *
 * @param d the SBMLDocument_t structure
 */
LIBSBML_EXTERN
void
SBMLDocument_free (SBMLDocument_t *d)
{
  delete d;
}


/**
 * Creates and returns a deep copy of the given SBMLDocument_t structure
 *
 * @param d the SBMLDocument_t structure
 * 
 * @return a (deep) copy of the SBMLDocument_t structure
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_clone (const SBMLDocument_t *d)
{
  return static_cast<SBMLDocument_t*>( d->clone() );
}


/**
 * Returns the SBML Level of the given SBMLDocument_t structure.
 *
 * @param d the SBMLDocument_t structure
 * 
 * @return the SBML Level number
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getLevel (const SBMLDocument_t *d)
{
  return d->getLevel();
}


/**
 * Returns the Version within the SBML Level of the given SBMLDocument_t
 * structure.
 *
 * @param d the SBMLDocument_t structure
 * 
 * @return the version number
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getVersion (const SBMLDocument_t *d)
{
  return d->getVersion();
}


/**
 * Returns the Model object stored in this SBMLDocument_t structure.
 *
 * @param d the SBMLDocument_t structure
 * 
 * @return the Model contained in this SBMLDocument_t structure.
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_getModel (SBMLDocument_t *d)
{
  return d->getModel();
}


/**
 * Sets the SBML Level and Version of this SBMLDocument, attempting to
 * convert the model as needed.
 *
 * This method is used to convert models between Levels and Versions of
 * SBML.  Generally, models can be converted upward without difficulty
 * (e.g., from SBML Level 1 to Level 2, or from an earlier version of
 * Level 2 to the latest version of Level 2).  Sometimes models can be
 * translated downward as well, if they do not use constructs specific to
 * more advanced Levels of SBML.
 *
 * Callers can also check compatibility directly using the methods
 * checkL1Compatibility(), checkL2v1Compatibility(), and 
 * checkL2v2Compatibility().
 * 
 * The valid combinations as of this release of libSBML are the
 * following: 
 *
 * @li Level 1 Version 1
 * @li Level 1 Version 2
 * @li Level 2 Version 1
 * @li Level 2 Version 2
 * @li Level 2 Version 3
 *
 * @param d the SBMLDocument_t structure
 *
 * @param level the desired SBML Level
 *
 * @param version the desired Version within the SBML Level
 *
 * @note Calling this method will not @em necessarily lead to successful
 * conversion.  If the conversion fails, it will be logged in the error
 * list associated with this SBMLDocument_t structure.  Callers should
 * consult getNumErrors() to find out if the conversion succeeded without
 * problems.  For conversions from Level 2 to Level 1, callers can also
 * check the Level of the model after calling this method to find out
 * whether it is Level 1.  (If the conversion to Level 1 failed, the Level
 * of this model will be left unchanged.)
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
 * Sets the model contained in the given SBMLDocument_t structure to a copy
 * of the given Model_t structure.
 *
 * @param d the SBMLDocument_t structure
 *
 * @param m the new Model_t structure to use.
 */
LIBSBML_EXTERN
void
SBMLDocument_setModel (SBMLDocument_t *d, const Model_t *m)
{
  d->setModel(m);
}


/**
 * Creates a new Model_t structure inside the given SBMLDocument_t
 * structure and returns a pointer to it.
 *
 * @param d the SBMLDocument_t structure
 *
 * @return the Model_t structure created
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_createModel (SBMLDocument_t *d)
{
  return d->createModel();
}


/**
 * Performs a set of consistency and validation checks on the given SBML
 * document.
 *
 * Callers should query the results of the consistency check by calling
 * getError().
 *
 * @param d the SBMLDocument_t structure
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
 * Performs a set of consistency checks on the document to establish
 * whether it is compatible with SBML Level 1 and can be converted to
 * Level 1.
 *
 * Callers should query the results of the consistency check by calling
 * getError().
 *
 * @param d the SBMLDocument_t structure
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
 * Performs a set of consistency checks on the document to establish
 * whether it is compatible with SBML Level 2 Version 1 and can be
 * converted to Level 2 Version 1.
 *
 * Callers should query the results of the consistency check by calling
 * getError().
 *
 * @param d the SBMLDocument_t structure
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
 * Performs a set of consistency checks on the document to establish
 * whether it is compatible with SBML Level 2 Version 2 and can be
 * converted to Level 2 Version 2.
 *
 * Callers should query the results of the consistency check by calling
 * getError().
 *
 * @param d the SBMLDocument_t structure
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
 * Performs a set of consistency checks on the document to establish
 * whether it is compatible with SBML Level 2 Version 3 and can be
 * converted to Level 2 Version 3.
 *
 * Callers should query the results of the consistency check by calling
 * getError().
 *
 * @param d the SBMLDocument_t structure
 *
 * @return the number of failed checks (errors) encountered.
 */
LIBSBML_EXTERN
unsigned int 
SBMLDocument_checkL2v3Compatibility (SBMLDocument_t *d)
{
  return d->checkL2v3Compatibility();
}



/**
 * Returns the nth error or warning encountered during parsing,
 * consistency checking, or attempted translation of this model.
 *
 * Callers can use method XMLError_getSeverity() on the result to assess
 * the severity of the problem.  The severity levels range from
 * informationl messages to fatal errors.
 *
 * @return the error or warning indexed by integer @p n, or return NULL
 * if n > (SBMLDocument_getNumErrors() - 1).
 *
 * @param d the SBMLDocument_t structure
 *
 * @param n the index of the error sought.
 *
 * @see SBMLDocument_getNumErrors(), SBMLDocument_setLevelAndVersion(),
 * SBMLDocument_checkConsistency(), SBMLDocument_checkL1Compatibility(),
 * SBMLDocument_checkL2v1Compatibility()
 * SBMLDocument_checkL2v2Compatibility(), SBMLReader_readSBML(),
 * SBMLReader_readSBMLFromString().
 */
LIBSBML_EXTERN
const SBMLError_t *
SBMLDocument_getError (SBMLDocument_t *d, unsigned int n)
{
  return d->getError(n);
}


/**
 * Returns the number of errors or warnings encountered during parsing,
 * consistency checking, or attempted translation of this model.
 *
 * @param d the SBMLDocument_t structure
 *
 * @return the number of errors or warnings encountered
 *
 * @see SBMLDocument_setLevelAndVersion(), SBMLDocument_checkConsistency(),
 * SBMLDocument_checkL1Compatibility(),
 * SBMLDocument_checkL2v1Compatibility()
 * SBMLDocument_checkL2v2Compatibility(), SBMLReader_readSBML(),
 * SBMLReader_readSBMLFromString().
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getNumErrors (const SBMLDocument_t *d)
{
  return d->getNumErrors();
}


/**
 * Prints to the given output stream all the errors or warnings
 * encountered during parsing, consistency checking, or attempted
 * translation of this model.
 *
 * If no errors have occurred, i.e., SBMLDocument_getNumErrors() == 0, no
 * output will be sent to the stream.
 *
 * The format of the output is:
 *
 *   N error(s):
 *     line NNN: (id) message
 *
 * @param d the SBMLDocument_t structure
 * 
 * @param stream the output stream where the messages should be printed
 */
LIBSBML_EXTERN
void
SBMLDocument_printErrors (SBMLDocument_t *d, FILE *stream)
{
  unsigned int numErrors = d->getNumErrors();

  if (numErrors > 0)
  {
    printf("%d error", numErrors);
    printf((numErrors > 1) ? "s:\n" : ":\n");
  
    for (unsigned int n = 0; n < numErrors; n++)
    {
      fprintf(stream, "  ");
      XMLError_print(d->getError(n), stream);
    }
  }
}


/**
 * @return the most recent SBML specification level (at the time this
 * libSBML was released).
 */
unsigned int
SBMLDocument_getDefaultLevel ()
{
  return SBMLDocument::getDefaultLevel();
}


/**
 * @return the most recent SBML specification version (at the time this
 * libSBML was released).
 */
unsigned int
SBMLDocument_getDefaultVersion ()
{
  return SBMLDocument::getDefaultVersion();
}



/** @endcond doxygen-c-only */
