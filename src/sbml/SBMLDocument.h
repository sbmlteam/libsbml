/**
 * \file    SBMLDocument.h
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


#ifndef SBMLDocument_h
#define SBMLDocument_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <iosfwd>

#include <sbml/SBMLErrorLog.h>
#include <sbml/SBase.h>


class Model;
class SBMLVisitor;
class XMLError;


class LIBSBML_EXTERN SBMLDocument: public SBase
{
public:

  /**
   * @return the most recent SBML specification level (at the time this
   * libSBML was released).
   */
  static unsigned int getDefaultLevel ();

  /**
   * @return the most recent SBML specification version (at the time this
   * libSBML was released).
   */
  static unsigned int getDefaultVersion ();


  /**
   * Creates a new SBMLDocument.  If not specified, the SBML level and
   * version attributes default to the most recent SBML specification (at
   * the time this libSBML was released).
   */
  SBMLDocument (unsigned int level = 0, unsigned int version = 0);

  /**
   * Destroys this SBMLDocument.
   */
  virtual ~SBMLDocument ();


  /**
   * Creates a copy of this SBMLDocument.
   */
  SBMLDocument (const SBMLDocument& rhs);

  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this SBMLDocument.
   */
  virtual SBase* clone () const;


  /**
   * @return the Model contained in this SBMLDocument.
   */
  const Model* getModel () const;

  /**
   * @return the Model contained in this SBMLDocument.
   */
  Model* getModel ();


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
  void setLevelAndVersion (unsigned int level, unsigned int version);

  /**
   * Sets the Model for this SBMLDocument to a copy of the given Model.
   */
  void setModel (const Model* m);


  /**
   * Creates a new Model (optionally with its id attribute set) inside this
   * SBMLDocument and returns it.
   */
  Model* createModel (const std::string& sid = "");


  /**
   * Performs a set of semantic consistency checks on the document.  Query
   * the results by calling getNumErrors() and getError().
   *
   * @return the number of failed checks (errors) encountered.
   */
  unsigned int checkConsistency ();

  /**
   * Performs a set of semantic consistency checks on the document to
   * establish whether it is compatible with L1 and can be converted.
   * Query the results by calling getNumErrors() and getError().
   *
   * @return the number of failed checks (errors) encountered.
   */
  unsigned int checkL1Compatibility ();


  /**
   * Performs a set of semantic consistency checks on the document to
   * establish whether it is compatible with L2v2 and can be converted.
   * Query the results by calling getNumErrors() and getError().
   *
   * @return the number of failed checks (errors) encountered.
   */
  unsigned int checkL2v1Compatibility ();


  /**
   * Performs a set of semantic consistency checks on the document to
   * establish whether it is compatible with L2v1 and can be converted.
   * Query the results by calling getNumErrors() and getError().
   *
   * @return the number of failed checks (errors) encountered.
   */
  unsigned int checkL2v2Compatibility ();


  /**
   * @return the nth error encountered during the parse of this
   * SBMLDocument or NULL if n > getNumErrors() - 1.
   */
  const XMLError* getError (unsigned int n) const;

  /**
   * @return the number of errors encountered during the parse of this
   * SBMLDocument.
   */
  unsigned int getNumErrors () const;

  /**
   * Prints all errors encountered during the parse of this SBMLDocument to
   * the given stream.  If no errors have occurred, i.e.  getNumErrors() ==
   * 0, no output will be sent to stream. The format of the output is:
   *
   *   N Error(s):
   *     line: (id) message
   */
  void printErrors (std::ostream& stream) const;


  /**
   * Sets the parent SBMLDocument of this SBML object.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;

  /**
   * @return the name of this element ie "sbml".
   */
  virtual const std::string& getElementName () const;

  /**
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  int getElementPosition () const;

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;

  /**
   * @return the SBMLErrorLog used to log errors during while reading and
   * validating SBML.
   */
  SBMLErrorLog* getErrorLog ();

  /**
   * @return the Namespaces associated with this SBML object
   */
  virtual XMLNamespaces* getNamespaces() const;


protected:

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual
  void readAttributes (const XMLAttributes& attributes);

  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  int mLevel;
  int mVersion;

  Model* mModel;

  SBMLErrorLog mErrorLog;

  friend class SBase;
  friend class SBMLReader;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include <stdio.h>


/**
 * Creates a new SBMLDocument and returns a pointer to it.
 *
 * The SBML level defaults to 2 and version defaults to 1.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_create (void);

/**
 * Creates a new SBMLDocument with the given level and version.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_createWith (unsigned int level, unsigned int version);

/**
 * Frees the given SBMLDocument.
 */
LIBSBML_EXTERN
void
SBMLDocument_free (SBMLDocument_t *d);

/**
 * @return a (deep) copy of this SBMLDocument.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_clone (const SBMLDocument_t *d);


/**
 * @return the level of this SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getLevel (const SBMLDocument_t *d);

/**
 * @return the version of this SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getVersion (const SBMLDocument_t *d);

/**
 * @return the Model associated with this SBMLDocument.
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_getModel (SBMLDocument_t *d);


/**
 * Sets the level and version of this SBMLDocument.  Valid
 * combinations are currently:
 *
 *   - Level 1 Version 1
 *   - Level 1 Version 2
 *   - Level 2 Version 1
 *   - Level 2 Version 2
 * @note Some models cannot be converted from their existing
 * level and version to other particular combinations.
 * This function checks whether the required conversion 
 * is possible.
 */
LIBSBML_EXTERN
void
SBMLDocument_setLevelAndVersion (  SBMLDocument_t *d
                                 , unsigned int    level
                                 , unsigned int    version );

/**
 * Sets the Model for this SBMLDocument to a copy of the given Model.
 */
LIBSBML_EXTERN
void
SBMLDocument_setModel (SBMLDocument_t *d, const Model_t *m);


/**
 * Creates a new Model inside this SBMLDocument and returns it.
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_createModel (SBMLDocument_t *d);


/**
 * Performs a set of semantic consistency checks on the document.  Query
 * the results by calling getNumErrors() and getError().
 *
 * @return the number of failed checks (errors) encountered.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_checkConsistency (SBMLDocument_t *d);


/**
 * @return the nth error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumErrors() - 1.
 */
LIBSBML_EXTERN
const XMLError_t *
SBMLDocument_getError (SBMLDocument_t *d, unsigned int n);

/**
 * @return the number of errors encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getNumErrors (const SBMLDocument_t *d);

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
SBMLDocument_printErrors (SBMLDocument_t *d, FILE *stream);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* SBMLDocument_h */
