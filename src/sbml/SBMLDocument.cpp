/**
 * \file    SBMLDocument.cpp
 * \brief   Top-level container for all things SBML
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include <iostream>
#include <list>

#include "validator/ConsistencyValidator.h"
#include "validator/L1CompatibilityValidator.h"
#include "util/StringBuffer.h"
#include "xml/ParseMessage.h"

#include "SBMLConvert.h"
#include "SBMLVisitor.h"
#include "Model.h"

#include "SBMLDocument.h"


using namespace std;


/**
 * Creates a new SBMLDocument.  The SBML level defaults to 2 and version
 * defaults to 1.
 */
LIBSBML_EXTERN
SBMLDocument::SBMLDocument (unsigned int level, unsigned int version) :
    SBase  ()
  , level  ( level   )
  , version( version )
  , model  ( NULL )
{
  init(SBML_DOCUMENT);
}


/**
 * Destroys this SBMLDocument.
 */
LIBSBML_EXTERN
SBMLDocument::~SBMLDocument ()
{
  delete model;
}


/**
 * Accepts the given SBMLVisitor.
 */
LIBSBML_EXTERN
void
SBMLDocument::accept (SBMLVisitor& v) const
{
  v.visit(*this);
  if (model != NULL) model->accept(v);
  v.leave(*this);
}


/**
 * Creates a new Model (optionally with its Id attribute set) inside this
 * SBMLDocument and returns it.  This covenience method is equivalent to:
 *
 *   setModel( Model() );
 */
LIBSBML_EXTERN
Model&
SBMLDocument::createModel (const std::string& sid)
{
  Model* m = new Model(sid);


  setModel(m);
  return *m;
}


/**
 * @return the level of this SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument::getLevel () const
{
  return level;
}


/**
 * @return the version of this SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument::getVersion () const
{
  return version;
}


/**
 * @return the Model associated with this SBMLDocument.
 */
LIBSBML_EXTERN
Model*
SBMLDocument::getModel ()
{
  return model;
}


/**
 * @return the nth warning encountered during the parse of this
 * SBMLDocument or NULL if n > getNumWarnings() - 1.
 */
LIBSBML_EXTERN
ParseMessage*
SBMLDocument::getWarning (unsigned int n)
{
  return static_cast<ParseMessage*>( warning.get(n) );
}


/**
 * @return the nth error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumErrors() - 1.
 */
LIBSBML_EXTERN
ParseMessage*
SBMLDocument::getError (unsigned int n)
{
  return static_cast<ParseMessage*>( error.get(n) );
}


/**
 * @return the nth fatal error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumFatals() - 1.
 */
LIBSBML_EXTERN
ParseMessage*
SBMLDocument::getFatal (unsigned int n)
{
  return static_cast<ParseMessage*>( fatal.get(n) );
}


/**
 * @return the number of warnings encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument::getNumWarnings () const
{
  return warning.getSize();
}


/**
 * @return the number of errors encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument::getNumErrors () const
{
  return error.getSize();
}


/**
 * @return the number of fatal errors encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument::getNumFatals () const
{
  return fatal.getSize();
}


/**
 * Prints all warnings encountered during the parse of this SBMLDocument to
 * the given stream.  If no warnings have occurred, i.e.  getNumWarnings()
 * == 0, no output will be sent to stream. The format of the output is:
 *
 *   N Warning(s):
 *     line: (id) message
 */
LIBSBML_EXTERN
void
SBMLDocument::printWarnings (std::ostream& stream)
{
  unsigned int n, size;

  /* HACK for MSVC6 support
   * stream can only print out chars!!
   */

  /* commented out for testing
  #if _MSC_VER < 7000
    if ((size = getNumWarnings()) > 0)
	{
      cout << size << " Warning(s):" << endl;
      for (n = 0; n < size; n++) cout << "  " << *(getWarning(n));
	}

  #else
  */
    if ((size = getNumWarnings()) > 0)
	{
      stream << size << " Warning(s):" << endl;
      for (n = 0; n < size; n++) stream << "  " << *(getWarning(n));
	}
 // #endif



 }


/**
 * Prints all errors encountered during the parse of this SBMLDocument to
 * the given stream.  If no errors have occurred, i.e.  getNumErrors() ==
 * 0, no output will be sent to stream. The format of the output is:
 *
 *   N Error(s):
 *     line: (id) message
 */
LIBSBML_EXTERN
void
SBMLDocument::printErrors (ostream& stream)
{
  unsigned int n, size;

  /* HACK for MSVC6 support
   * stream can only print out chars!!
   */
  /* commented out for testing

  #if _MSC_VER < 7000
    if ((size = getNumErrors()) > 0)
	{
      cout << size << " Error(s):" << endl;
      for (n = 0; n < size; n++) cout << "  " << *(getError(n));
	}

  #else
*/
    if ((size = getNumErrors()) > 0)
	{
      stream << size << " Error(s):" << endl;
      for (n = 0; n < size; n++) stream << "  " << *(getError(n));
	}
//  #endif
}


/**
 * Prints all fatals encountered during the parse of this SBMLDocument to
 * the given stream.  If no fatals have occurred, i.e.  getNumFatals() ==
 * 0, no output will be sent to stream. The format of the output is:
 *
 *   N Fatal(s):
 *     line: (id) message
 */
LIBSBML_EXTERN
void
SBMLDocument::printFatals (ostream& stream)
{
  unsigned int n, size;

  /* HACK for MSVC6 support
   * stream can only print out chars!!
   */
  /* commented out for testing

  #if _MSC_VER < 7000
    if ((size = getNumFatals()) > 0)
	{
      cout << size << " Fatal(s):" << endl;
      for (n = 0; n < size; n++) cout << "  " << *(getFatal(n));
	}

  #else
*/
    if ((size = getNumFatals()) > 0)
	{
      stream << size << " Fatal(s):" << endl;
      for (n = 0; n < size; n++) stream << "  " << *(getFatal(n));
	}
//  #endif

}


/**
 * Sets the level of this SBMLDocument to the given level number.  Valid
 * levels are currently 1 and 2.
 */
LIBSBML_EXTERN
void
SBMLDocument::setLevel (unsigned int level)
{
  if (this->level == 1 && level == 2)
  {
    this->level = 2;
    if (model != NULL) SBML_convertToL2((Model_t *) model, (SBase_t *) this);
  }
  else if (this->level == 2 && level == 1)
  {
	  /* put in consistency check */
    int nerrors = this->checkL1Compatibility();

	  if (nerrors == 0)
	  {
		  this->level = 1;
		  this->version = 2;

		  if (model != NULL) SBML_convertModelToL1((Model_t *) model, (SBase_t *) this);
	  }

  }
  else
  {
    this->level = level;

    if (this->level == 2)
    {
      version = 1;
    }
    else if (this->level == 1)
    {
      version = 2;
    }
  }
}


/**
 * Sets the version of this SBMLDocument to the given version number.
 * Valid versions are currently 1 and 2 for SBML L1 and 1 for SBML L2.
 */
LIBSBML_EXTERN
void
SBMLDocument::setVersion (unsigned int version)
{
  this->version = version;
}


/**
 * Sets the Model of this SBMLDocument to the given Model.
 * Any previously defined model is unset and freed.
 */
LIBSBML_EXTERN
void
SBMLDocument::setModel (Model* m)
{
  if (model == m) return;


  delete model;
  model = m;
}


/**
 * Performs a set of semantic consistency checks on the document.  Query
 * the results by calling getWarning(), getNumError(),and getNumFatal().
 *
 * @return the number of failed checks (errors) encountered.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument::checkConsistency ()
{
  unsigned int nerrors = 0;

  ConsistencyValidator validator;
  validator.init();


  if (getModel() == NULL)
  {
    List_add(&error, ParseMessage_createWith(1000, "No model present.", 0, 0));
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
}


/**
 * @deprecated use checkConsistency() instead.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument::validate ()
{
  return checkConsistency();
}

/**
 * Performs a set of semantic consistency checks on the document to establish
 * whether it is compatible with L1 and can be converted.  Query
 * the results by calling getWarning(), getNumError(),and getNumFatal().
 *
 * @return the number of failed checks (errors) encountered.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument::checkL1Compatibility ()
{
  unsigned int nerrors = 0;

  L1CompatibilityValidator validator;
  validator.init();


  if (getModel() == NULL)
  {
    List_add(&error, ParseMessage_createWith(1000, "No model present.", 0, 0));
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
}




/**
 * Creates a new SBMLDocument and returns a pointer to it.
 *
 * The SBML level defaults to 2 and version defaults to 1.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_create (void)
{
  return new(std::nothrow) SBMLDocument;
}


/**
 * Creates a new SBMLDocument with the given level and version.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_createWith (unsigned int level, unsigned int version)
{
  return new(std::nothrow) SBMLDocument(level, version);
}


/**
 * Creates a new Model inside this SBMLDocument and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   d->model = Model_create();
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_createModel (SBMLDocument_t *d)
{
  return (Model_t *) & static_cast<SBMLDocument*>(d)->createModel();
}


/**
 * Creates a new Model inside this SBMLDocument and returns a pointer to
 * it.  The name field of this Model is set to a copy of sid.
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_createModelWith (SBMLDocument_t *d, const char *sid)
{
  return (Model_t *) &
         static_cast<SBMLDocument*>(d)->createModel(sid ? sid : "");
}


/**
 * Frees the given SBMLDocument.
 */
LIBSBML_EXTERN
void
SBMLDocument_free (SBMLDocument_t *d)
{
  if (d == NULL) return;

  delete static_cast<SBMLDocument*>(d);
}


/**
 * @return the level of this SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getLevel (const SBMLDocument_t *d)
{
  return static_cast<const SBMLDocument*>(d)->getLevel();
}


/**
 * @return the version of this SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getVersion (const SBMLDocument_t *d)
{
  return static_cast<const SBMLDocument*>(d)->getVersion();
}


/**
 * @return the Model associated with this SBMLDocument.
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_getModel (SBMLDocument_t *d)
{
  return (Model_t *) static_cast<SBMLDocument*>(d)->getModel();
}


/**
 * @return the nth warning encountered during the parse of this
 * SBMLDocument or NULL if n > getNumWarnings() - 1.
 */
LIBSBML_EXTERN
ParseMessage_t *
SBMLDocument_getWarning (SBMLDocument_t *d, unsigned int n)
{
  return static_cast<SBMLDocument*>(d)->getWarning(n);
}


/**
 * @return the nth error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumErrors() - 1.
 */
LIBSBML_EXTERN
ParseMessage_t *
SBMLDocument_getError (SBMLDocument_t *d, unsigned int n)
{
  return static_cast<SBMLDocument*>(d)->getError(n);
}


/**
 * @return the nth fatal error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumFatals() - 1.
 */
LIBSBML_EXTERN
ParseMessage_t *
SBMLDocument_getFatal (SBMLDocument_t *d, unsigned int n)
{
  return static_cast<SBMLDocument*>(d)->getFatal(n);
}


/**
 * @return the number of warnings encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getNumWarnings (const SBMLDocument_t *d)
{
  return static_cast<const SBMLDocument*>(d)->getNumWarnings();
}


/**
 * @return the number of errors encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getNumErrors (const SBMLDocument_t *d)
{
  return static_cast<const SBMLDocument*>(d)->getNumErrors();
}


/**
 * @return the number of fatal errors encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getNumFatals (const SBMLDocument_t *d)
{
  return static_cast<const SBMLDocument*>(d)->getNumFatals();
}


/**
 * Prints all warnings encountered during the parse of this SBMLDocument to
 * the given stream.  If no warnings have occurred, i.e.
 * SBMLDocument_getNumWarnings(d) == 0, no output will be sent to
 * stream. The format of the output is:
 *
 *   N Warning(s):
 *     line: (id) message
 */
LIBSBML_EXTERN
void
SBMLDocument_printWarnings (SBMLDocument_t *d, FILE *stream)
{
  unsigned int n, size;

  if ((size = SBMLDocument_getNumWarnings(d)) > 0)
  {
    printf("%d Warning(s):\n", size);

  /*
    HACK for MSVC
    The stream pointer gets lost in a MSVC compile
  */
  /* commented out for testing

  #ifdef _MSC_VER
	for (n = 0; n < size; n++)
    {
      printf("  ");
      ParseMessage_print(SBMLDocument_getWarning(d, n), stream);
    }

  #else
 
   */
	for (n = 0; n < size; n++)
    {
      fprintf(stream, "  ");
      ParseMessage_print(SBMLDocument_getWarning(d, n), stream);
    }

 // #endif
  }
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
  unsigned int n, size;


  if ((size = SBMLDocument_getNumErrors(d)) > 0)
  {
    printf("%d Error(s):\n", size);
  /*
    HACK for MSVC
    The stream pointer gets lost in a MSVC compile
  */
  /* commented out for testing

  #ifdef _MSC_VER
	for (n = 0; n < size; n++)
    {
      printf("  ");
      ParseMessage_print(SBMLDocument_getError(d, n), stream);
    }

  #else
*/
	for (n = 0; n < size; n++)
    {
      fprintf(stream, "  ");
      ParseMessage_print(SBMLDocument_getError(d, n), stream);
    }

 // #endif
  }
}


/**
 * Prints all fatals encountered during the parse of this SBMLDocument to
 * the given stream.  If no fatals have occurred, i.e.
 * SBMLDocument_getNumFatals(d) == 0, no output will be sent to stream. The
 * format of the output is:
 *
 *   N Fatal(s):
 *     line: (id) message
 */
LIBSBML_EXTERN
void
SBMLDocument_printFatals (SBMLDocument_t *d, FILE *stream)
{
  unsigned int n, size;


  if ((size = SBMLDocument_getNumFatals(d)) > 0)
  {
    printf("%d Fatal(s):\n", size);
  /*
    HACK for MSVC
    The stream pointer gets lost in a MSVC compile
  */
  /* commented out for testing

  #ifdef _MSC_VER
	for (n = 0; n < size; n++)
    {
      printf("  ");
      ParseMessage_print(SBMLDocument_getFatal(d, n), stream);
    }

  #else
*/
   for (n = 0; n < size; n++)
    {
      fprintf(stream, "  ");
      ParseMessage_print(SBMLDocument_getFatal(d, n), stream);
    }

 // #endif

  }
}


/**
 * Sets the level of this SBMLDocument to the given level number.  Valid
 * levels are currently 1 and 2.
 */
LIBSBML_EXTERN
void
SBMLDocument_setLevel (SBMLDocument_t *d, unsigned int level)
{
  static_cast<SBMLDocument*>(d)->setLevel(level);
}


/**
 * Sets the version of this SBMLDocument to the given version number.
 * Valid versions are currently 1 and 2 for SBML L1 and 1 for SBML L2.
 */
LIBSBML_EXTERN
void
SBMLDocument_setVersion (SBMLDocument_t *d, unsigned int version)
{
  static_cast<SBMLDocument*>(d)->setVersion(version);
}


/**
 * Sets the Model of this SBMLDocument to the given Model.
 * Any previously defined model is unset and freed.
 */
LIBSBML_EXTERN
void
SBMLDocument_setModel (SBMLDocument_t *d, Model_t *m)
{
  static_cast<SBMLDocument*>(d)->setModel( static_cast<Model*>(m) );
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
  return static_cast<SBMLDocument*>(d)->checkConsistency();
}


/**
 * @deprecated use SBMLDocument_checkConsistency() instead.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_validate (SBMLDocument_t *d)
{
  return static_cast<SBMLDocument*>(d)->validate();
}
