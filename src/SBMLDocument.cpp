/**
 * Filename    : SBMLDocument.cpp
 * Description : Top-level container for all things SBML
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-10-14
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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

#include "sbml/SBMLConvert.h"
#include "sbml/StringBuffer.h"
#include "sbml/Validator.h"

#include "sbml/ParseMessage.hpp"
#include "sbml/Model.hpp"
#include "sbml/SBMLVisitor.hpp"

#include "sbml/SBMLDocument.h"
#include "sbml/SBMLDocument.hpp"


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
 *   %d Warning(s):
 *     Line %d, Col %d: %s
 *     ...
 *
 * This is a convenience method to aid in debugging.  For example:
 * printWarnings(cout).
 */
LIBSBML_EXTERN
void
SBMLDocument::printWarnings (std::ostream& stream)
{
  unsigned int   n, size;
  ParseMessage*  pm;


  if ((size = getNumWarnings()) > 0)
  {
    stream << size << " Warning(s):" << std::endl;

    for (n = 0; n < size; n++)
    {
      pm = getWarning(n);
      stream << "  Line " << pm->getLine()    <<
                ", Col "  << pm->getColumn()  <<
                ": "      << pm->getMessage() << std::endl;
    }

    stream << std::endl;
  }
}


/**
 * Prints all errors encountered during the parse of this SBMLDocument to
 * the given stream.  If no errors have occurred, i.e.  getNumErrors() ==
 * 0, no output will be sent to stream. The format of the output is:
 *
 *   %d Error(s):
 *     Line %d, Col %d: %s
 *     ...
 *
 * This is a convenience method to aid in debugging.  For example:
 * printErrors(cout).
 */
LIBSBML_EXTERN
void
SBMLDocument::printErrors (std::ostream& stream)
{
  unsigned int   n, size;
  ParseMessage*  pm;


  if ((size = getNumErrors()) > 0)
  {
    stream << size << " Error(s):" << std::endl;

    for (n = 0; n < size; n++)
    {
      pm = getError(n);
      stream << "  Line " << pm->getLine()    <<
                ", Col "  << pm->getColumn()  <<
                ": "      << pm->getMessage() << std::endl;
    }

    stream << std::endl;
  }
}


/**
 * Prints all fatals encountered during the parse of this SBMLDocument to
 * the given stream.  If no fatals have occurred, i.e.  getNumFatals() ==
 * 0, no output will be sent to stream. The format of the output is:
 *
 *   %d Fatal(s):
 *     Line %d, Col %d: %s
 *     ...
 *
 * This is a convenience method to aid in debugging.  For example:
 * printFatals(d, cout).
 */
LIBSBML_EXTERN
void
SBMLDocument::printFatals (std::ostream& stream)
{
  unsigned int   n, size;
  ParseMessage*  pm;


  if ((size = getNumFatals()) > 0)
  {
    stream << size << " Fatal(s):" << std::endl;

    for (n = 0; n < size; n++)
    {
      pm = getFatal(n);
      stream << "  Line " << pm->getLine()    <<
                ", Col "  << pm->getColumn()  <<
                ": "      << pm->getMessage() << std::endl;
    }

    stream << std::endl;
  }
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
  else
  {
    this->level = level;

    if (this->level == 2)
    {
      version = 1;
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
  unsigned int  nerrors;
  Validator_t*  v = Validator_createDefault();


  if (getModel() == NULL)
  {
    List_add(&error, ParseMessage_createWith(1000, "No model present.", 0, 0));
    nerrors = 1;
  }
  else
  {
    /**
     * HACK: Currently, the Validator uses 'id' instead of 'name',
     * irrespective of Level.
     */
    if (level == 1) getModel()->moveAllNamesToIds();

    nerrors = Validator_validate(v, (SBMLDocument_t*) this, (List_t*) &error);
    Validator_free(v);

    /* HACK: Change back... */
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
 *   %d Warning(s):
 *     Line %d, Col %d: %s
 *     ...
 *
 * This is a convenience function to aid in debugging.  For example:
 * SBMLDocument_printWarnings(d, stdout).
 */
LIBSBML_EXTERN
void
SBMLDocument_printWarnings (SBMLDocument_t *d, FILE *stream)
{
  unsigned int   n, size;
  ParseMessage_t *pm;


  if ((size = SBMLDocument_getNumWarnings(d)) > 0)
  {
    printf("%d Warning(s):\n", size);

    for (n = 0; n < size; n++)
    {
      pm = SBMLDocument_getWarning(d, n);
      printf( "  Line %d, Col %d: %s\n"
              , ParseMessage_getLine   (pm)
              , ParseMessage_getColumn (pm)
              , ParseMessage_getMessage(pm) );
    }

    printf("\n");
  }
}


/**
 * Prints all errors encountered during the parse of this SBMLDocument to
 * the given stream.  If no errors have occurred, i.e.
 * SBMLDocument_getNumErrors(d) == 0, no output will be sent to stream. The
 * format of the output is:
 *
 *   %d Error(s):
 *     Line %d, Col %d: %s
 *     ...
 *
 * This is a convenience function to aid in debugging.  For example:
 * SBMLDocument_printErrors(d, stdout).
 */
LIBSBML_EXTERN
void
SBMLDocument_printErrors (SBMLDocument_t *d, FILE *stream)
{
  unsigned int   n, size;
  ParseMessage_t *pm;


  if ((size = SBMLDocument_getNumErrors(d)) > 0)
  {
    printf("%d Error(s):\n", size);

    for (n = 0; n < size; n++)
    {
      pm = SBMLDocument_getError(d, n);
      printf( "  Line %d, Col %d: %s\n"
              , ParseMessage_getLine   (pm)
              , ParseMessage_getColumn (pm)
              , ParseMessage_getMessage(pm) );
    }

    printf("\n");
  }
}


/**
 * Prints all fatals encountered during the parse of this SBMLDocument to
 * the given stream.  If no fatals have occurred, i.e.
 * SBMLDocument_getNumFatals(d) == 0, no output will be sent to stream. The
 * format of the output is:
 *
 *   %d Fatal(s):
 *     Line %d, Col %d: %s
 *     ...
 *
 * This is a convenience function to aid in debugging.  For example:
 * SBMLDocument_printFatals(d, stdout).
 */
LIBSBML_EXTERN
void
SBMLDocument_printFatals (SBMLDocument_t *d, FILE *stream)
{
  unsigned int   n, size;
  ParseMessage_t *pm;


  if ((size = SBMLDocument_getNumFatals(d)) > 0)
  {
    printf("%d Fatal(s):\n", size);

    for (n = 0; n < size; n++)
    {
      pm = SBMLDocument_getFatal(d, n);
      printf( "  Line %d, Col %d: %s\n"
              , ParseMessage_getLine   (pm)
              , ParseMessage_getColumn (pm)
              , ParseMessage_getMessage(pm) );
    }

    printf("\n");
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
