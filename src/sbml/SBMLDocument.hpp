/**
 * Filename    : SBMLDocument.hpp
 * Description : Top-level container for all things SBML
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef SBMLDocument_hpp
#define SBMLDocument_hpp


#include "extern.h"

#include "SBase.hpp"
#include "List.hpp"
#include "ParseMessage.hpp"
#include "Model.hpp"

#include <iosfwd>


class SBMLDocument: public SBase
{
public:

  /**
   * Creates a new SBMLDocument.  The SBML level defaults to 2 and version
   * defaults to 1.
   */
  LIBSBML_EXTERN
  SBMLDocument (unsigned int level = 2, unsigned int version = 1);

  /**
   * Destroys this SBMLDocument.
   */
  LIBSBML_EXTERN
  virtual ~SBMLDocument ();


  /**
   * Creates a new Model (optionally with its Id attribute set) inside this
   * SBMLDocument and returns it.  This covenience method is equivalent to:
   *
   *   setModel( Model() );
   */
  LIBSBML_EXTERN Model& createModel (const std::string& sid = "");

  /**
   * @return the level of this SBMLDocument.
   */
  LIBSBML_EXTERN
  unsigned int getLevel () const;

  /**
   * @return the version of this SBMLDocument.
   */
  LIBSBML_EXTERN
  unsigned int getVersion () const;

  /**
   * @return the Model associated with this SBMLDocument.
   */
  LIBSBML_EXTERN
  Model* getModel ();

  /**
   * @return the nth warning encountered during the parse of this
   * SBMLDocument or NULL if n > getNumWarnings() - 1.
   */
  LIBSBML_EXTERN
  ParseMessage* getWarning (unsigned int n);

  /**
   * @return the nth error encountered during the parse of this
   * SBMLDocument or NULL if n > getNumErrors() - 1.
   */
  LIBSBML_EXTERN
  ParseMessage* getError (unsigned int n);

  /**
   * @return the nth fatal error encountered during the parse of this
   * SBMLDocument or NULL if n > getNumFatals() - 1.
   */
  LIBSBML_EXTERN
  ParseMessage* getFatal (unsigned int n);

  /**
   * @return the number of warnings encountered during the parse of this
   * SBMLDocument.
   */
  LIBSBML_EXTERN
  unsigned int getNumWarnings () const;

  /**
   * @return the number of errors encountered during the parse of this
   * SBMLDocument.
   */
  LIBSBML_EXTERN
  unsigned int getNumErrors () const;

  /**
   * @return the number of fatal errors encountered during the parse of
   * this SBMLDocument.
   */
  LIBSBML_EXTERN
  unsigned int getNumFatals () const;

  /**
   * Prints all warnings encountered during the parse of this SBMLDocument
   * to the given stream.  If no warnings have occurred, i.e.
   * getNumWarnings() == 0, no output will be sent to stream. The format of
   * the output is:
   *
   *   %d Warning(s):
   *     Line %d, Col %d: %s
   *     ...
   *
   * This is a convenience method to aid in debugging.  For example:
   * printWarnings(cout).
   */
  LIBSBML_EXTERN
  void printWarnings (std::ostream& stream);

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
  void printErrors (std::ostream& stream);

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
  void printFatals (std::ostream& stream);

  /**
   * Sets the level of this SBMLDocument to the given level number.  Valid
   * levels are currently 1 and 2.
   */
  LIBSBML_EXTERN
  void setLevel (unsigned int level);

  /**
   * Sets the version of this SBMLDocument to the given version number.
   * Valid versions are currently 1 and 2 for SBML L1 and 1 for SBML L2.
   */
  LIBSBML_EXTERN
  void setVersion (unsigned int version);

  /**
   * Sets the Model of this SBMLDocument to the given Model.
   * Any previously defined model is unset and freed.
   */
  LIBSBML_EXTERN
  void setModel (Model* m);

  /**
   * Performs semantic validation on the document.  Query the results by
   * calling getWarning(), getNumError(),and getNumFatal().
   *
   * @return the number of semantic validation errors encountered.
   */
  LIBSBML_EXTERN
  unsigned int validate ();


protected:

  unsigned int level;
  unsigned int version;

  List error;
  List fatal;
  List warning;

  Model* model;


  friend class SBMLFormatter;
  friend class SBMLHandler;
  friend class SBMLReader;
};



#endif  // SBMLDocument_hpp
