/**
 * Filename    : SBase.hpp
 * Description : Base object of all SBML objects
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-13
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


#ifndef SBase_hpp
#define SBase_hpp


#include <string>
#include "SBase.h"


extern "C" void SBaseTest_setup (void);


class SBase
{
public:

  virtual ~SBase ();

  /**
   * SBase "objects" are abstract, i.e., they are not created.  Rather,
   * specific "subclasses" are created (e.g., Model) and their SBASE_FIELDS
   * are initialized with this function.  The type of the specific
   * "subclass" is indicated by the given SBMLTypeCode.
   */
  LIBSBML_EXTERN
  void init (SBMLTypeCode_t tc);

  LIBSBML_EXTERN
  SBMLTypeCode_t getTypeCode () const;

  LIBSBML_EXTERN
  unsigned int getColumn () const;

  LIBSBML_EXTERN
  unsigned int getLine () const;

  LIBSBML_EXTERN
  const std::string& getMetaId () const;

  LIBSBML_EXTERN
  const std::string& getNotes () const;

  LIBSBML_EXTERN
  const std::string& getAnnotation () const;

  LIBSBML_EXTERN
  bool isSetMetaId () const;

  LIBSBML_EXTERN
  bool isSetNotes () const;

  LIBSBML_EXTERN
  bool isSetAnnotation () const;

  LIBSBML_EXTERN
  void setMetaId (const std::string& id);

  LIBSBML_EXTERN
  void setNotes (const std::string& xhtml);

  LIBSBML_EXTERN
  void setAnnotation (const std::string& xml);

  /**
   * @return the partial SBML that describes this SBML object.
   */
  LIBSBML_EXTERN
  char*
  SBase::toSBML (unsigned int level = 2, unsigned int version = 1);

  LIBSBML_EXTERN
  void unsetMetaId ();

  LIBSBML_EXTERN
  void unsetNotes ();

  LIBSBML_EXTERN
  void unsetAnnotation ();



protected:

  SBase ();


  SBMLTypeCode_t typecode;

  unsigned int line;
  unsigned int column;

  std::string metaid;
  std::string notes;
  std::string annotation;


  friend void SBaseTest_setup ();
  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // SBase_hpp
