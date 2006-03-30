/**
 * \file    XMLError.h
 * \brief   Represents errors (and messages) encountered during an XML parse
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


#ifndef XMLError_h
#define XMLError_h


#include <string>
#include "XMLExtern.h"


class LIBLAX_EXTERN XMLError
{
public:

  enum Severity { Info, Warning, Error, Fatal };

  /**
   * Creates a new XMLError to report that something occurred at the given
   * line and column.  Each XMLError also has an identification number, a
   * category, and a severity level associated with it.
   */
  XMLError
  (
      unsigned int        id       = 0
    , const std::string&  message  = ""
    , Severity            severity = Error
    , const std::string&  category = ""
    , unsigned int        line     = 0
    , unsigned int        column   = 0
  );

  /**
   * Destroys this XMLError.
   */
  virtual ~XMLError ();


  /**
   * @return the id of this XMLError.
   */
  unsigned int getId () const;

  /**
   * @return the message text of this XMLError.
   */
  const std::string& getMessage () const;

  /**
   * @return the line number where this XMLError ocurred.
   */
  unsigned int getLine () const;

  /**
   * @return the column number where this XMLError occurred.
   */
  unsigned int getColumn () const;

  /**
   * @return the severity of this XMLError.  XMLError severity levels
   * correspond to those defined in the XML specification (with the
   * addition of Info for informational messages).
   */
  Severity getSeverity () const;

  /**
   * @return the category of this XMLError.  A category is a string,
   * similiar in spirit to an XML namespace, which can be used to partition
   * errors into distinct groups.  Among other things, this can be used to
   * prevent id conflicts by uniquely identifying an XMLError by both id
   * and category.
   */
  const std::string& getCategory () const;

  /**
   * @return true if this XMLError is for informational purposes only,
   * false otherwise.
   */
  bool isInfo () const;

  /**
   * @return true if this XMLError is a warning, false otherwise.
   */
  bool isWarning () const;

  /**
   * @return true if this XMLError is an error, false otherwise.
   */
  bool isError () const;

  /**
   * @return true if this XMLError is a fatal error, false otherwise.
   */
  bool isFatal () const;

  /**
   * Sets the line number where this XMLError occurred.
   */
  void setLine (unsigned int line);

  /**
   * Sets the column number where this XMLError occurred.
   */
  void setColumn (unsigned int column);


protected:

  unsigned int  mId;

  std::string   mMessage;

  Severity      mSeverity;
  std::string   mCategory;

  unsigned int  mLine;
  unsigned int  mColumn;
};


#endif  /* XMLError_h */
