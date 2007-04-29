/**
 * @file    XMLError.h
 * @brief   Represents errors (and messages) encountered during an XML parse
 * @author  Ben Bornstein
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


#include <sbml/xml/XMLExtern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <iosfwd>
#include <string>


typedef class XMLError XMLError_t;


class LIBLAX_EXTERN XMLError
{
public:

  enum Severity { Info = 0, Warning = 1, Error = 2, Fatal = 3 };

  /**
   * Creates a new XMLError to report that something occurred at the given
   * line and column.  Each XMLError also has an identification number, a
   * category, and a severity level associated with it.
   *
   * @param id an unsigned int, the identification number of the error.
   * @param message a string, the error message.
   * @param severity XMLError::Severity, severity of the error.
   * @param category a string, the category to which the error belongs.
   * @param line an unsigned int, the line number at which the error occurs.
   * @param column an unsigned int, the column number at which the error occurs.
    *
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
   * Returns the id of this XMLError.
   *
   * @return the id of this XMLError.
   */
  unsigned int getId () const;

  /**
   * Returns the message text of this XMLError.
   *
   * @return the message text of this XMLError.
   */
  const std::string& getMessage () const;

  /**
   * Return the line number where this XMLError occurred.
   *
   * @return the line number where this XMLError occurred.
   */
  unsigned int getLine () const;

  /**
   * Return the column number where this XMLError occurred.
   *
   * @return the column number where this XMLError occurred.
   */
  unsigned int getColumn () const;

  /**
   * Return the severity of this XMLError.  XMLError severity levels
   * correspond to those defined in the XML specification (with the
   * addition of Info for informational messages).
   *
   * @return the severity of this XMLError.
   */
  Severity getSeverity () const;

  /**
   * Return the category of this XMLError.  A category is a string,
   * similiar in spirit to an XML namespace, which can be used to partition
   * errors into distinct groups.  Among other things, this can be used to
   * prevent id conflicts by uniquely identifying an XMLError by both id
   * and category.
   *
   * @return the category of this XMLError.
   */
  const std::string& getCategory () const;

  /**
   * Predicate returning @c true or @c false depending on whether 
   * this XMLError is for information only.
   *
   * @return @c true if this XMLError is for informational purposes only,
   * @c false otherwise.
   */
  bool isInfo () const;


  /**
   * Predicate returning @c true or @c false depending on whether 
   * this XMLError is a warning.
   *
   * @return @c true if this XMLError is a warning, @c false otherwise.
   */
  bool isWarning () const;


  /**
   * Predicate returning @c true or @c false depending on whether 
   * this XMLError is an error.
   *
   * @return @c true if this XMLError is an error, @c false otherwise.
   */
  bool isError () const;

  /**
   * Predicate returning @c true or @c false depending on whether 
   * this XMLError is a fatal error.
   *
   * @return @c true if this XMLError is a fatal error, @c false otherwise.
   */
  bool isFatal () const;

  /**
   * Sets the line number where this XMLError occurred.
   * 
   * @param line an unsigned int, the line number to set.
   */
  void setLine (unsigned int line);

  /**
   * Sets the column number where this XMLError occurred.
   * 
   * @param column an unsigned int, the column number to set.
   */
  void setColumn (unsigned int column);


#ifndef SWIG

  /**
   * Outputs this XMLError to stream in the following format (and followed
   * by a newline):
   *
   *   line: (id) message
   *
   * @param stream the output stream to write to.
   * @param errorthe XMLError to write.
   */
  LIBLAX_EXTERN
  friend
  std::ostream& operator<< (std::ostream& stream, const XMLError& error);

#endif  /* !SWIG */


protected:


  unsigned int  mId;

  std::string   mMessage;

  Severity      mSeverity;
  std::string   mCategory;

  unsigned int  mLine;
  unsigned int  mColumn;
};


#endif  /* __cplusplus */


typedef enum { Info = 0, Warning = 1, Error = 2, Fatal = 3 } XMLError_Severity;


#ifndef SWIG


BEGIN_C_DECLS
/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


//#ifndef __cplusplus
//typedef struct XMLError_t;
//#endif

LIBLAX_EXTERN
XMLError_t*
XMLError_create (void);

LIBLAX_EXTERN
XMLError_t*
XMLError_createWithIdAndMessage (unsigned int id, const char * message);

LIBLAX_EXTERN
void
XMLError_free(XMLError_t* error);


/**
 */
LIBLAX_EXTERN
unsigned int
XMLError_getId (const XMLError_t *error);


/**
 */
LIBLAX_EXTERN
const char *
XMLError_getMessage (const XMLError_t *error);


/**
 */
LIBLAX_EXTERN
unsigned int
XMLError_getLine (const XMLError_t *error);


/**
 */
LIBLAX_EXTERN
unsigned int
XMLError_getColumn (const XMLError_t *error);


/**
 *
 */
LIBLAX_EXTERN
XMLError_Severity
XMLError_getSeverity (const XMLError_t *error);


/**
 */
LIBLAX_EXTERN
const char *
XMLError_getCategory (const XMLError_t *error);


/**
 */
LIBLAX_EXTERN
int
XMLError_isInfo (const XMLError_t *error);


/**
 */
LIBLAX_EXTERN
int
XMLError_isWarning (const XMLError_t *error);


/**
 */
LIBLAX_EXTERN
int
XMLError_isError (const XMLError_t *error);


/**
 */
LIBLAX_EXTERN
int
XMLError_isFatal (const XMLError_t *error);


/**
 */
LIBLAX_EXTERN
void
XMLError_print (const XMLError_t *error, FILE *stream);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* XMLError_h */
