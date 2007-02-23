/**
 * \file    XMLErrorLog.h
 * \brief   Stores errors (and messages) encountered during an XML parse
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


#ifndef XMLErrorLog_h
#define XMLErrorLog_h

#include <sbml/xml/XMLExtern.h>
#include <sbml/xml/XMLError.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus

#include <string>
#include <vector>
#include <list>


class XMLParser;


class LIBLAX_EXTERN XMLErrorLog
{
public:

  /**
   * Used by attributeTypeError().
   */ 
  enum DataType { Boolean = 0, Double = 1, Integer = 2 };

  /**
   * Creates a new empty XMLErrorLog.
   */
  XMLErrorLog ();

  /**
   * Destroys this XMLErrorLog.
   */
  virtual ~XMLErrorLog ();


  /**
   * Logs the given XMLError.
   */
  void add (const XMLError& error);

  /**
   * Logs (copies) the XMLErrors in the given XMLError list to this
   * XMLErrorLog.
   */
  void add (const std::list<XMLError>& errors);


  /**
   * Logs an attribute datatype error.
   *
   * @param  name  Name of the attribute
   * @param  type  The datatype of the attribute value.
   */
  void attributeTypeError (const std::string& name, DataType type);

  /**
   * Logs an error indicating a required attribute was missing.
   *
   * @param  name  Name of the attribute
   */
  void attributeRequired (const std::string& name);

  /**
   * @return the nth XMLError in this log.
   */
  const XMLError* getError (unsigned int n) const;

  /**
   * @return the number of errors that have been logged.
   */
  unsigned int getNumErrors () const;

  /**
   * Sets the element name to use when logging attributeTypeError() and
   * attributeRequired() errors (optional).
   */
  void setElement (const std::string& name);

  /**
   * Sets the XMLParser for this XMLErrorLog.
   *
   * The XMLParser will be used to obtain the current line and column
   * number as XMLErrors are logged (if they have a line and column number
   * of zero).
   */
  void setParser (const XMLParser* p);


protected:

  std::vector<XMLError>  mErrors;
  std::string            mElement;
  const XMLParser*       mParser;
};

#endif  /* __cplusplus */


typedef enum { Boolean = 0, Double = 1, Integer = 2 } XMLErrorLog_DataType;


#ifndef SWIG

BEGIN_C_DECLS


/**
 * 
 **/
LIBLAX_EXTERN
XMLErrorLog_t *
XMLErrorLog_create (void);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLErrorLog_add (XMLErrorLog_t *log, const XMLError_t *error);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLErrorLog_attributeTypeError (XMLErrorLog_t *log,
				const char *name, 
				XMLErrorLog_DataType type);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLErrorLog_attributeRequired (XMLErrorLog_t *log, const char *name);


/**
 * 
 **/
LIBLAX_EXTERN
const XMLError_t *
XMLErrorLog_getError (const XMLErrorLog_t *log, unsigned int n);


/**
 * 
 **/
LIBLAX_EXTERN
unsigned int
XMLErrorLog_getNumErrors (const XMLErrorLog_t *log);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLErrorLog_setElement (XMLErrorLog_t *log, const char *name);




END_C_DECLS

#endif  /* !SWIG */
#endif  /* XMLErrorLog_h */
