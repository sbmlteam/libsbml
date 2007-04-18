/**
 * @file    XMLInputStream.h
 * @brief   XMLInputStream
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


#ifndef XMLInputStream_h
#define XMLInputStream_h

#include <sbml/xml/XMLExtern.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus

#include <string>

#include <sbml/xml/XMLTokenizer.h>


class XMLErrorLog;
class XMLParser;


class LIBLAX_EXTERN XMLInputStream
{
public:

  /**
   * Creates a new XMLInputStream.
   */
  XMLInputStream (  const char*        content
                  , bool               isFile   = true
                  , const std::string  library  = "" 
		  , XMLErrorLog*       errorLog = 0 );

  /**
   * Destroys this XMLInputStream.
   */
  virtual ~XMLInputStream ();

  /**
   * @return the encoding of the XML stream.
   */
  const std::string& getEncoding ();

  /**
   * @return an XMLErrorLog which can be used to log XML parse errors and
   * other validation errors (and messages).
   */
  XMLErrorLog* getErrorLog ();

  /**
   * @return true if end of file (stream) has been reached, false
   * otherwise.
   */
  bool isEOF () const;

  /**
   * @return true if a fatal error occurred while reading from this stream.
   */
  bool isError () const;

  /**
   * @return true if the stream is in a good state (i.e. isEOF() and
   * isError() are both false), false otherwise.
   */
  bool isGood () const;

  /**
   * Consumes the next XMLToken and return it.
   *
   * @return the next XMLToken or EOF (XMLToken.isEOF() == true).
   */
  XMLToken next ();

  /**
   * Returns the next XMLToken without consuming it.  A subsequent call to
   * either peek() or next() will return the same token.
   *
   * @return the next XMLToken or EOF (XMLToken.isEOF() == true).
   */
  const XMLToken& peek ();

  /**
   * Consume zero or more XMLTokens up to and including the corresponding
   * end XML element or EOF.
   */
  void skipPastEnd (const XMLToken& element);

  /**
   * Consume zero or more XMLTokens up to but not including the next XML
   * element or EOF.
   */
  void skipText ();

  /**
   * Sets the XMLErrorLog this stream will use to log errors.
   */
  void setErrorLog (XMLErrorLog* log);


  /**
   * Prints a string representation of the underlying token stream, for
   * debugging purposes.
   */
  std::string toString ();


protected:

  /**
   * Unitialized XMLInputStreams may only be created by subclasses.
   */
  XMLInputStream ();

  /**
   * Runs mParser until mTokenizer is ready to deliver at least one
   * XMLToken or a fatal error occurs.
   */
  void queueToken ();


  bool mIsError;

  XMLToken     mEOF;
  XMLTokenizer mTokenizer;
  XMLParser*   mParser;
};

#endif  /* __cplusplus */

#if 0

#ifndef SWIG

BEGIN_C_DECLS

/**
 * 
 **/
LIBLAX_EXTERN
XMLInputStream_t *
XMLInputStream_create (const char* content, int isFile, const char *library);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLInputStream_free (XMLInputStream_t *stream);


/**
 * 
 **/
LIBLAX_EXTERN
const char *
XMLInputStream_getEncoding (XMLInputStream_t *stream);


/**
 * 
 **/
LIBLAX_EXTERN
XMLErrorLog_t *
XMLInputStream_getErrorLog (XMLInputStream_t *stream);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLInputStream_isEOF (XMLInputStream_t *stream);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLInputStream_isError (XMLInputStream_t *stream);


/**
 * 
 **/
LIBLAX_EXTERN
int
XMLInputStream_isGood (XMLInputStream_t *stream);


/**
 * 
 **/
LIBLAX_EXTERN
XMLToken_t
XMLInputStream_next (XMLInputStream_t *stream);


/**
 * 
 **/
LIBLAX_EXTERN
const XMLToken_t *
XMLInputStream_peek (XMLInputStream_t *stream);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLInputStream_skipPastEnd (XMLInputStream_t *stream,
			    const XMLToken_t *element);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLInputStream_skipText (XMLInputStream_t *stream);


/**
 * 
 **/
LIBLAX_EXTERN
void
XMLInputStream_setErrorLog (XMLInputStream_t *stream, XMLErrorLog_t *log);



END_C_DECLS

#endif  /* !SWIG */

#endif

#endif  /* XMLInputStream_h */
