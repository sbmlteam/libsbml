/**
 * @file    XMLInputStream.h
 * @brief   XMLInputStream
 * @author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->
 *
 * @class XMLInputStream
 * @sbmlbrief{core} An interface to an XML input stream.
 */

#ifndef XMLInputStream_h
#define XMLInputStream_h

#include <sbml/xml/XMLExtern.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/common/operationReturnValues.h>
#include <sbml/SBMLNamespaces.h>


#ifdef __cplusplus

#include <string>

#include <sbml/xml/XMLTokenizer.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class XMLErrorLog;
class XMLParser;


class LIBLAX_EXTERN XMLInputStream
{
public:

  /**
   * Creates a new XMLInputStream.
   *
   * @p content the source of the stream.
   *
   * @p isFile boolean flag to indicate whether @p content is a file name.
   * If @c true, @p content is assumed to be the file from which the XML
   * content is to be read.  If @c false, @p content is taken to be a
   * string that @em is the content to be read.
   *
   * @p library the name of the parser library to use.
   *
   * @p errorLog the XMLErrorLog object to use.
   *
   * @if notcpp @htmlinclude warn-default-args-in-docs.html @endif@~
   */
  XMLInputStream (  const char*        content
                  , bool               isFile   = true
                  , const std::string  library  = "" 
                  , XMLErrorLog*       errorLog = NULL );


  /**
   * Destroys this XMLInputStream.
   */
  virtual ~XMLInputStream ();


  /**
   * Returns the encoding of the XML stream.
   *
   * @return the encoding of the XML stream.
   */
  const std::string& getEncoding ();


  /**
   * Returns the version of the XML stream.
   *
   * @return the version of the XML stream.
   */
  const std::string& getVersion ();


  /**
   * Returns an XMLErrorLog which can be used to log XML parse errors and
   * other validation errors (and messages).
   *
   * @return an XMLErrorLog which can be used to log XML parse errors and
   * other validation errors (and messages).
   */
  XMLErrorLog* getErrorLog ();


  /**
   * Returns true if end of file (stream) has been reached, false
   * otherwise.
   *
   * @return true if end of file (stream) has been reached, false
   * otherwise.
   */
  bool isEOF () const;


  /**
   * Returns true if a fatal error occurred while reading from this stream.
   *
   * @return true if a fatal error occurred while reading from this stream.
   */
  bool isError () const;


  /**
   * Returns true if the stream is in a good state (i.e. isEOF() and
   * isError() are both false), false otherwise.
   *
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
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif@~ The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  int setErrorLog (XMLErrorLog* log);


  /**
   * Prints a string representation of the underlying token stream, for
   * debugging purposes.
   */
  std::string toString ();


  /**
   * Returns the SBMLNamespaces object attached to this XMLInputStream
   * if it has been set, NULL otherwise.
   *
   * @return the SBMLNamespaces object or NULL if none has been set.
   */
  SBMLNamespaces * getSBMLNamespaces();

  
  /**
   * Sets the SBMLNamespaces object to allow this stream to reference
   * the available SBML namespaces being read.
   */
   void setSBMLNamespaces(SBMLNamespaces * sbmlns);


  /**
   * Analyses the tokens in the stream and returns the number of 
   * child tokens of the given element.
   *
   * @param elementName a string representing the name of the element
   * for which the number of children are to be determined.
   *
   * This function allows information from the input stream to be determined
   * without the  need to actually read and consume the tokens in
   * the stream. This functionality
   * is particularly utilized when reading MathML. 
   *
   * The function will return the number of child elements of the
   * element represented by the elementName supplied, i.e. the number
   * of child elements encountered before the closing tag for the
   * elementname supplied.  If the elementName
   * has not been supplied then the function assumes that it is reading
   * an apply element followed by a function element.
   *
   * @note This function assumes the stream has been read up to and 
   * including the element elementName.
   *
   * @return an unsigned int giving the number of children of the
   * element specified.
   */
  unsigned int determineNumberChildren(const std::string& elementName = "");
  
  
  /**
   * Analyses the tokens in the stream and returns the number of 
   * child tokens of the specified type within the given element.
   *
   * @param childName a string representing the name of the child
   * element whose number is to be determined.
   * @param container a string representing the name of the element
   * for which the number of children are to be determined.
   *
   * This function allows information from the input stream to be determined
   * without the  need to actually read and consume the tokens in
   * the stream. This functionality
   * is particularly utilized when reading MathML. 
   *
   * The function will return the number of child elements of the
   * element represented by the childName supplied within the element
   * specified by the container, i.e. the number
   * of child elements encountered before the closing tag for the
   * container supplied. 
   *
   * @note This function assumes the stream has been read up to and 
   * including the element container.
   *
   * @return an unsigned int giving the number of children of type childName
   * within the container element specified.
   */
  unsigned int determineNumSpecificChildren(const std::string& childName,  
    const std::string& container);

private:
  /**
   * Copy Constructor, made private so as to notify users, that copying an input stream is not supported. 
   */
  XMLInputStream (const XMLInputStream& other);


  /**
   * Assignment operator, made private so as to notify users, that copying an input stream is not supported. 
   */
  XMLInputStream& operator=(const XMLInputStream& other);


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
  bool requeueToken ();


  bool mIsError;

  XMLToken     mEOF;
  XMLTokenizer mTokenizer;
  XMLParser*   mParser;

  SBMLNamespaces* mSBMLns;

};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN

BEGIN_C_DECLS


/**
 * Creates a new empty XMLInputStream_t structure and returns a pointer to it.
 *
 * @return pointer to created XMLInputStream_t structure.
 *
 * @memberof XMLInputStream_t
 */
LIBLAX_EXTERN
XMLInputStream_t *
XMLInputStream_create (const char* content, int isFile, const char *library);


/**
 * Destroys this XMLInputStream_t structure.
 *
 * @param stream XMLInputStream_t structure to be freed.
 *
 * @memberof XMLInputStream_t
 */
LIBLAX_EXTERN
void
XMLInputStream_free (XMLInputStream_t *stream);


/**
 * Returns the encoding of the XML stream.
 *
 * @param stream XMLInputStream_t structure to be freed.
 *
 * @return the encoding of this XMLInputStream_t, as a pointer to a string.
 *
 * @memberof XMLInputStream_t
 */
LIBLAX_EXTERN
const char *
XMLInputStream_getEncoding (XMLInputStream_t *stream);


/**
 * @param stream XMLInputStream_t structure
 *
 * @memberof XMLInputStream_t
 */
LIBLAX_EXTERN
XMLErrorLog_t *
XMLInputStream_getErrorLog (XMLInputStream_t *stream);


/**
 * @param stream XMLInputStream_t structure
 *
 * @memberof XMLInputStream_t
 */
LIBLAX_EXTERN
int
XMLInputStream_isEOF (XMLInputStream_t *stream);


/**
 * @param stream XMLInputStream_t structure
 *
 * @memberof XMLInputStream_t
 */
LIBLAX_EXTERN
int
XMLInputStream_isError (XMLInputStream_t *stream);


/**
 * @param stream XMLInputStream_t structure
 *
 * @memberof XMLInputStream_t
 */
LIBLAX_EXTERN
int
XMLInputStream_isGood (XMLInputStream_t *stream);


/**
 * @param stream XMLInputStream_t structure
 *
 * @memberof XMLInputStream_t
 */
LIBLAX_EXTERN
XMLToken_t *
XMLInputStream_next (XMLInputStream_t *stream);


/**
 * @param stream XMLInputStream_t structure
 *
 * @memberof XMLInputStream_t
 */
LIBLAX_EXTERN
const XMLToken_t *
XMLInputStream_peek (XMLInputStream_t *stream);


/**
 * @param stream XMLInputStream_t structure
 *
 * @memberof XMLInputStream_t
 */
LIBLAX_EXTERN
void
XMLInputStream_skipPastEnd (XMLInputStream_t *stream,
                            const XMLToken_t *element);


/**
 * @param stream XMLInputStream_t structure
 *
 * @memberof XMLInputStream_t
 */
LIBLAX_EXTERN
void
XMLInputStream_skipText (XMLInputStream_t *stream);


/**
 * @param stream XMLInputStream_t structure
 *
 * @memberof XMLInputStream_t
 */
LIBLAX_EXTERN
int
XMLInputStream_setErrorLog (XMLInputStream_t *stream, XMLErrorLog_t *log);

END_C_DECLS

LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* XMLInputStream_h */
