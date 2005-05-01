/**
 * \file    Expat.hpp
 * \brief   C++ interface to expat XML parser
 * \author  Stefan Hoops <shoops@vt.edu>
 *
 * $Id$
 * $Source$
 */
/* Copyright (c) 2003 Stefan Hoops
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 *  Contributor(s):
 *    Ben Bornstein - Minor reformatting, modifications and integration
 *                    into the libsbml source tree.
 */


/**
 * This file describes the C++ interface to the expat library used by
 * COPASI.
 *
 * Created for Copasi by Stefan Hoops 2003
 * Copyright Stefan Hoops
 */


#ifndef Expat_h
#define Expat_h


#ifdef __cplusplus


#include <string>
#include <cassert>
#include <expat.h>


class Expat
{
/* Attributes */
protected:

  /**
   * The expat parser 
   */
  XML_Parser mParser;


public:
  /**
   * Default constructor
   */
  Expat();

  /**
   * Destructor
   */
  ~Expat();

  /**
   * Create the underlying expat parser.
   */
  bool create(const XML_Char* encoding = NULL, const XML_Char* sep = NULL);

  /**
   * Destroy the parser
   */
  void destroy();

  /**
   * Parse a block of character data
   *
   * @param  const char*  pBuffer
   * @param  int          length   (Default: -1 (zero terminated))
   * @param  bool         isFinal  (Default: true)
   *
   * @return bool Success
   */
  bool parse(const char* pBuffer, int length = -1, bool isFinal = true);

#ifdef WCHAR
  /**
   * Parse a block of wide character data
   *
   * @param  const WCHAR*  pBuffer
   * @param  int           length   (Default: -1 (zero terminated))
   * @param  bool          isFinal  (Default: true)
   *
   * @return bool Success
   */
  bool parse(const WCHAR* pBuffer, int length = -1, bool isFinal = true);
#endif

  /**
   * Parse internal buffer
   *
   * @param  int   length
   * @param  bool  isFinal  (Default: true)
   *
   * @return bool Success
   */
  bool parseBuffer(int length, bool isFinal = true);

  /**
   * Get the internal buffer
   */
  void* getBuffer(int length);

  /**
   * Enable/Disable the start element handler
   * @param  bool  enable  (Default: true)
   */
  void enableStartElementHandler(bool enable = true);

  /**
   * Enable/Disable end element handler
   * @param  bool  enable  (Default: true)
   */
  void enableEndElementHandler(bool enable = true);

  /**
   * Enable/Disable the element handlers
   * @param  bool  enable  (Default: true)
   */
  void enableElementHandler(bool enable = true);

  /**
   * Start element handler
   *
   * @param  const XML_Char*   name
   * @param  const XML_Char**  pAttrs
   */
  virtual void onStartElement(const XML_Char* name, const XML_Char** pAttrs);

  /**
   * End element handler
   *
   * @param  const XML_Char*  name
   */
  virtual void onEndElement(const XML_Char* name);

  /**
   * Enable/Disable the character data handler
   * @param  bool  enable  (Default: true)
   */
  void enableCharacterDataHandler(bool enable = true);

  /**
   * Character data handler
   *
   * @param  const XML_Char*  data
   * @param  int              length
   */
  virtual void onCharacterData(const XML_Char* data, int length);

  /**
   * Enable/Disable the processing instruction handler
   * @param  bool  enable  (Default: true)
   */
  void enableProcessingInstructionHandler(bool enable = true);

  /**
   * Processing instruction handler
   *
   * @param  const XML_Char*  target
   * @param  const XML_Char*  data
   */
  virtual
  void onProcessingInstruction(const XML_Char* target, const XML_Char* data);

  /**
   * Enable/Disable the comment handler
   * @param  bool  enable  (Default: true)
   */
  void enableCommentHandler(bool enable = true);

  /**
   * Comment handler
   * @param  const XML_Char*  data
   */
  virtual void onComment(const XML_Char * data);

  /**
   * Enable/Disable the start CDATA section handler
   * @param  bool  enable  (Default: true)
   */
  void enableStartCdataSectionHandler(bool enable = true);

  /**
   * Enable/Disable the end CDATA section handlers
   * @param  bool  enable  (Default: true)
   */
  void enableEndCdataSectionHandler(bool enable = true);

  /**
   * Enable/Disable the CDATA section handlers
   * @param  bool  enable  (Default: true)
   */
  void enableCdataSectionHandler(bool enable = true);

  /**
   * Start CDATA section handler
   */
  virtual void onStartCdataSection();

  /**
   * End CDATA section handler
   */
  virtual void onEndCdataSection();

  /**
   * Enable/Disable default handler
   *
   * @param  bool  enable  (Default: true)
   * @param  bool  expand  (Default: true)
   */
  void enableDefaultHandler(bool enable = true, bool expand = true);

  /**
   * Default handler
   *
   * @param  const XML_Char*  data
   * @param  int              length
   */
  virtual void onDefault(const XML_Char* data, int length);

  /**
   * Enable/Disable external entity ref handler
   * @param  bool  enable  (Default: true)
   */
  void enableExternalEntityRefHandler(bool enable = true);

  /**
   * External entity ref handler
   *
   * @param  const XML_Char*  context
   * @param  const XML_Char*  base
   * @param  const XML_Char*  systemID
   * @param  const XML_Char*  publicID
   */
  virtual bool onExternalEntityRef(const XML_Char* context,
                                   const XML_Char* base,
                                   const XML_Char* systemID,
                                   const XML_Char* publicID);

  /**
   * Enable/Disable unknown encoding handler
   * @param  bool  enable  (Default: true)
   */
  void enableUnknownEncodingHandler(bool enable = true);

  /**
   * Unknown encoding handler
   *
   * @param  const XML_Char*  name
   * @param  XML_Encoding*    pInfo
   */
  virtual bool onUnknownEncoding(const XML_Char* name, XML_Encoding* pInfo);

  /**
   * Enable/Disable start namespace handler
   * @param bool enable (Default: true)
   */
  void enableStartNamespaceDeclHandler(bool enable = true);

  /**
   * Enable/Disable end namespace handler
   * @param  bool  enable  (Default: true)
   */
  void enableEndNamespaceDeclHandler(bool enable = true);

  /**
   * Enable/Disable namespace handlers
   * @param  bool  enable  (Default: true)
   */
  void enableNamespaceDeclHandler(bool enable = true);

  /**
   * Enable/Disable namespace triplet reporting.  When enabled, and if the
   * Expat parser was created with namespace handling (i.e. a non-null
   * separator character given to create()), then for the
   * onStart/onEndElement() handlers, the element name will have the
   * following form:
   *
   *   ns-uri sep name sep ns-prefix
   *
   * (whitespace will not be present and is provided only for clarity)
   * where the namespace uri and/or the namespace prefix (and its
   * corresponding separator may not be present).
   *
   * @param bool enable (Default: true)
   */
  void enableNamespaceTriplets(bool enable = true);

  /**
   * Start namespace declaration handler
   *
   * @param  const XML_Char*  prefix
   * @param  const XML_Char*  URI
   */
  virtual void onStartNamespaceDecl(const XML_Char* prefix,
                                    const XML_Char* URI);

  /**
   * End namespace declaration handler
   * @param  const XML_Char*  prefix
   */
  virtual void onEndNamespaceDecl(const XML_Char* prefix);

  /**
   * Enable/Disable the XML declaration handler
   * @param  bool  enable  (Default: true)
   */
  void enableXmlDeclHandler(bool enable = true);

  /**
   * XML declaration handler
   *
   * @param  const XML_Char*  version
   * @param  const XML_Char*  encoding
   * @param  bool             standalone
   */
  virtual void onXmlDecl(const XML_Char* version,
                         const XML_Char* encoding,
                         bool            standalone);

  /**
   * Enable/Disable the start DOCTYPE declaration handler
   * @param  bool  enable  (Default: true)
   */
  void enableStartDoctypeDeclHandler(bool enable = true);

  /**
   * Enable/Disable the end DOCTYPE declaration handler
   * @param  bool  enable  (Default: true)
   */
  void enableEndDoctypeDeclHandler(bool enable = true);

  /**
   * Enable/Disable the DOCTYPE declaration handler
   * @param  bool  enable  (Default: true)
   */
  void enableDoctypeDeclHandler(bool enable = true);

  /**
   * Start DOCTYPE declaration handler
   *
   * @param  const XML_Char* doctypeName
   * @param  const XML_Char* sysID
   * @param  const XML_Char* pubID
   * @param  bool            hasInternalSubset
   */
  virtual void onStartDoctypeDecl(const XML_Char* doctypeName,
                                  const XML_Char* sysID,
                                  const XML_Char* pubID,
                                  bool            hasInternalSubset);

  /**
   * End DOCTYPE declaration handler
   */
  virtual void onEndDoctypeDecl();

  /**
   * @return the last error code
   */
  enum XML_Error getErrorCode();

  /** 
   * @return the error string for the given error code.
   *
   * @param  enum XML_Error  nError
   */
  static const XML_LChar* getErrorString(enum XML_Error nError);

  /**
   * @return the last error string
   */
  const XML_LChar* getErrorString();

  /**
   * @return the current byte index
   */
  long getCurrentByteIndex();

  /**
   * @return the current line number
   */
  int getCurrentLineNumber() const;

  /**
   * @return the current column number
   */
  int getCurrentColumnNumber();

  /**
   * @return the current byte count
   */
  int getCurrentByteCount();

  /**
   * @return the input context
   */
  const char* getInputContext(int* pnOffset, int* pnSize);

  /**
   * @return the version string
   */
  static const XML_LChar* getExpatVersion();

  /**
   * Get the version information
   *
   * @param  int  major
   * @param  int  minor
   * @param  int  micro
   */
  static void getExpatVersion(int& nMajor, int & nMinor, int & nMicro);


protected:

  /**
   * Start element handler wrapper
   *
   * @param  void*            pUserData
   * @param  const XML_Char*  name
   * @param  const XML_Char** pAttrs
   */
  static void startElementHandler(void*            pUserData,
                                  const XML_Char*  name,
                                  const XML_Char** pAttrs);

  /**
   * End element handler wrapper
   *
   * @param  void*            pUserData
   * @param  const XML_Char*  name
   */
  static void endElementHandler(void* pUserData, const XML_Char* name);

  /**
   * Character data handler wrapper
   *
   * @param  void*            pUserData
   * @param  const XML_Char*  data
   * @param  int              length
   */
  static void characterDataHandler(void*           pUserData,
                                   const XML_Char* data,
                                   int             length);

  /**
   * Processing instruction handler wrapper
   *
   * @param  void*            pUserData
   * @param  const XML_Char*  target
   * @param  const XML_Char*  data
   */
  static void processingInstructionHandler(void*           pUserData,
                                           const XML_Char* target,
                                           const XML_Char* data);

  /**
   * Comment handler wrapper
   *
   * @param  void*            pUserData
   * @param  const XML_Char*  data
   */
  static void commentHandler(void* pUserData,const  XML_Char* data);
                             
  /**
   * Start CDATA section wrapper
   *
   * @param  void*  pUserData
   */
  static void startCdataSectionHandler(void* pUserData);

  /**
   * End CDATA section wrapper
   *
   * @param  void*  pUserData
   */
  static void endCdataSectionHandler(void* pUserData);

  /**
   * Default wrapper
   *
   * @param void*            pUserData
   * @param const XML_Char*  data
   * @param int              length
   */
  static void defaultHandler(void* pUserData, const XML_Char* data, int length);

  /**
   * External entity ref wrapper.
   *
   * This is currently not working !!
   *
   * @param  XML_Parser parser
   * @param  const XML_Char*  context
   * @param  const XML_Char*  base
   * @param  const XML_Char*  systemID
   * @param  const XML_Char*  publicID
   */
  static int externalEntityRefHandler(XML_Parser      parser,
                                      const XML_Char* context,
                                      const XML_Char* base,
                                      const XML_Char* systemID,
                                      const XML_Char* publicID);

  /**
   * Unknown encoding wrapper
   *
   * @param  void*            pUserData
   * @param  const XML_Char*  name
   * @param  XML_Encoding*    pInfo
   */
  static int unknownEncodingHandler(void*           pUserData,
                                    const XML_Char* name,
                                    XML_Encoding*   pInfo);

  /**
   * Start namespace decl wrapper
   *
   * @param  void*            pUserData
   * @param  const XML_Char*  prefix
   * @param  const XML_Char*  URI
   */
  static void startNamespaceDeclHandler(void*           pUserData,
                                        const XML_Char* prefix,
                                        const XML_Char* URI);

  /**
   * End namespace decl wrapper
   *
   * @param  void*            pUserData
   * @param  const XML_Char*  prefix
   */
  static void endNamespaceDeclHandler(void* pUserData, const XML_Char* prefix);

  /**
   * XML declaration wrapper
   *
   * @param  void*            pUserData
   * @param  const XML_Char*  version
   * @param  const XML_Char*  encoding
   * @param  int              standalone
   */
  static void xmlDeclHandler(void*           pUserData,
                             const XML_Char* version,
                             const XML_Char* encoding,
                             int             standalone);

  /**
   * Start Doctype declaration wrapper
   *
   * @param  void*           pUserData
   * @param  const XML_Char* doctypeName
   * @param  const XML_Char* sysID
   * @param  const XML_Char* pubID
   * @param  int             hasInternalSubset
   */
  static void startDoctypeDeclHandler(void*           pUserData,
                                      const XML_Char* doctypeName,
                                      const XML_Char* sysID,
                                      const XML_Char* pubID,
                                      int             hasInternalSubset);

  /**
   * End Doctype declaration wrapper
   *
   * @param  void*  pUserData
   */
  static void endDoctypeDeclHandler(void* pUserData);
};


#endif  /* __cplusplus */
#endif  /* Expat_h */
