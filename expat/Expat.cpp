/**
 * \file    Expat.cpp
 * \brief   C++ interface to expat XML parser
 * \author  Stefan Hoops <shoops@vt.edu>
 *
 * $Id$
 * Source$
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


#include "Expat.h"


/**
 * Default constructor
 */
Expat::Expat() : mParser(NULL) {}


/**
 * Destructor
 */
Expat::~Expat()
{
  destroy();
}


/**
 * Create the underlying expat parser.
 */
bool
Expat::create (const XML_Char* encoding, const XML_Char* sep)
{
  // Destroy the old parser
  destroy();

  // If the encoding or seperator are empty, then NULL
  if (encoding != NULL && encoding [0] == 0)
    encoding = NULL;
  if (sep != NULL && sep [0] == 0)
    sep = NULL;

  // Create the new one
  mParser = XML_ParserCreate_MM(encoding, NULL, sep);
  if (mParser == NULL) return false;

  // Set the user data used in callbacks
  XML_SetUserData(mParser, (void *) this);
  return true;
}


/**
 * Destroy the parser
 */
void
Expat::destroy ()
{
  if (mParser != NULL)
  {
    XML_ParserFree(mParser);
    mParser = NULL;
  }
}


/**
 * Parse a block of character data
 *
 * @param  const char*  pBuffer
 * @param  int          length   (Default: -1 (zero terminated))
 * @param  bool         isFinal  (Default: true)
 *
 * @return bool Success
 */
bool
Expat::parse (const char* pBuffer, int length, bool isFinal)
{
  assert(mParser != NULL);

  // Get the length if not specified
  if (length < 0) length = strlen(pBuffer);

  // Invoke the parser
  return XML_Parse(mParser, pBuffer, length, isFinal) != 0;
}


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
bool
Expat::parse (const WCHAR* pBuffer, int length, bool isFinal)
{
  assert(mParser != NULL);
  
  // Get the length if not specified
  if (length < 0) length = wcslen(pBuffer) * 2;
  
  // Invoke the parser
  return XML_Parse(mParser, pBuffer, length, isFinal) != 0;
}
#endif


/**
 * Parse internal buffer
 *
 * @param  int   length
 * @param  bool  isFinal  (Default: true)
 *
 * @return bool Success
 */
bool
Expat::parseBuffer (int length, bool isFinal)
{
  assert(mParser != NULL);
  return XML_ParseBuffer(mParser, length, isFinal) != 0;
}


/**
 * Get the internal buffer
 */
void*
Expat::getBuffer (int length)
{
  assert(mParser != NULL);
  return XML_GetBuffer(mParser, length);
}


/**
 * Enable/Disable the start element handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableStartElementHandler (bool enable)
{
  assert(mParser != NULL);
  XML_SetStartElementHandler(mParser, enable ? startElementHandler : NULL);
}


/**
 * Enable/Disable end element handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableEndElementHandler (bool enable)
{
  assert(mParser != NULL);
  XML_SetEndElementHandler(mParser, enable ? endElementHandler : NULL);
}


/**
 * Enable/Disable the element handlers
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableElementHandler (bool enable)
{
  assert(mParser != NULL);

  enableStartElementHandler(enable);
  enableEndElementHandler  (enable);
}


/**
 * Start element handler
 *
 * @param  const XML_Char*   name
 * @param  const XML_Char**  pAttrs
 */
void
Expat::onStartElement(const XML_Char*, const XML_Char**)
{
  return;
}


/**
 * End element handler
 *
 * @param  const XML_Char*  name
 */
void
Expat::onEndElement (const XML_Char*)
{
  return;
}


/**
 * Enable/Disable the character data handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableCharacterDataHandler (bool enable)
{ 
  assert(mParser != NULL);
  XML_SetCharacterDataHandler(mParser, enable ? characterDataHandler : NULL);
}


/**
 * Character data handler
 *
 * @param  const XML_Char*  data
 * @param  int              length
 */
void
Expat::onCharacterData (const XML_Char* data, int length)
{
  return;
}


/**
 * Enable/Disable the processing instruction handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableProcessingInstructionHandler (bool enable)
{
  assert(mParser != NULL);

  XML_SetProcessingInstructionHandler
    (mParser, enable ? processingInstructionHandler : NULL);
}


/**
 * Processing instruction handler
 *
 * @param  const XML_Char*  target
 * @param  const XML_Char*  data
 */
void
Expat::onProcessingInstruction (const XML_Char*, const XML_Char*)
{
  return;
}


/**
 * Enable/Disable the comment handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableCommentHandler (bool enable)
{
  assert(mParser != NULL);
  XML_SetCommentHandler(mParser, enable ? commentHandler : NULL);
}


/**
 * Comment handler
 * @param  const XML_Char*  data
 */
void
Expat::onComment (const XML_Char *)
{
  return;
}


/**
 * Enable/Disable the start CDATA section handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableStartCdataSectionHandler (bool enable)
{
  assert(mParser != NULL);
  XML_SetStartCdataSectionHandler(mParser,
                                  enable ? startCdataSectionHandler : NULL);
}


/**
 * Enable/Disable the end CDATA section handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableEndCdataSectionHandler (bool enable)
{
  assert(mParser != NULL);
  XML_SetEndCdataSectionHandler(mParser,
                                enable ? endCdataSectionHandler : NULL);
}


/**
 * Enable/Disable the CDATA section handlers
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableCdataSectionHandler (bool enable)
{
  assert(mParser != NULL);

  enableStartCdataSectionHandler(enable);
  enableEndCdataSectionHandler  (enable);
}


/**
 * Start CDATA section handler
 */
void
Expat::onStartCdataSection ()
{
  return;
}


/**
 * End CDATA section handler
 */
void
Expat::onEndCdataSection ()
{
  return;
}


/**
 * Enable/Disable default handler
 *
 * @param  bool  enable  (Default: true)
 * @param  bool  expand  (Default: true)
 */
void
Expat::enableDefaultHandler (bool enable, bool expand)
{
  assert(mParser != NULL);

  if (expand)
  {
    XML_SetDefaultHandlerExpand(mParser, enable ? defaultHandler : NULL);
  }
  else
  {
    XML_SetDefaultHandler(mParser, enable ? defaultHandler : NULL);
  }
}


/**
 * Default handler
 *
 * @param  const XML_Char*  data
 * @param  int              length
 */
void
Expat::onDefault (const XML_Char*, int)
{
  return;
}


/**
 * Enable/Disable external entity ref handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableExternalEntityRefHandler (bool enable)
{
  assert(mParser != NULL);

  /* :TODO: This is currently broken */
  XML_SetExternalEntityRefHandler(mParser,
                                  enable ? externalEntityRefHandler : NULL);
}


/**
 * External entity ref handler
 *
 * @param  const XML_Char*  context
 * @param  const XML_Char*  base
 * @param  const XML_Char*  systemID
 * @param  const XML_Char*  publicID
 */
bool
Expat::onExternalEntityRef (const XML_Char*, const XML_Char*,
                            const XML_Char*, const XML_Char*)
{
  return false;
}


/**
 * Enable/Disable unknown encoding handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableUnknownEncodingHandler (bool enable)
{
  assert(mParser != NULL);
  XML_SetUnknownEncodingHandler(mParser,
                                enable ? unknownEncodingHandler : NULL,
                                NULL);
}


/**
 * Unknown encoding handler
 *
 * @param  const XML_Char*  name
 * @param  XML_Encoding*    pInfo
 */
bool
Expat::onUnknownEncoding (const XML_Char*, XML_Encoding*)
{
  return false;
}


/**
 * Enable/Disable start namespace handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableStartNamespaceDeclHandler (bool enable)
{
  assert(mParser != NULL);
  XML_SetStartNamespaceDeclHandler(mParser,
                                   enable ? startNamespaceDeclHandler : NULL);
}

/**
 * Enable/Disable end namespace handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableEndNamespaceDeclHandler (bool enable)
{
  assert(mParser != NULL);
  XML_SetEndNamespaceDeclHandler(mParser,
                                 enable ? endNamespaceDeclHandler : NULL);
}


/**
 * Enable/Disable namespace handlers
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableNamespaceDeclHandler (bool enable)
{
  enableStartNamespaceDeclHandler(enable);
  enableEndNamespaceDeclHandler  (enable);
}


/**
 * Start namespace declaration handler
 *
 * @param  const XML_Char*  prefix
 * @param  const XML_Char*  URI
 */
void
Expat::onStartNamespaceDecl (const XML_Char*, const XML_Char*)
{
  return;
}


/**
 * End namespace declaration handler
 * @param  const XML_Char*  prefix
 */
void
Expat::onEndNamespaceDecl (const XML_Char*)
{
  return;
}


/**
 * Enable/Disable the XML declaration handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableXmlDeclHandler (bool enable)
{
  assert(mParser != NULL);
  XML_SetXmlDeclHandler(mParser, enable ? xmlDeclHandler : NULL);
}


/**
 * XML declaration handler
 *
 * @param  const XML_Char*  version
 * @param  const XML_Char*  encoding
 * @param  bool             standalone
 */
void
Expat::onXmlDecl (const XML_Char*, const XML_Char*, bool)
{
  return;
}


/**
 * Enable/Disable the start DOCTYPE declaration handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableStartDoctypeDeclHandler (bool enable)
{
  assert(mParser != NULL);
  XML_SetStartDoctypeDeclHandler(mParser,
                                 enable ? startDoctypeDeclHandler : NULL);
}


/**
 * Enable/Disable the end DOCTYPE declaration handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableEndDoctypeDeclHandler (bool enable)
{
  assert(mParser != NULL);
  XML_SetEndDoctypeDeclHandler(mParser,
                               enable ? endDoctypeDeclHandler : NULL);
}


/**
 * Enable/Disable the DOCTYPE declaration handler
 * @param  bool  enable  (Default: true)
 */
void
Expat::enableDoctypeDeclHandler (bool enable)
{
  assert(mParser != NULL);

  enableStartDoctypeDeclHandler(enable);
  enableEndDoctypeDeclHandler  (enable);
}


/**
 * Start DOCTYPE declaration handler
 *
 * @param  const XML_Char* doctypeName
 * @param  const XML_Char* sysID
 * @param  const XML_Char* pubID
 * @param  bool            hasInternalSubset
 */
void
Expat::onStartDoctypeDecl(const XML_Char*, const XML_Char*,
                          const XML_Char*, bool)
{
  return;
}


/**
 * End DOCTYPE declaration handler
 */
void
Expat::onEndDoctypeDecl ()
{
  return;
}


/**
 * @return the last error code
 */
enum XML_Error
Expat::getErrorCode ()
{
  assert(mParser != NULL);
  return XML_GetErrorCode(mParser);
}


/** 
 * @return the error string for the given error code.
 *
 * @param  enum XML_Error  nError
 */
const XML_LChar*
Expat::getErrorString (enum XML_Error nError)
{
  return XML_ErrorString(nError);
}


/**
 * @return the last error string
 */
const XML_LChar*
Expat::getErrorString ()
{
  return XML_ErrorString( getErrorCode() );
}


/**
 * @return the current byte index
 */
long
Expat::getCurrentByteIndex ()
{
  assert(mParser != NULL);
  return XML_GetCurrentByteIndex(mParser);
}


/**
 * @return the current line number
 */
int
Expat::getCurrentLineNumber () const
{
  assert(mParser != NULL);
  return XML_GetCurrentLineNumber(mParser);
}


/**
 * @return the current column number
 */
int
Expat::getCurrentColumnNumber ()
{
  assert(mParser != NULL);
  return XML_GetCurrentColumnNumber(mParser);
}


/**
 * @return the current byte count
 */
int
Expat::getCurrentByteCount ()
{
  assert(mParser != NULL);
  return XML_GetCurrentByteCount(mParser);
}


/**
 * @return the input context
 */
const char*
Expat::getInputContext (int* pnOffset, int* pnSize)
{
  assert(mParser != NULL);
  return XML_GetInputContext(mParser, pnOffset, pnSize);
}


/**
 * @return the version string
 */
const XML_LChar*
Expat::getExpatVersion ()
{
  return XML_ExpatVersion();
}


/**
 * Get the version information
 *
 * @param  int  major
 * @param  int  minor
 * @param  int  micro
 */
void
Expat::getExpatVersion (int& nMajor, int& nMinor, int& nMicro)
{
  XML_Expat_Version v = XML_ExpatVersionInfo();


  nMajor = v.major;
  nMinor = v.minor;
  nMicro = v.micro;
}


/**
 * Start element handler wrapper
 *
 * @param  void*            pUserData
 * @param  const XML_Char*  name
 * @param  const XML_Char** pAttrs
 */
void
Expat::startElementHandler (void*            pUserData,
                            const XML_Char*  name,
                            const XML_Char** pAttrs)
{
  ((Expat *) pUserData)->onStartElement(name, pAttrs);
}


/**
 * End element handler wrapper
 *
 * @param  void*            pUserData
 * @param  const XML_Char*  name
 */
void
Expat::endElementHandler (void* pUserData, const XML_Char* name)
{
  ((Expat *) pUserData)->onEndElement(name);
}


/**
 * Character data handler wrapper
 *
 * @param  void*            pUserData
 * @param  const XML_Char*  data
 * @param  int              length
 */
void
Expat::characterDataHandler (void* pUserData, const XML_Char* data, int length)
{
  ((Expat *) pUserData)->onCharacterData(data, length);
}


/**
 * Processing instruction handler wrapper
 *
 * @param  void*            pUserData
 * @param  const XML_Char*  target
 * @param  const XML_Char*  data
 */
void
Expat::processingInstructionHandler (void*           pUserData,
                                     const XML_Char* target,
                                     const XML_Char* data)
{
  ((Expat *) pUserData)->onProcessingInstruction(target, data);
}


/**
 * Comment handler wrapper
 *
 * @param  void*            pUserData
 * @param  const XML_Char*  data
 */
void
Expat::commentHandler (void* pUserData, const XML_Char* data)
{
  ((Expat *) pUserData)->onComment(data);
}


/**
 * Start CDATA section wrapper
 *
 * @param  void*  pUserData
 */
void
Expat::startCdataSectionHandler (void* pUserData)
{
  ((Expat *) pUserData)->onStartCdataSection();
}


/**
 * End CDATA section wrapper
 *
 * @param  void*  pUserData
 */
void
Expat::endCdataSectionHandler (void* pUserData)
{
  ((Expat *) pUserData)->onEndCdataSection();
}


/**
 * Default wrapper
 *
 * @param void*            pUserData
 * @param const XML_Char*  data
 * @param int              length
 */
void
Expat::defaultHandler (void* pUserData, const XML_Char* data, int length)
{
  ((Expat *) pUserData)->onDefault(data, length);
}


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
int
Expat::externalEntityRefHandler (XML_Parser      pParser,
                                 const XML_Char* context,
                                 const XML_Char* base,
                                 const XML_Char* systemID,
                                 const XML_Char* publicID)
{
  /* :TODO: This is broken */
  return 0;
}


/**
 * Unknown encoding wrapper
 *
 * @param  void*            pUserData
 * @param  const XML_Char*  name
 * @param  XML_Encoding*    pInfo
 */
int
Expat::unknownEncodingHandler (void*           pUserData,
                               const XML_Char* name,
                               XML_Encoding*   pInfo)
{
  return ((Expat *) pUserData)->onUnknownEncoding(name, pInfo) ? 1 : 0;
}


/**
 * Start namespace decl wrapper
 *
 * @param  void*            pUserData
 * @param  const XML_Char*  prefix
 * @param  const XML_Char*  URI
 */
void
Expat::startNamespaceDeclHandler (void*           pUserData,
                                  const XML_Char* prefix,
                                  const XML_Char* URI)
{
  ((Expat *) pUserData)->onStartNamespaceDecl(prefix, URI);
}


/**
 * End namespace decl wrapper
 *
 * @param  void*            pUserData
 * @param  const XML_Char*  prefix
 */
void
Expat::endNamespaceDeclHandler (void* pUserData, const XML_Char* prefix)
{
  ((Expat *) pUserData)->onEndNamespaceDecl(prefix);
}


/**
 * XML declaration wrapper
 *
 * @param  void*            pUserData
 * @param  const XML_Char*  version
 * @param  const XML_Char*  encoding
 * @param  int              standalone
 */
void
Expat::xmlDeclHandler (void*           pUserData,
                       const XML_Char* version,
                       const XML_Char* encoding,
                       int             standalone)
{
  ((Expat *) pUserData)->onXmlDecl(version, encoding, standalone != 0);
}


/**
 * Start Doctype declaration wrapper
 *
 * @param  void*           pUserData
 * @param  const XML_Char* doctypeName
 * @param  const XML_Char* sysID
 * @param  const XML_Char* pubID
 * @param  int             hasInternalSubset
 */
void
Expat::startDoctypeDeclHandler (void*           pUserData,
                                const XML_Char* doctypeName,
                                const XML_Char* sysID,
                                const XML_Char* pubID,
                                int             hasInternalSubset)
{
  ((Expat *) pUserData)->onStartDoctypeDecl(doctypeName,
                                            sysID,
                                            pubID,
                                            hasInternalSubset != 0);
}


/**
 * End Doctype declaration wrapper
 *
 * @param  void*  pUserData
 */
void
Expat::endDoctypeDeclHandler (void* pUserData)
{
  ((Expat *) pUserData)->onEndDoctypeDecl();
}
