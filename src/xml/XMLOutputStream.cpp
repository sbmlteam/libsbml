/**
 * @file    XMLOutputStream.cpp
 * @brief   XMLOutputStream
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <iostream>
#include <sstream>
#include <fstream>

#include <cstdio>
#include <sbml/xml/XMLTriple.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/util/util.h>
#include <sbml/common/common.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/*
 * Creates a new XMLOutputStream that wraps stream.
 */
XMLOutputStream::XMLOutputStream (  std::ostream&       stream
                                  , const std::string&  encoding
                                  , bool                writeXMLDecl ) :
   mStream  ( stream   )
 , mEncoding( encoding )
 , mInStart ( false    )
 , mDoIndent( true     )
 , mIndent  ( 0        )
 , mInText  ( false    )
{
  unsetStringStream();
  mStream.imbue( locale::classic() );
  if (writeXMLDecl) this->writeXMLDecl();
}


/*
 * Writes the given XML end element name to this XMLOutputStream.
 */
void
XMLOutputStream::endElement (const std::string& name)
{
  if (mInStart)
  {
    mInStart = false;
    mStream << '/' << '>';
  }
  else if (mInText)
  {
    mInText = false;
    mStream << '<' << '/';
    writeName(name);
    mStream << '>';
  }
  else
  {
    downIndent();
    writeIndent(true); 

    mStream << '<' << '/';
    writeName(name);
    mStream << '>';
  }
}


/*
 * Writes the given XML end element 'prefix:name' to this
 * XMLOutputStream.
 */
void
XMLOutputStream::endElement (const XMLTriple& triple)
{
  if (mInStart)
  {
    mInStart = false;
    mStream << '/' << '>';
  }
  else if (mInText)
  {
    mInText = false;
    mStream << '<' << '/';
    writeName(triple);
    mStream << '>';
  }
  else
  {
    downIndent();
    writeIndent(true); 

    mStream << '<' << '/';
    writeName(triple);
    mStream << '>';
  }
}


/*
 * Turns automatic indentation on or off for this XMLOutputStream.
 */
void
XMLOutputStream::setAutoIndent (bool indent)
{
  mDoIndent = indent;
}


/*
 * Writes the given XML start element name to this XMLOutputStream.
 */
void
XMLOutputStream::startElement (const std::string& name)
{
  if (mInStart)
  {
    mStream << '>';
    upIndent();
  }

  mInStart = true;

  writeIndent();

  mStream << '<';
  writeName(name);
}


/*
 * Writes the given XML start element 'prefix:name' to this
 * XMLOutputStream.
 */
void
XMLOutputStream::startElement (const XMLTriple& triple)
{
  if (mInStart)
  {
    mStream << '>';
    upIndent();
  }

  mInStart = true;

  writeIndent();

  mStream << '<';
  writeName(triple);
}


/*
 * Writes the given XML start and end element name to this XMLOutputStream.
 */
void
XMLOutputStream::startEndElement (const std::string& name)
{
  if (mInStart)
  {
    mStream << '>';
    upIndent();
  }

  mInStart = false;

  writeIndent();

  mStream << '<';
  writeName(name);
  mStream << '/' << '>';
}


/*
 * Writes the given XML start and end element 'prefix:name' to this
 * XMLOutputStream.
 */
void
XMLOutputStream::startEndElement (const XMLTriple& triple)
{
  if (mInStart)
  {
    mStream << '>';
    upIndent();
  }

  mInStart = false;

  writeIndent();

  mStream << '<';
  writeName(triple);
  mStream << '/' << '>';
}


/*
 * Writes the given attribute, name="value" to this XMLOutputStream.
 */
void
XMLOutputStream::writeAttribute (const std::string& name, const std::string& value)
{
  if ( value.empty() ) return;

  mStream << ' ';

  writeName ( name  );
  writeValue( value );
}


/*
 * Writes the given attribute, prefix:name="value" to this
 * XMLOutputStream.
 */
void
XMLOutputStream::writeAttribute (const XMLTriple& triple, const std::string& value)
{
  mStream << ' ';

  writeName ( triple );
  writeValue( value  );
}


/*
 * Writes the given attribute, name="true" or name="false" to this
 * XMLOutputStream.
 */
void
XMLOutputStream::writeAttribute (const std::string& name, const bool& value)
{
  mStream << ' ';

  writeName ( name  );
  writeValue( value );
}


/*
 * Writes the given attribute, prefix:name="true" or prefix:name="false" to
 * this XMLOutputStream.
 */
void
XMLOutputStream::writeAttribute (const XMLTriple& triple, const bool& value)
{
  mStream << ' ';

  writeName ( triple );
  writeValue( value  );
}


/*
 * Writes the given attribute, name="value" to this XMLOutputStream.
 */
void
XMLOutputStream::writeAttribute (const std::string& name, const double& value)
{
  mStream << ' ';

  writeName ( name  );
  writeValue( value );
}


/*
 * Writes the given attribute, prefix:name="value" to this XMLOutputStream.
 */
void
XMLOutputStream::writeAttribute (const XMLTriple& triple, const double& value)
{
  mStream << ' ';

  writeName ( triple );
  writeValue( value  );
}


/*
 * Writes the given attribute, name="value" to this XMLOutputStream.
 */
void
XMLOutputStream::writeAttribute (const std::string& name, const long& value)
{
  mStream << ' ';

  writeName ( name  );
  writeValue( value );
}


/*
 * Writes the given attribute, prefix:name="value" to this XMLOutputStream.
 */
void
XMLOutputStream::writeAttribute (const XMLTriple& triple, const long& value)
{
  mStream << ' ';

  writeName ( triple );
  writeValue( value  );
}


/*
 * Writes the given attribute, name="value" to this XMLOutputStream.
 */
void
XMLOutputStream::writeAttribute (const std::string& name, const int& value)
{
  mStream << ' ';

  writeName ( name  );
  writeValue( value );
}


/*
 * Writes the given attribute, prefix:name="value" to this
 * XMLOutputStream.
 */
void
XMLOutputStream::writeAttribute (const XMLTriple& triple, const int& value)
{
  mStream << ' ';

  writeName ( triple );
  writeValue( value  );
}


/*
 * Writes the given attribute, name="value" to this XMLOutputStream.
 */
void
XMLOutputStream::writeAttribute (const std::string& name, const unsigned int& value)
{
  mStream << ' ';

  writeName ( name  );
  writeValue( value );
}


/*
 * Writes the given attribute, prefix:name="value" to this
 * XMLOutputStream.
 */
void
XMLOutputStream::writeAttribute (  const XMLTriple&     triple
                                 , const unsigned int&  value )
{
  mStream << ' ';

  writeName ( triple );
  writeValue( value  );
}


/*
 * Decreases the indentation level for this XMLOutputStream.
 */
void
XMLOutputStream::downIndent ()
{
  if (mDoIndent && mIndent) --mIndent;
}


/*
 * Increases the indentation level for this XMLOutputStream.
 */
void
XMLOutputStream::upIndent ()
{
  if (mDoIndent) ++mIndent;
}


/** @cond doxygen-libsbml-internal */
/*
 * Outputs indentation whitespace.
 */
void
XMLOutputStream::writeIndent (bool isEnd)
{
  if (mDoIndent)
  {
    if (mIndent > 0 || isEnd) mStream << endl;
    for (unsigned int n = 0; n < mIndent; ++n) mStream << ' ' << ' ';
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Outputs the given characters to the underlying stream.
 */
void
XMLOutputStream::writeChars (const std::string& chars)
{
  for (string::const_iterator c = chars.begin(); c != chars.end(); ++c)
  {
    *this << *c;
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Outputs name.
 */
void
XMLOutputStream::writeName (const std::string& name)
{
  writeChars(name);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Outputs prefix:name.
 */
void
XMLOutputStream::writeName (const XMLTriple& triple)
{
  if ( !triple.getPrefix().empty() )
  {
    writeChars( triple.getPrefix() );
    mStream << ':';
  }

  writeChars( triple.getName() );
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Outputs value in quotes.
 */
void
XMLOutputStream::writeValue (const std::string& value)
{
  mStream << '=' << '"';
  writeChars(value);
  mStream << '"';
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Outputs "true" or "false" in quotes.
 */
void
XMLOutputStream::writeValue (const bool& value)
{
  mStream << '=' << '"' << (value ? "true" : "false") << '"';
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Outputs the double value in quotes, or "INF", "-INF", or "NaN".
 */
void
XMLOutputStream::writeValue (const double& value)
{
  mStream << '=' << '"';

  if (value != value)
  {
    mStream << "NaN";
  }
  else if (value == numeric_limits<double>::infinity())
  {
    mStream << "INF";
  }
  else if (value == - numeric_limits<double>::infinity())
  {
    mStream << "-INF";
  }
  else
  {
    mStream.precision(LIBSBML_DOUBLE_PRECISION);
    mStream <<   value;
  }

  mStream << '"';
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Outputs the long value in quotes.
 */
void
XMLOutputStream::writeValue (const long& value)
{
  mStream << '=' << '"' << value << '"';
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Outputs the int value in quotes.
 */
void
XMLOutputStream::writeValue (const int& value)
{
  mStream << '=' << '"' << value << '"';
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Outputs the int value in quotes.
 */
void
XMLOutputStream::writeValue (const unsigned int& value)
{
  mStream << '=' << '"' << value << '"';
}
/** @endcond doxygen-libsbml-internal */


/*
 * Writes the XML declaration:
 * <?xml version="1.0" encoding="..."?>
 */
void
XMLOutputStream::writeXMLDecl ()
{
  mStream << "<?xml version=\"1.0\"";

  if ( !mEncoding.empty() ) writeAttribute("encoding", mEncoding);

  mStream << "?>";
  mStream << endl;
}


/*
 * Outputs the given characters to the underlying stream.
 */
XMLOutputStream&
XMLOutputStream::operator<< (const std::string& chars)
{
  if (mInStart)
  {
    mInStart = false;
    mStream << '>';
  }

  writeChars(chars);
  mInText = true;

  return *this;
}


/*
 * Outputs the given double to the underlying stream.
 */
XMLOutputStream&
XMLOutputStream::operator<< (const double& value)
{
  if (mInStart)
  {
    mInStart = false;
    mStream << '>';
  }

  mStream << value;

  return *this;
}


/*
 * Outputs the given long to the underlying stream.
 */
XMLOutputStream&
XMLOutputStream::operator<< (const long& value)
{
  if (mInStart)
  {
    mInStart = false;
    mStream << '>';
  }

  mStream << value;

  return *this;
}
/** @cond doxygen-libsbml-internal */

XMLOutputStringStream::XMLOutputStringStream (  std::ostringstream& stream
                   , const std::string&  encoding
                   , bool                writeXMLDecl):
  XMLOutputStream(stream, encoding, writeXMLDecl)
    , mString(stream)

{
  setStringStream();
}

XMLOutputFileStream::XMLOutputFileStream (  std::ofstream& stream
                   , const std::string&  encoding
                   , bool                writeXMLDecl):
  XMLOutputStream(stream, encoding, writeXMLDecl)

{
}

/** @endcond doxygen-libsbml-internal */

/** @cond doxygen-c-only */
/**
 * Creates a new XMLOutputStream_t that wraps std output stream.
 **/
LIBLAX_EXTERN
XMLOutputStream_t *
XMLOutputStream_createAsStdout (char * encoding, int writeXMLDecl)
{
  return new(nothrow) XMLOutputStream(std::cout, encoding, writeXMLDecl);
}


/**
 * Creates a new XMLOutputStream_t that wraps std string output stream.
 **/
LIBLAX_EXTERN
XMLOutputStream_t *
XMLOutputStream_createAsString (char * encoding, int writeXMLDecl)
{
  std::ostringstream *out = new std::ostringstream();

  return new(nothrow) XMLOutputStringStream(*out, encoding, writeXMLDecl);
}


/**
 * Creates a new XMLOutputStream_t that wraps std file output stream.
 **/
LIBLAX_EXTERN
XMLOutputStream_t *
XMLOutputStream_createFile (char * filename, char * encoding, int writeXMLDecl)
{
  std::ofstream *fout = new std::ofstream(filename, std::ios::out);
  return new(nothrow) XMLOutputFileStream(*fout, encoding, writeXMLDecl);
}


/**
 * Deletes this XMLOutputStream_t. 
 **/
LIBLAX_EXTERN
void
XMLOutputStream_free (XMLOutputStream_t *stream)
{
  delete static_cast<XMLOutputStream*>(stream);
}  


/**
 * Writes the XML declaration:
 * <?xml version="1.0" encoding="..."?>
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeXMLDecl (XMLOutputStream_t *stream)
{
  stream->writeXMLDecl();
}


/**
 * Increases the indentation level for this XMLOutputStream.
 */
LIBLAX_EXTERN
void
XMLOutputStream_upIndent(XMLOutputStream_t *stream)
{
  stream->upIndent();
}


/**
 * Decreases the indentation level for this XMLOutputStream.
 */
LIBLAX_EXTERN
void
XMLOutputStream_downIndent(XMLOutputStream_t *stream)
{
  stream->downIndent();
}


/**
  * Writes the given XML end element name to this XMLOutputStream.
  */
LIBLAX_EXTERN
void 
XMLOutputStream_endElement (XMLOutputStream_t *stream, const char* name)
{
  stream->endElement(name);
}


/**
 * Writes the given XML end element 'prefix:name' to this
 * XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_endElementTriple (XMLOutputStream_t *stream, 
                                  const XMLTriple_t *triple)
{
  stream->endElement(*triple);
}


/**
 * Turns automatic indentation on or off for this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_setAutoIndent (XMLOutputStream_t *stream, int indent)
{
  stream->setAutoIndent(static_cast<bool>(indent));
}


/**
 * Writes the given XML start element name to this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_startElement (XMLOutputStream_t *stream, const char* name)
{
  stream->startElement(name);
}


/**
 * Writes the given XML start element 'prefix:name' to this
 * XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_startElementTriple (XMLOutputStream_t *stream, 
                                    const XMLTriple_t *triple)
{
  stream->startElement(*triple);
}


/**
 * Writes the given XML start and end element name to this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_startEndElement (XMLOutputStream_t *stream, const char* name)
{
  stream->startEndElement(name);
}


/**
 * Writes the given XML start and end element 'prefix:name' to this
 * XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_startEndElementTriple (XMLOutputStream_t *stream, 
                                       const XMLTriple_t *triple)
{
  stream->startEndElement(*triple);
}


/**
 * Writes the given attribute, name="value" to this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeAttributeChars (XMLOutputStream_t *stream, 
                                     const char* name, const char* chars)
{
  stream->writeAttribute(name, string(chars));
}


/**
 * Writes the given attribute, prefix:name="value" to this
 * XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeAttributeCharsTriple (XMLOutputStream_t *stream, 
                                           const XMLTriple_t *triple,
                                           const char* chars)
{
  stream->writeAttribute(*triple, string(chars));
}


/**
 * Writes the given attribute, name="true" or name="false" to this
 * XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeAttributeBool (XMLOutputStream_t *stream, 
                                    const char* name,
                                    const int flag)
{
  stream->writeAttribute(name, static_cast<bool>(flag));
}


/**
 * Writes the given attribute, prefix:name="true" or prefix:name="false"
 * to this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeAttributeBoolTriple (XMLOutputStream_t *stream, 
                                          const XMLTriple_t *triple,
                                          const int flag)
{
  stream->writeAttribute(*triple, static_cast<bool>(flag));
}


/**
 * Writes the given attribute, name="value" to this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeAttributeDouble (XMLOutputStream_t *stream, 
                                      const char* name,
                                      const double value)
{
  stream->writeAttribute(name, value);
}


/**
 * Writes the given attribute, prefix:name="value" to this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeAttributeDoubleTriple (XMLOutputStream_t *stream, 
                                            const XMLTriple_t *triple,
                                            const double value)
{
  stream->writeAttribute(*triple, value);
}


/**
 * Writes the given attribute, name="value" to this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeAttributeLong (XMLOutputStream_t *stream, 
                                    const char* name,
                                    const long value)
{
  stream->writeAttribute(name, value);
}


/**
 * Writes the given attribute, prefix:name="value" to this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeAttributeLongTriple (XMLOutputStream_t *stream, 
                                          const XMLTriple_t *triple,
                                          const long value)
{
  stream->writeAttribute(*triple, value);
}


/**
 * Writes the given attribute, name="value" to this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeAttributeInt (XMLOutputStream_t *stream, 
                                   const char* name,
                                   const int value)
{
  stream->writeAttribute(name, value);
}


/**
 * Writes the given attribute, prefix:name="value" to this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeAttributeIntTriple (XMLOutputStream_t *stream, 
                                         const XMLTriple_t *triple,
                                         const int value)
{
  stream->writeAttribute(*triple, value);
}


/**
 * Writes the given attribute, name="value" to this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeAttributeUInt (XMLOutputStream_t *stream, 
                                    const char* name,
                                    const unsigned int value)
{
  stream->writeAttribute(name, value);
}


/**
 * Writes the given attribute, prefix:name="value" to this XMLOutputStream.
 */
LIBLAX_EXTERN
void 
XMLOutputStream_writeAttributeUIntTriple (XMLOutputStream_t *stream, 
                                          const XMLTriple_t *triple,
                                          const unsigned int value)
{
  stream->writeAttribute(*triple, value);
}


/**
 * Outputs the given characters to the underlying stream.
 */
LIBLAX_EXTERN
void
XMLOutputStream_writeChars (XMLOutputStream_t *stream, const char* chars)
{
  stream->operator <<(chars);
}


/**
 * Outputs the given double to the underlying stream.
 */
LIBLAX_EXTERN
void
XMLOutputStream_writeDouble (XMLOutputStream_t *stream, const double value)
{
  stream->operator <<(value);
}


/**
 * Outputs the given long to the underlying stream.
 */
LIBLAX_EXTERN
void
XMLOutputStream_writeLong (XMLOutputStream_t *stream, const long value)
{
  stream->operator <<(value);
}


/**
 * Returns the given string associated to the underlying string stream.
 */
LIBLAX_EXTERN
const char *
XMLOutputStream_getString(XMLOutputStream_t* stream)
{
  if (stream->getStringStream())
  {
    std::string buffer = static_cast <XMLOutputStringStream*>
                                                  (stream)->getString().str();
    return safe_strdup(buffer.c_str());
  }
  else
    return "";
}


/** @endcond doxygen-c-only */

