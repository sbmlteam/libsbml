/**
 * @file    XMLMemoryBuffer.h
 * @brief   XMLMemoryBuffer implements the XMLBuffer interface for files
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

#ifndef XMLMemoryBuffer_h
#define XMLMemoryBuffer_h

#include <sbml/xml/XMLBuffer.h>


/** @cond doxygen-libsbml-internal */

class XMLMemoryBuffer : public XMLBuffer
{
public:

  /**
   * Creates a XMLBuffer based on the given sequence of bytes in buffer.
   * This class does not take ownership of or copy buffer, it merely
   * references it.
   */
  XMLMemoryBuffer (const char* buffer, unsigned int length);


  /**
   * Destroys this XMLMemoryBuffer.
   */
  virtual ~XMLMemoryBuffer ();


  /**
   * Copies at most nbytes from this XMLMemoryBuffer to the memory pointed
   * to by destination.
   *
   * @return the number of bytes actually copied (may be 0).
   */
  virtual unsigned int copyTo (void* destination, unsigned int bytes);


  /**
   * @return true if there was an error reading from the underlying buffer
   * (i.e. it's null), false otherwise.
   */
  virtual bool error ();


private:

  XMLMemoryBuffer ();
  XMLMemoryBuffer (const XMLMemoryBuffer&);
  XMLMemoryBuffer& operator= (const XMLMemoryBuffer&);


  const char*   mBuffer;
  unsigned int  mLength;
  unsigned int  mOffset;
};

/** @endcond doxygen-libsbml-internal */

#endif  /* XMLMemoryBuffer_h */
