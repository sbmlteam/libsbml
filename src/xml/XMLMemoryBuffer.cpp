/**
 * @file    XMLMemoryBuffer.cpp
 * @brief   XMLMemoryBuffer implements the XMLBuffer interface for files
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


#include <cstring>
#include <sbml/xml/XMLMemoryBuffer.h>


/**
 * Creates a XMLBuffer based on the given sequence of bytes in buffer.
 * This class does not take ownership of or copy buffer, it merely
 * references it.
 */
XMLMemoryBuffer::XMLMemoryBuffer (const char* buffer, unsigned int length) :
   mBuffer( buffer )
 , mLength( length )
 , mOffset( 0      )
{
}


/**
 * Destroys this XMLMemoryBuffer.
 */
XMLMemoryBuffer::~XMLMemoryBuffer ()
{
}


/**
 * Copies at most nbytes from this XMLMemoryBuffer to the memory pointed
 * to by destination.
 *
 * @return the number of bytes actually copied (may be 0).
 */
unsigned int
XMLMemoryBuffer::copyTo (void* destination, unsigned int bytes)
{
  if (mOffset > mLength) return 0;
  if (mOffset + bytes > mLength) bytes = mLength - mOffset;

  memcpy(destination, mBuffer + mOffset, bytes);
  mOffset += bytes;

  return bytes;
}


/**
 * @return true if there was an error reading from the underlying buffer
 * (i.e. it's null), false otherwise.
 */
bool
XMLMemoryBuffer::error ()
{
  return (mBuffer == 0);
}
