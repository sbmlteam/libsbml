/**
 * \file    XMLFileBuffer.cpp
 * \brief   XMLFileBuffer implements the XMLBuffer interface
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


#include <cstdio>
#include "XMLFileBuffer.h"


using namespace std;


/**
 * Creates a XMLBuffer based on the given file.  The file will be opened
 * for reading.
 */
XMLFileBuffer::XMLFileBuffer (const string& filename) :
   mFilename( filename )
 , mStream  ( fopen(mFilename.c_str(), "r") )
{
}


/**
 * Destroys this XMLFileBuffer and closes the underlying file.
 */
XMLFileBuffer::~XMLFileBuffer ()
{
  if (mStream) fclose(mStream);
  mStream = 0;
}


/**
 * Copies at most nbytes from this XMLFileBuffer to the memory pointed to
 * by destination.
 *
 * @return the number of bytes actually copied (may be 0).
 */
unsigned int
XMLFileBuffer::copyTo (void* destination, unsigned int bytes)
{
  if (mStream)
  {
    return fread(destination, 1, bytes, mStream);
  }
  else
  {
    return 0;
  }
}


/**
 * @return true if there was an error reading from the underlying buffer,
 * false otherwise.
 */
bool
XMLFileBuffer::error ()
{
  if (mStream) return ferror(mStream);
  else return true;
}
