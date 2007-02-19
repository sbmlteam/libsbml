/**
 * \file    XMLFileBuffer.h
 * \brief   XMLFileBuffer implements the XMLBuffer interface for files
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


#ifndef XMLFileBuffer_h
#define XMLFileBuffer_h

#ifdef __cplusplus

#include <string>
#include <cstdio>

#include <sbml/xml/XMLBuffer.h>


class XMLFileBuffer : public XMLBuffer
{
public:

  /**
   * Creates a XMLBuffer based on the given file.  The file will be opened
   * for reading.
   */
  XMLFileBuffer (const std::string& filename);

  /**
   * Destroys this XMLFileBuffer and closes the underlying file.
   */
  virtual ~XMLFileBuffer ();


  /**
   * Copies at most nbytes from this XMLFileBuffer to the memory pointed to
   * by destination.
   *
   * @return the number of bytes actually copied (may be 0).
   */
  virtual unsigned int copyTo (void* destination, unsigned int bytes);

  /**
   * @return true if there was an error reading from the underlying buffer,
   * false otherwise.
   */
  virtual bool error ();


private:

  XMLFileBuffer ();
  XMLFileBuffer (const XMLFileBuffer&);
  XMLFileBuffer& operator= (const XMLFileBuffer&);


  std::string  mFilename;
  FILE*        mStream;
};

#endif  /* __cplusplus */

#endif  /* XMLFileBuffer_h */
