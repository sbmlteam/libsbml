/**
 * @file    XMLFileBuffer.h
 * @brief   XMLFileBuffer implements the XMLBuffer interface for files
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

#ifndef XMLFileBuffer_h
#define XMLFileBuffer_h

#ifdef __cplusplus

#include <string>
#include <cstdio>

#include <sbml/xml/XMLBuffer.h>


/** @cond doxygen-libsbml-internal */

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

/** @endcond doxygen-libsbml-internal */

#endif  /* __cplusplus */

#endif  /* XMLFileBuffer_h */
