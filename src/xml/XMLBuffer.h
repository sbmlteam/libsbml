/**
 * \file    XMLBuffer.h
 * \brief   XMLBuffer interface
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


#ifndef XMLBuffer_h
#define XMLBuffer_h


#include "XMLExtern.h"


class LIBLAX_EXTERN XMLBuffer
{
public:

  /**
   * Destroys this XMLBuffer.
   */
  virtual ~XMLBuffer ();

  /**
   * Copies at most nbytes from this XMLFileBuffer to the memory pointed to
   * by destination.
   *
   * @return the number of bytes actually copied (may be 0).
   */
  virtual unsigned int copyTo (void* destination, unsigned int bytes) = 0;

  /**
   * @return true if there was an error reading from the underlying buffer,
   * false otherwise.
   */
  virtual bool error () = 0;


protected:

  XMLBuffer ();
  XMLBuffer (const XMLBuffer&);
  XMLBuffer& operator= (const XMLBuffer&);
};


#endif  /* XMLBuffer_h */
