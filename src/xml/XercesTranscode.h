/**
 * \file    XercesTranscode.h
 * \brief   Transcodes a Xerces-C++ XMLCh* string to the local code page.
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


#ifndef XercesTranscode_h
#define XercesTranscode_h


#include <string>
#include <xercesc/util/XMLString.hpp>


/**
 * Transcodes a Xerces-C++ XMLCh* string to the local code page.  This
 * class offers implicit conversion to a C++ string and destroys the
 * transcoded buffer when the XercesTranscode object goes out of scope.
 */
class XercesTranscode
{
public:

  XercesTranscode (const XMLCh* s) :
    mBuffer( xercesc::XMLString::transcode(s) ) { }

  ~XercesTranscode     () { xercesc::XMLString::release(&mBuffer); }
  operator std::string () { return std::string(mBuffer); }


private:

  char* mBuffer;

  XercesTranscode  ();
  XercesTranscode  (const XercesTranscode&);
  XercesTranscode& operator= (const XercesTranscode&);

};


#endif  /* XercesTranscode_h */
