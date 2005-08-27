/**
 * \file    SBMLSchemaInputStream.cpp
 * \brief   Constructs the sbml schema in memory
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Sarah Keating
 *     SBML Team
 *     University of Hertfordshire
 *     Hatfield
 *     UK
 *
 *     http://www.sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "xml/common.h"
#include "SBMLSchemaInputStream.h"


#ifndef USE_EXPAT


SBMLSchemaInputStream::SBMLSchemaInputStream ( const char**         srcDocBytes
                                             , const unsigned int   byteCount
                                             , MemoryManager* const manager) :
   fBuffer(0)
 , fCapacity(byteCount)
 , fCurIndex(0)
 , fMemoryManager(manager)
{
  fSchema = srcDocBytes;
}


SBMLSchemaInputStream::~SBMLSchemaInputStream()
{
}


unsigned int 
SBMLSchemaInputStream ::readBytes(  XMLByte* const     toFill
                                  , const unsigned int maxToRead )
{
  //
  //  Figure out how much we can really read. Its the smaller of the
  //  amount available and the amount asked for.
  //
  const unsigned int available = (fCapacity - fCurIndex);
  if (!available)
    return 0;

  const unsigned int actualToRead =
    available < maxToRead ? available : maxToRead;

  // create a non constant pointer to the XMLByte buffer
  XMLByte * pp = toFill;


  // loop through each element of the const char * []
  // and copy to the buffer as XMLBytes
  int i = 0;
  while (strlen(fSchema[i]) > 0)
  {
    memcpy(pp, (const XMLByte*)&fSchema[i][fCurIndex], strlen(fSchema[i]));
    pp = pp+strlen(fSchema[i]);
    i++;
  }
    
  fCurIndex += actualToRead;
    
  return actualToRead;
}


unsigned int
SBMLSchemaInputStream::curPos () const
{
  return fCurIndex;
}


#endif /* !USE_EXPAT */
