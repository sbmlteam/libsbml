/**
 * \file    SBMLSchemaInputStream.h
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


#ifndef SBMLSchemaInputStream_h
#define SBMLSchemaInputStream_h

#ifndef USE_EXPAT
#  include <xercesc/util/XMemory.hpp>
#  include <xercesc/util/BinMemInputStream.hpp>
   using namespace xercesc;
#endif  /* !USE_EXPAT */

# include <string.h>

/**
 *  This class is a derivative of the standard xerces c 
 *  input source (InputStream) class. 
 */
class SBMLSchemaInputStream: public BinInputStream
{
public :
  
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------

     SBMLSchemaInputStream (const char * *        srcDocBytes
                          , const unsigned int    byteCount
                          , MemoryManager* const  manager = XMLPlatformUtils::fgMemoryManager);

    ~SBMLSchemaInputStream();


    unsigned int curPos() const;
    
    unsigned int readBytes (XMLByte* const  toFill, const unsigned int  maxToRead);


private :
    const XMLByte*  fBuffer;
    unsigned int    fCapacity;
    unsigned int    fCurIndex;
    MemoryManager*  fMemoryManager;
    const char **   fSchema;

};

#endif
