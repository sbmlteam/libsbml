/**
 * \file    SAX2AttributesMock.h
 * \brief   SAX2 Attributes mock object (to assist unit tests)
 * \author  Ben Bornstein
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
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 *   Stefan Hoops
 */


#ifndef SAX2AttributesMock_h
#define SAX2AttributesMock_h


#include "xml/common.h"


#ifdef __cplusplus


#ifdef USE_EXPAT
#  include "ExpatAttributes.h"
#else
#  include <xercesc/sax2/Attributes.hpp>
#  include <xercesc/util/XMLString.hpp>
   using xercesc::Attributes;
#endif  /* USE_EXPAT */


/**
 * SAX2AttributesMock
 *
 * This is an implementation of a SAX2 Attributes class (adhering to the
 * Attributes inferface).  It is meant primarily for test purposes, i.e.
 * to simulate Attributes that a SAX2 parser might deliver to a
 * DocumentHandler.
 *
 * All methods are implemented, but most simply return 0.  The following
 * methods are fully implemented:
 *
 * <ul>
 *   <li> <code> getLength()    </code>
 *   <li> <code> getLocalName() </code>, and
 *   <li> <code> getValue()     </code>
 * </ul>
 *
 * Also, <code>getQName()</code> simply calls and returns
 * <code>getLocalName()</code> as a simple way of handling SAX2 qnames.
 *
 * Attributes can be added via the add(localname, value) method, whose
 * arguments are char* as opposed to the more annoying XMLCh*.
 */
class SAX2AttributesMock : public Attributes
{
public:

  /* ----------------------------------------------------------------------
   *  Ctors and Dtors
   * ----------------------------------------------------------------------
   */

  /**
   * Creates an implementation of a SAX2 Attributes class (adhering to the
   * Attributes interface) capable of holding at most max Attributes.
   */
  SAX2AttributesMock (unsigned int max);

  virtual ~SAX2AttributesMock (void);


  /* ----------------------------------------------------------------------
   *  Implementation of the Attribute Interface
   * ----------------------------------------------------------------------
   */

  virtual unsigned int getLength() const;

  virtual const XMLCh* getURI       (const unsigned int index) const;
  virtual const XMLCh* getLocalName (const unsigned int index) const;
  virtual const XMLCh* getQName     (const unsigned int index) const;
  virtual const XMLCh* getType      (const unsigned int index) const;
  virtual const XMLCh* getValue     (const unsigned int index) const;

  virtual int getIndex (
                        const XMLCh* const  uri,
                        const XMLCh* const  localPart
                        ) const;

#ifdef USE_EXPAT
  virtual unsigned int getIndex(const XMLCh* const qName ) const;
#else
  virtual int getIndex(const XMLCh* const qName ) const;
#endif  /* USE_EXPAT */

  virtual const XMLCh* getType (
                                const XMLCh* const  uri,
                                const XMLCh* const  localPart
                                ) const;

  virtual const XMLCh* getType(const XMLCh* const qName) const;

  virtual const XMLCh* getValue (
                                 const XMLCh* const  uri,
                                 const XMLCh* const  localPart
                                 ) const;

  virtual const XMLCh* getValue (const XMLCh* const qName) const;


  /* ----------------------------------------------------------------------
   * Other methods
   * ----------------------------------------------------------------------
   */

  /**
   * If getLength() is less-than the maximum number of attributes (supplied
   * during construction), adds an Attribute with the given localname and
   * value to this set of Attributes, otherwise does nothing.
   */
  void add (const char* localname, const char* value);

private:

  XMLCh**       fLocalNames;
  XMLCh**       fValues;
  unsigned int  fMax;
  unsigned int  fLength;
};


#endif  /* __cplusplus */
#endif  /* SAX2AttributesMock_h */
